-- |
-- Module      : Streamly.External.Network.HTTP
-- Description : A small HTTP/1.1 client over TCP
-- License     : BSD-3-Clause
--
-- An HTTP/1.1 client over plain TCP, written with Streamly: the response
-- head is read with flatparse parsers and the body is a stream of byte
-- arrays.
--
-- > {-# LANGUAGE OverloadedRecordDot #-}
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Streamly.External.Network.HTTP (Response (..), fetch, get, header)
-- >
-- > main :: IO ()
-- > main = do
-- >   response <- fetch (get "example.org" 80 "/")
-- >   print (response.status, header "content-type" response)
--
-- Each request opens a connection and closes it afterwards, unless it
-- goes through a t'Session', which keeps its connection for the next. TLS is not
-- built in: 'withResponseOver' takes a connection opened by other means,
-- such as one of @streamly-network-tls@. No redirect is followed and no
-- content coding is decoded. Each wait is limited, and so is the head of a
-- response (see t'Limits'); the body is bounded only when asked
-- (@maxBodySize@), and the whole time taken never is: wrap the calls in
-- 'System.Timeout.timeout' as needed.
-- A status is never an error: look at 'status'.
module Streamly.External.Network.HTTP
  ( -- * Requests
    Request (..)
  , RequestBody (..)
  , Header
  , get
  , withHeaders
  , withBody

    -- * Responses
  , Response (..)
  , header
  , withResponse
  , fetch

    -- * Limits
  , Limits (..)
  , defaultLimits
  , noLimits
  , withResponseWithin
  , fetchWithin

    -- * Several requests over one connection
  , Session
  , withSession
  , withSessionOver
  , withResponseIn
  , fetchIn

    -- * Several servers, their connections kept
  , Client
  , withClient
  , withClientOver
  , withResponseFrom
  , fetchFrom

    -- * Other transports
  , Transport (..)
  , socketTransport
  , opened
  , limitTransport
  , withResponseOver
  , fetchOver
  , connect
  , connectWithin

    -- * Errors
  , HttpError (..)
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, putMVar, readMVar,
                                takeMVar)
import Control.Exception (IOException, SomeException, bracket, finally,
                          fromException, mask, mask_, onException, throwIO, try)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8 (map)
import Data.Char (toLower)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map, elems, empty, findWithDefault, insert, insertWith)
import Data.Map.Strict qualified as M (lookup)
import Data.Maybe (isJust)
import Data.Word (Word8)
import Network.Socket (Socket, close)
import Streamly.Data.Array (Array, create, reader)
import Streamly.Data.Array qualified as A (length)
import Streamly.Data.Fold (drainMapM)
import Streamly.Data.Stream (Stream, fold, unfoldEach, unfoldrM)
import Streamly.Data.Stream qualified as S (mapM)
import Streamly.Data.StreamK (StreamK, fromStream, uncons)
import Streamly.External.Network.HTTP.URL (Scheme (..))
import Streamly.External.Network.HTTP.Wire (Header, HttpError (..),
                                            Request (..), RequestBody (..),
                                            Response (..), header,
                                            readResponseReusable,
                                            readResponseWithin, renderRequest,
                                            validateRequest, withBody,
                                            withHeaders)
import Streamly.External.Network.TCP (Transport (..), connect, opened,
                                      socketTransport)
import Streamly.External.Network.TCP qualified as NT (connectWithin)
import System.Timeout (timeout)

-- | A @GET@ request for a target on a host and a port, without headers.
-- 'withHeaders' and 'withBody' change the fields that t'Request' shares
-- with t'Response'.
get :: String -> Int -> ByteString -> Request
get host port target = Request {method = "GET", host, port, target, headers = [], body = NoBody}

-- | Send a request and use the response while the connection is open.
--
-- The body is read from the connection as its stream is consumed: consume
-- it inside the function, and only once. The connection is closed when the
-- function returns, whether the body was read to its end or not.
--
-- The request is sent with @Connection: close@, in place of any
-- @Connection@ header of its own.
--
-- Throws 'HttpError' for a request that cannot be written and for a
-- response that cannot be read, and 'IOError' when the connection fails.
withResponse :: Request -> (Response (Stream IO (Array Word8)) -> IO a) -> IO a
withResponse = withResponseWithin defaultLimits

-- | Send a request and read the whole response into memory.
fetch :: Request -> IO (Response (Array Word8))
fetch = fetchWithin defaultLimits

-- | How long to wait, in microseconds, and how much to accept, in bytes.
-- 'Nothing' sets no limit.
data Limits = Limits
  { connectTimeout :: Maybe Int
  -- ^ For the connection to open.
  , idleTimeout :: Maybe Int
  -- ^ For the server to send its next bytes, whether the response has
  -- started or not, and to take the next bytes of the request. It bounds
  -- each wait, not the whole exchange: a response that keeps coming can
  -- take any time. Bound the whole with 'System.Timeout.timeout'.
  , maxHeadSize :: Maybe Int
  -- ^ For the status line and the headers of the response, interim
  -- responses included. 'HeadTooLarge' beyond.
  , maxBodySize :: Maybe Int
  -- ^ For the body of the response, as received. 'BodyTooLarge' beyond:
  -- at once when the response announces its length, from the stream
  -- otherwise.
  }
  deriving stock (Eq, Show)

-- | Thirty seconds to connect, thirty seconds of silence, a head of at
-- most 64 KiB, and a body of any size: set 'maxBodySize' before reading a
-- whole response into memory from a server that is not trusted.
defaultLimits :: Limits
defaultLimits = Limits {connectTimeout = Just 30_000_000, idleTimeout = Just 30_000_000, maxHeadSize = Just 65536, maxBodySize = Nothing}

-- | No limit at all.
noLimits :: Limits
noLimits = Limits {connectTimeout = Nothing, idleTimeout = Nothing, maxHeadSize = Nothing, maxBodySize = Nothing}

-- | Like 'withResponse', within other limits than 'defaultLimits'.
-- Throws 'ConnectTimedOut', 'ResponseTimedOut', 'HeadTooLarge' and
-- 'BodyTooLarge' when they are exceeded.
withResponseWithin :: Limits -> Request -> (Response (Stream IO (Array Word8)) -> IO a) -> IO a
withResponseWithin limits request use = do
  either throwIO pure (validateRequest request)
  -- The connection is opened unmasked, so that its time limit can stop
  -- it; connectWithin closes what it opened when it is stopped.
  mask $ \restore -> do
    socket <- restore (connectWithin limits.connectTimeout request.host request.port)
    restore (withResponseOver limits (socketTransport socket) request use) `finally` close socket

-- | Like 'fetch', within other limits than 'defaultLimits'.
fetchWithin :: Limits -> Request -> IO (Response (Array Word8))
fetchWithin limits request = withResponseWithin limits request collect

-- | The same transport, with a limit in microseconds on each wait: for
-- the next bytes received, and for bytes to be sent. Exceeding it throws
-- 'ResponseTimedOut'. 'Nothing' changes nothing.
limitTransport :: Maybe Int -> Transport -> Transport
limitTransport Nothing transport = transport
limitTransport (Just microseconds) transport =
  Transport
    { send = within . transport.send
    , receive = unfoldrM (within . uncons) (fromStream transport.receive)
    }
  where
    within :: IO a -> IO a
    within action = timeout microseconds action >>= maybe (throwIO ResponseTimedOut) pure

-- | Like 'withResponseWithin', over a connection that is already open:
-- the limits apply, except the time to connect. The host and the port of
-- the request only serve its @Host@ header. The request asks the server
-- to close the connection after the response: one exchange for each
-- transport. Opening and closing it are up to the caller.
withResponseOver :: Limits -> Transport -> Request -> (Response (Stream IO (Array Word8)) -> IO a) -> IO a
withResponseOver limits transport request use = do
  either throwIO pure (validateRequest request)
  fold (drainMapM limited.send) (renderRequest closing)
  response <- readResponseWithin limits.maxHeadSize limits.maxBodySize request.method (fromStream limited.receive)
  use response
  where
    limited = limitTransport limits.idleTimeout transport
    closing = withHeaders (filter (\(name, _) -> BS8.map toLower name /= "connection") request.headers <> [("Connection", "close")]) request

-- | Like 'fetchWithin', over a connection that is already open.
fetchOver :: Limits -> Transport -> Request -> IO (Response (Array Word8))
fetchOver limits transport request = withResponseOver limits transport request collect

-- | A server, and the connection to it that is kept from one request to
-- the next.
data Session = Session
  { opener :: IO (Transport, IO ())
  , limits :: Limits
  , live :: MVar (Maybe Live)
  -- ^ The connection kept, if any. Held during an exchange: the requests
  -- of a session go one at a time.
  }

-- | An open connection of a session.
data Live = Live
  { transport :: Transport
  , shut :: IO ()
  , input :: StreamK IO (Array Word8)
  -- ^ What is still to read from the connection.
  , received :: IORef Int
  -- ^ How many bytes came from the connection so far.
  }

-- | Use a session with a host and a port, over plain TCP. Its requests
-- reuse one connection when they can, instead of opening one each: see
-- 'withResponseIn'. The connection kept is closed at the end.
withSession :: Limits -> String -> Int -> (Session -> IO a) -> IO a
withSession limits host port = withSessionOver limits (connectWithin limits.connectTimeout host port >>= opened)

-- | Like 'withSession', with the way to open a connection to the server
-- left to an action, which gives a transport and what closes it: for TLS,
-- among others. The time to connect is up to that action; the other
-- limits apply.
withSessionOver :: Limits -> IO (Transport, IO ()) -> (Session -> IO a) -> IO a
withSessionOver limits opener = bracket (newSession limits opener) closeSession

newSession :: Limits -> IO (Transport, IO ()) -> IO Session
newSession limits opener = Session opener limits <$> newMVar Nothing

-- | Close the connection a session keeps. The session can still be used:
-- its next request opens another.
closeSession :: Session -> IO ()
closeSession session = mask_ $ do
  kept <- takeMVar session.live
  mapM_ (quietly . (.shut)) kept
  putMVar session.live Nothing

-- | Connections to any number of servers, kept from one request to the
-- next: a session for each server asked, and more than one when requests
-- to the same server overlap.
data Client = Client
  { connectTo :: Scheme -> String -> Int -> IO (Transport, IO ())
  , limits :: Limits
  , idle :: MVar (Maybe (Map (Scheme, String, Int) [Session]))
  -- ^ The sessions not in use, by server; 'Nothing' once the client is
  -- closed.
  }

-- | How many sessions not in use are kept for one server.
maxIdleSessions :: Int
maxIdleSessions = 4

-- | Use a client over plain TCP. Its requests ('withResponseFrom',
-- 'fetchFrom') reuse the connections it keeps, as those of a session do,
-- for every server they go to. A request over TLS is refused with
-- 'InvalidRequest': see @streamly-http-tls@ for a client that does both.
-- The connections kept are closed at the end.
withClient :: Limits -> (Client -> IO a) -> IO a
withClient limits = withClientOver limits $ \scheme host port -> case scheme of
  Https -> throwIO (InvalidRequest "this client only speaks in clear")
  Http -> connectWithin limits.connectTimeout host port >>= opened

-- | Like 'withClient', with the way to open a connection to a server left
-- to a function of the scheme, the host and the port, which gives a
-- transport and what closes it.
withClientOver :: Limits -> (Scheme -> String -> Int -> IO (Transport, IO ())) -> (Client -> IO a) -> IO a
withClientOver limits connectTo = bracket (Client connectTo limits <$> newMVar (Just empty)) finish
  where
    finish client = mask_ $ do
      kept <- takeMVar client.idle
      putMVar client.idle Nothing
      mapM_ closeSession (concat (maybe [] elems kept))

-- | Send a request to the host and the port it names, in clear or over
-- TLS, and use the response, as 'withResponseIn' does, over a connection
-- the client keeps for that server when one is free.
withResponseFrom :: Client -> Scheme -> Request -> (Response (Stream IO (Array Word8)) -> IO a) -> IO a
withResponseFrom client scheme request use = do
  either throwIO pure (validateRequest request)
  bracket acquire release (\session -> withResponseIn session request use)
  where
    server = (scheme, request.host, request.port)
    acquire = modifyMVar client.idle $ \kept -> case kept >>= M.lookup server of
      Just (session : others) -> pure (insert server others <$> kept, session)
      _ -> (kept,) <$> newSession client.limits (client.connectTo scheme request.host request.port)
    release session = do
      -- A session that has lost its connection is not worth keeping: it
      -- would only stand before one that still has its own.
      connected <- isJust <$> readMVar session.live
      keep <- modifyMVar client.idle $ \kept -> pure $ case kept of
        Just sessions | connected, length (findWithDefault [] server sessions) < maxIdleSessions -> (Just (insertWith (<>) server [session] sessions), True)
        _ -> (kept, False)
      if keep then pure () else closeSession session

-- | Send a request through a client and read the whole response into
-- memory. See 'withResponseFrom'.
fetchFrom :: Client -> Scheme -> Request -> IO (Response (Array Word8))
fetchFrom client scheme request = withResponseFrom client scheme request collect

-- | Send a request in a session and use the response, as 'withResponse'
-- does, over the connection of the session when there is one.
--
-- The connection is kept for the next request when the response allows
-- it: its body was consumed to its end inside the function, its length
-- was announced or it was chunked, and the server did not ask to close.
-- Otherwise it is closed, and the next request opens another.
--
-- A server may close a connection it finds idle. When a request on a
-- connection already used gets no answer at all, it is sent again once on
-- a new connection, unless its body is a stream, which cannot be read
-- twice. A server that had acted on the request before losing the
-- connection then acts twice.
--
-- The host and the port of the request only serve its @Host@ header: the
-- server is that of the session. Requests go one at a time; a second
-- thread waits for the first.
withResponseIn :: Session -> Request -> (Response (Stream IO (Array Word8)) -> IO a) -> IO a
withResponseIn session request use = do
  either throwIO pure (validateRequest request)
  -- The connection in hand, to close if the exchange fails.
  inHand <- newIORef Nothing
  mask $ \restore -> do
    current <- takeMVar session.live
    writeIORef inHand current
    (value, kept) <-
      restore (exchange inHand current) `onException` do
        readIORef inHand >>= mapM_ (quietly . (.shut))
        putMVar session.live Nothing
    putMVar session.live kept
    pure value
  where
    persistent = withHeaders (filter (\(name, _) -> BS8.map toLower name /= "connection") request.headers) request
    replayable = case request.body of
      BodyStream _ -> False
      _ -> True
    open inHand = do
      (transport, shut) <- session.opener
      received <- newIORef 0
      let limited = limitTransport session.limits.idleTimeout transport
          counted = S.mapM (\array -> array <$ modifyIORef' received (+ A.length array)) limited.receive
          connection = Live {transport = limited, shut, input = fromStream counted, received}
      connection <$ writeIORef inHand (Just connection)
    -- Send the request and read the head of the response.
    start connection = do
      fold (drainMapM connection.transport.send) (renderRequest persistent)
      readResponseReusable session.limits.maxHeadSize session.limits.maxBodySize request.method connection.input
    exchange inHand = \case
      Nothing -> open inHand >>= \connection -> start connection >>= complete inHand connection
      Just connection -> do
        before <- readIORef connection.received
        try (start connection) >>= \case
          Right started -> complete inHand connection started
          Left (problem :: SomeException) -> do
            after <- readIORef connection.received
            -- No answer at all on a connection already used: the server
            -- had closed it.
            if replayable && after == before && stale problem
              then do
                quietly connection.shut
                writeIORef inHand Nothing
                fresh <- open inHand
                start fresh >>= complete inHand fresh
              else throwIO problem
    stale problem = case (fromException problem, fromException problem) of
      (Just (_ :: IOException), _) -> True
      (_, Just (MalformedResponse _)) -> True
      _ -> False
    complete inHand connection (response, following) = do
      value <- use response
      following >>= \case
        Just rest -> pure (value, Just connection {input = rest})
        Nothing -> do
          writeIORef inHand Nothing
          quietly connection.shut
          pure (value, Nothing)

-- | Send a request in a session and read the whole response into memory.
-- See 'withResponseIn'.
fetchIn :: Session -> Request -> IO (Response (Array Word8))
fetchIn session request = withResponseIn session request collect

quietly :: IO () -> IO ()
quietly action = void (try @SomeException action)

collect :: Response (Stream IO (Array Word8)) -> IO (Response (Array Word8))
collect response = do
  bytes <- fold create (unfoldEach reader response.body)
  pure (bytes <$ response)

-- | Like 'connect', within a number of microseconds for the whole of it,
-- name resolution included. Throws 'ConnectTimedOut' beyond.
connectWithin :: Maybe Int -> String -> Int -> IO Socket
connectWithin limit host port = NT.connectWithin limit host port >>= maybe (throwIO ConnectTimedOut) pure
