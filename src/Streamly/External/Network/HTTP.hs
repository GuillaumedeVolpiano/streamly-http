-- |
-- Module      : Streamly.External.Network.HTTP
-- Description : A small HTTP/1.1 client over TCP
-- License     : BSD-3-Clause
--
-- An HTTP/1.1 client over plain TCP, written with Streamly: the response
-- head is read with Streamly parsers and the body is a stream of byte
-- arrays.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Streamly.External.Network.HTTP
-- >
-- > main :: IO ()
-- > main = do
-- >   response <- fetch (get "example.org" 80 "/")
-- >   print (response.status, header "content-type" response)
--
-- Each request opens a connection and closes it afterwards. TLS is not
-- built in: 'withResponseOver' takes a connection opened by other means,
-- such as one of @streamly-network-tls@. No redirect is followed and no
-- content coding is decoded. Each wait is limited (see 'Limits'), but
-- neither the whole time taken nor the size of a response: wrap the calls
-- in 'System.Timeout.timeout' and count the bytes of the stream as needed.
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

    -- * Other transports
  , Transport (..)
  , socketTransport
  , limitTransport
  , withResponseOver
  , fetchOver
  , connect
  , connectWithin

    -- * Errors
  , HttpError (..)
  ) where

import Control.Exception (IOException, bracketOnError, finally, mask, throwIO, try)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Char (toLower)
import Data.Word (Word8)
import Network.Socket qualified as Net
import Streamly.Data.Array (Array)
import Streamly.Data.Array qualified as Array
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream qualified as Stream
import Streamly.Data.StreamK qualified as StreamK
import Streamly.External.Network.HTTP.Wire
import Streamly.Network.Socket qualified as Socket
import System.Timeout (timeout)

-- | A @GET@ request for a target on a host and a port, without headers.
-- 'withHeaders' and 'withBody' change the fields that 'Request' shares
-- with 'Response'.
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

-- | How long to wait, in microseconds. 'Nothing' waits without end.
data Limits = Limits
  { connectTimeout :: Maybe Int
  -- ^ For the connection to open.
  , idleTimeout :: Maybe Int
  -- ^ For the server to send its next bytes, whether the response has
  -- started or not, and to take the next bytes of the request. It bounds
  -- each wait, not the whole exchange: a response that keeps coming can
  -- take any time. Bound the whole with 'System.Timeout.timeout'.
  }
  deriving stock (Eq, Show)

-- | Thirty seconds to connect, thirty seconds of silence.
defaultLimits :: Limits
defaultLimits = Limits {connectTimeout = Just 30_000_000, idleTimeout = Just 30_000_000}

-- | No limit at all.
noLimits :: Limits
noLimits = Limits {connectTimeout = Nothing, idleTimeout = Nothing}

-- | Like 'withResponse', within other limits than 'defaultLimits'.
-- Throws 'ConnectTimedOut' and 'ResponseTimedOut' when they are exceeded.
withResponseWithin :: Limits -> Request -> (Response (Stream IO (Array Word8)) -> IO a) -> IO a
withResponseWithin limits request use = do
  either throwIO pure (validateRequest request)
  -- The connection is opened unmasked, so that its time limit can stop
  -- it; connectWithin closes what it opened when it is stopped.
  mask $ \restore -> do
    socket <- restore (connectWithin limits.connectTimeout request.host request.port)
    restore (withResponseOver (limitTransport limits.idleTimeout (socketTransport socket)) request use) `finally` Net.close socket

-- | Like 'fetch', within other limits than 'defaultLimits'.
fetchWithin :: Limits -> Request -> IO (Response (Array Word8))
fetchWithin limits request = withResponseWithin limits request collect

-- | A connection that is already open, as the two things an exchange
-- needs. It lets a request go over something other than plain TCP: a TLS
-- connection, for HTTPS.
data Transport = Transport
  { send :: Array Word8 -> IO ()
  -- ^ Send bytes.
  , receive :: Stream IO (Array Word8)
  -- ^ The bytes received, until the other side ends the connection.
  }

-- | A TCP socket as a transport.
socketTransport :: Net.Socket -> Transport
socketTransport socket = Transport {send = Socket.putChunk socket, receive = Socket.readChunks socket}

-- | The same transport, with a limit in microseconds on each wait: for
-- the next bytes received, and for bytes to be sent. Exceeding it throws
-- 'ResponseTimedOut'. 'Nothing' changes nothing.
limitTransport :: Maybe Int -> Transport -> Transport
limitTransport Nothing transport = transport
limitTransport (Just microseconds) transport =
  Transport
    { send = within . transport.send
    , receive = Stream.unfoldrM (within . StreamK.uncons) (StreamK.fromStream transport.receive)
    }
  where
    within :: IO a -> IO a
    within action = timeout microseconds action >>= maybe (throwIO ResponseTimedOut) pure

-- | Like 'withResponse', over a connection that is already open. No time
-- limit is applied: see 'limitTransport'. The host
-- and the port of the request only serve its @Host@ header. The request
-- asks the server to close the connection after the response: one
-- exchange for each transport. Opening and closing it are up to the
-- caller.
withResponseOver :: Transport -> Request -> (Response (Stream IO (Array Word8)) -> IO a) -> IO a
withResponseOver transport request use = do
  either throwIO pure (validateRequest request)
  Stream.fold (Fold.drainMapM transport.send) (renderRequest closing)
  response <- readResponse request.method (StreamK.fromStream transport.receive)
  use response
  where
    closing = withHeaders (filter (\(name, _) -> BS8.map toLower name /= "connection") request.headers <> [("Connection", "close")]) request

-- | Like 'fetch', over a connection that is already open.
fetchOver :: Transport -> Request -> IO (Response (Array Word8))
fetchOver transport request = withResponseOver transport request collect

collect :: Response (Stream IO (Array Word8)) -> IO (Response (Array Word8))
collect response = do
  bytes <- Stream.fold Array.create (Stream.unfoldEach Array.reader response.body)
  pure (bytes <$ response)

-- | Open a TCP connection to a host and a port: to the first address of
-- the host that accepts it. Throws 'IOError'.
connect :: String -> Int -> IO Net.Socket
connect = connectWithin Nothing

-- | Like 'connect', within a number of microseconds for the whole of it,
-- name resolution included. Throws 'ConnectTimedOut' beyond.
connectWithin :: Maybe Int -> String -> Int -> IO Net.Socket
connectWithin limit host port = case limit of
  Nothing -> open
  Just microseconds -> timeout microseconds open >>= maybe (throwIO ConnectTimedOut) pure
  where
    open = do
      addresses <- Net.getAddrInfo (Just hints) (Just host) (Just (show port))
      attempt addresses
    hints = Net.defaultHints {Net.addrSocketType = Net.Stream}
    attempt [] = throwIO (userError ("no address for " <> host))
    attempt (address : others) = do
      outcome <- try (to address)
      case (outcome, others) of
        (Right socket, _) -> pure socket
        (Left (problem :: IOException), []) -> throwIO problem
        (Left _, _) -> attempt others
    to address =
      bracketOnError (Net.openSocket address) Net.close $ \socket -> do
        Net.connect socket (Net.addrAddress address)
        pure socket
