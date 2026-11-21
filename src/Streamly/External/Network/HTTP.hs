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
-- Each request opens a connection and closes it afterwards. There is no
-- TLS, no redirect is followed, no content coding is decoded, and nothing
-- limits the time taken or the size of a response: wrap the calls in
-- 'System.Timeout.timeout' and count the bytes of the stream as needed. A
-- status is never an error: look at 'status'.
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

    -- * Errors
  , HttpError (..)
  ) where

import Control.Exception (IOException, bracket, bracketOnError, throwIO, try)
import Data.ByteString (ByteString)
import Data.Word (Word8)
import Network.Socket qualified as Net
import Streamly.Data.Array (Array)
import Streamly.Data.Array qualified as Array
import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream qualified as Stream
import Streamly.Data.StreamK qualified as StreamK
import Streamly.External.Network.HTTP.Wire
import Streamly.Network.Socket qualified as Socket

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
-- Throws 'HttpError' for a request that cannot be written and for a
-- response that cannot be read, and 'IOError' when the connection fails.
withResponse :: Request -> (Response (Stream IO (Array Word8)) -> IO a) -> IO a
withResponse request use = do
  either throwIO pure (validateRequest request)
  bracket (connect request.host request.port) Net.close $ \socket -> do
    Stream.fold (Socket.writeChunks socket) (renderRequest request)
    response <- readResponse request.method (StreamK.fromStream (Socket.readChunks socket))
    use response

-- | Send a request and read the whole response into memory.
fetch :: Request -> IO (Response (Array Word8))
fetch request = withResponse request $ \response -> do
  bytes <- Stream.fold Array.create (Stream.unfoldEach Array.reader response.body)
  pure (bytes <$ response)

-- | Connect to the first address of the host that accepts the connection.
connect :: String -> Int -> IO Net.Socket
connect host port = do
  addresses <- Net.getAddrInfo (Just hints) (Just host) (Just (show port))
  attempt addresses
  where
    hints = Net.defaultHints {Net.addrSocketType = Net.Stream}
    attempt [] = throwIO (userError ("no address for " <> host))
    attempt (address : others) = do
      outcome <- try (open address)
      case (outcome, others) of
        (Right socket, _) -> pure socket
        (Left (problem :: IOException), []) -> throwIO problem
        (Left _, _) -> attempt others
    open address =
      bracketOnError (Net.openSocket address) Net.close $ \socket -> do
        Net.connect socket (Net.addrAddress address)
        pure socket
