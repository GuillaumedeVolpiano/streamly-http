-- |
--
-- = Simpler API
--
-- The API below is rather low-level. The "Network.HTTP.Simple" module provides
-- a higher-level API with built-in support for things like JSON request and
-- response bodies. For most users, this will be an easier place to start. You
-- can read the tutorial at:
--
-- <https://haskell-lang.org/library/http-client>
--
-- = Lower-level API
--
-- This module contains everything you need to initiate HTTP connections.  If
-- you want a simple interface based on URLs, you can use 'simpleHttp'. If you
-- want raw power, 'http' is the underlying workhorse of this package. Some
-- examples:
--
-- Unlike http-conduit, this module does not re-export Network.Http.Client.
--
-- > -- Just download an HTML document and print it.
-- > import Streamly.Network.HTTP
-- > import qualified Data.ByteString.Lazy as L
-- >
-- > main = simpleHttp "http://www.haskell.org/" >>= L.putStr
--
-- This example uses interleaved IO to write the response body to a file in
-- constant memory space.
--
-- > import Data.Conduit.Binary (sinkFile) -- Exported from the package conduit-extra
-- > import Streamly.Network.HTTP
-- > import Conduit (runConduit, (.|))
-- > import Control.Monad.Trans.Resource (runResourceT)
-- >
-- > main :: IO ()
-- > main = do
-- >      request <- parseRequest "http://google.com/"
-- >      manager <- newManager tlsManagerSettings
-- >      runResourceT $ do
-- >          response <- http request manager
-- >          runConduit $ responseBody response .| sinkFile "google.html"
--
-- The following headers are automatically set by this module, and should not
-- be added to 'requestHeaders':
--
-- * Cookie
--
-- * Content-Length
--
-- * Transfer-Encoding
--
-- Note: In previous versions, the Host header would be set by this module in
-- all cases. Starting from 1.6.1, if a Host header is present in
-- @requestHeaders@, it will be used in place of the header this module would
-- have generated. This can be useful for calling a server which utilizes
-- virtual hosting.
--
-- Use `cookieJar` If you want to supply cookies with your request:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Streamly.Network.HTTP
-- > import Network
-- > import Data.Time.Clock
-- > import Data.Time.Calendar
-- > import qualified Control.Exception as E
-- > import Network.HTTP.Types.Status (statusCode)
-- >
-- > past :: UTCTime
-- > past = UTCTime (ModifiedJulianDay 56200) (secondsToDiffTime 0)
-- >
-- > future :: UTCTime
-- > future = UTCTime (ModifiedJulianDay 562000) (secondsToDiffTime 0)
-- >
-- > cookie :: Cookie
-- > cookie = Cookie { cookie_name = "password_hash"
-- >                 , cookie_value = "abf472c35f8297fbcabf2911230001234fd2"
-- >                 , cookie_expiry_time = future
-- >                 , cookie_domain = "example.com"
-- >                 , cookie_path = "/"
-- >                 , cookie_creation_time = past
-- >                 , cookie_last_access_time = past
-- >                 , cookie_persistent = False
-- >                 , cookie_host_only = False
-- >                 , cookie_secure_only = False
-- >                 , cookie_http_only = False
-- >                 }
-- >
-- > main = do
-- >      request' <- parseRequest "http://example.com/secret-page"
-- >      manager <- newManager tlsManagerSettings
-- >      let request = request' { cookieJar = Just $ createCookieJar [cookie] }
-- >      fmap Just (httpLbs request manager) `E.catch`
-- >              (\ex -> case ex of
-- >                  HttpExceptionRequest _ (StatusCodeException res _) ->
-- >                      if statusCode (responseStatus res) == 403
-- >                        then (putStrLn "login failed" >> return Nothing)
-- >                        else return Nothing
-- >                  _ -> E.throw ex)
--
-- Cookies are implemented according to RFC 6265.
--
-- Note that by default, the functions in this package will throw exceptions
-- for non-2xx status codes. If you would like to avoid this, you should use
-- 'checkStatus', e.g.:
--
-- > import Data.Conduit.Binary (sinkFile)
-- > import Network.HTTP.Conduit
-- > import qualified Data.Conduit as C
-- > import Network
-- >
-- > main :: IO ()
-- > main = do
-- >      request' <- parseRequest "http://www.yesodweb.com/does-not-exist"
-- >      let request = request' { checkStatus = \_ _ _ -> Nothing }
-- >      manager <- newManager tlsManagerSettings
-- >      res <- httpLbs request manager
-- >      print res
--
-- By default, when connecting to websites using HTTPS, functions in this
-- package will throw an exception if the TLS certificate doesn't validate. To
-- continue the HTTPS transaction even if the TLS cerficate validation fails,
-- you should use 'mkManagerSetttings' as follows:
--
-- > import Network.Connection (TLSSettings (..))
-- > import Network.HTTP.Conduit
-- >
-- > main :: IO ()
-- > main = do
-- >     request <- parseRequest "https://github.com/"
-- >     let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
-- >     manager <- newManager settings
-- >     res <- httpLbs request manager
-- >     print res
--
-- For more information, please be sure to read the documentation in the
-- "Network.HTTP.Client" module.
module Streamly.External.Network.HTTP
  ( -- * Perform a request
    simpleHttp
  , httpLbs
  , http
  , -- *** Request body
    requestBodySource
  , requestBodySourceChunked
  , requestBodySourceIO
  , requestBodySourceChunkedIO
  , lbsResponse
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.ByteString.Char8  (pack)
import           Data.ByteString.Lazy   as L (ByteString)
import qualified Data.CaseInsensitive   as CI (mk)
import           Data.Word              (Word8)
import           Network.HTTP.Client    (Manager (..), Request (..),
                                         Response (..), withResponse)
import           Streamly.Data.Array    (Array)
import qualified Streamly.Data.Stream   as Stream (unfold)
import           Streamly.Data.Stream   (Stream)

type BodyReader = IO (Array Word8)

-- | Download the specified 'Request', returning the results as a 'Response'.
--
-- This is a simplified version of 'http' for the common case where you simply
-- want the response data as a simple datatype. If you want more power, such as
-- interleaved actions on the response body during download, you'll need to use
-- 'http' directly. This function is defined as:
--
-- @httpLbs = 'lbsResponse' <=< 'http'@
--
-- Even though the 'Response' contains a lazy bytestring, this
-- function does /not/ utilize lazy I/O, and therefore the entire
-- response body will live in memory. If you want constant memory
-- usage, you'll need to use @conduit@ packages's TOREVIEW
-- 'C.Source' returned by 'http'. TOREVIEW
--
-- This function will 'throwIO' an 'HttpException' for any
-- response with a non-2xx status code (besides 3xx redirects up
-- to a limit of 10 redirects). This behavior can be modified by
-- changing the 'checkStatus' field of your request.
httpLbs :: Request -> Manager -> IO (Response (Stream m Word8))
httpLbs req man =
  withResponse req man $ \res -> do
    bss <- brConsume $ responseBody res
    return res {responseBody = bss}

brConsume :: BodyReader -> IO (Stream m (Array Word8))
brConsume = undefined

-- | Download the specified URL, following any redirects, and
-- return the response body.
--
-- This function will 'throwIO' an 'HttpException' for any
-- response with a non-2xx status code (besides 3xx redirects up
-- to a limit of 10 redirects). It uses 'parseUrlThrow' to parse the
-- input. This function essentially wraps 'httpLbs'.
--
-- Note: Even though this function returns a lazy bytestring, it
-- does /not/ utilize lazy I/O, and therefore the entire response
-- body will live in memory. If you want constant memory usage,
-- you'll need to use the @conduit@ package and 'http' directly.
--
-- Note: This function creates a new 'Manager'. It should be avoided
-- in production code.
simpleHttp :: MonadIO m => String -> Stream m Word8
simpleHttp url = undefined

setConnectionClose :: Request -> Request
setConnectionClose req = undefined

http = undefined

requestBodySource = undefined

requestBodySourceChunked = undefined

requestBodySourceIO = undefined

requestBodySourceChunkedIO = undefined
