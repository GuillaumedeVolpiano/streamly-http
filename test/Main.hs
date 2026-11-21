-- | The message format on byte arrays cut anywhere, then the client against
-- a server on the loopback interface.
module Main (main) where

import Control.Concurrent.Async (wait, withAsync)
import Control.Exception (bracket, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Word (Word8)
import Network.Socket qualified as Net
import Network.Socket.ByteString qualified as NetBS
import Streamly.Data.Array (Array)
import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream qualified as Stream
import Streamly.Data.StreamK qualified as StreamK
import Streamly.External.ByteString qualified as StreamlyBS
import Streamly.External.Network.HTTP
import Streamly.External.Network.HTTP.Wire (readResponse, renderRequest, validateRequest)
import Test.Tasty (TestTree, defaultMain, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (Positive (..), ioProperty, testProperty, (===))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000) $
    testGroup
      "streamly-http"
      [ testGroup "reading a response" reading
      , testGroup "writing a request" writing
      , testGroup "over TCP" overTcp
      ]

-- | Read a response to a GET from the bytes, cut into pieces of a size.
readWhole :: Int -> ByteString -> IO (Response ByteString)
readWhole = readWholeFor "GET"

readWholeFor :: ByteString -> Int -> ByteString -> IO (Response ByteString)
readWholeFor method size bytes = do
  response <- readResponse method (StreamK.fromStream (Stream.fromList (map StreamlyBS.toArray (pieces size bytes))))
  content <- collect response.body
  pure (content <$ response)

collect :: Stream IO (Array Word8) -> IO ByteString
collect = fmap (BS.concat . map StreamlyBS.fromArray) . Stream.toList

pieces :: Int -> ByteString -> [ByteString]
pieces size bytes
  | BS.null bytes = []
  | otherwise = let (front, back) = BS.splitAt size bytes in front : pieces size back

failsWith :: HttpError -> IO a -> IO ()
failsWith expected action = do
  outcome <- try action
  case outcome of
    Left problem -> problem @?= expected
    Right _ -> assertFailure ("no error, expected " <> show expected)

isMalformed :: IO a -> IO ()
isMalformed action = do
  outcome <- try action
  case outcome of
    Left (MalformedResponse _) -> pure ()
    Left other -> assertFailure ("expected MalformedResponse, got " <> show other)
    Right _ -> assertFailure "no error, expected MalformedResponse"

fixed :: ByteString
fixed = "HTTP/1.1 200 OK\r\nContent-Type: image/jpeg\r\nContent-Length: 11\r\n\r\nhello world"

chunked :: ByteString
chunked =
  "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n"
    <> "5\r\nhello\r\n1;name=value\r\n \r\nA\r\nworld12345\r\n0\r\nTrailer: ignored\r\n\r\n"

reading :: [TestTree]
reading =
  [ testCase "a body delimited by Content-Length" $ do
      response <- readWhole 4096 fixed
      response @?= Response {status = 200, reason = "OK", headers = [("content-type", "image/jpeg"), ("content-length", "11")], body = "hello world"}
  , testCase "a header is found whatever its case" $ do
      response <- readWhole 4096 fixed
      header "Content-TYPE" response @?= Just "image/jpeg"
      header "etag" response @?= Nothing
  , testProperty "a Content-Length body does not depend on how the bytes are cut" $ \(Positive size) ->
      ioProperty $ do
        response <- readWhole size fixed
        pure (response.body === "hello world")
  , testCase "a chunked body, with a chunk extension and a trailer" $ do
      response <- readWhole 4096 chunked
      response.body @?= "hello world12345"
  , testProperty "a chunked body does not depend on how the bytes are cut" $ \(Positive size) ->
      ioProperty $ do
        response <- readWhole size chunked
        pure (response.body === "hello world12345")
  , testCase "a body that runs until the connection closes" $ do
      response <- readWhole 7 "HTTP/1.0 200 OK\r\nServer: old\r\n\r\nuntil the end"
      response.body @?= "until the end"
  , testCase "bytes beyond Content-Length are not part of the body" $ do
      response <- readWhole 5 (fixed <> "HTTP/1.1 500 Next\r\n\r\n")
      response.body @?= "hello world"
  , testCase "line feeds alone end the lines, and the reason may be missing" $ do
      response <- readWhole 3 "HTTP/1.1 404\nContent-Length:2\n\nno"
      (response.status, response.reason, response.body) @?= (404, "", "no")
  , testCase "no body after HEAD, 204 and 304, whatever the headers say" $ do
      afterHead <- readWholeFor "HEAD" 4096 fixed
      afterHead.body @?= ""
      noContent <- readWhole 4096 "HTTP/1.1 204 No Content\r\nContent-Length: 5\r\n\r\nhello"
      noContent.body @?= ""
      notModified <- readWhole 4096 "HTTP/1.1 304 Not Modified\r\nTransfer-Encoding: chunked\r\n\r\n"
      notModified.body @?= ""
  , testCase "interim responses are skipped" $ do
      response <- readWhole 6 ("HTTP/1.1 100 Continue\r\n\r\nHTTP/1.1 103 Early Hints\r\nLink: </a>\r\n\r\n" <> fixed)
      (response.status, response.body) @?= (200, "hello world")
  , testCase "a body cut short" $
      failsWith (TruncatedBody 5) (readWhole 4 "HTTP/1.1 200 OK\r\nContent-Length: 11\r\n\r\nhello ")
  , testCase "a chunk cut short" $
      failsWith (TruncatedBody 2) (readWhole 4 "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nhel")
  , testCase "a chunked body without its last chunk" $
      isMalformed (readWhole 4 "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nhello\r\n")
  , testCase "a chunk size that is not hexadecimal" $
      isMalformed (readWhole 4096 "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\nxyz\r\nhello\r\n")
  , -- With streamly-core 0.3.3 a parser repeated by 'many' gives back what
    -- it read when the input ends inside an element, and a following 'eof'
    -- would succeed: the head must end with its empty line instead.
    testCase "a head cut inside a header line" $
      isMalformed (readWhole 4096 "HTTP/1.1 200 OK\r\nContent-Type: image/jpeg\r\nContent-Le")
  , testCase "a head cut before its empty line" $
      isMalformed (readWhole 4096 "HTTP/1.1 200 OK\r\nContent-Type: image/jpeg\r\n")
  , testCase "no input at all" $
      isMalformed (readWhole 4096 "")
  , testCase "a header line without a colon" $
      isMalformed (readWhole 4096 "HTTP/1.1 200 OK\r\nnot a header\r\n\r\n")
  , testCase "a folded header line" $
      isMalformed (readWhole 4096 "HTTP/1.1 200 OK\r\nA: b\r\n c\r\n\r\n")
  , testCase "something that is not HTTP" $
      isMalformed (readWhole 4096 "SSH-2.0-OpenSSH\r\n")
  , testCase "a status code of two digits" $
      isMalformed (readWhole 4096 "HTTP/1.1 20 OK\r\n\r\n")
  , testCase "a Content-Length that is not a number" $ do
      failsWith (InvalidContentLength "12a") (readWhole 4096 "HTTP/1.1 200 OK\r\nContent-Length: 12a\r\n\r\n")
      failsWith (InvalidContentLength "-1") (readWhole 4096 "HTTP/1.1 200 OK\r\nContent-Length: -1\r\n\r\n")
  , testCase "a transfer coding other than chunked" $
      failsWith (UnsupportedTransferEncoding "gzip, chunked") (readWhole 4096 "HTTP/1.1 200 OK\r\nTransfer-Encoding: gzip, chunked\r\n\r\n")
  ]

rendered :: Request -> IO ByteString
rendered = collect . renderRequest

writing :: [TestTree]
writing =
  [ testCase "a GET" $ do
      bytes <- rendered (withHeaders [("Accept", "image/jpeg")] (get "example.org" 8080 "/a?b=c"))
      bytes @?= "GET /a?b=c HTTP/1.1\r\nHost: example.org:8080\r\nAccept: image/jpeg\r\nConnection: close\r\n\r\n"
  , testCase "the Host header leaves out port 80 and brackets an IPv6 address" $ do
      plain <- rendered (get "example.org" 80 "/")
      plain @?= "GET / HTTP/1.1\r\nHost: example.org\r\nConnection: close\r\n\r\n"
      v6 <- rendered (get "::1" 9330 "/")
      v6 @?= "GET / HTTP/1.1\r\nHost: [::1]:9330\r\nConnection: close\r\n\r\n"
  , testCase "a Host given by the caller is kept, the framing headers are not" $ do
      bytes <- rendered (withHeaders [("host", "virtual"), ("Content-Length", "99"), ("connection", "keep-alive")] (get "127.0.0.1" 80 "/"))
      bytes @?= "GET / HTTP/1.1\r\nhost: virtual\r\nConnection: close\r\n\r\n"
  , testCase "a body in bytes" $ do
      bytes <- rendered (withBody (BodyBytes "hello") (get "h" 80 "/") {method = "POST"})
      bytes @?= "POST / HTTP/1.1\r\nHost: h\r\nConnection: close\r\nContent-Length: 5\r\n\r\nhello"
  , testCase "an empty body in bytes still announces its length" $ do
      bytes <- rendered (withBody (BodyBytes "") (get "h" 80 "/") {method = "POST"})
      bytes @?= "POST / HTTP/1.1\r\nHost: h\r\nConnection: close\r\nContent-Length: 0\r\n\r\n"
  , testCase "a body in a stream is chunked, empty arrays left out" $ do
      let chunks = Stream.fromList (map StreamlyBS.toArray ["hello", "", "0123456789abcdef"])
      bytes <- rendered (withBody (BodyStream chunks) (get "h" 80 "/") {method = "PUT"})
      bytes @?= "PUT / HTTP/1.1\r\nHost: h\r\nConnection: close\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nhello\r\n10\r\n0123456789abcdef\r\n0\r\n\r\n"
  , testCase "requests that would run a field into the next are refused" $ do
      let refused request = case validateRequest request of
            Left (InvalidRequest _) -> pure ()
            other -> assertFailure ("expected InvalidRequest, got " <> show other)
      validateRequest (withHeaders [("Accept", "a, b")] (get "example.org" 80 "/a?b=c")) @?= Right ()
      refused (get "example.org" 80 "/a b")
      refused (get "example.org" 80 "/a\r\nX: y")
      refused (get "example.org" 80 "a")
      refused (get "exa mple.org" 80 "/")
      refused (get "" 80 "/")
      refused (get "example.org" 0 "/")
      refused (get "example.org" 80 "/") {method = "GE T"}
      refused (withHeaders [("A", "b\r\nC: d")] (get "example.org" 80 "/"))
      refused (withHeaders [("A B", "c")] (get "example.org" 80 "/"))
  ]

-- | Run a server that answers one connection: it reads the request head
-- (and what came with it), sends the answer in pieces and closes. Gives the
-- port to the action, and returns the request read with its result.
withServer :: [ByteString] -> (Int -> IO a) -> IO (ByteString, a)
withServer answer action =
  bracket open Net.close $ \listener -> do
    port <- Net.socketPort listener
    withAsync (serve listener) $ \server -> do
      result <- action (fromIntegral port)
      request <- wait server
      pure (request, result)
  where
    open = do
      listener <- Net.socket Net.AF_INET Net.Stream Net.defaultProtocol
      Net.setSocketOption listener Net.ReuseAddr 1
      Net.bind listener (Net.SockAddrInet 0 (Net.tupleToHostAddress (127, 0, 0, 1)))
      Net.listen listener 1
      pure listener
    serve listener =
      bracket (fst <$> Net.accept listener) Net.close $ \peer -> do
        request <- readHead peer ""
        mapM_ (NetBS.sendAll peer) answer
        pure request
    readHead peer seen
      | "\r\n\r\n" `BS.isInfixOf` seen = pure seen
      | otherwise = do
          more <- NetBS.recv peer 4096
          if BS.null more then pure seen else readHead peer (seen <> more)

overTcp :: [TestTree]
overTcp =
  [ testCase "fetch sends the request and reads the whole body" $ do
      let image = BS.pack (take 100_000 (cycle [0 .. 255]))
          answer = ["HTTP/1.1 200 OK\r\nContent-Type: image/jpeg\r\nContent-Le", "ngth: 100000\r\n\r\n"] <> pieces 30_000 image
      (request, (port, response)) <- withServer answer $ \port ->
        (,) port <$> fetch (withHeaders [("Accept", "image/jpeg")] (get "127.0.0.1" port "/api/image/key?width=300"))
      request @?= "GET /api/image/key?width=300 HTTP/1.1\r\nHost: 127.0.0.1:" <> BS8.pack (show port) <> "\r\nAccept: image/jpeg\r\nConnection: close\r\n\r\n"
      (response.status, header "content-type" response) @?= (200, Just "image/jpeg")
      StreamlyBS.fromArray response.body @?= image
  , testCase "withResponse gives the body as a stream, here chunked" $ do
      let answer = ["HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n3\r\nabc\r\n", "2\r\nde\r\n0\r\n\r\n"]
      (_, arrays) <- withServer answer $ \port ->
        withResponse (get "localhost" port "/") $ \response -> map StreamlyBS.fromArray <$> Stream.toList response.body
      BS.concat arrays @?= "abcde"
  , testCase "a status is not an error" $ do
      (_, response) <- withServer ["HTTP/1.1 400 Bad Request\r\nContent-Length: 20\r\n\r\nInvalid Scale: bogus"] $ \port ->
        fetch (get "127.0.0.1" port "/")
      (response.status, response.reason, StreamlyBS.fromArray response.body) @?= (400, "Bad Request", "Invalid Scale: bogus")
  , testCase "a connection closed inside the body" $ do
      outcome <- try (withServer ["HTTP/1.1 200 OK\r\nContent-Length: 10\r\n\r\nabc"] $ \port -> fetch (get "127.0.0.1" port "/"))
      case outcome of
        Left problem -> problem @?= TruncatedBody 7
        Right _ -> assertFailure "no error, expected TruncatedBody"
  , testCase "an invalid request is refused before any connection" $
      failsWithInvalid (fetch (get "127.0.0.1" 1 "/a b"))
  , testCase "nothing listening is an IOError" $ do
      port <- bracket closedPort pure pure
      outcome <- try (fetch (get "127.0.0.1" port "/"))
      case outcome of
        Left (_ :: IOError) -> pure ()
        Right _ -> assertFailure "no error, expected an IOError"
  ]
  where
    failsWithInvalid action = do
      outcome <- try action
      case outcome of
        Left (InvalidRequest _) -> pure ()
        Left other -> assertFailure ("expected InvalidRequest, got " <> show other)
        Right _ -> assertFailure "no error, expected InvalidRequest"
    -- A port that was free a moment ago: bound, then closed.
    closedPort = bracket (Net.socket Net.AF_INET Net.Stream Net.defaultProtocol) Net.close $ \socket -> do
      Net.bind socket (Net.SockAddrInet 0 (Net.tupleToHostAddress (127, 0, 0, 1)))
      fromIntegral <$> Net.socketPort socket
