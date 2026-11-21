-- | The message format on byte arrays cut anywhere, then the client against
-- a server on the loopback interface.
module Main (main) where

import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Word (Word8)
import GHC.Clock (getMonotonicTime)
import Network.Socket qualified as Net
import Network.Socket.ByteString qualified as NetBS
import Streamly.Data.Array (Array)
import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream qualified as Stream
import Streamly.Data.StreamK qualified as StreamK
import Streamly.External.ByteString qualified as StreamlyBS
import Streamly.External.Network.HTTP
import Streamly.External.Network.HTTP.URL
import Streamly.External.Network.HTTP.Wire (readResponseHead, readResponseWithin, renderRequest, validateRequest)
import Test.Tasty (TestTree, defaultMain, localOption, mkTimeout, testGroup)
import System.Timeout (timeout)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
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
      , testGroup "URLs" urls
      , testGroup "over TCP" overTcp
      ]

-- | Read a response to a GET from the bytes, cut into pieces of a size.
readWhole :: Int -> ByteString -> IO (Response ByteString)
readWhole = readWholeFor "GET"

readWholeFor :: ByteString -> Int -> ByteString -> IO (Response ByteString)
readWholeFor = readWholeLimited Nothing Nothing

-- | Like 'readWhole', with the largest head and the largest body accepted.
readWholeWithin :: Maybe Int -> Maybe Int -> Int -> ByteString -> IO (Response ByteString)
readWholeWithin maxHead maxBody = readWholeLimited maxHead maxBody "GET"

readWholeLimited :: Maybe Int -> Maybe Int -> ByteString -> Int -> ByteString -> IO (Response ByteString)
readWholeLimited maxHead maxBody method size bytes = do
  response <- readResponseWithin maxHead maxBody method (StreamK.fromStream (Stream.fromList (map StreamlyBS.toArray (pieces size bytes))))
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
fixed = fixedHead <> "hello world"

fixedHead :: ByteString
fixedHead = "HTTP/1.1 200 OK\r\nContent-Type: image/jpeg\r\nContent-Length: 11\r\n\r\n"

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
  , testCase "the head alone, with what follows a switch of protocols" $ do
      let input = StreamK.fromStream (Stream.fromList (map StreamlyBS.toArray (pieces 9 "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\n\r\n\129\5hello")))
      (found, rest) <- readResponseHead input
      (found.status, header "upgrade" found) @?= (101, Just "websocket")
      following <- collect (StreamK.toStream rest)
      following @?= "\129\5hello"
  , testProperty "a head of exactly the size allowed is read, with its body, however the bytes are cut" $ \(Positive size) ->
      ioProperty $ do
        response <- readWholeWithin (Just (BS.length fixedHead)) Nothing size fixed
        pure (response.body === "hello world")
  , testProperty "a head one byte longer than allowed is refused, however the bytes are cut" $ \(Positive size) ->
      ioProperty $ do
        outcome <- try (readWholeWithin (Just (BS.length fixedHead - 1)) Nothing size fixed)
        pure (fmap (.status) outcome === Left (HeadTooLarge (BS.length fixedHead - 1)))
  , testCase "headers without end are refused" $ do
      let endless = "HTTP/1.1 200 OK\r\n" <> BS.concat (replicate 10_000 "X-Padding: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\r\n")
      failsWith (HeadTooLarge 65536) (readWholeWithin (Just 65536) Nothing 4096 endless)
  , testCase "the size allowed to the head counts the interim responses" $ do
      let interim = "HTTP/1.1 100 Continue\r\n\r\n"
      response <- readWholeWithin (Just (BS.length interim + BS.length fixedHead)) Nothing 5 (interim <> fixed)
      response.body @?= "hello world"
      failsWith (HeadTooLarge (BS.length fixedHead)) (readWholeWithin (Just (BS.length fixedHead)) Nothing 5 (interim <> fixed))
  , testCase "a body of the size allowed is read, a longer one refused: announced, chunked, until the end" $ do
      let untilEnd = "HTTP/1.0 200 OK\r\n\r\nhello world"
      announced <- readWholeWithin Nothing (Just 11) 4 fixed
      announced.body @?= "hello world"
      failsWith (BodyTooLarge 10) (readWholeWithin Nothing (Just 10) 4 fixed)
      inChunks <- readWholeWithin Nothing (Just 16) 4 chunked
      inChunks.body @?= "hello world12345"
      failsWith (BodyTooLarge 15) (readWholeWithin Nothing (Just 15) 4 chunked)
      toTheEnd <- readWholeWithin Nothing (Just 11) 4 untilEnd
      toTheEnd.body @?= "hello world"
      failsWith (BodyTooLarge 10) (readWholeWithin Nothing (Just 10) 4 untilEnd)
  , testCase "too long a body that is announced is refused before it is read" $ do
      -- Only the head is given: reading a body would fail otherwise.
      outcome <- try (readResponseWithin Nothing (Just 10) "GET" (StreamK.fromStream (Stream.fromList [StreamlyBS.toArray fixedHead])))
      fmap (.status) outcome @?= Left (BodyTooLarge 10)
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
      bytes @?= "GET /a?b=c HTTP/1.1\r\nHost: example.org:8080\r\nAccept: image/jpeg\r\n\r\n"
  , testCase "the Host header leaves out port 80 and brackets an IPv6 address" $ do
      plain <- rendered (get "example.org" 80 "/")
      plain @?= "GET / HTTP/1.1\r\nHost: example.org\r\n\r\n"
      v6 <- rendered (get "::1" 9330 "/")
      v6 @?= "GET / HTTP/1.1\r\nHost: [::1]:9330\r\n\r\n"
  , testCase "a Host given by the caller is kept, the framing headers are not" $ do
      bytes <- rendered (withHeaders [("host", "virtual"), ("Content-Length", "99"), ("connection", "Upgrade")] (get "127.0.0.1" 80 "/"))
      bytes @?= "GET / HTTP/1.1\r\nhost: virtual\r\nconnection: Upgrade\r\n\r\n"
  , testCase "a body in bytes" $ do
      bytes <- rendered (withBody (BodyBytes "hello") (get "h" 80 "/") {method = "POST"})
      bytes @?= "POST / HTTP/1.1\r\nHost: h\r\nContent-Length: 5\r\n\r\nhello"
  , testCase "an empty body in bytes still announces its length" $ do
      bytes <- rendered (withBody (BodyBytes "") (get "h" 80 "/") {method = "POST"})
      bytes @?= "POST / HTTP/1.1\r\nHost: h\r\nContent-Length: 0\r\n\r\n"
  , testCase "a body in a stream is chunked, empty arrays left out" $ do
      let chunks = Stream.fromList (map StreamlyBS.toArray ["hello", "", "0123456789abcdef"])
      bytes <- rendered (withBody (BodyStream chunks) (get "h" 80 "/") {method = "PUT"})
      bytes @?= "PUT / HTTP/1.1\r\nHost: h\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nhello\r\n10\r\n0123456789abcdef\r\n0\r\n\r\n"
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

urls :: [TestTree]
urls =
  [ testCase "a URL with everything" $
      parseUrl "https://Example.ORG:8443/a/b?c=d#e" @?= Right (Url Https "example.org" 8443 "/a/b?c=d")
  , testCase "the port of the scheme, and the root as the path" $ do
      parseUrl "http://example.org" @?= Right (Url Http "example.org" 80 "/")
      parseUrl "HTTPS://example.org" @?= Right (Url Https "example.org" 443 "/")
      parseUrl "http://example.org?q=1" @?= Right (Url Http "example.org" 80 "/?q=1")
      parseUrl "http://example.org/#top" @?= Right (Url Http "example.org" 80 "/")
  , testCase "addresses as hosts" $ do
      parseUrl "http://127.0.0.1:9330/api/image/k" @?= Right (Url Http "127.0.0.1" 9330 "/api/image/k")
      parseUrl "http://[::1]:9330/" @?= Right (Url Http "::1" 9330 "/")
      parseUrl "https://[2001:DB8::1]/x" @?= Right (Url Https "2001:db8::1" 443 "/x")
  , testCase "what is refused" $ do
      let refusedUrl text = either (const (pure ())) (\found -> assertFailure ("accepted " <> show text <> " as " <> show found)) (parseUrl text)
      mapM_
        refusedUrl
        [ ""
        , "example.org"
        , "ftp://example.org/"
        , "http:/example.org"
        , "http://"
        , "http://user@example.org/"
        , "http://user:secret@example.org/"
        , "http://example.org:/"
        , "http://example.org:0/"
        , "http://example.org:65536/"
        , "http://example.org:80x/"
        , "http://example.org/a b"
        , "http://example.org/a\r\nX: y"
        , "http://[::1/"
        ]
  , testCase "a URL is written back, the port of the scheme left out" $ do
      map (fmap renderUrl . parseUrl) ["https://example.org:443/a?b", "http://example.org:8080", "http://[::1]:80/x"]
        @?= [Right "https://example.org/a?b", Right "http://example.org:8080/", Right "http://[::1]/x"]
  , testCase "references are resolved against a URL" $ do
      let base = Url Http "example.org" 8080 "/a/b/c?q=1"
          to reference = fmap renderUrl (resolve base reference)
      to "https://other.example/x" @?= Right "https://other.example/x"
      to "//other.example/x" @?= Right "http://other.example/x"
      to "/x/y?z" @?= Right "http://example.org:8080/x/y?z"
      to "d" @?= Right "http://example.org:8080/a/b/d"
      to "../d/./e?f#g" @?= Right "http://example.org:8080/a/d/e?f"
      to "../../../d" @?= Right "http://example.org:8080/d"
      to "./" @?= Right "http://example.org:8080/a/b/"
      to ".." @?= Right "http://example.org:8080/a/"
      to "?other" @?= Right "http://example.org:8080/a/b/c?other"
      to "" @?= Right "http://example.org:8080/a/b/c?q=1"
      to "#frag" @?= Right "http://example.org:8080/a/b/c?q=1"
      to "/a b" @?= Left "a space or a control character in a reference"
      either (const (pure ())) (assertFailure . show) (to "ftp://example.org/")
  , testCase "the request for a URL" $ do
      bytes <- either fail (rendered . urlRequest) (parseUrl "https://example.org:8443/a?b")
      bytes @?= "GET /a?b HTTP/1.1\r\nHost: example.org:8443\r\n\r\n"
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

-- | Limits short enough for the tests.
quick :: Limits
quick = defaultLimits {connectTimeout = Just 2_000_000, idleTimeout = Just 150_000}

-- | A server that reads the request, sends the pieces of an answer with a
-- pause in microseconds before each, then holds the connection open.
withSlowServer :: Int -> [ByteString] -> (Int -> IO a) -> IO a
withSlowServer pause answer action =
  bracket open Net.close $ \listener -> do
    port <- Net.socketPort listener
    withAsync (serve listener) $ \_ -> action (fromIntegral port)
  where
    open = do
      listener <- Net.socket Net.AF_INET Net.Stream Net.defaultProtocol
      Net.setSocketOption listener Net.ReuseAddr 1
      Net.bind listener (Net.SockAddrInet 0 (Net.tupleToHostAddress (127, 0, 0, 1)))
      Net.listen listener 1
      pure listener
    serve listener =
      bracket (fst <$> Net.accept listener) Net.close $ \peer -> do
        _ <- NetBS.recv peer 4096
        mapM_ (\piece -> threadDelay pause >> NetBS.sendAll peer piece) answer
        threadDelay 10_000_000

-- | A server that sends the pieces of an answer at once, then holds the
-- connection open without a word.
withHeldServer :: [ByteString] -> (Int -> IO a) -> IO a
withHeldServer = withSlowServer 0

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
  , testCase "the client closes the connection, whatever the request says" $ do
      (request, _) <- withServer ["HTTP/1.1 204 No Content\r\n\r\n"] $ \port ->
        fetch (withHeaders [("connection", "keep-alive")] (get "127.0.0.1" port "/"))
      BS.drop (BS.length request - 23) request @?= "\r\nConnection: close\r\n\r\n"
      BS.isInfixOf "keep-alive" request @?= False
  , testCase "over a transport of the caller: what is sent, and the response read" $ do
      sent <- newIORef []
      let answer = map StreamlyBS.toArray ["HTTP/1.1 200 OK\r\nContent-Le", "ngth: 5\r\n\r\nhel", "lo"]
          transport = Transport {send = \array -> modifyIORef sent (StreamlyBS.fromArray array :), receive = Stream.fromList answer}
      response <- fetchOver defaultLimits transport (get "example.org" 443 "/secure")
      (response.status, StreamlyBS.fromArray response.body) @?= (200, "hello")
      request <- BS.concat . reverse <$> readIORef sent
      request @?= "GET /secure HTTP/1.1\r\nHost: example.org:443\r\nConnection: close\r\n\r\n"
  , testCase "a status is not an error" $ do
      (_, response) <- withServer ["HTTP/1.1 400 Bad Request\r\nContent-Length: 20\r\n\r\nInvalid Scale: bogus"] $ \port ->
        fetch (get "127.0.0.1" port "/")
      (response.status, response.reason, StreamlyBS.fromArray response.body) @?= (400, "Bad Request", "Invalid Scale: bogus")
  , testCase "a connection closed inside the body" $ do
      outcome <- try (withServer ["HTTP/1.1 200 OK\r\nContent-Length: 10\r\n\r\nabc"] $ \port -> fetch (get "127.0.0.1" port "/"))
      case outcome of
        Left problem -> problem @?= TruncatedBody 7
        Right _ -> assertFailure "no error, expected TruncatedBody"
  , testCase "a server that accepts and says nothing" $ do
      outcome <- withHeldServer [] $ \port -> try (fetchWithin quick (get "127.0.0.1" port "/"))
      outcome @?= Left ResponseTimedOut
  , testCase "a server that stops in the middle of the body" $ do
      outcome <- withHeldServer ["HTTP/1.1 200 OK\r\nContent-Length: 10\r\n\r\nabc"] $ \port ->
        try (fetchWithin quick (get "127.0.0.1" port "/"))
      outcome @?= Left ResponseTimedOut
  , testCase "a slow response is not a silent one: each wait is bounded, not the whole" $ do
      let answer = "HTTP/1.1 200 OK\r\nContent-Length: 6\r\n\r\n" : map BS.singleton (BS.unpack "slowly")
      response <- withSlowServer 60_000 answer $ \port -> fetchWithin quick (get "127.0.0.1" port "/")
      StreamlyBS.fromArray response.body @?= "slowly"
  , testCase "without limits a held connection is waited for" $ do
      outcome <- withHeldServer [] $ \port -> timeout 300_000 (fetchWithin noLimits (get "127.0.0.1" port "/"))
      fmap (.status) outcome @?= Nothing
  , testCase "a limited transport bounds receiving and sending" $ do
      let silent = Transport {send = \_ -> pure (), receive = Stream.repeatM (threadDelay 5_000_000 >> pure (StreamlyBS.toArray ""))}
          deaf = Transport {send = \_ -> threadDelay 5_000_000, receive = Stream.nil}
      failsWith ResponseTimedOut (fetchOver noLimits (limitTransport (Just 50_000) silent) (get "example.org" 80 "/"))
      failsWith ResponseTimedOut (fetchOver noLimits (limitTransport (Just 50_000) deaf) (get "example.org" 80 "/"))
      failsWith ResponseTimedOut (fetchOver noLimits {idleTimeout = Just 50_000} silent (get "example.org" 80 "/"))
  , testCase "connecting gives up in time" $ do
      -- An address that answers nothing where it is routed, and is refused
      -- at once where it is not: either way the wait is bounded.
      before <- getMonotonicTime
      outcome <- try (try (connectWithin (Just 200_000) "10.255.255.1" 81))
      after <- getMonotonicTime
      case outcome of
        Left (_ :: IOError) -> pure ()
        Right (Left ConnectTimedOut) -> pure ()
        Right (Left other) -> assertFailure ("expected ConnectTimedOut, got " <> show other)
        Right (Right socket) -> Net.close socket
      assertBool ("waited " <> show (after - before) <> " s") (after - before < 2)
  , testCase "the sizes allowed apply over TCP" $ do
      let answer = ["HTTP/1.1 200 OK\r\nContent-Length: 20\r\n\r\n", "01234567890123456789"]
      (_, tooLong) <- withServer answer $ \port -> try (fetchWithin defaultLimits {maxBodySize = Just 19} (get "127.0.0.1" port "/"))
      fmap (.status) tooLong @?= Left (BodyTooLarge 19)
      (_, fits) <- withServer answer $ \port -> fetchWithin defaultLimits {maxBodySize = Just 20} (get "127.0.0.1" port "/")
      StreamlyBS.fromArray fits.body @?= "01234567890123456789"
      (_, longHead) <- withServer answer $ \port -> try (fetchWithin defaultLimits {maxHeadSize = Just 20} (get "127.0.0.1" port "/"))
      fmap (.status) longHead @?= Left (HeadTooLarge 20)
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
