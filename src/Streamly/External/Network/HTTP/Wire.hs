-- |
-- Module      : Streamly.External.Network.HTTP.Wire
-- Description : HTTP/1.1 messages as streams of byte arrays
-- License     : BSD-3-Clause
--
-- The HTTP/1.1 message format (RFC 9112), independent of any transport: a
-- request becomes a stream of byte arrays, and a response is read from one.
-- "Streamly.External.Network.HTTP" runs these over a TCP connection; use
-- this module directly to run them over another transport.
module Streamly.External.Network.HTTP.Wire
  ( -- * Requests
    Request (..)
  , RequestBody (..)
  , Header
  , withHeaders
  , withBody
  , validateRequest
  , renderRequest

    -- * Responses
  , Response (..)
  , header
  , readResponse
  , readResponseWithin
  , readResponseReusable
  , readResponseHead
  , readResponseHeadWithin

    -- * Requests, as a server reads them
  , RequestHead (..)
  , requestMethod
  , requestTarget
  , requestHeader
  , readRequestHeadWithin

    -- * Errors
  , HttpError (..)

    -- * Parsers
  , responseHead
  , requestHead
  , chunkSizeLine
  ) where

import Control.Applicative ((<|>))
import Control.Exception (Exception, throwIO, try)
import Control.Monad (unless, void, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import FlatParse.Basic qualified as FP
import Numeric (showHex)
import Streamly.Data.Array (Array)
import Streamly.Data.Array qualified as Array
import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream qualified as Stream
import Streamly.Data.StreamK (StreamK)
import Streamly.Data.StreamK qualified as StreamK
import Streamly.External.ByteString qualified as StreamlyBS
import Streamly.External.FlatParse qualified as Flat

-- | A header name and its value.
type Header = (ByteString, ByteString)

-- | A request to one server.
--
-- 'renderRequest' adds @Host@ (unless given here) and the header that
-- delimits the body; @Content-Length@ and @Transfer-Encoding@ given here
-- are dropped.
data Request = Request
  { method :: ByteString
  -- ^ For example @GET@.
  , host :: String
  -- ^ A host name or an address, without brackets around an IPv6 address.
  , port :: Int
  , target :: ByteString
  -- ^ The path and the query, already percent-encoded: @\/a\/b?c=d@.
  , headers :: [Header]
  , body :: RequestBody
  }

-- | The body of a request.
data RequestBody
  = NoBody
  | -- | Sent with a @Content-Length@.
    BodyBytes ByteString
  | -- | Sent with the chunked transfer coding, one chunk per array.
    BodyStream (Stream IO (Array Word8))

-- | The request with these headers instead of its own.
withHeaders :: [Header] -> Request -> Request
withHeaders headers (Request method host port target _ body) = Request {method, host, port, target, headers, body}

-- | The request with this body instead of its own.
withBody :: RequestBody -> Request -> Request
withBody body (Request method host port target headers _) = Request {method, host, port, target, headers, body}

-- | A response. 'readResponse' gives the names of the headers in lower
-- case, in the order received.
data Response body = Response
  { status :: Int
  , reason :: ByteString
  , headers :: [Header]
  , body :: body
  }
  deriving stock (Eq, Show, Functor)

-- | The first header of that name, whatever the case of the name.
header :: ByteString -> Response body -> Maybe ByteString
header name response = lookup (lowerAscii name) response.headers

-- | What can go wrong with a message. Failures of the connection itself
-- stay the 'IOError's of the transport.
data HttpError
  = -- | The request cannot be written safely: what is wrong with it.
    InvalidRequest String
  | -- | The response head or a chunk size could not be parsed: the reason
    -- given by the parser.
    MalformedResponse String
  | -- | A @Content-Length@ that is not a number.
    InvalidContentLength ByteString
  | -- | A @Transfer-Encoding@ other than @chunked@.
    UnsupportedTransferEncoding ByteString
  | -- | The input ended inside the body: the number of bytes still expected
    -- (of the current chunk, for a chunked body).
    TruncatedBody Int
  | -- | The head of a request could not be parsed, on the side of a
    -- server: the reason given by the parser.
    MalformedRequest String
  | -- | The head of the response, or of the request on the side of a
    -- server, is longer than allowed: the limit, in bytes.
    HeadTooLarge Int
  | -- | The body of the response is longer than allowed: the limit, in
    -- bytes.
    BodyTooLarge Int
  | -- | The connection could not be opened within the time allowed.
    ConnectTimedOut
  | -- | The server went silent for longer than allowed, before or inside
    -- its response, or stopped reading the request.
    ResponseTimedOut
  deriving stock (Eq, Show)

instance Exception HttpError

-- | Check that a request can be written: a method, a target starting with
-- a slash, a port, and no control character or space that would let a
-- field run into the next one.
validateRequest :: Request -> Either HttpError ()
validateRequest request = do
  check (not (BS.null request.method) && BS.all isTokenByte request.method) "the method is not a token"
  check ("/" `BS.isPrefixOf` request.target || request.target == "*") "the target does not start with a slash"
  check (BS.all (\byte -> byte > space && byte /= 127) request.target) "the target holds a space or a control character"
  check (not (null request.host) && all hostChar request.host) "the host is empty or holds a space or a control character"
  check (request.port > 0 && request.port < 65536) "the port is out of range"
  mapM_ checkHeader request.headers
  where
    check ok problem = unless ok (Left (InvalidRequest problem))
    hostChar char = char > ' ' && char < '\DEL' && char `notElem` ("/[]" :: String)
    checkHeader (name, value) = do
      check (not (BS.null name) && BS.all isTokenByte name) ("the header name " <> show name <> " is not a token")
      check (BS.all (\byte -> byte >= space && byte /= 127 || byte == tab) value) ("the value of the header " <> show name <> " holds a control character")

-- | The bytes of a request: the head in one array, then the body.
-- The request is not checked: see 'validateRequest'.
renderRequest :: Request -> Stream IO (Array Word8)
renderRequest request = case request.body of
  NoBody -> Stream.fromPure (headWith [])
  BodyBytes bytes ->
    Stream.fromList
      (headWith [("Content-Length", BS8.pack (show (BS.length bytes)))] : [StreamlyBS.toArray bytes | not (BS.null bytes)])
  BodyStream chunks ->
    Stream.cons (headWith [("Transfer-Encoding", "chunked")]) $
      Stream.concatMap chunk (Stream.filter ((> 0) . Array.length) chunks)
        `Stream.append` Stream.fromPure (StreamlyBS.toArray "0\r\n\r\n")
  where
    headWith framing =
      StreamlyBS.toArray . BS.concat $
        [request.method, " ", request.target, " HTTP/1.1\r\n"]
          <> concatMap written (hostHeader <> kept <> framing)
          <> ["\r\n"]
    written (name, value) = [name, ": ", value, "\r\n"]
    kept = filter (\(name, _) -> lowerAscii name `notElem` owned) request.headers
    owned = ["content-length", "transfer-encoding"]
    hostHeader
      | any (\(name, _) -> lowerAscii name == "host") request.headers = []
      | otherwise = [("Host", authority)]
    authority =
      (if ':' `elem` request.host then "[" <> BS8.pack request.host <> "]" else BS8.pack request.host)
        <> (if request.port == 80 then "" else ":" <> BS8.pack (show request.port))
    chunk array =
      Stream.fromList
        [ StreamlyBS.toArray (BS8.pack (showHex (Array.length array) "\r\n"))
        , array
        , StreamlyBS.toArray "\r\n"
        ]

-- | Read a response from the bytes received, given the method of the
-- request (a response to @HEAD@ has no body).
--
-- Interim responses (status 1xx, except 101) are skipped. The body is read
-- from the input as the stream is consumed, so the stream must be consumed
-- while the input is still open, and only once. It ends where the response
-- says it ends; what follows in the input is not read. The trailer fields
-- of a chunked body are not read either.
--
-- Throws 'HttpError': here for the head, from the stream for the body.
readResponse :: ByteString -> StreamK IO (Array Word8) -> IO (Response (Stream IO (Array Word8)))
readResponse = readResponseWithin Nothing Nothing

-- | Like 'readResponse', with the largest head and the largest body
-- accepted, in bytes; 'Nothing' accepts any. The head counts the interim
-- responses skipped; the body is counted as received, after the chunked
-- coding is undone.
--
-- Throws 'HeadTooLarge' here. 'BodyTooLarge' is thrown here when the
-- response announces too long a body, and from the stream otherwise.
readResponseWithin :: Maybe Int -> Maybe Int -> ByteString -> StreamK IO (Array Word8) -> IO (Response (Stream IO (Array Word8)))
readResponseWithin maxHead maxBody method input = do
  ((_, found), rest) <- versionedHeadWithin maxHead input
  framing <- either throwIO pure (bodyFraming method found)
  case (framing, maxBody) of
    (Fixed announced, Just limit) | announced > limit -> throwIO (BodyTooLarge limit)
    _ -> pure ()
  pure (maybe id limitBody maxBody (bodyStream Nothing framing rest) <$ found)

-- | Like 'readResponseWithin', for a connection that may carry another
-- exchange afterwards. The action given with the response tells, once the
-- body has been consumed, whether the connection can go on, and gives the
-- input that follows the response if it can. It cannot when the body was
-- not read to its end, when only the end of the connection ends the body,
-- when the server speaks HTTP\/1.0 without asking to keep the connection,
-- or when it says @Connection: close@.
--
-- Unlike 'readResponseWithin', this reads the trailer fields of a chunked
-- body, to find where the next response starts.
readResponseReusable
  :: Maybe Int
  -> Maybe Int
  -> ByteString
  -> StreamK IO (Array Word8)
  -> IO (Response (Stream IO (Array Word8)), IO (Maybe (StreamK IO (Array Word8))))
readResponseReusable maxHead maxBody method input = do
  ((minor, found), rest) <- versionedHeadWithin maxHead input
  framing <- either throwIO pure (bodyFraming method found)
  case (framing, maxBody) of
    (Fixed announced, Just limit) | announced > limit -> throwIO (BodyTooLarge limit)
    _ -> pure ()
  after <- newIORef Nothing
  let wishes = maybe [] (map (lowerAscii . BS8.strip) . BS.split 44) (header "connection" found)
      persistent = "close" `notElem` wishes && (minor >= 1 || "keep-alive" `elem` wishes)
      following = if persistent then readIORef after else pure Nothing
  pure (maybe id limitBody maxBody (bodyStream (Just after) framing rest) <$ found, following)

-- | Read the head of a response, and give what follows it in the input:
-- the body, or after a status 101 the bytes of the protocol switched to.
-- Interim responses (status 1xx, except 101) are skipped.
--
-- Throws 'MalformedResponse'.
readResponseHead :: StreamK IO (Array Word8) -> IO (Response (), StreamK IO (Array Word8))
readResponseHead = readResponseHeadWithin Nothing

-- | Like 'readResponseHead', with the largest head accepted, in bytes,
-- interim responses included; 'Nothing' accepts any. Throws 'HeadTooLarge'
-- beyond.
readResponseHeadWithin :: Maybe Int -> StreamK IO (Array Word8) -> IO (Response (), StreamK IO (Array Word8))
readResponseHeadWithin limit input = do
  ((_, found), rest) <- versionedHeadWithin limit input
  pure (found, rest)

-- | The head of a request, as a server receives it: the method, the
-- target, and the headers with their names in lower case. (Without field
-- names, which would make updates of a 'Request' ambiguous.)
data RequestHead = RequestHead ByteString ByteString [Header]
  deriving stock (Eq, Show)

-- | The method of a request received, such as @GET@.
requestMethod :: RequestHead -> ByteString
requestMethod (RequestHead method _ _) = method

-- | The target of a request received: its path and its query.
requestTarget :: RequestHead -> ByteString
requestTarget (RequestHead _ target _) = target

-- | The first header of that name in the head of a request, whatever the
-- case of the name.
requestHeader :: ByteString -> RequestHead -> Maybe ByteString
requestHeader name (RequestHead _ _ headers) = lookup (lowerAscii name) headers

-- | Read the head of a request, with the largest head accepted, in bytes
-- ('Nothing' accepts any), and give what follows it in the input: the
-- body, or after an upgrade the bytes of the protocol switched to.
--
-- Throws 'MalformedRequest', and 'HeadTooLarge' beyond the limit.
readRequestHeadWithin :: Maybe Int -> StreamK IO (Array Word8) -> IO (RequestHead, StreamK IO (Array Word8))
readRequestHeadWithin limit = parsed MalformedRequest (HeadTooLarge allowed) allowed requestHead
  where
    allowed = fromMaybe maxBound limit

-- | The head of a response with the minor version of its protocol: 0 for
-- HTTP\/1.0, 1 for HTTP\/1.1. Interim responses are skipped; the bytes
-- allowed are for all the heads read.
versionedHeadWithin :: Maybe Int -> StreamK IO (Array Word8) -> IO ((Int, Response ()), StreamK IO (Array Word8))
versionedHeadWithin limit = go allowed
  where
    allowed = fromMaybe maxBound limit
    go budget input = do
      ((size, versioned@(_, found)), rest) <- parsed MalformedResponse (HeadTooLarge allowed) budget (sized versionedHead) input
      if found.status >= 100 && found.status < 200 && found.status /= 101
        then go (budget - size) rest
        else pure (versioned, rest)
    sized parser = FP.withByteString parser (\found bytes -> pure (BS.length bytes, found))

-- | Run a parser on the start of the input, which it may read no further
-- than a number of bytes, and give what follows. Throws the error made of
-- the reason when the input does not parse, and the other error when it
-- would need more bytes than allowed.
parsed :: (String -> HttpError) -> HttpError -> Int -> FP.Parser String a -> StreamK IO (Array Word8) -> IO (a, StreamK IO (Array Word8))
parsed malformed tooLong limit parser input =
  Flat.parseBreak limit parser input >>= \case
    (Right found, rest) -> pure (found, rest)
    (Left Flat.Mismatch, _) -> throwIO (malformed "the input ended too early")
    (Left (Flat.ParserError problem), _) -> throwIO (malformed problem)
    (Left (Flat.TooLong _), _) -> throwIO tooLong

-- | Stop a body that grows beyond a number of bytes.
limitBody :: Int -> Stream IO (Array Word8) -> Stream IO (Array Word8)
limitBody limit body = Stream.concatEffect $ do
  total <- newIORef 0
  pure $
    Stream.mapM
      ( \array -> do
          size <- (+ Array.length array) <$> readIORef total
          when (size > limit) (throwIO (BodyTooLarge limit))
          array <$ writeIORef total size
      )
      body

-- | How the end of a body is found (RFC 9112, section 6.3).
data Framing
  = Empty
  | Fixed Int
  | Chunked
  | UntilClose

bodyFraming :: ByteString -> Response () -> Either HttpError Framing
bodyFraming method response
  | method == "HEAD" || response.status < 200 || response.status == 204 || response.status == 304 = Right Empty
  | Just coding <- header "transfer-encoding" response =
      if lowerAscii (BS8.strip coding) == "chunked"
        then Right Chunked
        else Left (UnsupportedTransferEncoding coding)
  | Just raw <- header "content-length" response =
      case BS8.readInt raw of
        Just (n, "") | BS.all isDigit raw -> Right (Fixed n)
        _ -> Left (InvalidContentLength raw)
  | otherwise = Right UntilClose

data BodyState
  = -- | Before the size line of a chunk.
    AtChunkSize (StreamK IO (Array Word8))
  | -- | Inside the data: the bytes still expected, the input, and whether
    -- this is a chunk (then a line end and another chunk follow).
    InData Int (StreamK IO (Array Word8)) Bool

-- | The body, read from the input as the stream is consumed. With a
-- place to leave it, the input that follows the body is left there once
-- the body has been read to its end, when the body says where it ends.
bodyStream :: Maybe (IORef (Maybe (StreamK IO (Array Word8)))) -> Framing -> StreamK IO (Array Word8) -> Stream IO (Array Word8)
bodyStream after framing input = case framing of
  Empty -> Stream.concatEffect (Stream.nil <$ leave input)
  UntilClose -> StreamK.toStream input
  Fixed n -> Stream.unfoldrM step (InData n input False)
  Chunked -> Stream.unfoldrM step (AtChunkSize input)
  where
    leave rest = mapM_ (\place -> writeIORef place (Just rest)) after
    -- The lines between the chunks are short.
    short = parsed MalformedResponse (MalformedResponse "a line between chunks that is too long") 4096
    step (AtChunkSize from) = do
      (size, rest) <- short chunkSizeLine from
      if size /= 0
        then step (InData size rest True)
        else do
          -- The trailer fields are only read to find what follows them;
          -- a trailer that cannot be read ends the use of the connection,
          -- not the body.
          case after of
            Nothing -> pure ()
            Just _ -> do
              trailer <- try (parsed MalformedResponse (MalformedResponse "a trailer that is too long") 65536 (FP.skipMany headerLine *> lineEnd) rest)
              either (\(_ :: HttpError) -> pure ()) (leave . snd) trailer
          pure Nothing
    step (InData 0 from chunked)
      | chunked = do
          ((), rest) <- short chunkEnd from
          step (AtChunkSize rest)
      | otherwise = Nothing <$ leave from
    step (InData remaining from chunked) = do
      next <- StreamK.uncons from
      case next of
        Nothing -> throwIO (TruncatedBody remaining)
        Just (array, rest)
          | size == 0 -> step (InData remaining rest chunked)
          | size <= remaining -> pure (Just (array, InData (remaining - size) rest chunked))
          | otherwise -> do
              front <- Stream.fold (Array.createOf remaining) (Stream.take remaining (Array.read array))
              back <- Stream.fold (Array.createOf (size - remaining)) (Stream.drop remaining (Array.read array))
              pure (Just (front, InData 0 (StreamK.cons back rest) chunked))
          where
            size = Array.length array

-- | The status line, the header lines and the empty line that ends them.
-- The names of the headers are given in lower case, the values without the
-- spaces around them. Lines end with a line feed, after an optional
-- carriage return. Header lines folded over several lines are refused.
--
-- Like the other parsers here, it fails while a line is not all there, so
-- that it can be run with @parseBreak@ of @streamly-flatparse@ as the
-- bytes come, and it stops on an error as soon as a whole line is wrong.
responseHead :: FP.Parser String (Response ())
responseHead = snd <$> versionedHead

-- | 'responseHead', with the minor version of the protocol of the server.
versionedHead :: FP.Parser String (Int, Response ())
versionedHead = do
  (minor, status, reason) <- line "the first line is not the status line of HTTP/1" $ do
    FP.byteString "HTTP/1."
    minor <- digit
    FP.word8 space
    digits <- FP.byteStringOf (FP.skipSome (satisfyByte isDigit))
    when (BS.length digits /= 3) FP.failed
    reason <- trimmed <$> FP.takeRest
    pure (minor, BS.foldl' (\total byte -> total * 10 + fromIntegral (byte - 48)) 0 digits, reason)
  found <- FP.many headerLine
  lineEnd
  pure (minor, Response {status, reason, headers = found, body = ()})

-- | The request line, the header lines and the empty line that ends them,
-- read as 'responseHead' reads those of a response.
requestHead :: FP.Parser String RequestHead
requestHead = do
  (method, target) <- line "the first line is not the request line of HTTP/1" $ do
    method <- FP.byteStringOf (FP.skipSome (satisfyByte isTokenByte))
    FP.word8 space
    target <- FP.byteStringOf (FP.skipSome (satisfyByte (\byte -> byte > space && byte /= 127)))
    FP.word8 space
    FP.byteString "HTTP/1."
    void digit
    FP.skipMany (FP.word8 carriageReturn)
    pure (method, target)
  found <- FP.many headerLine
  lineEnd
  pure (RequestHead method target found)

-- | A header line: its name in lower case, and its value without the
-- spaces around it. Fails, without an error, on the empty line that ends
-- the headers.
headerLine :: FP.Parser String Header
headerLine = do
  FP.fails lineEnd
  line "a header line is not a name, a colon and a value" $ do
    name <- FP.byteStringOf (FP.skipSome (satisfyByte isTokenByte))
    FP.word8 colon
    value <- trimmed <$> FP.takeRest
    pure (lowerAscii name, value)

-- | The line that starts a chunk: its size in hexadecimal, then extensions
-- that are ignored.
chunkSizeLine :: FP.Parser String Int
chunkSizeLine = line "a chunk does not start with its size in hexadecimal" $ do
  digits <- FP.byteStringOf (FP.skipSome (satisfyByte isHexDigit))
  -- Fifteen digits always fit in an Int.
  when (BS.length digits > 15) FP.failed
  void FP.takeRest
  pure (BS.foldl' (\total byte -> total * 16 + hexValue byte) 0 digits)
  where
    isHexDigit byte = isDigit byte || (byte >= 65 && byte <= 70) || (byte >= 97 && byte <= 102)
    hexValue byte
      | isDigit byte = fromIntegral (byte - 48)
      | byte >= 97 = fromIntegral (byte - 87)
      | otherwise = fromIntegral (byte - 55)

-- | A whole line, read by a parser that must read all of it, the line
-- feed left out. It fails while the line feed has not come, and stops on
-- an error, with a reason, when the line is there and is not what the
-- parser reads.
line :: String -> FP.Parser String a -> FP.Parser String a
line problem content = do
  size <- FP.lookahead (BS.length <$> FP.byteStringOf (FP.skipMany (satisfyByte (/= lineFeed))) <* FP.word8 lineFeed)
  found <- FP.isolate size content `FP.cut` problem
  FP.word8 lineFeed
  pure found

-- | The end of a line: a line feed, after carriage returns if any.
lineEnd :: FP.Parser String ()
lineEnd = FP.skipMany (FP.word8 carriageReturn) *> FP.word8 lineFeed

-- | The end of the line that follows the data of a chunk: an error if
-- something else is there.
chunkEnd :: FP.Parser String ()
chunkEnd = FP.skipMany (FP.word8 carriageReturn) *> (FP.word8 lineFeed <|> (FP.lookahead FP.anyWord8 *> FP.err "the data of a chunk is not followed by a line end"))

satisfyByte :: (Word8 -> Bool) -> FP.Parser String ()
satisfyByte accepted = FP.anyWord8 >>= \byte -> if accepted byte then pure () else FP.failed

digit :: FP.Parser String Int
digit = FP.anyWord8 >>= \byte -> if isDigit byte then pure (fromIntegral (byte - 48)) else FP.failed

-- | Without the spaces, the tabs and the carriage returns around.
trimmed :: ByteString -> ByteString
trimmed = BS.dropWhile blank . BS.dropWhileEnd blank
  where
    blank byte = byte == space || byte == tab || byte == carriageReturn

-- | The bytes allowed in a method or a header name (RFC 9110, tchar).
isTokenByte :: Word8 -> Bool
isTokenByte byte =
  isDigit byte
    || (byte >= 65 && byte <= 90)
    || (byte >= 97 && byte <= 122)
    || BS.elem byte "!#$%&'*+-.^_`|~"

isDigit :: Word8 -> Bool
isDigit byte = byte >= 48 && byte <= 57

lowerAscii :: ByteString -> ByteString
lowerAscii = BS.map (\byte -> if byte >= 65 && byte <= 90 then byte + 32 else byte)

space, tab, colon, lineFeed, carriageReturn :: Word8
space = 32
tab = 9
colon = 58
lineFeed = 10
carriageReturn = 13
