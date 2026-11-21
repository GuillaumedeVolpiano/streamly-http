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
  , readResponseHead

    -- * Errors
  , HttpError (..)

    -- * Parsers
  , responseHead
  , chunkSizeLine
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless, void, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Word (Word8)
import Numeric (showHex)
import Streamly.Data.Array (Array)
import Streamly.Data.Array qualified as Array
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Parser (Parser)
import Streamly.Data.Parser qualified as Parser
import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream qualified as Stream
import Streamly.Data.StreamK (StreamK)
import Streamly.Data.StreamK qualified as StreamK
import Streamly.External.ByteString qualified as StreamlyBS

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
          <> concatMap line (hostHeader <> kept <> framing)
          <> ["\r\n"]
    line (name, value) = [name, ": ", value, "\r\n"]
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
readResponse method input = do
  (found, rest) <- readResponseHead input
  framing <- either throwIO pure (bodyFraming method found)
  pure (bodyStream framing rest <$ found)

-- | Read the head of a response, and give what follows it in the input:
-- the body, or after a status 101 the bytes of the protocol switched to.
-- Interim responses (status 1xx, except 101) are skipped.
--
-- Throws 'MalformedResponse'.
readResponseHead :: StreamK IO (Array Word8) -> IO (Response (), StreamK IO (Array Word8))
readResponseHead input = do
  (found, rest) <- parseOrThrow responseHead input
  if found.status >= 100 && found.status < 200 && found.status /= 101
    then readResponseHead rest
    else pure (found, rest)

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

bodyStream :: Framing -> StreamK IO (Array Word8) -> Stream IO (Array Word8)
bodyStream framing input = case framing of
  Empty -> Stream.nil
  UntilClose -> StreamK.toStream input
  Fixed n -> Stream.unfoldrM step (InData n input False)
  Chunked -> Stream.unfoldrM step (AtChunkSize input)
  where
    step (AtChunkSize from) = do
      (size, rest) <- parseOrThrow chunkSizeLine from
      if size == 0 then pure Nothing else step (InData size rest True)
    step (InData 0 from chunked)
      | chunked = do
          ((), rest) <- parseOrThrow lineEnd from
          step (AtChunkSize rest)
      | otherwise = pure Nothing
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

parseOrThrow :: Parser Word8 IO a -> StreamK IO (Array Word8) -> IO (a, StreamK IO (Array Word8))
parseOrThrow parser input = do
  (outcome, rest) <- Array.parseBreak (Array.toParserK parser) input
  case outcome of
    Left (Parser.ParseError problem) -> throwIO (MalformedResponse problem)
    Right value -> pure (value, rest)

-- | The status line, the header lines and the empty line that ends them.
-- The names of the headers are given in lower case, the values without the
-- spaces around them. Lines end with a line feed, after an optional
-- carriage return. Header lines folded over several lines are refused.
responseHead :: Monad m => Parser Word8 m (Response ())
responseHead = do
  void (Parser.listEq (BS.unpack "HTTP/1."))
  void (Parser.satisfy isDigit)
  void (Parser.satisfy (== space))
  digits <- Parser.takeWhile1 isDigit Fold.toList
  when (length digits /= 3) (Parser.die "the status code does not have three digits")
  reason <- restOfLine
  found <- Parser.many headerLine Fold.toList
  lineEnd
  pure Response {status = foldl' (\total digit -> total * 10 + fromIntegral (digit - 48)) 0 digits, reason, headers = found, body = ()}
  where
    headerLine = do
      name <- Parser.takeWhile1 isTokenByte Fold.toList
      void (Parser.satisfy (== colon))
      value <- restOfLine
      pure (lowerAscii (BS.pack name), value)

-- | The line that starts a chunk: its size in hexadecimal, then extensions
-- that are ignored.
chunkSizeLine :: Monad m => Parser Word8 m Int
chunkSizeLine = do
  digits <- Parser.takeWhile1 isHexDigit Fold.toList
  -- Fifteen digits always fit in an Int.
  when (length digits > 15) (Parser.die "the chunk size is too large")
  void restOfLine
  pure (foldl' (\total digit -> total * 16 + hexValue digit) 0 digits)
  where
    isHexDigit byte = isDigit byte || (byte >= 65 && byte <= 70) || (byte >= 97 && byte <= 102)
    hexValue byte
      | isDigit byte = fromIntegral (byte - 48)
      | byte >= 97 = fromIntegral (byte - 87)
      | otherwise = fromIntegral (byte - 55)

-- | The bytes up to the end of the line, without the spaces around them.
restOfLine :: Monad m => Parser Word8 m ByteString
restOfLine = do
  bytes <- Parser.takeWhile (/= lineFeed) Fold.toList
  void (Parser.satisfy (== lineFeed))
  pure (BS.dropWhile blank (BS.dropWhileEnd blank (BS.pack bytes)))
  where
    blank byte = byte == space || byte == tab || byte == carriageReturn

lineEnd :: Monad m => Parser Word8 m ()
lineEnd = do
  Parser.dropWhile (== carriageReturn)
  void (Parser.satisfy (== lineFeed))

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
