-- |
-- Module      : Streamly.External.Network.HTTP.URL
-- Description : The URLs of HTTP, read and resolved
-- License     : BSD-3-Clause
--
-- URLs with the schemes @http@ and @https@ (RFC 3986, RFC 9110 section
-- 4.2): reading one, writing it back, resolving the reference of a
-- redirect against it, and making a request for it.
--
-- A URL is taken as it is written: nothing is percent-encoded or decoded.
module Streamly.External.Network.HTTP.URL
  ( Url (..)
  , Scheme (..)
  , defaultPort
  , parseUrl
  , renderUrl
  , resolve
  , urlRequest

    -- * Parser
  , url
  ) where

import Control.Applicative ((<|>))
import Control.Monad (unless, void, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Char (toLower)
import Data.Word (Word8)
import FlatParse.Basic qualified as FP
import Streamly.External.Network.HTTP.Wire (Request (..), RequestBody (..))

-- | Whether the connection is in clear or over TLS.
data Scheme
  = Http
  | Https
  deriving stock (Eq, Ord, Show)

-- | The port of a scheme when the URL names none: 80 and 443.
defaultPort :: Scheme -> Int
defaultPort = \case
  Http -> 80
  Https -> 443

-- | A URL, without its fragment.
data Url = Url
  { scheme :: Scheme
  , host :: String
  -- ^ In lower case; an IPv6 address without its brackets.
  , port :: Int
  , target :: ByteString
  -- ^ The path and the query, @\/@ at least.
  }
  deriving stock (Eq, Show)

-- | Read a URL such as @https:\/\/example.org:8443\/a\/b?c=d#e@. The
-- fragment is dropped. A URL with a user name is refused, and so is one
-- that holds a space or a control character.
parseUrl :: ByteString -> Either String Url
parseUrl bytes = case FP.runParser url bytes of
  FP.OK found _ -> Right found
  FP.Fail -> Left "not an http or https URL"
  FP.Err problem -> Left ("not an http or https URL: " <> problem)

-- | The parser behind 'parseUrl'. It reads the input to its end.
url :: FP.Parser String Url
url = do
  scheme <- (Https <$ keyword "https://") <|> (Http <$ keyword "http://")
  host <- bracketed <|> named
  port <- (FP.word8 colon *> number) <|> pure (defaultPort scheme)
  path <- FP.byteStringOf (FP.skipMany (satisfyByte (\byte -> byte /= question && byte /= hash)))
  query <- FP.byteStringOf (FP.skipMany (satisfyByte (/= hash)))
  void FP.takeRest
  when (BS.any (\byte -> byte <= 32 || byte == 127) (path <> query)) (FP.err "a space or a control character")
  unless (BS.null path || BS.head path == slash) (FP.err "something between the host and the path")
  pure Url {scheme, host, port, target = (if BS.null path then BS.singleton slash else path) <> query}
  where
    keyword = mapM_ (\expected -> satisfyByte ((== expected) . lowerByte)) . BS.unpack
    -- An IPv6 address between brackets.
    bracketed = do
      FP.word8 91
      address <- FP.byteStringOf (FP.skipSome (satisfyByte (\byte -> isHexDigit byte || byte == colon || byte == 46)))
      FP.word8 93
      pure (map toLower (BS8.unpack address))
    named = map toLower . BS8.unpack <$> FP.byteStringOf (FP.skipSome (satisfyByte hostByte))
    -- Letters, digits and what else a name or an IPv4 address may hold.
    -- An at sign, which would end a user name, is not among them.
    hostByte byte = isAlphaNum byte || BS.elem byte "-._~%!$&'()*+,;="
    number = do
      digits <- FP.byteStringOf (FP.skipSome (satisfyByte isDigit))
      let value = BS.foldl' (\total byte -> total * 10 + fromIntegral (byte - 48)) 0 digits :: Int
      if BS.length digits > 5 || value < 1 || value > 65535 then FP.failed else pure value
    satisfyByte accepted = FP.anyWord8 >>= \byte -> if accepted byte then pure () else FP.failed

-- | Write a URL, the port left out when it is the one of the scheme.
renderUrl :: Url -> ByteString
renderUrl found =
  BS.concat
    [ case found.scheme of Http -> "http://"; Https -> "https://"
    , if ':' `elem` found.host then "[" <> BS8.pack found.host <> "]" else BS8.pack found.host
    , if found.port == defaultPort found.scheme then "" else ":" <> BS8.pack (show found.port)
    , found.target
    ]

-- | Resolve a reference, such as the @Location@ of a redirect, against the
-- URL it was found at: a whole URL, @\/\/host\/path@, @\/path@, @?query@
-- or a path relative to the one of the URL.
resolve :: Url -> ByteString -> Either String Url
resolve base reference
  | BS.null trimmed = Right base
  | "//" `BS.isPrefixOf` trimmed = parseUrl (schemeName <> ":" <> trimmed)
  | "/" `BS.isPrefixOf` trimmed = at (removeDots path <> query)
  | "?" `BS.isPrefixOf` trimmed = at (basePath <> trimmed)
  | hasScheme = parseUrl trimmed
  | otherwise = at (removeDots (directory <> path) <> query)
  where
    -- The reference, without its fragment.
    trimmed = BS.takeWhile (/= hash) reference
    (path, query) = BS.break (== question) trimmed
    basePath = BS.takeWhile (/= question) base.target
    directory = BS.dropWhileEnd (/= slash) basePath
    schemeName = case base.scheme of Http -> "http"; Https -> "https"
    hasScheme =
      let (name, rest) = BS.span (\byte -> isAlphaNum byte || BS.elem byte "+-.") trimmed
       in not (BS.null name) && ":" `BS.isPrefixOf` rest
    at target
      | BS.any (\byte -> byte <= 32 || byte == 127) target = Left "a space or a control character in a reference"
      | otherwise = Right (Url base.scheme base.host base.port target)

-- | Remove the @.@ and @..@ segments of an absolute path (RFC 3986,
-- section 5.2.4).
removeDots :: ByteString -> ByteString
removeDots path = "/" <> BS.intercalate "/" (reverse kept) <> (if trailing then "/" else "")
  where
    segments = BS.split slash (BS.drop 1 path)
    trailing = case reverse segments of
      final : _ -> final `elem` ["", ".", ".."] && path /= "/"
      [] -> False
    kept = foldl' step [] segments
    step done segment
      | segment == "." || segment == "" = done
      | segment == ".." = drop 1 done
      | otherwise = segment : done

-- | A @GET@ request for a URL, without headers.
urlRequest :: Url -> Request
urlRequest found = Request {method = "GET", host = found.host, port = found.port, target = found.target, headers = [], body = NoBody}

isDigit, isHexDigit, isAlphaNum :: Word8 -> Bool
isDigit byte = byte >= 48 && byte <= 57
isHexDigit byte = isDigit byte || (lowerByte byte >= 97 && lowerByte byte <= 102)
isAlphaNum byte = isDigit byte || (lowerByte byte >= 97 && lowerByte byte <= 122)

lowerByte :: Word8 -> Word8
lowerByte byte = if byte >= 65 && byte <= 90 then byte + 32 else byte

colon, slash, question, hash :: Word8
colon = 58
slash = 47
question = 63
hash = 35
