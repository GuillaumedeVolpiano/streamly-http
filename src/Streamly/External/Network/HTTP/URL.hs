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
import Control.Monad (void, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Char (toLower)
import Data.Functor.Identity (Identity (..))
import Data.Word (Word8)
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Parser (Parser)
import Streamly.Data.Parser qualified as Parser
import Streamly.Data.Stream qualified as Stream
import Streamly.External.ByteString qualified as StreamlyBS
import Streamly.External.Network.HTTP.Wire (Request (..), RequestBody (..))

-- | Whether the connection is in clear or over TLS.
data Scheme
  = Http
  | Https
  deriving stock (Eq, Show)

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
parseUrl bytes = case runIdentity (Stream.parse url (Stream.unfold StreamlyBS.reader bytes)) of
  Left (Parser.ParseError problem) -> Left ("not an http or https URL: " <> problem)
  Right found -> Right found

-- | The parser behind 'parseUrl'.
url :: Monad m => Parser Word8 m Url
url = do
  scheme <- (Https <$ keyword "https://") <|> (Http <$ keyword "http://")
  host <- bracketed <|> named
  port <- (Parser.satisfy (== colon) *> number) <|> pure (defaultPort scheme)
  path <- Parser.takeWhile (\byte -> byte /= question && byte /= hash) Fold.toList
  query <- Parser.takeWhile (/= hash) Fold.toList
  void (Parser.takeWhile (const True) Fold.drain)
  when (any (\byte -> byte <= 32 || byte == 127) (path <> query)) (Parser.die "a space or a control character")
  case path of
    first : _ | first /= slash -> Parser.die "something between the host and the path"
    _ -> pure ()
  pure Url {scheme, host, port, target = BS.pack ((if null path then [slash] else path) <> query)}
  where
    keyword text = void (Parser.listEqBy (\one other -> lowerByte one == lowerByte other) (BS.unpack text))
    -- An IPv6 address between brackets.
    bracketed = do
      void (Parser.satisfy (== 91))
      address <- Parser.takeWhile1 (\byte -> isHexDigit byte || byte == colon || byte == 46) Fold.toList
      void (Parser.satisfy (== 93))
      pure (map toLower (BS8.unpack (BS.pack address)))
    named = do
      name <- Parser.takeWhile1 hostByte Fold.toList
      pure (map toLower (BS8.unpack (BS.pack name)))
    -- Letters, digits and what else a name or an IPv4 address may hold.
    -- An at sign, which would end a user name, is not among them.
    hostByte byte = isAlphaNum byte || BS.elem byte "-._~%!$&'()*+,;="
    number = do
      digits <- Parser.takeWhile1 isDigit Fold.toList
      let value = foldl' (\total digit -> total * 10 + fromIntegral (digit - 48)) 0 digits :: Int
      when (length digits > 5 || value < 1 || value > 65535) (Parser.die "a port out of range")
      pure value

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
