# streamly-http

A small HTTP/1.1 client written with [Streamly](https://streamly.composewell.com/).
The response head is read with Streamly parsers; the body is a stream of
byte arrays, whether it is delimited by `Content-Length`, by the chunked
transfer coding or by the end of the connection.

```haskell
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream qualified as Stream
import Streamly.External.Network.HTTP

main :: IO ()
main = do
  -- The whole response in memory.
  response <- fetch (get "example.org" 80 "/")
  print (response.status, header "content-type" response)

  -- The body as a stream, read while the connection is open.
  arrays <- withResponse (get "example.org" 80 "/") $ \streamed ->
    Stream.fold Fold.length streamed.body
  print arrays
```

## Modules

- `Streamly.External.Network.HTTP`: the client over TCP (`get`, `fetch`,
  `withResponse`).
- `Streamly.External.Network.HTTP.Wire`: the message format alone
  (`renderRequest`, `readResponse`), to run it over another transport.

## Limits of this version

- Plain TCP only: no TLS.
- One connection per request (`Connection: close`).
- Redirects are not followed and content codings are not decoded.
- No URL parsing: a request names its host, its port and its target.
- No limit on time or size: use `System.Timeout.timeout`, and count the
  bytes of the body stream if the server is not trusted.
- A status is never an error: look at `status`.
