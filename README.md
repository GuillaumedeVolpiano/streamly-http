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
- `Streamly.External.Network.HTTP.URL`: `http` and `https` URLs, read,
  written back and resolved against one another.

## Limits of this version

- TLS is not built in. `withResponseOver` and `fetchOver` take a connection
  opened by other means, as a `Transport`: one of
  [streamly-network-tls](https://github.com/GuillaumedeVolpiano/streamly-network-tls)
  gives HTTPS.
- One connection per request (`Connection: close`).
- Redirects are not followed and content codings are not decoded.
- A request names its host, its port and its target. URLs are read by
  `Streamly.External.Network.HTTP.URL`; fetching by URL, with the scheme
  choosing between TCP and TLS, is what
  [streamly-http-tls](https://github.com/GuillaumedeVolpiano/streamly-http-tls)
  adds.
- Each wait is limited (`Limits`: thirty seconds to connect, thirty
  seconds of silence by default), but not the whole exchange nor the size
  of a response: use `System.Timeout.timeout`, and count the bytes of the
  body stream if the server is not trusted.
- A status is never an error: look at `status`.
