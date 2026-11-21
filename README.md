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

## Several requests to one server

```haskell
import Streamly.External.Network.HTTP

main :: IO ()
main = withSession defaultLimits "example.org" 80 $ \session -> do
  first <- fetchIn session (get "example.org" 80 "/a")
  second <- fetchIn session (get "example.org" 80 "/b")
  print (first.status, second.status)
```

The connection is kept when the body was read to its end, its length was
announced or it was chunked, and the server did not ask to close. A
connection that the server has closed while idle is replaced without a
word: the request is sent again once, unless its body is a stream.

For several servers, `withClient` keeps a session for each, and more than
one for a server whose requests overlap:

```haskell
main :: IO ()
main = withClient defaultLimits $ \client -> do
  first <- fetchFrom client Http (get "example.org" 80 "/a")
  second <- fetchFrom client Http (get "example.net" 80 "/b")
  print (first.status, second.status)
```

## Modules

- `Streamly.External.Network.HTTP`: the client over TCP (`get`, `fetch`,
  `withResponse`).
- `Streamly.External.Network.HTTP.Wire`: the message format alone
  (`renderRequest`, `readResponse`), to run it over another transport. It
  also reads the head of a request, for the servers of protocols that
  start with an HTTP exchange.
- `Streamly.External.Network.HTTP.URL`: `http` and `https` URLs, read,
  written back and resolved against one another.

## Limits of this version

- TLS is not built in. `withResponseOver` and `fetchOver` take a connection
  opened by other means, as a `Transport`: one of
  [streamly-network-tls](https://github.com/GuillaumedeVolpiano/streamly-network-tls)
  gives HTTPS.
- One connection per request (`Connection: close`), unless the requests go
  through a session: `withSession` keeps the connection to a server for
  the next request when the response allows it.
- Redirects are not followed and content codings are not decoded.
- A request names its host, its port and its target. URLs are read by
  `Streamly.External.Network.HTTP.URL`; fetching by URL, with the scheme
  choosing between TCP and TLS, is what
  [streamly-http-tls](https://github.com/GuillaumedeVolpiano/streamly-http-tls)
  adds.
- `Limits` bounds each wait (thirty seconds to connect, thirty seconds of
  silence by default) and the head of a response (64 KiB by default). The
  whole exchange is not bounded: use `System.Timeout.timeout`. The body is
  not either by default: set `maxBodySize` when the server is not trusted.
- A status is never an error: look at `status`.

## Building

[streamly-network-tcp](https://github.com/GuillaumedeVolpiano/streamly-network-tcp) is not on Hackage yet: `cabal.project` fetches it from GitHub, at its
release. To build against a copy checked out next to this repository
instead, pass `--project-file=cabal.project.siblings`.
