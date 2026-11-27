# Revision history for streamly-http

## 0.3.0.2 -- 2026-11-27

* No change of interface. The dependency bounds are stated once in the
  cabal file, and the imports list the names they use.
* The examples of the documentation import the names they use; each was
  typechecked.

## 0.3.0.1 -- 2026-11-27

* Builds with GHC 9.8 and 9.10 as well as 9.12: the language is GHC2021
  with the extensions GHC2024 adds, and the bounds on `base` and on the
  libraries that come with the compiler are wider.
* `foldl'` is taken from `Data.List`: the Prelude only has it since
  GHC 9.10.
* The links of the documentation say which name they mean, where a type
  and its constructor, or two modules, share one.

## 0.3.0.0 -- 2026-11-21

* The parsers are flatparse parsers, run through streamly-flatparse: a
  head is read about seventeen times faster. `responseHead`, `requestHead`,
  `chunkSizeLine` and `url` change their type. A response that is not HTTP
  is refused at its first line.

## 0.2.0.0 -- 2026-11-21

* Time limits: `Limits`, with thirty seconds to connect and thirty seconds
  of silence by default, in `withResponse` and `fetch`; `withResponseWithin`
  and `fetchWithin` take others. `limitTransport` gives them to a transport
  of the caller, and `connectWithin` to a TCP connection.
* Size limits in `Limits`: the head of a response is at most 64 KiB by
  default (`maxHeadSize`); the body can be bounded (`maxBodySize`, no limit
  by default). `readResponseWithin` and `readResponseHeadWithin` take them
  at the level of the message format.
* `withResponseOver` and `fetchOver` take the `Limits`.
* Sessions: `withSession` keeps the connection to a server from one
  request to the next (`withResponseIn`, `fetchIn`), and opens another
  when the server has closed it; `withSessionOver` for other transports.
  `readResponseReusable` tells where a response ends on its connection.
* Built on streamly-network-tcp, which now holds `Transport`,
  `socketTransport` and `connect`; they are still exported from here.
* Clients: `withClient` keeps sessions for every server asked
  (`withResponseFrom`, `fetchFrom`), several for one server when requests
  overlap; `withClientOver` for other transports.
* The head of a request, as a server reads it: `RequestHead`,
  `readRequestHeadWithin`. `HttpError` gains `MalformedRequest`.
* `HttpError` gains `ConnectTimedOut`, `ResponseTimedOut`, `HeadTooLarge`
  and `BodyTooLarge`.

## 0.1.0.0 -- 2026-11-21

* An HTTP/1.1 client over TCP: `get`, `fetch`, `withResponse`.
* The message format on streams of byte arrays: `renderRequest`,
  `readResponse`. Bodies delimited by `Content-Length`, by the chunked
  transfer coding or by the end of the connection.
* Requests over a connection of the caller (`Transport`,
  `withResponseOver`, `fetchOver`), for HTTPS among others.
* `http` and `https` URLs: `parseUrl`, `renderUrl`, `resolve`,
  `urlRequest`.
