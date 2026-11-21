# Revision history for streamly-http

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
