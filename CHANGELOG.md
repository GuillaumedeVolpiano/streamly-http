# Revision history for streamly-http

## 0.1.0.0 -- unreleased

* An HTTP/1.1 client over TCP: `get`, `fetch`, `withResponse`.
* The message format on streams of byte arrays: `renderRequest`,
  `readResponse`. Bodies delimited by `Content-Length`, by the chunked
  transfer coding or by the end of the connection.
* Requests over a connection of the caller (`Transport`,
  `withResponseOver`, `fetchOver`), for HTTPS among others.
* `http` and `https` URLs: `parseUrl`, `renderUrl`, `resolve`,
  `urlRequest`.
