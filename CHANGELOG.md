# Revision history for streamly-http

## 0.1.0.0 -- 2026-11-28

* First release on Hackage.
* An HTTP/1.1 client over TCP: `get`, `fetch`, `withResponse`. Bodies
  delimited by `Content-Length`, by the chunked transfer coding or by the
  end of the connection; heads read with flatparse parsers.
* Limits of time and of size (`Limits`), on every wait, on the head of a
  response and, when asked, on its body.
* Sessions (`withSession`) and clients (`withClient`), which keep
  connections from one request to the next.
* Requests over a connection of the caller (`Transport`,
  `withResponseOver`, `fetchOver`), for HTTPS among others.
* The message format alone, on streams of byte arrays
  (`Streamly.External.Network.HTTP.Wire`), which also reads the head of a
  request.
* `http` and `https` URLs: `parseUrl`, `renderUrl`, `resolve`,
  `urlRequest`.
