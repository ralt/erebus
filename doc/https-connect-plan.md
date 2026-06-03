# Implementation plan: HTTPS `CONNECT` tunnelling

Status: **planned** (roadmap Phase 10). This is a design note, not yet
implemented. It records the investigation behind the "Can it proxy HTTPS?"
limitation so the work can be picked up later without re-deriving it.

## Goal

Let the outbound proxy handle the HTTP `CONNECT` method, so a local client can
open an HTTPS (or any TLS / arbitrary-TCP) connection to a VPN resource through
erebus. Today the proxy speaks plain HTTP/1.x only; `CONNECT` is the standard
way an explicit proxy tunnels opaque bytes.

A `CONNECT` proxy does exactly two things:

1. reply `200 Connection Established` to the local client, then
2. become a blind, byte-for-byte two-way pipe between the local client and the
   target `host:port`.

We never terminate TLS — the client's handshake and encrypted data flow straight
through. This is a tunnel, like any real proxy's `CONNECT`.

## Why this is small

Both halves already exist in the codebase; the work is mostly gluing them
together.

- **The byte pipe already exists.** `%relay (vpn-socket os-socket)` in
  `src/socket.lisp` pumps bytes both ways between a user-space VPN socket and an
  OS socket until either side closes. It is what inbound port-forwarding uses.
  `CONNECT` is the mirror image: the VPN socket is an *outbound* `socket-connect`
  to the `CONNECT` target, and the OS socket is the *local client*. Same
  function, roles swapped.
- **Opening the outbound connection already exists.** `socket-connect` (active
  open) and `%resolve-hostname` are already used by the plain-HTTP path in
  `src/http.lisp`.
- **Hunchentoot does not fight a raw socket takeover.** Verified against
  `hunchentoot-v1.3.1`:
  - `CONNECT` is in `+valid-request-methods+` (`headers.lisp`), so the request
    parses cleanly and `(h:request-method request)` returns `:connect`. The
    `host:port` authority-form URI does not choke the parser.
  - `hunchentoot:detach-socket` (exported, `acceptor.lisp`) is a built-in
    takeover hook: it sets `*finish-processing-socket*` and leaves
    `*close-hunchentoot-stream*` NIL, so after the handler returns we own the
    socket and Hunchentoot will not close it or try to read another request.
  - Setting `hunchentoot::*headers-sent*` to `t` inside the handler suppresses
    Hunchentoot's automatic response (`request.lisp` only calls `start-output`
    `unless *headers-sent*`). This is the same recipe websocket libraries use.

## Sketch

In `acceptor-dispatch-request` (`src/http.lisp`), branch on the method:

```lisp
(if (eq (h:request-method request) :connect)
    (%handle-connect a request)
    ;; ... existing plain-HTTP path ...
    )
```

`%handle-connect` then:

```
1. parse host:port from (h:request-uri request)          ; authority form, e.g. "example.internal:443"
2. vpn = (socket-connect (%client a)
                         :host (%resolve-hostname host)
                         :port port)                      ; reuse the existing active open
3. write "HTTP/1.1 200 Connection Established\r\n\r\n"    ; to the raw client stream
   to the local client; finish-output
4. (setf hunchentoot::*headers-sent* t)                  ; suppress Hunchentoot's auto-response
5. (h:detach-socket a)                                   ; we own the socket from here
6. (%relay vpn <local-client-socket>)                    ; reuse the existing relay, roles mirrored
7. close both sides (see "Closing" below)
```

## The one fiddly bit: client-side readiness polling

`%relay` calls `usocket:wait-for-input` on the OS socket to pump without
busy-spinning. Hunchentoot's `request` object does **not** expose the underlying
`usocket` socket via a reader (only `remote-addr`/`remote-port`/`content-stream`,
see `request.lisp`). Two ways around it:

- **Preferred:** override `acceptor-make-request` on the erebus `acceptor` to
  stash the `socket` argument (which `process-connection` passes in) on a request
  subclass slot, then hand that socket to `%relay` unchanged. About three lines,
  and lets `%relay` be reused verbatim.
- **Alternative:** write a `%relay` variant that uses `listen` on the request's
  `content-stream` instead of `wait-for-input` on a socket. Simpler to wire, but
  `listen` is slightly less reliable through Hunchentoot's stream wrappers.

## Closing

`detach-socket` leaves `*close-hunchentoot-stream*` NIL, so Hunchentoot will
**not** close the client stream — `%handle-connect` is responsible for closing
both the VPN socket (`socket-close`) and the local client socket itself, in an
`unwind-protect`, mirroring how `%handle-exposed-connection` cleans up today.

## Constraints and caveats (carry these into the docs when shipped)

- **TLS stays opaque.** We do not terminate or inspect it. No certificate
  handling, no TLS-to-backend on our side — the bytes are just relayed. (That is
  the point; it is what makes this small.)
- **Performance.** The user-space TCP stack is stop-and-wait (one piece at a
  time, wait for confirmation). TLS handshakes are turn-based and fine, but a
  large HTTPS download is the same "several times slower than a normal proxy"
  story as plain HTTP today. Functional, not fast. See
  [`stress-results.md`](stress-results.md).
- **Half-duplex relay.** `%relay` polls each side in turn rather than streaming
  both directions at once (the stack's one-queue-per-connection rule). Fine for
  request/response HTTPS; heavy simultaneous bidirectional streaming inside the
  tunnel would feel the serialization.
- **Fragmentation is already handled.** The stack segments large writes to fit
  the tun MTU, so big TLS records are not a new problem.

## Testing

Add a docker-based test mirroring the existing proxy tests: run an HTTPS backend
inside the openvpn container (e.g. nginx with a self-signed cert), then from the
host do `https_proxy=http://127.0.0.1:11023 curl -k https://<vpn-ip>/` and assert
the body round-trips. Reuse the container/test helpers in `t/package.lisp`.

## Docs to update when shipped

- `README.md` — the "What works less well" / proxy bullets.
- `doc/erebus.1` — the `[proxy-out]` description and the LIMITATIONS section
  (currently states "no HTTPS `CONNECT` tunnelling").
- `site/index.html` — the "Can it proxy HTTPS?" FAQ and the "wrong tool" list.
- `ROADMAP.md` — move this item from Phase 10's list to done.
