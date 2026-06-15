# Implementation plan: HTTPS `CONNECT` tunnelling

Status: **implemented** (roadmap Phase 10).

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

## Step-by-step implementation

### Step 1 — Expose the Hunchentoot socket to the handler

`%relay` needs the raw `usocket` for `wait-for-input`. `acceptor-dispatch-request` only receives a `hunchentoot:request`, which does not expose the socket.

In `src/http.lisp`, capture the socket in a special variable from a
`process-connection :around` method (which receives the socket as its second
argument) so the handler can reach it:

```lisp
(defvar *http-client-socket* nil)

(defmethod h:process-connection :around ((a acceptor) socket)
  (let ((*http-client-socket* socket))
    (call-next-method)))
```

`process-connection` wraps the whole request, so `*http-client-socket*` is
bound for the duration of `%handle-connect`.

### Step 2 — Add `%handle-connect` in `src/http.lisp`

Branch on method at the top of `acceptor-dispatch-request`:

```lisp
(defmethod h:acceptor-dispatch-request ((a acceptor) request)
  (if (eq (h:request-method request) :connect)
      (%handle-connect a request)
      (multiple-value-bind (host port)
          (%parse-host-header request)
        ;; ... existing plain-HTTP path unchanged ...
        )))
```

`%handle-connect` does:

```
1. Parse host:port from (h:request-uri request)       ; authority-form, e.g. "example.internal:443"
2. vpn = (socket-connect (%client a)
                          :host (%resolve-hostname host)
                          :port port)                   ; reuse the existing active open
3. Write the CONNECT success status line          ; to (u:socket-stream *http-client-socket*)
   finish-output                                  ; NB: build it with explicit #\Return #\Linefeed --
                                                  ; "\r\n" is NOT an escape in CL string literals
4. (setf hunchentoot::*headers-sent* t)                ; suppress Hunchentoot's auto-response
5. (h:detach-socket a)                                  ; we own the socket from here
6. os-socket = *http-client-socket*                     ; the captured usocket
7. (%relay vpn os-socket)                               ; reuse the existing relay, roles mirrored
8. Close both sides in unwind-protect (socket-close vpn and u:socket-close os-socket)
```

The raw client socket is the one captured in `*http-client-socket*`.

### Step 3 — No changes to `socket.lisp`

`%relay`, `socket-connect`, `%resolve-hostname` are all reused as-is.

### Step 4 — Test

Add a test. Two options, simplest first:

**Option A (minimal):** Spin up a TLS echo server on the VPN side via the container `pre` hook, `CONNECT` through the proxy, write bytes, read them back and assert round-trip.

**Option B (fuller):** Configure nginx in the docker container to also listen on 443 with a self-signed cert, then from the host do `https_proxy=http://127.0.0.1:<port> curl -k https://10.8.0.1/` and assert 200 + body.

Reuse the existing `with-docker-container`, `with-test-client`, `with-proxy` helpers from `t/package.lisp`. Place the test in `t/openvpn-statickey.lisp` (or a new file if the test setup is large enough to warrant one).

### Step 5 — Documentation updates

- `README.md` — update the proxy capability bullets.
- `doc/erebus.1` — remove the "no HTTPS CONNECT tunnelling" sentence from the LIMITATIONS section (lines 283-287).
- `docs/index.html` — update the FAQ "Can it proxy HTTPS?" answer from "Not yet" to "Yes, via CONNECT tunnelling (opaque byte relay)."
- `ROADMAP.md` — mark HTTPS CONNECT as done in Phase 10.
- `doc/https-connect-plan.md` — update status from "ready to implement" to "implemented."

## Constraints and caveats

- **TLS stays opaque.** We do not terminate or inspect it. No certificate
  handling, no TLS-to-backend on our side — the bytes are just relayed.
- **Performance.** The user-space TCP stack is stop-and-wait. TLS handshakes are
  turn-based and fine, but a large HTTPS download is the same "several times
  slower than a normal proxy" story as plain HTTP today. Functional, not fast.
- **Half-duplex relay.** `%relay` polls each side in turn rather than streaming
  both directions at once. Fine for request/response HTTPS; heavy simultaneous
  bidirectional streaming inside the tunnel would feel the serialization.
- **Fragmentation is already handled.** The stack segments large writes to fit
  the tun MTU, so big TLS records are not a new problem.

## Estimated scope

| File | Change |
|---|---|
| `src/http.lisp` | ~25 new lines (request subclass, `%handle-connect`, method dispatch) |
| `t/openvpn-statickey.lisp` | ~40 lines (test) |
| `t/Dockerfile` | Maybe: self-signed cert generation in the container |
| `README.md`, `doc/erebus.1`, `docs/index.html`, `ROADMAP.md` | One sentence each |
