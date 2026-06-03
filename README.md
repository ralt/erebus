# erebus

This project is an experimental, user-space VPN client that exposes access to private networks through a local HTTP proxy.

Instead of configuring system-wide tunnels, kernel interfaces, or requiring elevated privileges, erebus runs entirely in user space and forwards traffic explicitly through a proxy interface.

The initial focus is interoperability with OpenVPN servers (in both UDP and TCP modes), with longer-term support planned for IPsec (ESP) and IKE.

The primary goal is the ability to converse with VPNs in constrained environments.

## What this is

At a high level, erebus implements:

- a rootless VPN client
- user-space TCP/IP handling for VPN traffic
- outbound access to VPN resources via a local HTTP proxy
- inbound access, allowing local services to be exposed to the VPN

Rather than modifying the host networking stack, erebus:

- establishes VPN sessions itself
- constructs and parses IP packets in user space
- maintains minimal TCP state for both outbound and inbound connections
- forwards HTTP traffic between local clients or services and the VPN

The result is a VPN agent rather than a system-wide VPN.

## Why this exists

Traditional VPN clients assume:
- control over the host networking stack
- kernel support (TUN/TAP, XFRM)
- administrative privileges
- global routing changes

That model does not work well in many environments:
- containers
- sandboxed applications
- CI systems
- developer tooling
- restricted or multi-tenant hosts

erebus explores a different approach:

- no root privileges required
- no kernel tunneling
- explicit, opt-in connectivity through a proxy
- bidirectional traffic: outbound to VPN resources, inbound exposing local services

It also aims to make VPN protocols easier to understand by implementing them incrementally, in readable user-space code, without relying on kernel abstractions... but mostly as a side benefit for the author.

## What this is not

erebus is not:
- a drop-in replacement for full VPN clients
- a system-wide VPN solution
- focused on maximum performance (initially)
- a complete TCP/IP stack implementation

Correctness, interoperability, and clarity come first.

## How it works (conceptually)

```
HTTP client
↓
local HTTP proxy
↓
user-space TCP/IP implementation
↓
virtual IP packets
↓
VPN protocol framing (OpenVPN / ESP)
↓
UDP socket
↓
VPN server
```

All VPN-related logic lives in user space. The operating system kernel is not involved in routing, TCP state, or encryption for VPN traffic.

## Protocol scope

Short term:
- OpenVPN with static-key encryption (UDP/TCP, tun mode)
- Outbound HTTP proxy for VPN resources
- Minimal TCP stack for outbound connections
- Inbound TCP handling to expose local services
- Bidirectional IP packet flow over the VPN

Longer term:
- TLS control channel for OpenVPN
- IPsec ESP tunnel support
- IKEv2 key management for IPsec
- Performance improvements, compression, and advanced TCP handling

## Project status

This is an early-stage project. Current support includes:

- Long-running connection to openvpn server in static key mode (compatible with most of openvpn options in this mode)
- Basic userspace ICMP stack (just request/reply)
- Basic unperformant userspace TCP stack
    - Connection setup (SYN/SYN-ACK/ACK) and orderly teardown (FIN handshake)
    - Reads and writes spanning multiple segments, so payloads larger than one TCP segment work in both directions (responses *and* requests)
- A local HTTP/1.x proxy that forwards requests to VPN resources over the userspace TCP stack
    - Resolves hostnames, follows `Content-Length` / chunked / close-delimited response framing, and keeps the local client connection alive
- Inbound TCP port-forwarding: expose a local service to VPN peers
    - The userspace TCP stack accepts peer-initiated connections (passive open) and relays each to a local service (e.g. `127.0.0.1:8080`)
    - Configured under `[proxy-in]` as `label = <vpn-port> <local-host>:<local-port>`; the relay is single-threaded per connection (simplicity over throughput)

The roadmap prioritizes incremental progress and interoperability over completeness.

## Development environment

In order to help out during development, the `erebus/test` package provides a few helpers, most notably:

- `(create-container NAME FOLDER VPN-LOCAL-PORT)`: creates a docker container `NAME` with the appropriate configuration later available in `FOLDER`, exposing its VPN port to `localhost:VPN-LOCAL-PORT`
- `(prepare-container NAME FOLDER)`: runs all the preparation steps for the openvpn server configuration
- `(start-services NAME)`: starts openvpn and nginx
- `(cleanup-container NAME FOLDER)`: cleanups the container and its folder
- `(run-in-container NAME COMMAND)`: runs an arbitrary shell command inside the container

This lets you quickly setup a development environment with openvpn server running inside a docker container. You can edit the configuration in `FOLDER` after `prepare-container` and before `start-services`. Quick tip though: you need to do that from within a container; feel free to use `(run-in-container NAME COMMAND)`: the `FOLDER` will have root permissions, so you most likely won't be able to edit it from the REPL directly.

### Manual, interactive testing

For poking at a running setup by hand (rather than through the automated test suite), the `erebus/test` package exposes a small REPL workflow that spins a container up, leaves it running, and points a real erebus proxy at it:

```lisp
(in-package :erebus/test)

(dev-vpn-up)                      ; build the image if needed, start an openvpn container
                                  ; (e.g. (dev-vpn-up :proto "tcp-server") for TCP)
(defparameter *c (dev-client))    ; connect an erebus client (pass :protocol :stream for TCP)
(defparameter *p (dev-proxy *c))  ; start the HTTP proxy on localhost:11023

;; now, from a shell:
;;   http_proxy=http://localhost:11023 curl http://10.8.0.1

(hunchentoot:stop *p)
(disconnect *c)
(dev-vpn-down)                    ; tear the container down
```

To go the other way and expose a *local* service to the VPN, run a server on
the host and forward an exposed VPN port to it:

```lisp
;; with *c connected as above, and e.g. `python3 -m http.server 8080`
;; running on the host:
(defparameter *e (dev-expose *c :vpn-port 8080 :host "127.0.0.1" :port 8080))

;; from inside the container (the VPN peer):
;;   (run-in-container "erebus-dev" "curl -s http://10.8.0.2:8080/")

(unexpose *e)
```

There is also a one-shot smoke test that exercises the whole proxy path (404, a large fragmented response, and a `Connection: close` request) against a throwaway container:

```sh
sbcl --script t/manual-verify.lisp
```

`dev-vpn-up` accepts the same options as the server-config helper, so for example a backend service can be launched alongside openvpn with `(dev-vpn-up :pre "nohup echo-server &")`.

## Documentation

- **Man page** — [`doc/erebus.1`](doc/erebus.1): options, the full
  configuration reference (sections, ciphers, digests), examples, and a candid
  limitations section. Read it formatted with `make man`.
- **Architecture & trade-offs** — [`doc/architecture.md`](doc/architecture.md):
  why everything runs in user space, how the layers fit, what the embedded
  TCP/IP stack gives up, and the performance vs portability vs safety bargain.
- **Project website** — [`docs/index.html`](docs/index.html): a small,
  self-contained page (no build step) with the elevator pitch, a diagram, use
  cases, and an FAQ. Open it in a browser, or serve the `docs/` directory
  (it is also what GitHub Pages publishes).
- **OpenVPN static-key wire format** —
  [`doc/openvpn-static-key-protocol-version-1.md`](doc/openvpn-static-key-protocol-version-1.md):
  the protocol itself, independent of this implementation.
- **Performance** — [`doc/stress-results.md`](doc/stress-results.md): measured
  cost and how to reproduce it (`make stress`).
- **Roadmap** — [`ROADMAP.md`](ROADMAP.md): the incremental plan and what later
  phases add.

## License

GPLv3 License.
