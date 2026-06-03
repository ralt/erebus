# ROADMAP

This document describes the incremental implementation plan for the project.
Each phase is intended to produce a working, testable system before moving on to the next.

The focus is on **progressive capability**, not completeness.

## ✅ Phase 1 — OpenVPN static key, minimal implementation

Goal: implement the simplest possible OpenVPN-compatible client using a pre-shared static key.

Scope:

* OpenVPN static-key mode
* No control channel
* No TLS

Key tasks:

* Parse `static.key`
* Implement:

  * packet ID counter
  * AES encryption
  * HMAC authentication
* Serialize and deserialize
* Interoperate with a minimal OpenVPN server in static-key mode
* Exchange raw payload bytes successfully

Outcome:

* A minimal, ESP-like OpenVPN client
* Cryptographic and packet-format foundation

## ✅ Phase 2 — Complete static-key support and good network citizen

Goal: make the OpenVPN client a good OpenVPN citizen in static-key mode. Being a good citizen means replying to ICMP packets that the server sends.

Scope:

* Support a configuration file
* Support all static-key modes (ciphers, key directions, compression)
* Support ICMP packets sent by the server

Key tasks:
* Implement command line options parsing
  * Implement compiling a binary to be able to use those as well
* Define and implement a format for the configuration file
* Use configuration file to define static-key modes
* Implement static-key modes (i.e. what OpenVPN calls wire format v1)
  * Support for various ciphers and modes (CBC, etc)
  * Support for the 3 key directions
  * Support for compression
* Analyze, identify and implement replies to ICMP packets that OpenVPN server sends
* Write down full readable documentation of the wire format v1

Outcome:
* An OpenVPN client supporting all static-key options that acts as a good citizen on the VPN network

## ✅ Phase 3 — Transport abstraction (UDP and TCP)

Goal: make the OpenVPN client transport-agnostic early.

Scope:

* Support both UDP and TCP transport modes
* Static-key mode only
* Identical packet logic across transports

Key tasks:

* Define a transport abstraction layer:

  * send packet
  * receive packet
* Implement:

  * UDP transport (datagram-based)
  * TCP transport (length-prefixed stream)
* Ensure packet parsing logic is independent of transport
* Allow configuration-time selection of UDP or TCP

Outcome:

* One OpenVPN implementation that can switch between UDP and TCP
* No protocol logic duplicated between transports

## ✅ Phase 4 — Embedded TCP/IP stack over VPN traffic

Goal: implement a TCP/IP stack that runs entirely inside the VPN data channel.

Scope:

* TCP over VPN payloads
* No reliance on kernel networking
* Client-initiated connections only

Key tasks:

* Implement minimal IP and TCP packet handling
* Maintain TCP connection state (sequence numbers, ACKs)
* Map TCP packets to and from VPN data packets
* Support reliable byte streams over the VPN tunnel

Out of scope:

* Full TCP feature set (RTO, flow control, RACK, congestion control, window scaling, etc.)
    See https://en.wikipedia.org/wiki/Transmission_Control_Protocol for a pretty complete list of features

Outcome:

* A functional TCP/IP stack embedded inside VPN traffic
* Foundation for higher-level protocols (e.g. HTTP) without OS-level networking

## ✅ Phase 5 — Local HTTP proxy over OpenVPN

Goal: expose VPN access through a local, developer-friendly interface.

Scope:

* HTTP/1.x
* Explicit proxy model
* No system-wide routing

Key tasks:

* Implement local HTTP proxy listener
* Translate HTTP requests into TCP connections over the VPN
* Forward responses back to local clients
* TCP fragmentation to handle responses bigger than 1kB (and, symmetrically, request bodies bigger than one segment)
* Hostname addresses instead of IP addresses
* Support for closing TCP connections and keep-alive

Outcome:

* Practical access to VPN resources
* Clear demonstration of rootless design goals

## ✅ Phase 6 — Expose local services to the VPN

Goal: allow VPN peers to access selected local services through the client.

Scope:

* Explicit TCP port forwarding
* Opt-in exposure only

Key tasks:

* Accept incoming VPN connections (passive open in the userspace TCP stack)
* Forward traffic to local services (e.g. local HTTP server)
* Handle bidirectional data flow
* Apply basic access controls

Outcome:

* Bidirectional VPN interaction without kernel networking
* Enables lightweight ingress use cases

Notes:

* The userspace TCP stack now accepts peer-initiated connections (LISTEN →
  SYN-RECEIVED → ESTABLISHED) in addition to opening its own. An exposed port
  is configured under `[proxy-in]` (`label = <vpn-port> <local-host>:<local-port>`)
  and each inbound connection is relayed to the local service.
* The relay is intentionally single-threaded per connection: the stop-and-wait
  stack uses one packet queue per connection, so it polls each side in turn
  rather than reading and writing concurrently. Simplicity over throughput.
* Basic access controls (per-port source allow-lists) are intentionally
  deferred; exposure being opt-in via config is the only control for now.

## ✅ Phase 7 — Documentation, positioning, and user-facing clarity

Goal: clearly explain what this project **is**, **is not**, and **why it exists**, making it approachable for users while being honest about trade-offs and limitations.

Scope:

* End-user documentation
* Architectural explanation
* Positioning and expectations setting
* No new protocol features

Key tasks:

* Write a Unix-style **man page**:
  * Installation
  * Configuration
  * Command-line options
  * Typical usage patterns
* Create a small **project website**:
  * What the project does
  * How it differs from traditional VPN clients
  * Rootless design goals
* Document all supported options and modes:
  * What works well
  * What works less well
  * Known limitations and intentional constraints
* Explain architectural trade-offs:
  * User-space networking vs kernel VPNs
  * Embedded TCP/IP stack limitations
  * Performance vs portability vs safety
* Provide clear use cases:
  * Where this tool shines
  * Where it is *not* the right tool
* Add lightweight “marketing” material:
  * One-paragraph elevator pitch
  * Short examples and diagrams
  * FAQ-style clarifications

Outcome:

* A project that is understandable without reading the source code
* Clear expectations for users and contributors
* Reduced support and onboarding friction
* A solid foundation for adoption and feedback

Notes:

* Deliverables landed:
  * `doc/erebus.1` — a Unix man page (read it with `make man`) covering
    options, the full configuration reference (sections, ciphers, digests),
    examples, files, and an honest limitations section.
  * `doc/architecture.md` — the user-space-vs-kernel rationale, the embedded
    TCP/IP stack's design and what it gives up, and the performance vs
    portability vs safety trade-off, with a data-flow diagram.
  * `site/index.html` — a small, self-contained project website (no build
    step): elevator pitch, how it works, use cases (where it shines / where
    it's the wrong tool), and an FAQ.
* No new protocol features, by design.

## Phase 8 — OpenVPN TLS support (UDP and TCP)

Goal: add full TLS-based OpenVPN support on top of an already working transport and proxy stack.

Scope:

* OpenVPN TLS mode
* Both UDP and TCP transports
* Control and data channels

Key tasks:

* Implement:

  * `P_CONTROL_HARD_RESET_*`
  * TLS handshake inside control packets
* Integrate TLS library (e.g. CL+SSL / OpenSSL)
* Parse and apply `PUSH_REPLY`
* Derive and rotate data-channel keys
* Reuse existing transport abstraction

Outcome:

* Fully standards-compatible OpenVPN client
* Rootless operation preserved

## Phase 9 — ESP / IKEv2 (strongSwan interoperability)

Goal: extend the architecture beyond OpenVPN to standard IPsec.

Scope:

* ESP data plane
* IKEv2 control plane
* strongSwan as reference server

Key tasks:

* Implement IKEv2 exchanges
* Establish ESP Security Associations
* Reuse existing crypto and transport abstractions

Outcome:

* Generalized VPN client architecture
* Support for industry-standard IPsec deployments

## Phase 10 — Performance and advanced features (optional)

Goal: improve efficiency and robustness after correctness is established.

Potential areas:

* socks5 proxy (required if we want to support forwarding UDP)
* UDP (probably not so optional, just not in the path of an http proxy)
* Proxy to TLS backends
* Full ICMP feature set
* Full TCP feature set (RTO, flow control, RACK, congestion control, window scaling, etc.)
    See https://en.wikipedia.org/wiki/Transmission_Control_Protocol for a pretty complete list of features
* HTTP/2
* HTTP/3 (UDP is a dependency)

These are intentionally deferred until all major protocol features are stable.
