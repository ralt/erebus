# ROADMAP

This document describes the incremental implementation plan for the project.
Each phase is intended to produce a working, testable system before moving on to the next.

The focus is on **progressive capability**, not completeness.

## Phase 1 — OpenVPN static key, minimal implementation

Goal: implement the simplest possible OpenVPN-compatible client using a pre-shared static key.

Scope:

* OpenVPN static-key mode
* No control channel
* No TLS
* `P_DATA_V1` packets only

Key tasks:

* Parse `static.key`
* Implement:

  * packet ID counter
  * AES encryption
  * HMAC authentication
* Serialize and deserialize `P_DATA_V1`
* Interoperate with a minimal OpenVPN server in static-key mode
* Exchange raw payload bytes successfully

Outcome:

* A minimal, ESP-like OpenVPN client
* Cryptographic and packet-format foundation

## Phase 2 — Transport abstraction (UDP and TCP)

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

Good catch — you’re right, Phase 3 is really about **embedding a TCP/IP stack inside the VPN payload**, not about “rootless” as a goal in itself. The rootless aspect is a consequence, not the focus.

Below is a **clean rewording of Phase 3 only**, keeping the rest of the roadmap unchanged and consistent in tone.

You can replace just that section in your `ROADMAP.md`.

---

## Phase 3 — Embedded TCP/IP stack over VPN traffic

Goal: implement a TCP/IP stack that runs entirely inside the VPN data channel.

Scope:

* TCP over VPN payloads
* No reliance on kernel networking
* Client-initiated connections only

Key tasks:

* Implement minimal IP and TCP packet handling
* Maintain TCP connection state (sequence numbers, ACKs)
* Handle segmentation and reassembly
* Map TCP packets to and from VPN data packets
* Support reliable byte streams over the VPN tunnel

Out of scope:

* UDP
* ICMP
* Full TCP feature set (window scaling, SACK, etc.)

Outcome:

* A functional TCP/IP stack embedded inside VPN traffic
* Foundation for higher-level protocols (e.g. HTTP) without OS-level networking

## Phase 4 — Local HTTP proxy over OpenVPN

Goal: expose VPN access through a local, developer-friendly interface.

Scope:

* HTTP/1.x
* Explicit proxy model
* No system-wide routing

Key tasks:

* Implement local HTTP proxy listener
* Translate HTTP requests into TCP connections over the VPN
* Forward responses back to local clients
* Support multiple concurrent proxied connections

Outcome:

* Practical access to VPN resources
* Clear demonstration of rootless design goals

## Phase 5 — Expose local services to the VPN

Goal: allow VPN peers to access selected local services through the client.

Scope:

* Explicit TCP port forwarding
* Opt-in exposure only

Key tasks:

* Accept incoming VPN connections
* Forward traffic to local services (e.g. local HTTP server)
* Handle bidirectional data flow
* Apply basic access controls

Outcome:

* Bidirectional VPN interaction without kernel networking
* Enables lightweight ingress use cases

## Phase 6 — OpenVPN TLS support (UDP and TCP)

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

## Phase 7 — ESP / IKEv2 (strongSwan interoperability)

Goal: extend the architecture beyond OpenVPN to standard IPsec.

Scope:

* ESP data plane
* IKEv2 control plane
* strongSwan as reference server

Key tasks:

* Implement IKEv2 exchanges
* Establish ESP Security Associations
* Reuse existing crypto and transport abstractions
* Compare and align with OpenVPN static-key semantics

Outcome:

* Generalized VPN client architecture
* Support for industry-standard IPsec deployments

## Phase 8 — Performance and advanced features (optional)

Goal: improve efficiency and robustness after correctness is established.

Potential areas:

* TCP stack optimizations
* UDP, ICMP, full TCP feature set
* Packet batching
* Compression

These are intentionally deferred until all major protocol features are stable.
