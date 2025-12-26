# Roadmap

This roadmap describes the incremental implementation steps needed to realize the design described in `README.md`.

Each phase builds directly on the previous one and should leave the codebase in a working, testable state.

---

## Phase 0 — Project skeleton & transport

Goal: establish a reliable UDP transport and internal structure.

- Project layout and core abstractions
- UDP socket management
- Send/receive loop
- Logging and packet inspection tooling
- No protocol logic yet

Exit condition:
- erebus can send and receive arbitrary UDP payloads

---

## Phase 1 — OpenVPN control channel (no auth, no crypto)

Goal: establish an OpenVPN session using the simplest possible server configuration.

Server assumptions:
- UDP
- tun mode
- `cipher none`
- `auth none`

erebus responsibilities:
- OpenVPN packet framing
- Control packet opcodes
- Session ID generation and tracking
- Packet ID tracking
- HARD_RESET exchange
- Minimal control packet retransmission

Exit condition:
- erebus successfully completes session setup
- Server responds consistently to control packets

---

## Phase 2 — PUSH_REPLY handling & session state

Goal: extract and maintain VPN session parameters.

- Parse `PUSH_REPLY`
- Extract assigned virtual IP
- Extract routes and MTU
- Store session state internally
- Reject data packets before session establishment

Exit condition:
- erebus knows its virtual IP and routing scope

---

## Phase 3 — OpenVPN data channel (plaintext)

Goal: exchange raw IP packets over the VPN.

- Implement `P_DATA` packet handling
- Inject raw IP packets as data payloads
- Receive and parse incoming IP packets
- No encryption or authentication

Exit condition:
- IP packets successfully traverse the VPN in both directions

---

## Phase 4 — IPv4 packet construction & parsing

Goal: reliably build and interpret IPv4 packets.

- IPv4 header construction
- Header checksum calculation
- Packet parsing and validation
- Fixed MTU
- No fragmentation

Exit condition:
- Valid IPv4 packets are accepted by VPN peers

---

## Phase 5 — Minimal TCP implementation (outbound)

Goal: support enough TCP to initiate connections to VPN resources.

Required features:
- TCP header construction
- TCP checksum (pseudo-header)
- 3-way handshake (client-initiated)
- Sequence and acknowledgment tracking
- PSH + ACK data transfer
- Basic receive buffering
- Connection teardown via RST or FIN

Explicitly deferred:
- Retransmissions
- Congestion control
- Window scaling
- TCP options

Exit condition:
- A TCP connection can be established to a VPN resource
- Data flows reliably from VPN to erebus

---

## Phase 6 — Outbound HTTP proxy

Goal: expose VPN access via a local HTTP proxy.

- Accept HTTP requests from local clients
- Map requests to outbound virtual TCP connections
- Forward responses back to clients
- Handle multiple concurrent connections (basic)

Exit condition:
- Standard HTTP clients (e.g. curl) can access VPN resources

---

## Phase 7 — Inbound TCP support (listening in user-space)

Goal: accept connections initiated from the VPN toward erebus.

- Recognize inbound TCP SYN packets
- Maintain per-connection TCP state (server role)
- Complete TCP handshakes initiated remotely
- Route packets to local listeners
- Support basic port mapping rules

Exit condition:
- VPN peers can establish TCP connections to the erebus' virtual IP

---

## Phase 8 — Exposing local services to the VPN

Goal: make local resources reachable from within the VPN.

- Map VPN-facing ports to local services
  - e.g. `10.8.0.6:8080 → 127.0.0.1:3000`
- Forward inbound TCP streams to local sockets
- Translate responses back into VPN packets
- Enforce explicit exposure rules (no implicit listening)

Exit condition:
- A service running locally is reachable from inside the VPN
- No system-wide port binding or root privileges required

---

## Phase 9 — Static-key encryption (OpenVPN)

Goal: add confidentiality and integrity to VPN traffic.

- Static shared key support
- Symmetric encryption
- HMAC authentication
- Packet replay protection

Exit condition:
- Encrypted sessions function correctly in both directions

---

## Phase 10 — TLS control channel (OpenVPN)

Goal: support standard OpenVPN key negotiation.

- TLS handshake inside OpenVPN control packets
- Server certificate validation
- Data channel key derivation
- Rekeying (initially optional)

Exit condition:
- Interoperability with standard OpenVPN servers using TLS

---

## Phase 11 — IPsec ESP

Goal: support IPsec-style tunneling using the same proxy model.

- ESP tunnel mode
- Manual Security Association configuration
- Replay protection
- Algorithm abstraction shared with OpenVPN

Exit condition:
- Bidirectional IP traffic over ESP without kernel XFRM

---

## Phase 12 — IKEv2

Goal: automate key management for IPsec.

- IKEv2 handshake
- SA negotiation
- Rekeying and lifetimes
- Algorithm negotiation

Exit condition:
- Fully dynamic IPsec tunnel establishment

---

## Deferred work

These are intentionally postponed until correctness is established:

- Performance optimizations
- Compression
- Advanced TCP behavior
- Flow control and backpressure
- Observability and metrics
- Protocol extensions

---

## Notes

Outbound and inbound traffic are treated symmetrically at the IP and TCP layers.
Differences are handled at the connection management and policy layers.

The incremental structure of this roadmap is intentional and should be
preserved. The exception might be on support for OpenVPN TLS vs IPsec
ESP/IKEv2, as these are independent.
