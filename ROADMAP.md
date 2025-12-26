# Roadmap

This roadmap describes the incremental implementation steps needed to realize the design described in `README.md`.

Each stage builds directly on the previous one and should leave the codebase in a working, testable state.

---

## Phase 0 — Project skeleton & transport

Goal: establish a reliable UDP transport and internal structure.

- Project layout and core abstractions
- UDP socket management
- Send/receive loop
- Logging and packet inspection tooling
- No protocol logic yet

Exit condition:
- Client can send and receive arbitrary UDP payloads

---

## Phase 1 — OpenVPN control channel (no auth, no crypto)

Goal: establish an OpenVPN session using the simplest possible server configuration.

Server assumptions:
- UDP
- tun mode
- `cipher none`
- `auth none`

Client responsibilities:
- OpenVPN packet framing
- Control packet opcodes
- Session ID generation and tracking
- Packet ID tracking
- HARD_RESET exchange
- Control packet retransmission (minimal)

Exit condition:
- Client successfully completes session setup
- Server responds consistently to control packets

---

## Phase 2 — PUSH_REPLY handling & session state

Goal: extract and maintain VPN session parameters.

- Parse `PUSH_REPLY`
- Extract assigned virtual IP
- Extract routes and MTU
- Store session state internally
- Reject data packets before session is established

Exit condition:
- Client knows its virtual IP and can reason about routing

---

## Phase 3 — OpenVPN data channel (plaintext)

Goal: exchange raw IP packets over the VPN.

- Implement `P_DATA` packet handling
- Inject raw IP packets as data payloads
- Receive and parse incoming IP packets
- No encryption or authentication

Exit condition:
- IP packets successfully traverse the VPN
- Responses are observable and parsed

---

## Phase 4 — IPv4 packet construction & parsing

Goal: reliably build and interpret IPv4 packets.

- IPv4 header construction
- Header checksum calculation
- Packet parsing and validation
- Fixed MTU
- No fragmentation

Exit condition:
- Valid IPv4 packets are accepted by peers on the VPN

---

## Phase 5 — Minimal TCP implementation

Goal: support enough TCP to carry HTTP traffic.

Required features:
- TCP header construction
- TCP checksum (pseudo-header)
- 3-way handshake
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
- Data flows in both directions

---

## Phase 6 — HTTP proxy integration

Goal: expose VPN connectivity via a local HTTP proxy.

- Accept HTTP requests from local clients
- Map requests to virtual TCP connections
- Forward responses back to clients
- Handle multiple concurrent connections (basic)

Exit condition:
- Standard HTTP clients (e.g. curl) can access VPN resources

---

## Phase 7 — Static-key encryption (OpenVPN)

Goal: add confidentiality and integrity to the data channel.

- Static shared key support
- Symmetric encryption
- HMAC authentication
- Packet replay protection

Exit condition:
- Encrypted OpenVPN sessions function correctly
- Invalid or replayed packets are rejected

---

## Phase 8 — TLS control channel (OpenVPN)

Goal: support standard OpenVPN key negotiation.

- TLS handshake inside OpenVPN control packets
- Server certificate validation
- Data channel key derivation
- Rekeying (initially optional)

Exit condition:
- Interoperability with standard OpenVPN servers using TLS

---

## Phase 9 — IPsec ESP (userspace)

Goal: support IPsec-style tunneling using the same proxy model.

- ESP tunnel mode
- Manual Security Association configuration
- Replay protection
- Algorithm abstraction shared with OpenVPN

Exit condition:
- IP packets successfully traverse an ESP tunnel without kernel XFRM

---

## Phase 10 — IKEv2

Goal: automate key management for IPsec.

- IKEv2 handshake
- SA negotiation
- Rekeying and lifetimes
- Algorithm negotiation

Exit condition:
- Fully dynamic IPsec tunnel establishment

---

## Deferred / ongoing work

These are intentionally postponed until correctness is established:

- Performance optimizations
- Compression
- Advanced TCP behavior
- Extensive error recovery
- Observability and metrics
- Protocol extensions

---

## Notes

The ordering of (most) phases is intentional. Later phases assume correctness and stability in earlier ones. The exception are phases 8 and 9/10 that might be interleaved (i.e. IPsec ESP with IKEv2 support might be implemented before OpenVPN TLS support).
