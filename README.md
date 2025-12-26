# erebus

This project is an experimental, user-space VPN client that exposes access to private networks through a local HTTP proxy.

Instead of configuring system-wide tunnels, kernel interfaces, or requiring elevated privileges, erebus runs entirely in user space and forwards traffic explicitly through a proxy interface.

The initial focus is interoperability with OpenVPN servers, with longer-term support planned for IPsec (ESP) and IKE.

---

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

---

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

---

## What this is not

erebus is not:
- a drop-in replacement for full VPN clients
- a system-wide VPN solution
- focused on maximum performance (initially)
- a complete TCP/IP stack implementation

Correctness, interoperability, and clarity come first.

---

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
``

All VPN-related logic lives in user space. The operating system kernel is not involved in routing, TCP state, or encryption for VPN traffic.

---

## Protocol scope

Short term:
- OpenVPN (UDP, tun mode)
- Outbound HTTP proxy for VPN resources
- Minimal TCP stack for outbound connections
- Inbound TCP handling to expose local services
- Bidirectional IP packet flow over the VPN

Longer term:
- Static-key encryption for OpenVPN
- TLS control channel for OpenVPN
- IPsec ESP tunnel support (user-space)
- IKEv2 key management for IPsec
- Performance improvements, compression, and advanced TCP handling

---

## Incremental development

erebus is built in small, testable steps. Each step will result in a working system, even if it is incomplete.

Examples include:
- establishing a VPN session without encryption
- injecting raw IPv4 packets
- adding minimal TCP handshake support
- proxying a single HTTP request
- introducing encryption only after data flow is correct

For a detailed breakdown of implementation stages, see `ROADMAP.md`.

---

## Project status

This is an early-stage project.

Expect:
- incomplete features
- rough edges
- changing APIs

The roadmap prioritizes incremental progress and interoperability over completeness.
