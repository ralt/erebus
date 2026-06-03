# Architecture and trade-offs

This document explains *how* erebus is built and, more importantly, *why* it is
built that way. erebus makes a deliberate, unusual choice — do all the VPN work
in user space — and that choice cascades into everything else. If you understand
the choice, the limitations stop looking like bugs and start looking like the
price of a feature.

If you just want to *use* erebus, read the [man page](erebus.1) (`make man`) or
the [README](../README.md). This document is for understanding the design.

## The one decision everything follows from

A conventional VPN client asks the kernel to do the networking:

```
your app → kernel sockets → kernel routing table → tun/tap device → VPN daemon → wire
```

It creates a `tun`/`tap` interface, rewrites the host's routing table so traffic
is steered into the tunnel, and lets the kernel's mature TCP/IP stack carry the
packets. This is fast and complete — you get the real TCP stack for free — but it
requires:

- **root / `CAP_NET_ADMIN`** to create the interface and edit routes,
- **kernel support** (`tun`/`tap`, sometimes `XFRM`), and
- **global, system-wide effect**: every app on the host is now affected.

erebus refuses all three. It does the networking *itself*, in the process, in
user space:

```
local client → erebus HTTP proxy → erebus user-space TCP/IP → OpenVPN framing → UDP/TCP socket → VPN server
```

Nothing creates an interface. Nothing edits a route. Nothing needs privilege
beyond opening an ordinary client socket. The cost is that erebus must *be* the
TCP/IP stack for VPN traffic — and a hand-written, intentionally minimal stack
is never going to match the kernel's. That single trade — **portability and
rootlessness in exchange for completeness and throughput** — is the whole story.

## Why bother (user-space vs kernel VPNs)

The kernel approach is the right default on a machine you control. erebus exists
for the machines you *don't*:

- **containers** without `--privileged` or the `NET_ADMIN` capability,
- **CI runners** and other ephemeral, locked-down environments,
- **sandboxed apps** and multi-tenant hosts where editing global routes is
  antisocial or forbidden,
- **developer tooling** that wants to reach a VPN resource without taking over
  the whole machine's networking.

In all of these, "just run the OpenVPN client as root and add a route" is not
available. erebus trades the kernel's stack for the ability to run *at all*, with
no privileges, affecting *only* the traffic you explicitly route through it.

The secondary benefit (mostly for the author): implementing the protocols
incrementally in readable user-space code makes them legible. There is no kernel
abstraction hiding what an OpenVPN data packet or a TCP handshake actually is —
it is all right there in `src/`.

## How the pieces fit

The codebase is small and layered; each layer only knows about the one below it.

| Layer | File | Responsibility |
|-------|------|----------------|
| HTTP proxy (outbound) | `src/http.lisp` | Hunchentoot acceptor; translate HTTP ⇄ a VPN TCP stream |
| Port forward (inbound) | `src/socket.lisp` (`expose`/`%relay`) | Accept VPN connections, relay to a local OS socket |
| User-space sockets / TCP | `src/socket.lisp` | Active & passive open, segments, ACKs, orderly close |
| IP / TCP / ICMP packets | `src/ip.lisp` | Build and parse IPv4, TCP, and ICMP-echo, with checksums |
| OpenVPN static-key | `src/openvpn.lisp` | Encrypt/auth (AES/ARIA/Camellia-CBC + HMAC), packet IDs, ping, demux |
| Transport | `src/vpn-connection.lisp` | UDP datagrams or length-prefixed TCP stream; reader/writer threads |

Data flows down on the way out and up on the way in:

```
            outbound (proxy-out)                 inbound (proxy-in)
   local HTTP client                       VPN peer
        │                                       │
        ▼                                       ▼
   HTTP proxy  (http.lisp)              accept + relay (socket.lisp)
        │                                       │
        ▼                                       ▼
   user-space TCP socket  ◄────────────────────┘   (socket.lisp)
        │
        ▼
   IPv4 / TCP packet  (ip.lisp)
        │
        ▼
   OpenVPN encrypt + HMAC + packet-id  (openvpn.lisp)
        │
        ▼
   UDP datagram  /  TCP length-prefixed frame  (vpn-connection.lisp)
        │
        ▼
   OpenVPN server
```

The transport layer is genuinely abstract: a single reader thread and a single
writer thread move opaque packets, and the only difference between UDP and TCP
is datagram-vs-stream framing. No protocol logic is duplicated between the two
(this was the point of roadmap Phase 3). Inbound and outbound share the *same*
user-space TCP stack; they differ only in who initiates the connection (active
vs passive open).

## The embedded TCP/IP stack, and what it gives up

This is where the trade-off is most visible. The kernel's TCP is decades of
tuning; erebus's is a few hundred readable lines. It implements the parts that
are needed for *correctness* and skips the parts that are needed for *speed*.

What it does:

- **Connection setup** — three-way handshake, both as the initiator
  (`%active-open`) and as the responder for inbound forwards (`%passive-open`,
  including resending the SYN-ACK if the peer retransmits its SYN).
- **Orderly teardown** — the FIN handshake, with a *bounded* drain so a peer
  that never FINs (e.g. an HTTP keep-alive server) can't wedge us.
- **Fragmentation in both directions** — a write larger than one segment is
  split into `+max-tcp-payload+` (1400 B) chunks so it fits the tun MTU; a read
  larger than one segment is reassembled across segments. So payloads bigger
  than a single TCP segment work for both requests and responses.
- **Demultiplexing** by the local four-tuple, with one packet queue per
  connection.

What it deliberately does **not** do:

- **Stop-and-wait, not a sliding window.** erebus sends one segment and waits
  for its ACK before sending the next, and acknowledges every inbound segment.
  There is no window, no pipelining, no in-flight data. This is the single
  biggest throughput limiter and it is a conscious choice: it makes the stack
  trivial to reason about and impossible to get into a confusing in-flight
  state.
- **One queue per connection, no concurrent duplex.** A connection is never read
  and written from two threads at once, because its single packet queue would
  race. The inbound relay (`%relay`) is therefore single-threaded *per
  connection*: it polls each side in turn rather than pumping both directions
  concurrently. (Different connections *do* run in parallel — each has its own
  queue and its own thread.)
- **No congestion control, RTO tuning, RACK, SACK, or window scaling.** None of
  the machinery that lets real TCP fill a fat pipe or recover gracefully from
  loss. Retransmission is minimal.
- **IPv4 only; ICMP is echo-reply only.** Enough to be a good citizen on the VPN
  (answer pings), not a general ICMP implementation.

The honest summary: this stack is built to *converse* — request/response traffic
to VPN resources — not to move bulk data.

## Performance vs portability vs safety

You can have a fast VPN client or a rootless, portable one; erebus picks the
latter and is upfront about the bill.

- **Portability / rootlessness** is the headline feature and is never traded
  away. No interface, no route, no capability, no kernel module — runs wherever
  a process can open a socket.
- **Safety / clarity** is the second priority. The stop-and-wait design and the
  one-queue-per-connection rule exist as much for *legibility* as anything: the
  code is meant to be read and understood, and a simple state machine has fewer
  ways to be subtly wrong. Correctness and interoperability come before
  features.
- **Performance** is what pays for the other two, and it is *explicitly deferred*
  (roadmap Phase 10). The measured cost lives in
  [`stress-results.md`](stress-results.md): for small, latency-sensitive
  request/response traffic the overhead is small (on one run, comparable to or
  better than a conventional proxy); for bulk transfer erebus is several times
  slower than a normal proxy and far slower than direct loopback, because a
  large body is hundreds of segments and each one is a full
  encrypt → send → wait-for-ACK → decrypt round trip. Pipelining / windowing is
  the lever to pull if throughput ever becomes a goal — and it is precisely the
  thing the current design leaves on the table on purpose.

## Where this lands you

erebus is the right tool when you need *explicit, scoped, unprivileged* access to
a VPN resource — a developer reaching an internal HTTP service from a container,
a CI job hitting a private endpoint, exposing one local port to a VPN peer. It is
the wrong tool when you need a system-wide VPN, line-rate bulk transfer, TLS-mode
OpenVPN, or a complete TCP stack. Those aren't oversights; they're the other side
of the rootless coin.

## Further reading

- [`README.md`](../README.md) — project overview and current status.
- [`erebus.1`](erebus.1) — the man page (`make man` to read it formatted).
- [`openvpn-static-key-protocol-version-1.md`](openvpn-static-key-protocol-version-1.md)
  — the OpenVPN static-key wire format, independent of this implementation.
- [`stress-results.md`](stress-results.md) — measured performance and how to
  reproduce it (`make stress`).
- [`ROADMAP.md`](../ROADMAP.md) — the incremental plan and what later phases add.
