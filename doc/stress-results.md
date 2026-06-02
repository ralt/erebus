# Stress / performance results

This is a snapshot of what the erebus HTTP proxy costs, relative to a
conventional proxy and to no proxy at all. Reproduce it with `make stress`
(see `t/stress.lisp`).

## What is measured

The same nginx resource is fetched from the host three ways, and each path
is timed over many sequential requests:

| label       | path                                                            |
|-------------|-----------------------------------------------------------------|
| `erebus`    | host → erebus proxy (userspace TCP) → OpenVPN → nginx            |
| `tinyproxy` | host → tinyproxy (a conventional proxy, OS sockets) → nginx      |
| `direct`    | host → nginx, no proxy                                           |

Two workloads:

- **Small responses** — nginx's 404 body (~146 B). Dominated by
  per-request / round-trip overhead.
- **Large responses** — a 262144 B (256 KiB) file. Dominated by throughput.

Each request uses a fresh connection (`Connection: close`), because erebus
does not pool connections to the VPN backend — so this compares like for
like on connection setup + teardown.

## Caveats — read before quoting these numbers

- **This is not apples-to-apples, by design.** Only `erebus` crosses the
  VPN and runs a user-space TCP stack; `tinyproxy` and `direct` use the
  kernel's. That difference *is* the cost we want to see.
- The numbers below are a **single run inside a nested-docker development
  environment**. They are illustrative of orders of magnitude, not
  authoritative benchmarks; they will vary significantly by machine,
  kernel, and load. Re-run `make stress` to get numbers for your setup.
- erebus's TCP stack is intentionally simple: it sends one segment and
  waits for its ACK before sending the next (no window / no pipelining),
  and it acknowledges every inbound segment. That lockstep is the main
  throughput cost and is expected — performance is explicitly deferred in
  the roadmap.
- `tinyproxy`'s small-response latency came out high (see below); that
  likely reflects its own per-request handling rather than a fundamental
  property of "a normal proxy". Treat it as one reference point, not a
  gold standard.

## Results (2026-06-03, single run, nested docker)

```
Small responses (nginx 404, ~146 B) -- request/round-trip overhead
  erebus        30 reqs     0.03s      937.5 req/s       0.13 MB/s        1.07 ms/req
  tinyproxy     30 reqs     0.43s       69.4 req/s       0.01 MB/s       14.40 ms/req
  direct        30 reqs     0.01s     3750.0 req/s       0.52 MB/s        0.27 ms/req

Large responses (262144 B) -- throughput
  erebus         8 reqs     0.31s       25.6 req/s       6.41 MB/s       39.00 ms/req
  tinyproxy      8 reqs     0.14s       58.8 req/s      14.71 MB/s       17.00 ms/req
  direct         8 reqs     0.00s     2000.0 req/s     500.00 MB/s        0.50 ms/req

erebus is 2.3x slower than tinyproxy and 78.0x slower than direct (large)
```

## Reading the numbers

- **Small responses:** erebus did fine here (~1 ms/request), comfortably
  ahead of tinyproxy on this run and within an order of magnitude of a
  direct loopback fetch. A tiny request/response is just a few segments
  each way, so the userspace path is cheap.
- **Large responses:** this is where the cost shows. At ~6 MB/s erebus is
  ~2–3× slower than the conventional proxy and ~80× slower than raw
  loopback. The culprit is the per-segment send-and-wait-for-ACK design:
  a 256 KiB body is ~180 segments, each a full encrypt → send → wait →
  decrypt round trip. Pipelining/windowing would be the lever to pull if
  throughput ever becomes a goal.

The headline: for the latency-sensitive, modest-payload traffic erebus is
built for (developer access to VPN resources), the overhead is small;
bulk transfer is where the user-space stack pays for its simplicity.
