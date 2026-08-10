# Peer statistics endpoint

Status: **as-built reference**. A cheap, always-on `GET /head/stats` (and Prometheus
`GET /head/metrics`) that reports live operational metrics for a single peer without touching storage
or perturbing the hot path.

## Motivation

Today the only ways to see what a peer is doing are the logs (which, under the shipped
`logback-docker.xml`, run at `root=warn` and hide all the fast-consensus INFO detail) and the
`/head/blocks` listing (which returns *every* block — thousands of entries — and says nothing about
rates). Answering simple operational questions — "how many requests per second is this peer taking?",
"are blocks filling up or running near-empty?", "how much of the load is local vs pulled from
peers?" — currently means scraping logs or post-processing the full block list.

We want a single endpoint that answers those questions in O(1), is safe to poll continuously, and
cannot measurably affect throughput.

## Goals

- Report, per peer:
  - **local incoming requests** — total counter + TPS (now / 1m / 5m / 15m, `top`-style).
  - **peer-to-peer requests** — total counter *per head peer* + TPS the same way.
  - **block statistics** — total minor/major, average and maximum block size (events), and EWMA
    throughput rates for blocks/sec and requests-in-blocks/sec (now / 1m / 5m / 15m).
  - **stack statistics** — total hard-confirmed, last stack number, seconds since last hard-confirm,
    mean inter-stack gap, and average/maximum stack size (blocks absorbed).
- Ship **two representations of the same registry**: a human/dashboard JSON view (`GET /head/stats`)
  and a **Prometheus** text exposition (`GET /head/metrics`) — both from day one.
- Be **cheap**: no RocksDB reads, no scan of the block history, no actor `ask` on the hot path.
- Be **safe to poll**: a monitoring agent hitting it every second must not compete with consensus.

## Non-goals

- Durable metrics. Counters are process-lifetime and reset on restart (see
  [Resolved decisions](#resolved-decisions)).
- A full time-series database. This is a point-in-time snapshot; graphing is the caller's job — a
  Prometheus scrape target (`GET /head/metrics`) is provided, but not storage/retention.

## Design principles

1. **Push, don't pull.** Each counter is incremented at the moment its event happens — a single
   atomic add (nanoseconds, no allocation, no lock). The endpoint never scans anything.
2. **Derive rolling views off the hot path.** One background fiber, waking at a fixed cadence (1 Hz),
   updates the EWMA load averages. The hot path only ever increments.
3. **Snapshot read, no actor ask.** The endpoint materializes the current numbers from an in-memory
   registry, mirroring how `/ready` reads a `NodeStatus` value instead of issuing a `GetState` to an
   actor (`HydrozoaServer` already threads `nodeStatus: IO[NodeStatus]` into the routes — the stats
   registry is threaded the same way).

## Architecture: the `PeerMetrics` registry

One object, created at boot, shared **by reference** to the actors that emit events and to
`HydrozoaRoutes`.

```scala
/** Process-lifetime peer metrics. Hot-path methods are lock-free side effects; `snapshot`
  * produces a consistent read for the /head/stats endpoint. */
final class PeerMetrics private (
    // ---- cumulative counters (hot path) ----
    localAccepted:      AtomicLong,                 // RequestSequencer -> Right(id)
    localRejScreening:  AtomicLong,                 // RequestSequencer -> Left(Rejected), screening
    localRejBackpressure: AtomicLong,               // RequestSequencer -> Left(Rejected), window full
    peerRequests:       Map[HeadPeerId, LongAdder], // requests ingested from each remote peer
    blocksMinor:        AtomicLong,
    blocksMajor:        AtomicLong,
    blockEventsSum:     AtomicLong,                 // Σ events, for the running average
    blockEventsMax:     AtomicLong,                 // max via CAS
    stacksTotal:        AtomicLong,
    lastStackNum:       AtomicLong,
    lastStackMillis:    AtomicLong,                 // wall-clock of the last hard-confirm
    stackGapSumMillis:  AtomicLong,                 // Σ inter-stack gaps, for the mean
    stackBlocksSum:     AtomicLong,                 // Σ blocks absorbed, for the average
    stackBlocksMax:     AtomicLong,                 // max via CAS
    startedAtMillis:    Long,
    // ---- derived, owned by the 1 Hz sampler fiber ----
    rolling:            AtomicReference[PeerMetrics.Rolling]
):
    // hot path (called from the owning actor)
    def onLocalAccepted(): Unit
    def onLocalRejected(kind: RejectionKind): Unit
    def onPeerRequests(from: HeadPeerId, n: Int): Unit
    def onBlockConfirmed(isMajor: Boolean, events: Int): Unit
    def onStackConfirmed(stackNum: StackNumber, blocksAbsorbed: Int): Unit

    // read path (called from the HTTP thread)
    def snapshot: PeerStats
```

### Threading model — `AtomicLong` vs `LongAdder`

Each cats-actor drains its mailbox **serially**, so a counter written from inside one actor is
**single-writer**. It still needs *safe publication* to the HTTP reader thread — a bare `var Long`
is not enough — but a plain `AtomicLong` suffices (no write contention, so `LongAdder`'s striping
buys nothing):

- `localAccepted` / `localRej*` — written only by `RequestSequencer` → `AtomicLong`.
- `blocks*` / `blockEvents*` — written only by `FastConsensusActor` → `AtomicLong`.

Only the **peer-request** counters can be written concurrently (ingestion fans in from multiple
peer-liaison actors/threads), so those use `LongAdder`, which is built for high-contention adds.

The `rolling` view (the EWMA rates) is written only by the single sampler fiber, so a `@volatile`
reference / `AtomicReference` publishes it to readers.

## Metrics catalog

| Metric | Type | Notes |
|---|---|---|
| `localAccepted` | counter + rate | requests this peer sequenced successfully |
| `localRejScreening` | counter | rejected by L1/L2 screening |
| `localRejBackpressure` | counter | rejected by the sequencer window (backpressure) |
| `peerRequests[p]` | counter + rate, per peer | requests pulled from remote head peer `p` |
| `blocksMinor` / `blocksMajor` | counter | soft-confirmed blocks by type |
| avg block size | derived | `blockEventsSum / (blocksMinor + blocksMajor)` |
| max block size | gauge | `blockEventsMax` |
| block rate | EWMA | blocks/sec: now / 1m / 5m / 15m |
| requests-in-blocks rate | EWMA | req/sec: now / 1m / 5m / 15m |
| local & peer TPS | EWMA | now / 1m / 5m / 15m |
| `stacksTotal` | counter | hard-confirmed stacks |
| last stack number | gauge | `lastStackNum` |
| seconds since last hard-confirm | derived | `now - lastStackMillis` |
| mean inter-stack gap | derived | `stackGapSumMillis / (stacksTotal - 1)` |
| avg / max stack size | derived / gauge | blocks absorbed: `stackBlocksSum / stacksTotal`, `stackBlocksMax` |
| uptime | derived | `now - startedAtMillis` |

## Instrumentation points

Three call sites, all on paths that already exist — no new plumbing:

| Metric | Hook |
|---|---|
| local accepted / rejected(kind) | `RequestSequencer` — the `Right(id)` vs `Left(UserRequest.Rejected(...))` decision branches (screening vs backpressure are already distinct branches). Counting here — not in the route — also captures any non-HTTP submission path. |
| peer-to-peer requests per peer | the request lane where a remote peer's requests enter the local mempool (the peer liaison / puller receive path); `onPeerRequests(from, batch.size)`. |
| blocks minor/major + event count | `FastConsensusActor.completeCell` — the single funnel every soft-confirmed block passes through. It already computes `brief`, already switches `Minor`/`Major`, and `brief.requests.size` is the event count. One `metrics.onBlockConfirmed(isMajor, brief.requests.size)` next to the existing `BlockSoftConfirmed` emission. |
| stacks + size | `SlowConsensusActor.completeStack` — the single funnel where a `Stack.HardConfirmed` is produced. `stackBrief.stackNum` and `lastBlockNum - firstBlockNum + 1` (blocks absorbed) give the size. One `metrics.onStackConfirmed(stackNum, blocksAbsorbed)`. |

Because `completeCell` / `completeStack` are the *only* places a soft-confirmed block / hard-confirmed
stack is produced, those stats need exactly one line each and can never drift from reality.

## All rates are EWMA

There are **no fixed-window rings**. Every rate — local requests, per-peer requests, block
production, and requests-in-blocks — is a top-style EWMA load average (now / 1m / 5m / 15m). The
sampler holds one small set of EWMA accumulators (a few doubles each) and never stores per-second
history, so nothing has to be recomputed or aged out.

## TPS load averages (EWMA)

An **exponentially weighted moving average** keeps *one number* instead of a window of samples; each
new sample pushes the average toward itself and old samples fade geometrically. This is exactly what
`top`/`uptime` load averages are, so we mirror them.

### Recurrence

```
avgₙ = avgₙ₋₁ · decay + sampleₙ · (1 − decay)
where decay = exp(−Δt / τ)
```

- **τ (tau)** — the time constant / memory length. After τ seconds a sample's weight has decayed to
  1/e ≈ 37%. The `top`-style triple uses τ = 60s, 300s, 900s.
- **Δt** — elapsed time since the last update.
- **sample** — the instantaneous rate this tick = `(events since last tick) / Δt`. Feed the *rate*
  (req/s), not the raw count, so the units stay stable when Δt jitters.

`decay` is the fraction of the old value kept; `1 − decay` is how much the new sample counts. Long τ →
decay near 1 → smooth/sluggish. Short τ → snappy/noisy. Note a "1-minute load average" is not "the
average over exactly the last 60s" — it is a smoothing with ~1-minute memory. That soft edge is the
point.

### How `top` does it

`top` samples every 5s and folds into three EWMAs. With Δt = 5s:

| horizon | τ | decay = exp(−5/τ) |
|---|---|---|
| 1 min | 60 s | 0.9200 |
| 5 min | 300 s | 0.9835 |
| 15 min | 900 s | 0.9945 |

(The kernel stores these fixed-point as `EXP_1=1884, EXP_5=2014, EXP_15=2037` out of 2048.)

### Implementation

```scala
/** One EWMA horizon. Written only by the sampler fiber; `get` is read cross-thread. */
final class Ewma(tauSeconds: Double):
    @volatile private var v: Double = 0.0
    def observe(ratePerSec: Double, dtSeconds: Double): Unit =
        val decay = math.exp(-dtSeconds / tauSeconds)   // recompute per tick — see below
        v = v * decay + ratePerSec * (1.0 - decay)
    def get: Double = v
```

Driven by the same 1 Hz sampler that rolls the ring:

```scala
val dt      = (now - last).toDouble                 // actual elapsed, seconds
val delta   = cumulativeNow - cumulativeAtLastTick  // events this interval
val instant = delta / dt                            // req/s "now"
ewma1m.observe(instant, dt); ewma5m.observe(instant, dt); ewma15m.observe(instant, dt)
```

Cost: three `exp` + three multiply-adds per second.

### Practical choices

- **Recompute `decay` from the measured Δt each tick.** `top` hard-codes constants for a fixed 5s
  cadence; if the sampler fiber is ever late (GC, scheduling) a fixed constant silently overweights
  that interval. `exp(−Δt/τ)` from the real Δt is self-correcting and free.
- **Sampler interval.** 1s (finer than `top`'s 5s) gives a responsive "now" and smooth 1/5/15 lines.
  Report **"now"** as the last tick's raw `instant` (or the ring's last-10s count / 10); 1m/5m/15m
  are the EWMAs.
- **Cold start.** Initialize to 0.0; averages ramp over ~τ (like `top` right after boot). Optional
  bias correction `v / (1 − decayⁿ)` for the first few ticks — not worth it, the ramp is expected.
- **Idle decay is free and correct.** No traffic → `instant = 0` → averages glide to 0, matching
  `top` when load drops.
- **Single-writer visibility.** Only the sampler writes the EWMA accumulators → the published
  `Rolling` snapshot (one `AtomicReference`) is what the endpoint reads.

## The endpoints

Two read-only, unauthenticated endpoints over the **same** `metrics.snapshot`:

```scala
// GET /head/stats  — human/dashboard JSON
private val statsEndpoint: ServerEndpoint[Any, IO] =
    endpoint.get.in("head" / "stats")
        .name("getHeadStats").tag("Observability")
        .out(jsonBody[PeerStatsView])
        .errorOut(errorOut)
        .description("Live operational metrics for this peer. Cheap; process-lifetime counters.")
        .serverLogic(_ => IO(metrics.snapshot).map(ApiDto.mkPeerStatsView).map(Right(_)))

// GET /head/metrics  — Prometheus text exposition (version 0.0.4)
private val metricsEndpoint: ServerEndpoint[Any, IO] =
    endpoint.get.in("head" / "metrics")
        .name("getHeadMetrics").tag("Observability")
        .out(stringBody.and(header(Header.contentType(
              MediaType.unsafeParse("text/plain; version=0.0.4; charset=utf-8")))))
        .errorOut(errorOut)
        .description("Same metrics in Prometheus exposition format.")
        .serverLogic(_ => IO(metrics.snapshot).map(PrometheusFormat.render).map(Right(_)))
```

DTO sketch (final shapes live in `ApiDto`):

```scala
final case class RateView(now: Double, load1m: Double, load5m: Double, load15m: Double)
final case class CounterWithRate(total: Long, rate: RateView)

final case class BlockStatsView(
    minor: Long,
    major: Long,
    avgEvents: Double,
    maxEvents: Long,
    blockRate: RateView,   // blocks/sec, EWMA
    requestRate: RateView  // requests-in-blocks/sec, EWMA
)

final case class StackStatsView(
    total: Long,
    lastStackNumber: Long,
    secondsSinceLastHardConfirm: Long,
    meanInterStackGapSeconds: Double,
    avgBlocksAbsorbed: Double,
    maxBlocksAbsorbed: Long
)

final case class PeerStatsView(
    uptimeSeconds: Long,
    localRequests: CounterWithRate,
    localRejectedScreening: Long,
    localRejectedBackpressure: Long,
    peerRequests: Map[Int, CounterWithRate],  // keyed by head-peer id
    blocks: BlockStatsView,
    stacks: StackStatsView
)
```

### Prometheus mapping

The Prometheus idiom is to expose **monotonic counters** (`_total`) and let the server compute rates
with `rate(...[5m])`; the EWMA rates are a human convenience and are exposed as *gauges*. Both
endpoints read the same snapshot, so they never disagree. Naming (`hydrozoa_` prefix, `_total`
suffix on counters, labels for dimensions):

```
# HELP hydrozoa_local_requests_total Requests this peer sequenced successfully.
# TYPE hydrozoa_local_requests_total counter
hydrozoa_local_requests_total 1234
# TYPE hydrozoa_local_requests_rejected_total counter
hydrozoa_local_requests_rejected_total{reason="screening"} 5
hydrozoa_local_requests_rejected_total{reason="backpressure"} 4210
# TYPE hydrozoa_peer_requests_total counter
hydrozoa_peer_requests_total{peer="1"} 987
# TYPE hydrozoa_blocks_total counter
hydrozoa_blocks_total{type="minor"} 3860
hydrozoa_blocks_total{type="major"} 2
# TYPE hydrozoa_block_events_max gauge
hydrozoa_block_events_max 1000
# TYPE hydrozoa_stacks_total counter
hydrozoa_stacks_total 10
# TYPE hydrozoa_stack_last_number gauge
hydrozoa_stack_last_number 10
# TYPE hydrozoa_seconds_since_last_hard_confirm gauge
hydrozoa_seconds_since_last_hard_confirm 42
# TYPE hydrozoa_local_requests_load gauge
hydrozoa_local_requests_load{window="1m"} 3.7
```

`PrometheusFormat.render(snapshot): String` is a pure function over the snapshot — no framework
dependency, trivially unit-testable line-by-line.

## Cost analysis

- **Hot path:** one atomic add per request / block — nanoseconds, no allocation, no lock. `LongAdder`
  only where writes actually contend (peer ingestion).
- **Background:** one fiber per second rolling ~a few KB and doing 3 `exp`s.
- **Endpoint:** a bounded snapshot (O(1) counters + one pass over the ring), no I/O.

It cannot meaningfully affect throughput — which is the requirement.

## Resolved decisions

- **EWMA only — no fixed-window rings.** Every rate (local, per-peer, block, requests-in-blocks) is a
  top-style EWMA load average (now / 1m / 5m / 15m). No per-second history is kept, so nothing has to
  be recomputed or aged out.
- **No persistence.** Counters are in-memory and process-lifetime; they reset on restart. Documented in
  the endpoint descriptions. (Persisting would add hot-path cost for little operational value.)
- **Sampler cadence: 1 Hz.** The EWMA τ values are independent of it.
- **Stacks are first-class**, not an extension — instrumented at `SlowConsensusActor.completeStack`.
- **Prometheus from day one** — `GET /head/metrics` alongside the JSON `GET /head/stats`, both over the
  same snapshot.
- **Auth.** Both endpoints are read-only and non-sensitive, so unauthenticated like `/head/info` and
  `/head/blocks`.

## Later / out of scope

- **Block-size distribution** (p50/p95 via a small fixed-bucket histogram) — average+max ship now;
  percentiles can follow if the cap behavior needs finer visibility.
