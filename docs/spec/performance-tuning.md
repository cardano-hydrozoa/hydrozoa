# Performance tuning

How a Hydrozoa head's performance is measured and tuned:
where its limits come from, which instruments show each one,
how to take a measurement that can be trusted, and which settings move it.

This page is for someone looking for a head's limit,
or about to change a setting that moves one.
It holds the framework, not results:
measured numbers change with every code and config change.

## Optimization targets

A head is limited by chains of serial work, not by raw hardware.
A head can be far below every core,
heap and disk limit and still be at its ceiling,
because each block waits on work that cannot overlap.
These are the targets, roughly in the order a request meets them.
Each names the instrument that shows it and what moves it.

- **Admission.** A peer accepts a request only while it has headroom against
  what has been soft-confirmed, and sheds the rest.
  Shown by the rejection counters and sequencer headroom.
  Moved by `backpressureCoefficient` and `maxRequestsPerBlock`.
- **Per-request work on the block's critical path.** The leader applies each
  request in a block to the L2 ledger in order,
  and every follower replays the block before it soft-acks.
  Hashing, encoding,
  mempool bookkeeping and logging are paid once per request on this path.
  Shown by the lead and replay timings per request,
  and by a CPU or allocation profile of the leader.
  Moved by code: anything removed from this path is removed once per request.
- **Block cadence and the soft-consensus round.** A block is soft-confirmed only
  when every head peer has acked it,
  so each block costs a round across the mesh.
  Each peer spends much of that round waiting on the others.
  Shown by the soft-consensus timing and the block rate.
  Moved by `softBlockMinPeriod` and the block gate.
- **Replication to a distant peer.** A coil,
  or any peer behind a long network path,
  pulls the block lane one block per round trip (`maxPerReply` is 1 on that
  lane; see [liaison-backpressure.md](liaison-backpressure.md)).
  Its ingestion is then a property of the geography, not of its hardware.
  [The ceiling model](#the-round-trip-ceiling-model) says when that binds.
  Shown by the peer's block rate and its lag behind the head.
  Moved by the block period,
  and bounded on the serving side by the lane ceilings.
- **The evacuation commitment.** The KZG commitment over the evacuation map
  grows with the number of entries.
  Measured in isolation by the JMH harness in
  [`benchmark/`](../../benchmark/README.md).
- **The slow cycle.** Stack hard confirmation and L1 effect derivation run
  behind block production.
  They bound finality, and when they fall behind,
  the block backlog grows ([slow-consensus.md](slow-consensus.md)).
  Shown by the seconds since the last hard confirmation and the phase
  `StackComposer` is waiting in.
  Moved by `hardStackMinPeriod`.
- **Accumulation.** Some limits are levels that grow with uptime, not rates:
  live fibers and timers, liaison outboxes, process memory, and the store.
  Shown by the runtime gauges and the store's size and open time.
  Moved by bounding each one: lane ceilings and outbox sizes,
  the store's memory and WAL limits.
  These limits fail on a long run, and a short throughput run never sees them.

A **benchmark** of a target is a repeatable measurement of it under stated
conditions.
It takes one of two forms.
A microbenchmark measures an isolated operation, such as the commitment,
with the JMH harness.
A load measurement drives a deployed head and reads the target's instrument from
the stats endpoints.

### The round-trip ceiling model

For a peer that pulls blocks one per round trip:

```
r = RTT / T + b · λ
```

- `RTT` is the round trip to the serving peer.
- `T` is the block period.
- `b` is the per-request replay cost that delays the peer's next pull.
- `λ` is the request rate.

`r` is the ratio of the head's block production to the peer's ingestion.
Below 1, the peer keeps up and its lag settles to the stack-period floor.
Above 1, its backlog grows from one stack to the next.
It grows geometrically rather than linearly,
because each stack waits for the previous one before it closes.
At exactly 1 nothing restores a lag once it opens.

Two consequences follow.
A period below the round trip puts `r` above 1 at any load, including none.
And the smallest stable period is `T_min = RTT / (1 − b · λ)`,
so `softBlockMinPeriod` must sit above that for the most distant peer whose acks
a stack's hard confirmation waits on.

`RTT` here is latency only.
That holds while a reply's bytes are small against the path's bandwidth-delay
product, which a block brief is.
A lane that carries large batches, such as the request lane,
must add the transmission time.

## Instruments

| question | instrument |
|---|---|
| How fast is this peer taking requests, and is it shedding any? | `GET /head/stats` and `GET /head/metrics`: request rates (now, 1m, 5m, 15m), rejections split into screening and backpressure, sequencer headroom ([peer-stats-endpoint.md](peer-stats-endpoint.md)) |
| Where does a block's time go? | Block-lifecycle timings on the same endpoints: lead, replay and soft-consensus, with per-request averages and the slowest blocks by number |
| Is the block lane being shaped? | `blockGate` on the stats endpoints: multiplier, backlog, holds, drains ([rate-limiter.md](rate-limiter.md)) |
| Is the slow cycle keeping up? | Stack counts, seconds since the last hard confirmation, blocks absorbed per stack, and the `StackComposer` phase it is waiting in |
| Is something accumulating? | Runtime gauges: suspended fibers, outstanding timers, locally queued fibers, live threads, process memory |
| What did the head do, and when? | The store, which keeps every request, verdict and block brief ([persistence-and-crash-recovery.md](persistence-and-crash-recovery.md)) |
| What does one operation cost in isolation? | The JMH harness in `benchmark/` |
| What is a JVM peer doing? | JFR or async-profiler (CPU, wall-clock and allocation); a thread dump when a peer stops making progress |

## Method

The loop is: find the binding constraint, change one thing, measure again.

**Choosing what to measure**

- **Judge saturation per process, against that process's own limits.** Compare a
  process's hottest thread with one core, its heap with its own `-Xmx`,
  and disk with `%util`.
  A reading divided by the whole machine cannot show whether any one process is
  at its ceiling.
- **Read a rate for throughput and a level for accumulation.** A throughput run
  reports requests per second, so it cannot find a leak however long it runs.
  Leaks show in the level gauges,
  on soak runs long enough for the level to matter.
- **Prefer CPU-seconds per request to throughput.** It carries across machines
  and does not depend on what else the host was doing.

**Setting up the run**

- **Measure on the deployment's shape.** Use the same instance class and the
  same topology, including the real network path between peers.
  A single machine with no network latency studies the right quantities but
  can't show the round trip that makes them bind.
  A burstable instance throttles under sustained load,
  so it cannot measure throughput at all.
- **Use real, ledger-valid payloads.** Invalid filler is rejected early,
  never exercises absorption, settlement or major blocks,
  and hides whatever lives on those paths.
- **Approach a ceiling from below.** Overload collapses throughput,
  so a run that starts past the ceiling measures the collapse.
  Read the head's accepted rate, not the load generator's offered rate.
- **Confirm what is running** before measuring: the commit,
  the effective config and the JVM options of every peer.
  Record a result with those conditions, the machine and the load.
  A number measured before a change to the code or config it depends on
  describes a different system.
- **Run performance measurements one at a time.** Contention moves a throughput
  or memory number.
  Correctness runs may share a machine,
  but repeat a resource-shaped failure (an out-of-memory error,
  a timeout) on its own before believing it.

**Reading the result**

- **Compare like with like:** the same ramp step, the same load,
  the same head state.
  A resource measured at one state does not bound it at another.
  An L2 ledger costs more with a large state than with a small one.
- **Keep a negative control.** A setting or change credited with an improvement
  should stop mattering where the model says it should.
  A gate that shapes under load should be inert at low load.

## Tuning settings

| setting | scope | what it moves | reference |
|---|---|---|---|
| `rateLimits.softBlockMinPeriod` | node-local | the block period `T` | [rate-limiter.md](rate-limiter.md) |
| `rateLimits.hardStackMinPeriod` | node-local | the stack period: finality, and how often the head pays for a stack's effects | [rate-limiter.md](rate-limiter.md) |
| `rateLimits.blockBacklog*`, `blockGate*` | node-local | where the block gate starts shaping, and how hard | [rate-limiter.md](rate-limiter.md) |
| `maxRequestsPerBlock`, `backpressureCoefficient` | head-wide, fixed for the head's life | block size, and how far admission may run ahead of soft confirmation | [head-params-hash.md](head-params-hash.md), [liaison-backpressure.md](liaison-backpressure.md) |
| `peerLiaisonMaxRequestsPerBatch`, `peerLiaisonOutboxDepth` | node-local | request-lane reply width, and the outbox each lane holds for a slow peer | [liaison-backpressure.md](liaison-backpressure.md) |
| `HYDROZOA_ROCKSDB_*` environment variables | node-local | store memory, open files and WAL size | [`RocksDbBackendStore.scala`](../../src/main/scala/hydrozoa/multisig/persistence/rocksdb/RocksDbBackendStore.scala) |

Head-wide parameters are folded into `headParamsHash` and pinned on L1 when the
head is initialized, so they are chosen once, before the head starts.
Node-local settings change only how a peer paces its own work,
never what it signs,
so peers may run different values and change them with a restart.

Two rules govern the periods:

- **The gate thresholds are multiples of one stack cycle.** One cycle is
  `hardStackMinPeriod / softBlockMinPeriod` blocks.
  Change either period and move the backlog limits with it,
  or the gate's setpoint shifts without notice.
- **A shorter stack period cannot buy a shorter block period.**
  `hardStackMinPeriod` does not appear in the ceiling model.
  It sets the floor a lag settles to, and how often the head pays for a stack.
