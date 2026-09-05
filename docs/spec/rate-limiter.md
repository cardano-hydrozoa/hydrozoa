# Rate limiter

A generic actor that bounds the rate at which selected messages flow between two actors on one
lane. It slows the fast and slow consensus cycles — longer block and stack durations — without
changing any consensus logic.

This page is for someone tuning a head's cadence or adding a new throttled lane.

## How it works

`Limiter[Msg]` (`multisig/consensus/limiter/Limiter.scala`) sits *between* an upstream actor and a
`downstream` actor. Its own `ActorRef` is wired to the upstream in place of `downstream`, so
everything the upstream sends to `downstream` passes through it first. It accepts
`Msg | LimiterControl`: the control messages are consumed for its own account and never forwarded,
so the downstream actor needs no case for them.

It is a **spacing gate**. The limiter remembers the monotonic time of its own last release and
holds the next throttled message until `period` has elapsed since then:

- A message mixing in `LimiterTimestamp` is throttled. It is released when
  `now >= lastRelease + period`, and held otherwise.
- `period` is the message's own `minPeriod` divided by the gate multiplier, which is `1.0` on a
  lane with no gate.
- A message that does **not** mix in `LimiterTimestamp` is forwarded immediately when nothing is
  held, and queued behind whatever is held otherwise.
- A message whose `limiterExempt` is `true` is released without pacing, in arrival order, and does
  not restart the spacing clock. Pacing a message that ends the lane adds no ongoing backlog and
  only costs latency; releasing it ahead of the queue would reorder the lane.

⛔ The gate comes from the limiter's memory, never from the message. Gating on
`msg.limiterTimestamp + minPeriod` is an **age filter**: it delays each message by a fixed amount
and lets the arrival rate through unchanged, so it bounds latency rather than rate. It coincides
with a rate limit only on a lane that is already single-flight, where releasing one message is what
creates the next. Do not "simplify" to it.

Spacing is measured on `IO.monotonic`, so a wall-clock step cannot open or freeze the gate and the
resolution of `limiterTimestamp` does not matter. That resolution is coarse: `BlockCreationEndTime`
is a `QuantizedInstant` snapped to a one-second Cardano slot, ten times coarser than the block
lane's default period.

Ordering is **strict FIFO** across one explicit queue. A hold is a chain of short sleeps, each
followed by a self-sent `LimiterControl.Tick`, rather than one sleep for the whole wait — so the
mailbox drains between slices and a drain signal is acted on at most one slice late. The slice is
the gate's `slice` on a gated lane and 1 s on an ungated one.

`LimiterTimestamp` (`limiter/LimiterTimestamp.scala`) is the marker trait:

```scala
trait LimiterTimestamp {
  def limiterTimestamp: Instant                           // end-time of the upstream work
  def minPeriod(using RateLimits.Section): FiniteDuration  // min gap for this lane
  def limiterExempt: Boolean = false                       // released unpaced, in arrival order
}
```

`limiterTimestamp` is not consulted for gating. It stays on the trait because it names what the
message represents, and it is what a future lane would key on if one ever needed the age.

## How it is wired today

Two lanes are throttled, spawned in `HeadMultisigRegimeManager.preStartLocal` and exposed on
`Connections` as `blockWeaverLimiter` / `stackComposerLimiter`:

| Lane | Throttled message | `minPeriod` | Gate |
|------|-------------------|-------------|------|
| `FastConsensusActor → BlockWeaver` | `Block.SoftConfirmed` | `softBlockMinPeriod` | `blockLimiterGate` |
| `SlowConsensusActor → StackComposer` | `Stack.HardConfirmed` | `hardStackMinPeriod` | none |

Only the *upstream's* reference to the downstream is routed through the limiter; other senders to
the same downstream (`JointLedger`, `PeerLiaisonHeadToHead`, …) keep their direct handles.

A coil spawns no limiters at all: `CoilMultisigRegimeManager` aliases both slots to the unthrottled
handles and sets `blockRateGate = None`.

⚠️ Each lane is expected to be **single-flight** — upstream has at most one throttled message
outstanding, because the release of one is what produces the next. The limiter does not enforce
this. A second queued throttled message emits `QueueDepthUnexpected` at `warn` and is then handled
by ordinary FIFO queueing, which stays correct if the invariant is ever relaxed.

## The backlog gate

The block lane carries a `LimiterGate` on top of the spacing gate, making its period dynamic. The
spacing gate bounds the floor on cycle time; the backlog gate bounds the ceiling on outstanding
work. Neither subsumes the other.

The limiter counts throttled messages it releases. On each `LimiterControl.DownstreamDrained` it
folds that count into an EWMA and re-derives a multiplier from the filtered value:

```
headroom h = clamp((backlogHardLimit - residual) / (backlogHardLimit - backlogSoftLimit), 0, 1)
multiplier = floor + (1 - floor) * (1 - (1 - h)^2)
period     = ceil(minPeriod / multiplier)      // applied only when multiplier < 1
```

`1 - (1 - h)^2` is flat near full headroom and steepens as it runs out. ⛔ Not `h^2`, whose highest
gain sits where the system is healthy: the loop's dead time is a full downstream cycle, and high
gain across long dead time produces a limit cycle.

`SlowConsensusActor` sends `DownstreamDrained` to `Connections.blockRateGate` on each stack hard
confirmation — one event per downstream cycle, strictly later than the peer's own hard ack, so it
subsumes it. It cannot deadlock against the gate: hard confirmation needs hard acks and the stack
clock, never new blocks, so throttling the block lane to its floor can never suppress the event
that lifts the throttle.

⛔ The gate is **opt-in**, and that is load-bearing. The stack lane uses the same class and is never
sent a drain signal; a lane that counted releases unconditionally would stretch its period without
bound, silently, and only under integration load.

⛔ `floor` is never `0`. Block production is self-clocked by the soft confirmations this lane
releases, so a full stop would stop the clock that later reopens the gate.

## Config

`RateLimits` (`config/node/operation/multisig/RateLimits.scala`) holds every knob. It is part of
`NodeOperationMultisigConfig`, so every actor that already takes that config can read them.

| Field | Default | Meaning |
|---|---|---|
| `softBlockMinPeriod` | `100.milliseconds` | Block lane's minimum gap |
| `hardStackMinPeriod` | `30.seconds` | Stack lane's minimum gap |
| `blockBacklogSoftLimit` | `600` | Backlog below which the block gate is fully open |
| `blockBacklogHardLimit` | `3000` | Backlog at which the multiplier reaches the floor |
| `blockGateFloor` | `0.02` | Slowest the block lane is ever shaped to, as a fraction of its rate |
| `blockGateSmoothing` | `0.3` | EWMA weight on the newest cycle's residual |
| `blockGateSlice` | `150.milliseconds` | Longest single sleep before the limiter re-reads its mailbox |

⚠️ **Node-local.** These change only the cadence at which a peer offers work to its own downstream,
never what it sends, so peers may run different values without diverging. The cost is a
leader-dependent cadence under rotation; operators should align informally.

The `Decoder` is hand-written rather than derived so that every gate field may be **absent** — a
node whose config fails to decode does not start at all, and the defaults are chosen so adopting
the gate needs no config edit. Set the two periods to zero to disable throttling.

## Observability

The limiter traces at `debug`, and nodes run at `WARN`, so the gate is also published to the stats
endpoints (`docs/spec/peer-stats-endpoint.md`). Nothing in the control path reads those values
back; the multiplier acted on is the one in the actor's state.

| Surface | Where |
|---|---|
| `/head/stats` | `blockGate: { multiplier, backlog, residual, holds, drains }` |
| `/metrics` | `hydrozoa_block_gate_{multiplier,backlog,residual,holds_total,drains_total}` |
| Logs | `Limiter.BlockWeaver` / `Limiter.StackComposer`, under the `Limiter` logger |

`LimiterEvent` variants: `Started`, `HoldingMsg` (once when a hold begins, not once per slice),
`GateUpdated`, `QueueDepthUnexpected` (the only one at `warn`).

## Adding a new throttled lane

1. **Mark the message.** Make the message type extend `LimiterTimestamp`, implementing
   `limiterTimestamp` (the instant the work it represents finished) and `minPeriod` (reading a knob
   from `RateLimits.Section`). Override `limiterExempt` for a message that ends the lane.
2. **Add the knob.** Add a `FiniteDuration` field + accessor to `RateLimits` for the new lane, and
   a matching branch in its hand-written `Decoder`.
3. **Spawn + wire.** In `HeadMultisigRegimeManager.preStartLocal`, spawn
   `Limiter[DownstreamMsg](downstream, config, tracerLocal)`, add its handle to `Connections`, and
   point the *upstream* actor's reference-to-downstream at that handle (leave other senders on the
   direct handle).
4. **Only if the lane needs a backlog gate.** Pass `gate = Some(...)` and `metrics = Some(metrics)`,
   and arrange for something to send `LimiterControl.DownstreamDrained` once per downstream cycle.
   Without that signal the gate ratchets the period toward infinity.

Steps 1–3 are transparent to both actors.

## Notes and caveats

- **Defaults gate everything.** With non-zero defaults the limiter is active in production *and*
  tests.
- **Failure takes the node down.** `MultisigRegimeManagerBase` escalates every supervisor
  directive, so the limiter is not restarted and anything held is lost. That is the safe outcome
  for a self-clocked lane: upstream waiting forever on a release that was silently dropped is a
  stall with nothing in the log, whereas a crash restarts and recovers its position from
  persistence. ⛔ Do not catch exceptions here to "be safe" — swallowing one converts the crash into
  the stall.
- **Wall-clock vs virtual time.** Under `TestControl` the slice sleeps advance virtual time, and a
  hold is one sleep per slice rather than one for the whole wait, which inflates the timeline
  `TestControl` must replay; under a real clock (e.g. stage4 WS) they are genuine delays.
- **L1 validity windows.** Throttling the slow cycle delays L1 effect submission. If a period
  exceeds an effect's validity window (settlement TTL, fallback start), the happy path expires and
  the head falls back. To exercise throttling *and* have effects land, keep the periods inside the
  L1 timing slack.
- **No majors denominator.** The multiplier is derived from released blocks alone. Trading load
  makes almost no majors (2 in ~240k blocks) and a threshold for them needs a deposit-flood run to
  calibrate. `m = min(...)` over named signals leaves the seam.
