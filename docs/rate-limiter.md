# Rate limiter

A small, generic actor that throttles the rate of selected messages flowing between two
actors on a single lane. It is used to slow down the fast and slow consensus cycles (longer
block / stack durations) without changing any consensus logic.

## How it works

`Limiter[Msg]` (`multisig/consensus/limiter/Limiter.scala`) is an actor that sits *between* an
upstream actor and a `downstream` actor. Its own `ActorRef` is wired to the upstream in place
of `downstream`, so everything the upstream sends to `downstream` passes through it first.

It is **stateless** — it keeps no memory of previous messages. For each incoming message:

- If the message mixes in `LimiterTimestamp`, the limiter computes
  `gate = msg.limiterTimestamp + msg.minPeriod` and, if `gate` is still in the future, sleeps
  until then before forwarding; otherwise it forwards immediately.
- Any message that does **not** mix in `LimiterTimestamp` is forwarded with no delay.

So each throttled message gates *itself* from the timestamp it carries — there is no
"remember when I last forwarded" bookkeeping.

Ordering is **strict FIFO**: there is one mailbox and the `IO.sleep` happens inside `receive`,
so a held message blocks every later message (throttled or not) on that lane until the hold
elapses. Using an actor (rather than a fiber per message) is what guarantees no out-of-order
or concurrent delivery to `downstream`.

`LimiterTimestamp` (`limiter/LimiterTimestamp.scala`) is the marker trait:

```scala
trait LimiterTimestamp {
  def limiterTimestamp: Instant                          // when the upstream work "ended"
  def minPeriod(using RateLimits.Section): FiniteDuration // min wall-clock gap for this lane
}
```

`RateLimits` (`config/node/operation/multisig/RateLimits.scala`) holds the per-lane minimum
periods (`softBlockMinPeriod`, `hardStackMinPeriod`, …). It is part of
`NodeOperationMultisigConfig`, so every actor that already takes that config can read the knobs.

## How it is wired today

Two lanes are throttled, spawned in `MultisigRegimeManager` and exposed on `Connections` as
`blockWeaverLimiter` / `stackComposerLimiter`:

| Lane | Throttled message | `limiterTimestamp` | `minPeriod` |
|------|-------------------|--------------------|-------------|
| `ConsensusActor → BlockWeaver` | `Block.SoftConfirmed` | block-creation end-time | `softBlockMinPeriod` |
| `SlowConsensusActor → StackComposer` | `Stack.HardConfirmed` | brief `creationEndTime` | `hardStackMinPeriod` |

Only the *upstream's* reference to the downstream is routed through the limiter; other senders
to the same downstream keep their direct handle. The `Limiter` logs under the `Limiter` logger
name (present in all three `logback.xml`).

## Adding a new throttled lane

1. **Mark the message.** Make the message type extend `LimiterTimestamp`, implementing
   `limiterTimestamp` (the wall-clock instant the work it represents finished) and `minPeriod`
   (reading a knob from `RateLimits.Section`).
2. **Add the knob.** Add a `FiniteDuration` field + accessor to `RateLimits` for the new lane.
3. **Spawn + wire.** In `MultisigRegimeManager.preStartLocal`, spawn
   `Limiter[DownstreamMsg](downstream, config, tracerLocal)`, add its handle to `Connections`,
   and point the *upstream* actor's reference-to-downstream at that handle (leave other senders
   on the direct handle).

That's all — the throttle is transparent to both actors.

## Notes and caveats

- **Defaults gate everything.** With non-zero defaults the limiter is active in production *and*
  tests; set the periods to zero to disable.
- **Wall-clock vs virtual time.** Under `TestControl` the `IO.sleep` advances virtual time (and
  inflates the timeline TestControl must replay); under a real clock (e.g. stage4 WS) it is a
  genuine wall-clock delay.
- **L1 validity windows.** Throttling the slow cycle delays L1 effect submission. If a period
  exceeds an effect's validity window (settlement TTL, fallback start), the happy path expires
  and the head falls back. To exercise throttling *and* have effects land, keep the periods
  inside the L1 timing slack.
