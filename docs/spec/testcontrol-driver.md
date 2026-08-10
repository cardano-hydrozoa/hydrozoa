# TestControl Driver in Integration Suites

This document explains how `ModelBasedSuite` uses cats-effect `TestControl` to run model-based integration tests with a simulated virtual clock. It covers the design rationale, the communication protocol between the inner and outer IO programs, and the tick-driving functions.

## Why TestControl

Hydrozoa's protocol involves real time: block effects have deadlines, and fallback transactions become valid after a delay. A realistic test scenario might span hours or days of protocol time.

Running such tests against real time would be impractical — a test that validates a 24-hour head lifetime would take 24 hours. `TestControl` solves this by replacing the real clock with a virtual one that only advances when explicitly told to. The protocol actors see the virtual clock as real; the test driver advances it instantly.

The integration tests that use a mock Carda mno backend (stage1 `Mock` mode) run entirely under `TestControl`. Tests using a real Yaci backend run against real time with ordinary `IO.sleep` delays.

## Two Runtimes

`TestControl` offers automatic execution (`tickAll`, `tick`) but we do not use it. Those methods advance the clock to the next timer on every iteration and would loop indefinitely over the per-actor 1-second ping loop, giving no opportunity to interleave test commands between clock advances.

Instead, we drive the scheduler manually via `tc.tickOne` and `tc.advance` from a separate runtime — the outer driver. This gives us precise control: advance exactly the delay declared by each command, drain all resulting actor work, then run the next command.

`ModelBasedSuite` therefore splits execution into two runtimes running concurrently:

- **Inner**: the SUT (actor system, consensus logic, ledger) runs here, on the `TestControl` scheduler. All `IO.sleep` calls are intercepted by the virtual clock and do not consume real time.
- **Outer**: the test driver runs here, on the standard `global` runtime. It steps the inner forward with `tc.tickOne` and moves the virtual clock forward with `tc.advance`.

The inner is started with `TestControl.execute(innerIO)`, which returns a `TestControl[A]` handle. Nothing in the inner runs until the outer calls `tc.tickOne` or `tc.advance`.

## cats-actors Internals: Why This Works

Understanding why the driver works requires knowing how cats-actors delivers messages.

### Mailbox: Deferred-based, not sleep-based

`Mailboxes.createMailbox` blocks on `Deferred.get` when both the normal and priority queues are empty. When a message is sent to an actor (`actorRef ! msg`), the mailbox calls `deferred.complete(())`, which re-enqueues the mailbox fiber in the scheduler as immediately eligible. There is no `IO.sleep` involved in message delivery.

This means: every enqueued actor message produces an immediately-eligible fiber. `tc.tickOne` can process it without needing `tc.nextInterval` or `tc.advance`. The entire chain — request sent → actor wakes → processes message → sends reply → requester wakes — is driven purely by `tickOne` returning `true`.

### The 1-second Ping Loop

Each actor runs a `Temporal[F].sleep(1.second)` ping loop (in `ActorCell.start`). This calls `handlePing → receiveTimeout.checkTimeout`. It is load-bearing for `CardanoLiaison`, `EvacuationActor`, and `DisputeActor`, which use `setReceiveTimeout`.

Under `TestControl`, this sleep is virtual. When the driver advances the clock by 1 second or more (via `tc.advance`), the ping timers become eligible and are drained by the `tickOne` loop. For advances shorter than 1 second, the ping does not fire and does not require `nextInterval`.

### shutdownSut: IO.sleep for Polling

`waitForIdle` (called from `shutdownSut`) polls the actor system's idle state with `IO.sleep(100 millis)`. This sleep is also virtual under `TestControl`. It is the reason the driver needs a special shutdown phase — see the Sentinel section below.

## Communication Protocol

The inner and outer runtimes need to coordinate: the inner must tell the outer how far to advance time, and the outer must tell the inner when it is safe to run the next command.

### AtomicReference: inner → outer

```scala
val pendingDelay = new AtomicReference[Option[(FiniteDuration, Deferred[IO, Unit])]](None)
```

Before each command, the inner writes `Some((delay, gate))` here, then immediately blocks on `gate.get`. The outer polls this reference between `tickOne` calls to detect the signal.

`AtomicReference` is used — not `Deferred` — because the outer must poll it without blocking. If the outer blocked waiting for a signal, it could not call `tickOne`, and the inner could never make progress to post the signal. That would be a deadlock.

A `cats.effect.std.Queue` with `tryTake` would be a more idiomatic alternative, but we use `AtomicReference` for now to keep things simple.

### Deferred Gate: outer → inner

The gate carried inside `pendingDelay` is a `Deferred[IO, Unit]`. After advancing the clock, the outer calls `gate.complete(())` to unblock the inner.

This cross-runtime completion is safe. CE3's `Deferred` uses an `AtomicReference` internally. When `complete` is called from the outer's real runtime, it fires callbacks that re-enqueue the inner's suspended fiber in the `TestControl` scheduler. The next call to `tickOne` will run it.

## Full Execution Flow

### Phase 1: Startup Pump

```
tickUntilAdvancing(tc, done = pendingDelay.isDefined || tc.results.isDefined)
```

The inner starts from the `TestControl` epoch (time 0). `startupSut` typically fast-forwards to the current real-world time using `IO.sleep`. Under `TestControl`, this sleep needs `tc.advance` to fire. The startup pump uses `tickUntilAdvancing` (which falls back to `tc.nextInterval` when no fiber is immediately eligible) to handle these sleeps and advance the virtual clock to the present.

The pump stops when the inner has posted the first `pendingDelay` signal (meaning `startupSut` is complete and the first command's gate is ready) or when `tc.results` is defined (empty command list — inner ran to completion during startup).

### Phase 2: Command Loop

For each command the outer iterates:

**Step 5 — Read signal:**
```scala
val (delay, gate) = pendingDelay.getAndSet(None).get
totalAdvanced.addAndGet(delay.toNanos)
```

**Step 6 — Advance clock:**
```scala
if delay > Duration.Zero then tc.advance(delay) else IO.unit
```
`tc.advance` requires a strictly positive duration, so zero-delay commands (e.g. `StartBlockCommand`) skip this call.

**Step 7 — Release inner:**
```scala
gate.complete(())
```
The inner's `gate.get` unblocks. The inner is now immediately eligible in the `TestControl` scheduler.

**Step 8 — `tickUntil`:**
```scala
tickUntil(tc, pendingDelay.isDefined || tc.results.isDefined)
```

Calls `tickOne` in a loop until it returns `false` (all immediately-eligible fibers exhausted), then checks whether the inner has posted the next signal. If it has, the iteration is complete. If not, something is wrong — error is raised.

Why `tickOne` first, `done` check second: when `tickOne → false`, ALL eligible fibers have run. Only at that point is the SUT fully settled. Checking `done` before draining would miss side-effect actor messages triggered by the command.

Why `tickUntil` (not `tickUntilAdvancing`) between commands: between commands there must be no clock advances. If a clock advance were needed, it would mean the SUT has an unexpected `IO.sleep` in its command path, which is a bug. `tickUntil` raises an error in that case rather than silently advancing.

### Phase 3: Sentinel and Shutdown

After the command fold, the inner cannot proceed directly to `shutdownSut` — `shutdownSut` calls `waitForIdle`, which polls with `IO.sleep(100 millis)`. If it did, the last command's `tickUntil` would exhaust all fibers (inner blocked on sleep), find no signal, and error.

Instead the inner posts a **sentinel signal**:

```scala
shutdownGate <- Deferred[IO, Unit]
_ <- IO(pendingDelay.set(Some((Duration.Zero, shutdownGate))))
_ <- shutdownGate.get          // inner blocks here
shutdownProp <- shutdownSut(s, sut)
```

The last command's `tickUntil` sees `pendingDelay.isDefined = true` and returns cleanly (inner is now blocked on `shutdownGate.get`, so `tickOne → false` correctly signals "all work done").

The outer then enters Phase 3:

```scala
IO(pendingDelay.getAndSet(None)).flatMap {
    case Some((_, gate)) => gate.complete(())   // release inner into shutdownSut
    case None            => IO.unit             // no sentinel (startup crash / empty commands)
} >> tickUntilAdvancing(tc, tc.results.isDefined)
```

`tickUntilAdvancing` handles the `waitForIdle` sleeps via `tc.nextInterval` + `tc.advance` until `tc.results` becomes defined.

## The Tick-Driving Functions

### `tickUntil` — strict, between commands

```
tickOne → true  → recurse
tickOne → false → done? true  → return
                       false → ERROR: "SUT deadlock or unexpected IO.sleep"
```

Used in the command loop (Phase 2). The invariant: between commands, if all immediately-eligible fibers are exhausted and the inner has not signalled, something is wrong. An unexpected `IO.sleep` in the SUT's command path would cause this and must be caught, not silently worked around.

### `tickUntilAdvancing` — permissive, startup and shutdown

```
tickOne → true  → recurse
tickOne → false → done? true  → return
                       false → nextInterval → advance → recurse
                                (deadlock if nextInterval = 0)
```

Used in the startup pump (Phase 1) and the shutdown drain (Phase 3). In these phases the inner legitimately uses `IO.sleep` — for fast-forwarding to the current time during startup, and for polling idle state during shutdown. The `nextInterval` fallback handles those sleeps. The caller supplies the tracer that the per-advance warn routes through:

- **Startup pump** passes `log` — startup typically advances once or twice (the epoch → current-time fast-forward, possibly a follow-on actor ping), and surfacing those is useful diagnostic.
- **Shutdown drain** passes `ContraTracer.nullTracer` — `waitForIdle` polls with `IO.sleep` and per-actor 1-second ping loops fire on staggered offsets, generating hundreds to thousands of small advances per trial that would otherwise drown the log.

A genuine deadlock (no eligible fibers + `nextInterval == 0`) is still surfaced via the framework `log` and `IO.raiseError` regardless of which tracer the caller passed.

## totalAdvanced: Exact Virtual Time Tracking

`totalAdvanced` accumulates only the explicit command delays. `nextInterval` advances during startup and shutdown are excluded — they represent test infrastructure overhead (fast-forward to present, `waitForIdle` polling, ping-loop draining), not protocol simulated time. The result is printed at the end.

## Expected Log Output

A healthy run produces **one or two `tickUntilAdvancing` WARN lines per test case during startup**, with no per-advance output during the command loop or the shutdown drain:

```
08:53:35.817 WARN  org.scalacheck.commands.ModelBasedSuite
tickUntilAdvancing: no eligible fibers — advancing 2000000000000000000 nanoseconds to next timer
```

This entry is the **startup fast-forward** (`2000000000000000000` ns ≈ 63 years). `startupSut` calls:

```scala
IO.sleep(FiniteDuration(state.getCurrentTime.instant.toEpochMilli, MILLISECONDS))
```

The model's initial time is anchored to a point far in the future relative to the `TestControl` epoch (time 0), so the sleep duration is enormous. `tickUntilAdvancing` finds no eligible fibers at epoch, calls `tc.nextInterval` which returns this huge duration, and advances to it. After this advance the startup IO resumes, actors are created, and the first command's gate is posted — so `tickUntilAdvancing` exits without a second `nextInterval` call.

The shutdown drain also fires `tickUntilAdvancing`, but with `ContraTracer.nullTracer` for the per-advance warn. `shutdownSut` calls `sut.system.waitForIdle(maxTimeout = stackDrainTimeout)`. `ActorSystemDebugSyntax.waitForIdle` (cats-actors 2.0.1) executes a hardcoded 100 ms stabilization sleep after confirming children are idle:

```scala
_ <- children.waitForIdle(checkSchedulerIdle)
_ <- Temporal[F].sleep(100 milliseconds)   // always fires, even when already idle
```

Each iteration produces a `tickUntilAdvancing` cycle: `tickOne → false`, `tc.nextInterval` → some small interval (100 ms `waitForIdle` poll, or the next ping fire), advance, repeat. These are correct and expected; they are silenced because their volume would obscure the rest of the log.

### What a broken run looks like

Because per-advance logging is silenced during shutdown, the failure signals are:

- **`tickUntil: fibers exhausted but signal not received` error** — between commands a clock advance was needed. Either the SUT has an unexpected `IO.sleep` in its command path, or a `Deferred.get` is never completed. The strict `tickUntil` raises this rather than silently advancing.
- **`TestControl deadlock: no eligible fibers and predicate not satisfied` error** — `tickUntilAdvancing` saw `tc.nextInterval == 0` (no future timers) while `done` was still false. Indicates the inner is wedged with no scheduled work.
- **Test never terminates** — `tc.results` never becomes defined. Suggests a fiber leak (a never-cancelled background loop keeps `tickUntilAdvancing` recursing on its 1-second ping). Check that all background fibers are cancelled in the SUT's `Resource` finalizer.

To investigate an unexpected sleep in shutdown, temporarily pass `log` (instead of `ContraTracer.nullTracer`) to the shutdown-drain `tickUntilAdvancing` call in `runCommandsWithTestControl`.
