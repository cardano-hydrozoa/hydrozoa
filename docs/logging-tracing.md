# Logging and Tracing

## Terminology

This document uses the term **contextual logging** to mean attaching key-value metadata to log
entries so that context (requestId, peerId, runId, …) travels with every log call in a given scope.

We avoid the term *structured logging*, which is ambiguous — it is commonly used to mean log output
formatted as machine-readable records (JSON, logfmt), which is a separate and orthogonal concern.

## Problem

SLF4J MDC relies on thread-locals. Cats Effect fibers hop between OS threads, so MDC values vanish mid-fiber. The result: no reliable way to attach per-request context (requestId, peerId, runId, …) to log entries.

## Solution: Contravariant Tracer + IOLocal

Three pieces compose:

| Layer | Role |
|---|---|
| `Slf4jTracer = LogEvent => IO[Unit]` | Contravariant emitter — `contramap` accumulates context without replacement |
| `IOLocal[Slf4jTracer]` | Fiber-safe ambient carrier; child fibers inherit a copy of the parent's value at fork time |
| `given IOLocal[Slf4jTracer]` | Static accessor threaded implicitly from startup — zero call-site boilerplate |

## Core Types

```scala
// src/main/scala/hydrozoa/lib/logging/Slf4jTracer.scala

enum Level { case Trace, Debug, Info, Warn, Error }

case class LogEvent(
    level:      Level,
    msg:        String,
    ctx:        Map[String, String]  = Map.empty,  // ambient key-value context
    cause:      Option[Throwable]    = None,
    routingKey: Option[String]       = None         // SLF4J logger name; see below
)

type Slf4jTracer = LogEvent => IO[Unit]
```

### Why `routingKey` is `Option[String]`

`routingKey` routes to the correct SLF4J/Logback logger, preserving the logger hierarchy and
per-component level config (e.g. `<logger name="hydrozoa.multisig" level="DEBUG"/>`).

`None` means **"fill from the ambient tracer"** — the tracer installed by `routeLocal` supplies the
name. This is the correct default for every `Slf4jTracer.info/debug/…` call: the actor's own routing key
is applied automatically without any call-site annotation.

`Some("X")` is an **explicit override** used only in pure functions returning `Traced[A]` that need
sub-component routing independent of whichever actor happens to call `.logWith`. The conditional
logic in `routeLocal` leaves `Some` values untouched, so explicit routing survives the actor's
permanent enrichment.

`LogEvent.ctx` carries ambient key-value pairs prepended to the message as `[k=v k=v] msg`.

## API

```scala
object Slf4jTracer:
    // Create a fresh IOLocal seeded with the base tracer.
    // All routing is set per-fiber via routeLocal in each actor's preStartLocal.
    def makeLocal: IO[IOLocal[Slf4jTracer]]

    // Permanently set the routing key for this fiber.
    // Only fills events with routingKey == None; explicit Some values pass through unchanged.
    // Call from preStartLocal — NOT preStart (see cats-actors fiber note below).
    def routeLocal(name: String)(using IOLocal[Slf4jTracer]): IO[Unit]

    // Permanently contramaps the tracer — no restore.
    // Prefer routeLocal for routing. Use this only for other permanent per-fiber enrichment.
    def updateLocal(f: LogEvent => LogEvent)(using IOLocal[Slf4jTracer]): IO[Unit]

    // Permanently add key-value pairs to ctx — no restore.
    // Use in preStartLocal to bake session-wide context (e.g. peer number) into the fiber so
    // all child actors spawned from it inherit the context automatically.
    def updateLocalCtx(kvs: (String, String)*)(using IOLocal[Slf4jTracer]): IO[Unit]

    // Enrich tracer for the duration of fa, then restore.
    def scoped[A](f: LogEvent => LogEvent)(fa: IO[A])(using IOLocal[Slf4jTracer]): IO[A]

    // Convenience: ctx key-value pairs only, scoped to fa.
    def scopedCtx[A](kvs: (String, String)*)(fa: IO[A])(using IOLocal[Slf4jTracer]): IO[A]

    def trace(msg: String)(using IOLocal[Slf4jTracer]): IO[Unit]
    def debug(msg: String)(using IOLocal[Slf4jTracer]): IO[Unit]
    def info (msg: String)(using IOLocal[Slf4jTracer]): IO[Unit]
    def warn (msg: String)(using IOLocal[Slf4jTracer]): IO[Unit]
    def error(msg: String, cause: Option[Throwable] = None)(using IOLocal[Slf4jTracer]): IO[Unit]
```

## Usage Patterns

### Startup (Main / startupSut)

One `IOLocal[Slf4jTracer]` is created once and passed to all actors. Global session context (e.g. peer
IP, run ID) is baked in at startup via `scopedCtx` around actor spawning — all forked fibers
inherit it permanently.

```scala
// IOApp entry point / test startupSut
for {
    tracerLocal <- Slf4jTracer.makeLocal
    given IOLocal[Slf4jTracer] = tracerLocal
    _ <- Slf4jTracer.info(s"Creating new SUT [$label/$runId]")
    // All actors spawned here inherit tracerLocal.
    // Wrap per-peer spawning in scopedCtx to embed peer context permanently:
    _ <- Slf4jTracer.scopedCtx("peer" -> peerIp)(actorSystem.actorOf(MyActor(tracerLocal, ...)))
} yield Stage4Sut(..., tracerLocal = tracerLocal)
```

### cats-actors: preStart runs in the parent fiber

**This is the key constraint for routing setup.**

In cats-actors, the `preStart` lifecycle method runs in the **parent actor's fiber** during
`actorOf`. The actor's own fiber is only spawned afterwards (via `supervisor.supervise`). This
means calling `routeLocal` (or any `IOLocal` mutation) inside `preStart` corrupts the *parent's*
routing — not the actor's.

The correct pattern is to send a self-message from `preStart` and do all fiber-local setup in the
message handler, which executes in the actor's own fiber:

```scala
// preStart runs in the PARENT fiber — only send the init message here.
override def preStart: IO[Unit] =
    context.self ! MyActor.Init

// This runs in the actor's OWN fiber — safe to call routeLocal here.
private def preStartLocal: IO[Unit] =
    for {
        _ <- Slf4jTracer.routeLocal("hydrozoa.MyActor")
        _ <- initializeConnections
        ...
    } yield ()
```

### Full actor example

```scala
// Pure function — returns Traced[A] so it can log without touching IO.
// Explicit routingKey so sub-component routing survives .logWith in any actor.
def computeHeader(prev: BlockHeader, timing: TxTiming): Traced[BlockHeader.Minor] = {
    val header = BlockHeader.Minor(...)
    (header, List(LogEvent(Level.Debug, s"computed header $header", routingKey = Some("MyActor"))))
}

final case class MyActor(
    tracerLocal: IOLocal[Slf4jTracer],
    // ... other fields ...
) extends Actor[IO, MyActor.Request] {

    // Makes all Slf4jTracer.* calls resolve the given implicitly throughout the class.
    given IOLocal[Slf4jTracer] = tracerLocal

    // preStart runs in the PARENT fiber — only send the init message.
    override def preStart: IO[Unit] =
        context.self ! MyActor.Init

    override def receive: Receive[IO, MyActor.Request] = PartialFunction.fromFunction {
        case MyActor.Init          => preStartLocal
        case req: ProcessBlock     => processBlock(req)
    }

    // Runs in this actor's OWN fiber — safe to call routeLocal.
    private def preStartLocal: IO[Unit] =
        for {
            _ <- Slf4jTracer.routeLocal("hydrozoa.MyActor")
            _ <- initializeConnections
        } yield ()

    private def processBlock(req: ProcessBlock): IO[Unit] =
        // All log calls inside this scope get blockNum in their context.
        Slf4jTracer.scopedCtx("blockNum" -> s"${req.blockNum: Int}") {
            for {
                _ <- Slf4jTracer.info("processing block")
                // .logWith emits the Traced events into the ambient tracer, returns the value.
                // Events pick up blockNum from the surrounding scopedCtx automatically.
                header <- computeHeader(req.prevHeader, req.timing).logWith
                _ <- Slf4jTracer.debug(s"header: $header")
            } yield ()
        }
}
```

### SutCommands (integration tests)

```scala
given SutCommand[MyCmd, Unit, Stage1Sut] with {
    override def run(cmd: MyCmd, sut: Stage1Sut): IO[Unit] =
        given IOLocal[Slf4jTracer] = sut.tracerLocal
        Slf4jTracer.info(">> MyCmd") >> ...
}
```

## Current Integration Points

| Location | Status |
|---|---|
| `stage1/Suite.startupSut` | Fully wired; `Slf4jTracer.makeLocal` + `given` + all log calls |
| `stage1/Sut.SutCommands` | All commands use `given IOLocal[Slf4jTracer] = sut.tracerLocal` |
| `stage4/Suite.startupSut` | Wired; one `tracerLocal` threaded to all actors; `scopedCtx("peer")` per peer |
| `stage4/Sut.Stage4SutCommands` | Commands use `given IOLocal[Slf4jTracer] = sut.tracerLocal` |
| `JointLedger` | `given IOLocal[Slf4jTracer]` in class body; `preStartLocal` calls `routeLocal`; all log calls via `Tracer.*`; `blockNum` via `scopedCtx` wrapping `startBlock`/`completeBlock*` bodies |
| `CardanoLiaison` | `given IOLocal[Slf4jTracer]` in class body; `preStartLocal` calls `routeLocal` |
| `BlockWeaver` | `given IOLocal[Slf4jTracer]` in class body; `receive` PreStart case calls `routeLocal`; state machine still uses old `logger` (migration pending) |
| `MultisigRegimeManager` | Accepts `tracerLocal: IOLocal[Slf4jTracer]`; `preStartLocal` calls `routeLocal` then `updateLocalCtx("peer")` — all child actors inherit the peer number automatically |
| `TxTiming.blockCanStayMinor` | Returns `Traced[Boolean]`; `routingKey = Some("TxTiming")` |
| `BlockHeader.nextHeader*` | Returns `Traced[...]`; `routingKey = Some("BlockHeader")` |

## IOLocal Policy

### For transient context: closures only

**Never call `tracerLocal.set` directly for per-message or per-expression context.** Use
`Slf4jTracer.scoped` or `Slf4jTracer.scopedCtx` instead — they wrap an `IO[A]` via closure and restore
on exit.

```scala
// Correct — scope-bounded, self-restoring
Slf4jTracer.scopedCtx("blockNum" -> s"$blockNum")(someIO)

// Wrong — leaks if restore is missed
tracerLocal.set(ev => t(ev.copy(ctx = ev.ctx + ("blockNum" -> s"$blockNum"))))
```

**Why:** If the restore is missed (error path, early return, future refactor), the enriched tracer
leaks into all subsequent messages on that actor — corrupting every log entry that follows.

### For actor lifetime: routeLocal in preStartLocal

`Slf4jTracer.routeLocal` is the sanctioned permanent mutation for routing. Call it as the first step
in the actor's `preStartLocal` (the Init message handler), not in `preStart` — `preStart` runs
in the parent fiber and would corrupt the parent's routing for all actors spawned after it.

`Slf4jTracer.updateLocal` is available for other permanent per-fiber enrichment (non-routing).

### For session-wide context: bake into IOLocal at spawn time

Global context (peer IP, run ID) that should appear on all actors' log entries is added via
`scopedCtx` around the actor spawn calls. Because child fibers inherit the parent's IOLocal value
at fork time and retain it permanently, the context is baked in even after the `scopedCtx` scope
exits.

```scala
// Peer IP is permanently inherited by all actors forked inside this scope.
Slf4jTracer.scopedCtx("peer" -> peerIp) {
    actorSystem.actorOf(JointLedger(tracerLocal, ...))
}
```

## Typed per-actor `ContraTracer`s

Above the ambient `IOLocal[Slf4jTracer]` sits a per-actor typed event channel. Each actor declares its
own event ADT plus a `…EventFormat` object that turns events into `LogEvent`s; the actor takes a
`ContraTracer[IO, MyEvent]` in its constructor and emits via `tracer.traceWith(event)`. Examples:
`FastConsensusActorEvent`, `CardanoLiaisonEvent`, `StackComposerEvent`, `SlowConsensusActorEvent`,
`MultisigRegimeManagerEvent`, `JointLedgerEvent`.

### Anatomy of a `…EventFormat`

```scala
// src/main/scala/hydrozoa/multisig/consensus/CardanoLiaisonEventFormat.scala
object CardanoLiaisonEventFormat:
    private def routingKey(peerNum: HeadPeerNumber): String = s"CardanoLiaison.$peerNum"
    private def baseCtx(peerNum: HeadPeerNumber): Map[String, String] =
        Map("peer" -> peerNum.toString)

    // Human sink: total — every event renders to one line.
    def humanFormat(peerNum: HeadPeerNumber)(e: CardanoLiaisonEvent): LogEvent = …

    // JSONL sink: partial — only events with a wire shape return Some.
    def jsonlFormat(peerNum: HeadPeerNumber)(e: CardanoLiaisonEvent): Option[LogEvent] = …
```

Two conventions hold across the codebase:
- `humanFormat` is `Event => LogEvent` (always emits) and uses a per-actor `routingKey` like
  `"CardanoLiaison.<peerNum>"` so Logback can route per actor/peer.
- `jsonlFormat` is `Event => Option[LogEvent]` (skips events not on the trace wire) and pins
  `routingKey = "hydrozoa.trace"` so a single Logback appender captures the `HTRACE|…` lines.

### Building a `ContraTracer` from a format

`Slf4jTracer.sink: ContraTracer[IO, LogEvent]` is the SLF4J back-end. Lift it through your format with
`contramap` (total) or `traceMaybe` (partial), and combine sinks with `|+|` (`Semigroup`):

```scala
import cats.implicits.*
import hydrozoa.lib.logging.{ContraTracer, Tracer}

val clTracer: ContraTracer[IO, CardanoLiaisonEvent] =
    Slf4jTracer.sink.contramap(CardanoLiaisonEventFormat.humanFormat(peerNum))
        |+| Slf4jTracer.sink.traceMaybe(CardanoLiaisonEventFormat.jsonlFormat(peerNum))
```

`Slf4jTracer.sink` resolves the SLF4J logger from `LogEvent.routingKey` per event, so the two sinks
write to two different Logback loggers (`CardanoLiaison.<peerNum>` and `hydrozoa.trace`) with no
extra wiring. `Slf4jTracer.sink` requires `given IOLocal[Slf4jTracer]` in scope — for tests, create one with
`Slf4jTracer.makeLocal`.

### Custom in-process sinks

A `ContraTracer` is just `A => IO[Unit]`, so any side-effect can be a sink. Stage 4 uses this to
capture block briefs into a `Ref` for end-of-test assertions, then combines it with the SLF4J
text sink:

```scala
val captureSink: ContraTracer[IO, JointLedgerEvent] =
    ContraTracer.emit[IO, JointLedgerEvent] {
        case JointLedgerEvent.BriefProduced(b) => blockBriefsRef.update(_ :+ b)
        case _                                 => IO.unit
    }
val textSink: ContraTracer[IO, JointLedgerEvent] =
    Slf4jTracer.sink.contramap(JointLedgerEventFormat.humanFormat(peerNum))
val jlTracer: ContraTracer[IO, JointLedgerEvent] = captureSink |+| textSink
```

### Tests: getting a human-readable per-actor tracer

In a unit / integration test, replace `ContraTracer.nullTracer[IO, MyEvent]` with the human sink.
Minimal recipe:

```scala
for {
    tracerLocal <- Slf4jTracer.makeLocal
    given IOLocal[Slf4jTracer] = tracerLocal
    jlTracer = Slf4jTracer.sink.contramap(
                   JointLedgerEventFormat.humanFormat(HeadPeerNumber.zero)
               )
    jl <- system.actorOf(JointLedger(config, conns, ledger, jlTracer, persistence))
} yield ()
```

`HeadPeerNumber.zero` (or whichever peer the test impersonates) only affects the rendered label.
Add `|+| Slf4jTracer.sink.traceMaybe(JointLedgerEventFormat.jsonlFormat(peerNum))` if you also want the
JSONL trace.

### Logback configuration

Each per-actor `routingKey` is a distinct SLF4J logger name. To see (or silence) a per-actor
stream, add a matching `<logger name="…">` to every `logback.xml` in the subproject — see the
"Logging" note in `CLAUDE.md` for the file list. The `hydrozoa.trace` logger captures every
`HTRACE|…` line across all actors; route it to its own file appender when you need a structured
event log.

### Adding a new typed tracer to an actor

1. Define `MyActorEvent` (sealed ADT of every event the actor wants to publish).
2. Define `MyActorEventFormat.humanFormat` (total) and, if needed, `jsonlFormat` (partial).
3. Add `tracer: ContraTracer[IO, MyActorEvent]` to the actor's constructor; emit with
   `tracer.traceWith(event)`.
4. Build the `ContraTracer` at the call site (see "Building a `ContraTracer`" above).
5. Add the new `routingKey` to every `logback.xml` in the subproject.

## Backend

The base tracer wraps log4cats `Slf4jLogger`. Existing Logback config
(`integration/src/test/resources/logback.xml`, `logback.xml`) continues to work unchanged. Context
is formatted as a message prefix (`[k=v …] msg`) rather than SLF4J MDC, which is intentional —
MDC is thread-local and not fiber-safe.
