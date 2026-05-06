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
| `Tracer = LogEvent => IO[Unit]` | Contravariant emitter — `contramap` accumulates context without replacement |
| `IOLocal[Tracer]` | Fiber-safe ambient carrier; child fibers inherit a copy of the parent's value at fork time |
| `given IOLocal[Tracer]` | Static accessor threaded implicitly from startup — zero call-site boilerplate |

## Core Types

```scala
// src/main/scala/hydrozoa/lib/logging/Tracer.scala

enum Level { case Trace, Debug, Info, Warn, Error }

case class LogEvent(
    level:      Level,
    msg:        String,
    ctx:        Map[String, String]  = Map.empty,  // ambient key-value context
    cause:      Option[Throwable]    = None,
    routingKey: Option[String]       = None         // SLF4J logger name; see below
)

type Tracer = LogEvent => IO[Unit]
```

### Why `routingKey` is `Option[String]`

`routingKey` routes to the correct SLF4J/Logback logger, preserving the logger hierarchy and
per-component level config (e.g. `<logger name="hydrozoa.multisig" level="DEBUG"/>`).

`None` means **"fill from the ambient tracer"** — the tracer installed by `routeLocal` supplies the
name. This is the correct default for every `Tracer.info/debug/…` call: the actor's own routing key
is applied automatically without any call-site annotation.

`Some("X")` is an **explicit override** used only in pure functions returning `Traced[A]` that need
sub-component routing independent of whichever actor happens to call `.logWith`. The conditional
logic in `routeLocal` leaves `Some` values untouched, so explicit routing survives the actor's
permanent enrichment.

`LogEvent.ctx` carries ambient key-value pairs prepended to the message as `[k=v k=v] msg`.

## API

```scala
object Tracer:
    // Create a fresh IOLocal seeded with the base tracer.
    // All routing is set per-fiber via routeLocal in each actor's preStartLocal.
    def makeLocal: IO[IOLocal[Tracer]]

    // Permanently set the routing key for this fiber.
    // Only fills events with routingKey == None; explicit Some values pass through unchanged.
    // Call from preStartLocal — NOT preStart (see cats-actors fiber note below).
    def routeLocal(name: String)(using IOLocal[Tracer]): IO[Unit]

    // Permanently contramaps the tracer — no restore.
    // Prefer routeLocal for routing. Use this only for other permanent per-fiber enrichment.
    def updateLocal(f: LogEvent => LogEvent)(using IOLocal[Tracer]): IO[Unit]

    // Permanently add key-value pairs to ctx — no restore.
    // Use in preStartLocal to bake session-wide context (e.g. peer number) into the fiber so
    // all child actors spawned from it inherit the context automatically.
    def updateLocalCtx(kvs: (String, String)*)(using IOLocal[Tracer]): IO[Unit]

    // Enrich tracer for the duration of fa, then restore.
    def scoped[A](f: LogEvent => LogEvent)(fa: IO[A])(using IOLocal[Tracer]): IO[A]

    // Convenience: ctx key-value pairs only, scoped to fa.
    def scopedCtx[A](kvs: (String, String)*)(fa: IO[A])(using IOLocal[Tracer]): IO[A]

    def trace(msg: String)(using IOLocal[Tracer]): IO[Unit]
    def debug(msg: String)(using IOLocal[Tracer]): IO[Unit]
    def info (msg: String)(using IOLocal[Tracer]): IO[Unit]
    def warn (msg: String)(using IOLocal[Tracer]): IO[Unit]
    def error(msg: String, cause: Option[Throwable] = None)(using IOLocal[Tracer]): IO[Unit]
```

## Usage Patterns

### Startup (Main / startupSut)

One `IOLocal[Tracer]` is created once and passed to all actors. Global session context (e.g. peer
IP, run ID) is baked in at startup via `scopedCtx` around actor spawning — all forked fibers
inherit it permanently.

```scala
// IOApp entry point / test startupSut
for {
    tracerLocal <- Tracer.makeLocal
    given IOLocal[Tracer] = tracerLocal
    _ <- Tracer.info(s"Creating new SUT [$label/$runId]")
    // All actors spawned here inherit tracerLocal.
    // Wrap per-peer spawning in scopedCtx to embed peer context permanently:
    _ <- Tracer.scopedCtx("peer" -> peerIp)(actorSystem.actorOf(MyActor(tracerLocal, ...)))
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
        _ <- Tracer.routeLocal("hydrozoa.MyActor")
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
    tracerLocal: IOLocal[Tracer],
    // ... other fields ...
) extends Actor[IO, MyActor.Request] {

    // Makes all Tracer.* calls resolve the given implicitly throughout the class.
    given IOLocal[Tracer] = tracerLocal

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
            _ <- Tracer.routeLocal("hydrozoa.MyActor")
            _ <- initializeConnections
        } yield ()

    private def processBlock(req: ProcessBlock): IO[Unit] =
        // All log calls inside this scope get blockNum in their context.
        Tracer.scopedCtx("blockNum" -> s"${req.blockNum: Int}") {
            for {
                _ <- Tracer.info("processing block")
                // .logWith emits the Traced events into the ambient tracer, returns the value.
                // Events pick up blockNum from the surrounding scopedCtx automatically.
                header <- computeHeader(req.prevHeader, req.timing).logWith
                _ <- Tracer.debug(s"header: $header")
            } yield ()
        }
}
```

### SutCommands (integration tests)

```scala
given SutCommand[MyCmd, Unit, Stage1Sut] with {
    override def run(cmd: MyCmd, sut: Stage1Sut): IO[Unit] =
        given IOLocal[Tracer] = sut.tracerLocal
        Tracer.info(">> MyCmd") >> ...
}
```

## Current Integration Points

| Location | Status |
|---|---|
| `stage1/Suite.startupSut` | Fully wired; `Tracer.makeLocal` + `given` + all log calls |
| `stage1/Sut.SutCommands` | All commands use `given IOLocal[Tracer] = sut.tracerLocal` |
| `stage4/Suite.startupSut` | Wired; one `tracerLocal` threaded to all actors; `scopedCtx("peer")` per peer |
| `stage4/Sut.Stage4SutCommands` | Commands use `given IOLocal[Tracer] = sut.tracerLocal` |
| `JointLedger` | `given IOLocal[Tracer]` in class body; `preStartLocal` calls `routeLocal`; all log calls via `Tracer.*`; `blockNum` via `scopedCtx` wrapping `startBlock`/`completeBlock*` bodies |
| `CardanoLiaison` | `given IOLocal[Tracer]` in class body; `preStartLocal` calls `routeLocal` |
| `BlockWeaver` | `given IOLocal[Tracer]` in class body; `receive` PreStart case calls `routeLocal`; state machine still uses old `logger` (migration pending) |
| `MultisigRegimeManager` | Accepts `tracerLocal: IOLocal[Tracer]`; `preStartLocal` calls `routeLocal` then `updateLocalCtx("peer")` — all child actors inherit the peer number automatically |
| `TxTiming.blockCanStayMinor` | Returns `Traced[Boolean]`; `routingKey = Some("TxTiming")` |
| `BlockHeader.nextHeader*` | Returns `Traced[...]`; `routingKey = Some("BlockHeader")` |

## IOLocal Policy

### For transient context: closures only

**Never call `tracerLocal.set` directly for per-message or per-expression context.** Use
`Tracer.scoped` or `Tracer.scopedCtx` instead — they wrap an `IO[A]` via closure and restore
on exit.

```scala
// Correct — scope-bounded, self-restoring
Tracer.scopedCtx("blockNum" -> s"$blockNum")(someIO)

// Wrong — leaks if restore is missed
tracerLocal.set(ev => t(ev.copy(ctx = ev.ctx + ("blockNum" -> s"$blockNum"))))
```

**Why:** If the restore is missed (error path, early return, future refactor), the enriched tracer
leaks into all subsequent messages on that actor — corrupting every log entry that follows.

### For actor lifetime: routeLocal in preStartLocal

`Tracer.routeLocal` is the sanctioned permanent mutation for routing. Call it as the first step
in the actor's `preStartLocal` (the Init message handler), not in `preStart` — `preStart` runs
in the parent fiber and would corrupt the parent's routing for all actors spawned after it.

`Tracer.updateLocal` is available for other permanent per-fiber enrichment (non-routing).

### For session-wide context: bake into IOLocal at spawn time

Global context (peer IP, run ID) that should appear on all actors' log entries is added via
`scopedCtx` around the actor spawn calls. Because child fibers inherit the parent's IOLocal value
at fork time and retain it permanently, the context is baked in even after the `scopedCtx` scope
exits.

```scala
// Peer IP is permanently inherited by all actors forked inside this scope.
Tracer.scopedCtx("peer" -> peerIp) {
    actorSystem.actorOf(JointLedger(tracerLocal, ...))
}
```

## Relation to ProtocolTracer

`ProtocolTracer` (`hydrozoa.lib.tracing`) is a domain-specific structured tracer for conformance
checking (JSONL/`HTRACE|` lines). It is a future merge candidate: its `jsonLines` implementation
could be reimplemented as a `Tracer` backend, with event fields carried in `LogEvent.ctx` or a
structured variant of `LogEvent`.

## Backend

The base tracer wraps log4cats `Slf4jLogger`. Existing Logback config
(`integration/src/test/resources/logback.xml`, `logback.xml`) continues to work unchanged. Context
is formatted as a message prefix (`[k=v …] msg`) rather than SLF4J MDC, which is intentional —
MDC is thread-local and not fiber-safe.
