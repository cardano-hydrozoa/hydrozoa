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
| `IOLocal[Tracer]` | Fiber-safe ambient carrier; child fibers inherit parent's value automatically |
| `given IOLocal[Tracer]` | Static accessor threaded implicitly from startup — zero call-site boilerplate |

## Core Types

```scala
// src/main/scala/hydrozoa/lib/logging/Tracer.scala

enum Level { case Debug, Info, Warn, Error }

case class LogEvent(
    level:  Level,
    msg:    String,
    ctx:    Map[String, String] = Map.empty,  // ambient key-value context
    cause:  Option[Throwable]  = None,
    logger: String             = "gummiworm"  // SLF4J routing key
)

type Tracer = LogEvent => IO[Unit]
```

`LogEvent.logger` routes to the correct SLF4J/Logback logger, preserving the logger hierarchy and per-component level config (e.g. `<logger name="hydrozoa.multisig" level="DEBUG"/>`). It is a routing field, not part of context.

`LogEvent.ctx` carries ambient key-value pairs prepended to the message as `[k=v k=v] msg`.

## API

```scala
object Tracer:
    // Create an IOLocal pre-seeded for a component.
    // Routes to `name` by default; overridable via scoped.
    def makeLocal(name: String): IO[IOLocal[Tracer]]

    // Permanently contramaps the tracer — no restore. For actor preStart only.
    def updateLocal(f: LogEvent => LogEvent)(using IOLocal[Tracer]): IO[Unit]

    // Enrich tracer for the duration of fa, then restore.
    // Use for both logger routing and ctx enrichment.
    def scoped[A](f: LogEvent => LogEvent)(fa: IO[A])(using IOLocal[Tracer]): IO[A]

    // Convenience: ctx key-value pairs only.
    def scopedCtx[A](kvs: (String, String)*)(fa: IO[A])(using IOLocal[Tracer]): IO[A]

    def trace(msg: String)(using IOLocal[Tracer]): IO[Unit]
    def debug(msg: String)(using IOLocal[Tracer]): IO[Unit]
    def info (msg: String)(using IOLocal[Tracer]): IO[Unit]
    def warn (msg: String)(using IOLocal[Tracer]): IO[Unit]
    def error(msg: String, cause: Option[Throwable] = None)(using IOLocal[Tracer]): IO[Unit]
```

## Usage Patterns

### Startup (Main / startupSut)

```scala
// IOApp entry point
Tracer.makeLocal("gummiworm.node").flatMap { tracerLocal =>
    given IOLocal[Tracer] = tracerLocal
    // ... rest of program
}

// Integration test startupSut
for {
    tracerLocal <- Tracer.makeLocal(s"stage1/$runId")
    given IOLocal[Tracer] = tracerLocal
    _ <- Tracer.info(s"Creating new SUT [$label/$runId]")
    // ... build actors
} yield Stage1Sut(..., tracerLocal = tracerLocal)
```

### Full actor example

```scala
// Pure function — returns Traced[A] so it can log without touching IO.
def computeHeader(prev: BlockHeader, timing: TxTiming): Traced[BlockHeader.Minor] = {
    val header = BlockHeader.Minor(...)
    (header, List(LogEvent(Level.Debug, s"computed header $header", logger = "MyActor")))
}

final case class MyActor(
    // ... other fields ...
    tracerLocal: IOLocal[Tracer]
) extends Actor[IO, MyActor.Request] {

    // Makes Tracer.* calls resolve the given implicitly throughout the class.
    given IOLocal[Tracer] = tracerLocal

    // Fix the SLF4J routing key for this actor's entire lifetime.
    override def preStart: IO[Unit] =
        Tracer.updateLocal(_.copy(logger = "hydrozoa.MyActor")) >>
            (context.self ! MyActor.Init)

    override def receive: Receive[IO, MyActor.Request] = PartialFunction.fromFunction {
        case req: MyActor.ProcessBlock => processBlock(req)
    }

    private def processBlock(req: ProcessBlock): IO[Unit] =
        // All log calls inside this scope get blockNum in their context.
        Tracer.scopedCtx("blockNum" -> s"${req.blockNum: Int}") {
            for {
                _ <- Tracer.info("processing block")
                // .logWith emits the Traced events into the ambient tracer, returns the value.
                // The events pick up blockNum from the surrounding scopedCtx automatically.
                header <- computeHeader(req.prevHeader, req.timing).logWith
                _ <- Tracer.debug(s"header: $header")
            } yield ()
        }
}
```

`updateLocal` is a permanent contramap — no restore. Use it only in `preStart` where the change must hold for the actor's lifetime. For anything tied to a single message or expression, use `scopedCtx`.

Scopes nest correctly — inner `scoped` contramaps the current tracer, so outer context is preserved and restored on exit.

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
| `stage4/Suite.startupSut` | Wired; `tracerLocal` threaded to BlockWeaver and JointLedger |
| `stage4/Sut.Stage4SutCommands` | Commands use `given IOLocal[Tracer] = sut.tracerLocal` |
| `JointLedger` | `given IOLocal[Tracer]` in class body; `preStart` uses `updateLocal`; all log calls via `Tracer.*`; `blockNum` via `scopedCtx` wrapping `startBlock`/`completeBlock*` bodies |
| `BlockWeaver` | `tracerLocal` field; state machine still uses old `logger` (migration pending) |
| `MultisigRegimeManager` | Creates `tracerLocal <- Tracer.makeLocal(nodeId)`; threads to actors |
| `TxTiming.blockCanStayMinor` | Returns `Traced[Boolean]`; routing key `"TxTiming"` |
| `BlockHeader.nextHeader*` | Returns `Traced[...]`; routing key `"BlockHeader"` |

## IOLocal Policy

### For transient context: closures only

**Never call `tracerLocal.set` directly for per-message or per-expression context.** Use `Tracer.scoped` or `Tracer.scopedCtx` instead — they wrap an `IO[A]` via closure and restore on exit.

```scala
// Correct — scope-bounded, self-restoring
Tracer.scopedCtx("blockNum" -> s"$blockNum")(someIO)

// Wrong — leaks if restore is missed
tracerLocal.set(ev => t(ev.copy(ctx = ev.ctx + ("blockNum" -> s"$blockNum"))))
```

**Why:** If the restore is missed (error path, early return, future refactor), the enriched tracer leaks into all subsequent messages on that actor — corrupting every log entry that follows.

### For actor lifetime: updateLocal in preStart

`Tracer.updateLocal` is the one sanctioned permanent mutation. Use it in `preStart` to fix `LogEvent.logger` for the actor's lifetime. It is safe there because `preStart` runs exactly once before any messages, so there is nothing to corrupt and nothing to restore.

## Relation to ProtocolTracer

`ProtocolTracer` (`hydrozoa.lib.tracing`) is a domain-specific structured tracer for conformance checking (JSONL/`HTRACE|` lines). It is a future merge candidate: its `jsonLines` implementation could be reimplemented as a `Tracer` backend, with event fields carried in `LogEvent.ctx` or a structured variant of `LogEvent`.

## Backend

The base tracer wraps log4cats `Slf4jLogger`. Existing Logback config (`integration/src/test/resources/logback.xml`, `logback.xml`) continues to work unchanged. Context is formatted as a message prefix (`[k=v …] msg`) rather than SLF4J MDC, which is intentional — MDC is thread-local and not fiber-safe.
