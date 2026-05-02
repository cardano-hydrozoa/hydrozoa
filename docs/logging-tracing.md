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

    // Enrich tracer for the duration of fa, then restore.
    // Use for both logger routing and ctx enrichment.
    def scoped[A](f: LogEvent => LogEvent)(fa: IO[A])(using IOLocal[Tracer]): IO[A]

    // Convenience: ctx key-value pairs only.
    def scopedCtx[A](kvs: (String, String)*)(fa: IO[A])(using IOLocal[Tracer]): IO[A]

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

### Per-message context (actors)

```scala
def receive(msg: MyMsg)(using IOLocal[Tracer]): IO[Unit] =
    Tracer.scopedCtx("requestId" -> msg.id.toString, "peer" -> peerId) {
        Tracer.info("handling message") >> process(msg)
    }
```

### Component routing (set SLF4J logger name)

```scala
// At actor construction or handler entry
Tracer.scoped(_.copy(logger = "hydrozoa.multisig.JointLedger")) {
    Tracer.scopedCtx("blockNum" -> blockNum.toString) {
        Tracer.debug("processing request")
    }
}
```

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
| `Main.run` | `IOLocal[Tracer]` created, `given` in scope; actors not yet migrated |
| `stage1/Suite.startupSut` | Fully wired; `Tracer.makeLocal` + `given` + all log calls |
| `stage1/Sut.SutCommands` | All commands use `given IOLocal[Tracer] = sut.tracerLocal` |
| `stage4/startupSut` | Follow same pattern when written |
| `JointLedger`, other actors | Pending migration |

## Relation to ProtocolTracer

`ProtocolTracer` (`hydrozoa.lib.tracing`) is a domain-specific structured tracer for conformance checking (JSONL/`HTRACE|` lines). It is a future merge candidate: its `jsonLines` implementation could be reimplemented as a `Tracer` backend, with event fields carried in `LogEvent.ctx` or a structured variant of `LogEvent`.

## Backend

The base tracer wraps log4cats `Slf4jLogger`. Existing Logback config (`integration/src/test/resources/logback.xml`, `logback.xml`) continues to work unchanged. Context is formatted as a message prefix (`[k=v …] msg`) rather than SLF4J MDC, which is intentional — MDC is thread-local and not fiber-safe.
