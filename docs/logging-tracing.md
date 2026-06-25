# Logging and Tracing

This repository handles logging via typed `ContraTracer`s, ported from the haskell at [avieth/contractracer](https://github.com/avieth/contra-tracer).
With contractracers, components that do "logging" are not aware of the logging backend _at all_. 
They only emit typed events (via `tracer.traceWith(event)` or similar), and the eventual caller applies functions to
format and render these events within some effect monad `M`.

```scala
sealed trait MyEvent
object MyEvent:
    final case class Started(at: Instant) extends MyEvent
    final case class Stalled(reason: String) extends MyEvent

object MyEventFormat:
    def humanFormat(peerNum: HeadPeerNumber)(e: MyEvent): LogEvent =
        val ev = LogEvent.From.forPeer("MyActor", peerNum)
        import ev.*
        e match
            case MyEvent.Started(at)  => info(s"started at $at")
            case MyEvent.Stalled(why) => warn(s"stalled: $why")

final class MyActor(tracer: ContraTracer[IO, MyEvent]) extends Actor[IO, Request]:
    // tracer.traceWith(MyEvent.Started(now))
```

This gives:
- **Typed contexts.** Each variant carries the data it needs; the format produces the string.
- **Composition.** A `ContraTracer` is `A => F[Unit]` for some `F[_]: Monad`. Combine via `|+|`
  (`Semigroup`): one sink can write the human-readable line, another captures into a `Ref` for
  inspection during test, and a third can log to a telemetry server.

## Default to polymorphic `F[_]: Monad`

When a function takes a `ContraTracer` as a parameter, default to keeping `F` polymorphic:

```scala
def step[F[_]: Monad](tracer: ContraTracer[F, MyEvent])(in: Input): F[Result] = ...
```

Specialize the type parameter to a concrete `F` (almost always `IO`) only when the surrounding
code is intrinsically tied to that monad — e.g. inside `Actor[IO, ...]`, a `Resource[IO, ...]`
constructor, or a class that already holds `Ref[IO, ...]`/`IO.realTime`/etc. In those cases the
field type is `ContraTracer[IO, MyEvent]` because the whole class is IO-bound, not because the
tracer needs IO.

The polymorphic shape has two compounding benefits:

- **`nullTracer` is free.** `ContraTracer[F, X]`'s `Monoid.empty` instantiates the whole arrow
  as `Squelching`, so the upstream `contramapM`/`squelchUnlessM`/payload-construction work in
  the chain is never run — see `src/test/scala/hydrozoa/lib/logging/ContraTracerDemo.scala`
  (Demo 5) for the proof.
- **Pure callers don't need IO.** A `Gen[A]` body, a synchronous tx builder, or any other
  `F = Id`/`F = Eval` context can call the same function and get back `Id[Result] = Result`.
  No `IO.unsafeRunSync`, no rewiring.

The two existing tracer flavours plug straight into this shape:

- **Typed event ADT** (the per-actor pattern): `def step[F[_]: Monad](tracer: ContraTracer[F,
  MyEvent])`. IO callers pass a `ContraTracer[IO, MyEvent]`; pure callers lift the SLF4J sink
  through `Slf4jTracer.ioToId` (an `IO ~> Id` natural transformation) via `.natTracer(...)`.
- **`Slf4jMsg`** (the `log.info(...)` pattern): the extension methods (`info`, `warn`, …) are
  already declared `extension [F[_]: Monad](t: ContraTracer[F, Slf4jMsg])`, so call sites work
  in any `F` without further plumbing — see `Slf4jMsg.scala:54`.

See [`Polymorphic effect: end-to-end`](#polymorphic-effect-end-to-end) below for a worked
example showing the same function consumed from an IO actor and from a synchronous `Gen`.

## Core types

```scala
// src/main/scala/hydrozoa/lib/logging/Slf4jTracer.scala

enum Level { case Trace, Debug, Info, Warn, Error }

case class LogEvent(
    level:      Level,
    msg:        String,
    ctx:        Map[String, String]  = Map.empty,
    cause:      Option[Throwable]    = None,
    routingKey: Option[String]       = None  // SLF4J logger name
)

type Slf4jTracer = ContraTracer[IO, LogEvent]

object Slf4jTracer:
    val sink: ContraTracer[IO, LogEvent]   // SLF4J back-end; resolves logger from routingKey
```

`Slf4jTracer.sink` is a plain `val`. It is the entry point any per-component tracer is built
from:

```scala
val myTracer: ContraTracer[IO, MyEvent] =
    Slf4jTracer.sink.contramap(MyEventFormat.humanFormat(peerNum))
```

`LogEvent.From(ctx, routingKey)` is a partial-applied factory (used inside `…EventFormat`
objects) that fixes the routing key + base context once and lets each match arm read like
`info("…")` / `warn("…")` / etc.

## EventFormat conventions

- `humanFormat(peerNum)(e): LogEvent` — total. Every event variant renders to one line. Routing
  key is per-component, often suffixed with the peer number (`"CardanoLiaison.<peerNum>"`).

Lift through `Slf4jTracer.sink`:

```scala
val tracer: ContraTracer[IO, MyEvent] =
    Slf4jTracer.sink.contramap(MyEventFormat.humanFormat(peerNum))
```

The full set of typed events and formatters in the tree is discoverable as `*Event.scala` /
`*EventFormat.scala` — read those for the conventions used in this codebase.

## Custom in-process sinks

A `ContraTracer` is `A => IO[Unit]`, so any side-effect can be a sink. For example, Stage 4 captures block
briefs into a `Ref` for end-of-test assertions, combined with the SLF4J sink:

```scala
val captureSink: ContraTracer[IO, JointLedgerEvent] =
    ContraTracer.emit[IO, JointLedgerEvent] {
        case JointLedgerEvent.BriefProduced(b) => blockBriefsRef.update(_ :+ b)
        case _                                 => IO.unit
    }
val jlTracer: ContraTracer[IO, JointLedgerEvent] =
    captureSink |+| Slf4jTracer.sink.contramap(JointLedgerEventFormat.humanFormat(peerNum))
```

## Composing tracers across a regime

`MultisigRegimeManager` takes one `ContraTracer[IO, MultisigRegimeManagerEvent]` and
`contramap`s it down to per-actor channels:

```scala
private val jlTracer  = tracer.contramap(MultisigRegimeManagerEvent.JL.apply)
private val fcaTracer = tracer.contramap(MultisigRegimeManagerEvent.FCA.apply)
private val casTracer = tracer.contramap(MultisigRegimeManagerEvent.CAS.apply)
// …
```

`MultisigRegimeManagerEvent` is the sum of every typed event in the regime;
`MultisigRegimeManagerEventFormat` delegates each case to the producer's own formatter. The
wiring layer (`Main` / test harness) composes **one** sink per regime — and the wrapper exists
specifically so that a single capture `Ref` at the MRM boundary can observe every event flowing
through the regime.

Infrastructure that's instantiated outside the regime (`Persistence`, `RemoteL2Ledger`, HTTP
server, the WS transports created in `MultisigRegimeManager.resource`) takes its own
`ContraTracer[IO, …Event]` directly — built by the call site as
`Slf4jTracer.sink.contramap(…EventFormat.humanFormat)`.

## Polymorphic effect: end-to-end

Worked example of the default from above. `TxTiming.blockCanStayMinor` is polymorphic on `F`
and emits one `TxTimingEvent` per call:

```scala
def blockCanStayMinor[F[_]: Monad](
    tracer: ContraTracer[F, TxTimingEvent]
)(end: BlockCreationEndTime, fallback: FallbackTxStartTime): F[Boolean]
```

- **IO callers** (`JointLedger`, an `Actor[IO, ...]`) build a sub-tracer from the actor's typed
  tracer and instantiate `F = IO`:
  ```scala
  private val tmTracer: ContraTracer[IO, TxTimingEvent] =
      tracer.contramap(JointLedgerEvent.TimingEvent.apply)
  ```
  Then `txTiming.blockCanStayMinor(tmTracer)(...)` returns `IO[Boolean]`, threaded into the
  surrounding `for` comprehension.
- **Synchronous callers** (Stage 1 Model `Gen` bodies, tx builders) lift the same SLF4J sink
  through `Slf4jTracer.ioToId` (an `IO ~> Id` natural transformation) via
  [[ContraTracer.natTracer]] and instantiate `F = Id`:
  ```scala
  private val tmTracer: ContraTracer[cats.Id, TxTimingEvent] =
      Slf4jTracer.sink
          .contramap(TxTimingEventFormat.humanFormat)
          .natTracer(Slf4jTracer.ioToId)
  ```
  Then `txTiming.blockCanStayMinor(tmTracer)(...)` returns `Boolean` directly (because
  `Id[Boolean] = Boolean`). The events reach the same SLF4J back-end as the IO path.

There is **one** sink (`Slf4jTracer.sink: ContraTracer[IO, LogEvent]`). Synchronous code
reaches it through the NT. Don't silence at the call site with `ContraTracer.nullTracer` —
silence at the Logback level instead (`<logger name="X" level="off"/>`).

The same shape is used for `BlockHeader.nextHeader*` (typed events: `BlockHeaderEvent`) and
for the `Slf4jMsg` extension methods (`log.info(...)` etc., which are declared
`extension [F[_]: Monad](t: ContraTracer[F, Slf4jMsg])`).

## Logger-like logging without a typed event ADT: `Slf4jMsg`

App entry points (`hydrozoa.app.Main`, `Bootstrap`, `Migrate`),
test scaffolding (suite/sut classes, `ModelBasedSuite`), and infrastructure callers that don't
merit a typed event ADT hold a `ContraTracer[F, Slf4jMsg]` — a generic sum of
`Trace/Debug/Info/Warn/Error` lines, paired with extension methods so callers write
`log.info("…")` rather than `log.traceWith(Slf4jMsg.Info("…"))`:

```scala
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer, info}

private val log: ContraTracer[IO, Slf4jMsg] =
    Slf4jTracer.sink.contramap(Slf4jMsgFormat.humanFormat("hydrozoa.app.Main"))

log.info("Starting Hydrozoa node...")
```

The extension methods are polymorphic on `F[_]: Monad`, so the same shape works for pure code
(`F = cats.Id`) that can't run an `IO[Unit]` — `Gen` generators, synchronous tx builders, etc.
Lift the IO sink through `Slf4jTracer.ioToId` (an `IO ~> Id` natural transformation):

```scala
import cats.implicits.toContravariantOps
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer, debug, trace}

// In a `Gen[A]` body or any pure synchronous code path:
private val log: ContraTracer[cats.Id, Slf4jMsg] =
    Slf4jTracer.sink
        .contramap(Slf4jMsgFormat.humanFormat("Stage1.Model"))
        .natTracer(Slf4jTracer.ioToId)

val _ = log.debug("hello from Gen")   // Unit, runs the SLF4J side-effect synchronously
```

`src/main/scala/hydrozoa/lib/logging/TracingDemo.scala` shows both flavors end-to-end. Run
it with `sbt "runMain hydrozoa.lib.logging.TracingDemo"`.

Both forms are still typed `ContraTracer`s — they compose with `|+|`, `contramap`, custom
sinks, etc. The only difference from a per-actor event ADT is that the message string is the
payload, so the format is fixed and shared.

Do **not** use `Logging.loggerIO(name)`, `Logging.logger(name)`, `org.slf4j.LoggerFactory`, or
`IOLocal[Slf4jTracer]` directly. All four are absent from the production code path.

## Logback configuration

Each routing key is a distinct SLF4J logger name. To see (or silence) a per-component stream,
add a matching `<logger name="…">` to **every** `logback.xml` in the subproject — keep them in
sync:

- root: `src/main/resources/logback.xml` and `src/test/resources/logback.xml`
- `integration`: `integration/src/test/resources/logback.xml`

The `hydrozoa.trace` logger captures every `HTRACE|…` line across all actors; route it to its
own file appender when you want a structured event log.

## Note on MDC and fiber-hopping

SLF4J's `MDC` (`Mapped Diagnostic Context`) is a global `Map[String, String]` populated with
`MDC.put("requestId", "abc")` and referenced in a Logback pattern as `%X{requestId}`. Each OS
thread has its own MDC, stored in a `ThreadLocal`.

Under cats-effect, fibers hop between OS threads at suspension points (`IO.sleep`, queue
`take`, etc.). So a fiber that does `MDC.put("requestId", "abc")` on thread A and then
suspends may resume on thread B — thread B's MDC has no `requestId`, and the log line prints
either empty or, worse, picks up whatever leftover value another fiber put there. The
JVM-standard way to attach context to log lines is fundamentally fiber-unsafe.

Hydrozoa never used MDC. Context lives inside `LogEvent.ctx: Map[String, String]` and renders
as a message prefix (`[k=v …] msg`). Because the context is data, not ambient state, fiber
hops don't matter and no `IOLocal` is needed to carry it. The typed-events-everywhere design
takes this one step further: each variant of an `XYZEvent` ADT carries its own typed payload,
so there's nothing global at all.

## Backend

The base sink wraps log4cats's `Slf4jLogger`. Existing Logback config continues to work
unchanged.
