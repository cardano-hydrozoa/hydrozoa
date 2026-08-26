# Spike: replace SLF4J + Logback + log4cats with scribe

**Status:** decision spike, PoC + corrected benchmark run. **Recommendation: GO** (viable). The
level-API win is proven, and scribe's *default synchronous* path is lossless and fast (~397k ev/s
through a no-op writer, **0 drops**). An earlier **HOLD was a benchmarking error**: it measured
scribe's opt-in `AsynchronousLogHandle` — a crude, non-default component whose drain thread writes one
record then `Thread.sleep(1)` (a ~1000 rec/s ceiling, source-verified) — which the real sink never
uses. Migrate on the synchronous handle; for the high-volume trace file, put async at the cats-effect
layer (bounded `Queue` + consumer fiber), never scribe's `AsynchronousLogHandle`.

## PoC results (measured, this repo)

`lib/logging/ScribeTracer.scala` is a drop-in `ContraTracer[IO, LogEvent]` sink on scribe. It compiles
and works; the level gate reimplements via `scribe.Logger.get(name).includes(level)` with zero
`org.slf4j`. ✅ Criterion 1 (the decisive one) proven in-repo. Its emit is `IO { logger.trace(msg) }`
— scribe's **synchronous default handle** (`LogHandlerBuilder.handle = SynchronousLogHandle`,
source-verified): a direct format-and-write, no queue, **no drop**.

`src/test/.../ScribeLoadTest.scala` bursts 320 000 `trace` emits from 32 CE fibers through the sink,
against a counting no-op writer (measures the framework/queue mechanism, not disk):

| wiring | drops | throughput | notes |
|---|---|---|---|
| **synchronous** (the sink's real path) | **0 / 320 000** | **~397k ev/s** | scribe default; lossless |
| **ce-async** (bounded `Queue` + 1 consumer) | **0 / 320 000** | **~384k ev/s** | idiomatic `AsyncAppender` replacement; write off producers, backpressure not drop |
| scribe `AsynchronousLogHandle` (`DropNew`, buf 8192) | **319 401 / 320 000 (99.8%)** | ~1k rec/s drain | opt-in, non-default; **do not use** |

**Why the async handle drops (source-verified, scribe 3.19.0 `handler/AsynchronousLogHandle.scala`).**
Its background thread runs `while(true){ if (flushNext()) sleep(1) else sleep(10) }`, and `flushNext()`
polls and writes exactly **one** record. So it drains ~1 record/ms ≈ 1000 rec/s regardless of buffer
size; under a faster burst the 8192 buffer fills in ~8ms and `DropNew` discards the rest. This is
scribe's weakest, opt-in component — not its default, and not what a cats-effect app should touch. The
prior HOLD benchmarked exactly this handle, so its ~99.9%-drop figure was real but irrelevant to the
migration.

**The correct async, when we want writes off the consensus fibers** (as Logback's `AsyncAppender` does
today for the trace JSONL): a bounded cats-effect `Queue[IO, LogEvent]` drained by one consumer fiber
that performs the synchronous scribe write. Measured lossless at ~384k ev/s with real backpressure
(`offer` suspends the producing fiber when full) rather than dropping — strictly better than both
scribe's handle and Logback's `neverBlock` drop policy, and we own the drain (batchable). scribe's own
tagline is "the fastest JVM logger" on the **synchronous** path; the framework's intent is that you do
not need a background thread at all.

## Context

The logging overhaul (removing the `Logging` module, the level-gated deferred-render `sink`,
events carrying raw domain objects) left the backend coupling pinned to a single file,
`lib/logging/Slf4jTracer.scala`: it uses log4cats (`Slf4jLogger.getLoggerFromName[IO]`) to emit and
a per-trace `org.slf4j.LoggerFactory.getLogger(name).isXEnabled` to gate on level. Those are the only
`org.slf4j`/`log4cats` references in the tree (enforced by the `DisableSyntax` rule). The
`ContraTracer[IO, LogEvent]` abstraction and `LogEvent`/`Rendered`/`From` are backend-agnostic.

[scribe](https://github.com/outr/scribe) is a Scala-native logger (3.19.0, `com.outr`) with a
cats-effect module. Replacing the backend would drop `logback-classic`, `log4cats-slf4j`, and
`org.slf4j` from the tracer while keeping the exact `ContraTracer` surface.

## scribe does not replace `ContraTracer` — it's a backend leaf

`ContraTracer[F, A]` and scribe live at different layers, and the migration only touches the lower one:

- **`ContraTracer` is a typed-event bus** — a contravariant functor over *domain* events with
  `contramap` (adapt the event type), a `Semigroup`/`Monoid` (`|+|`, fan-out to N consumers), and the
  arrow-laziness. Its currency is a typed `MyEvent`, not a log line.
- **scribe is a logging backend** — its currency is a `LogRecord` (level + rendered message + MDC),
  written to console/file. It is one **leaf sink** (`ContraTracer[IO, LogEvent]`).

So scribe replaces **only `Slf4jTracer.sink`**; everything above it is untouched. This matters for two
things the team relies on:

- **Test event-bus.** Tests do `ContraTracer[IO, XEvent](e => ref.update(e :: _))` and compose
  `slf4jLeg |+| capture`, asserting on the typed event's *fields*. scribe can't do this — by the time
  anything reaches it the event is a rendered `LogRecord` and the domain fields are gone. This lives in
  `ContraTracer` and is unaffected by swapping the sink (the capture leg is a different leaf).
- **Future telemetry / multiple sinks.** scribe's multiple handlers are multiple *outputs of one log
  record*; `ContraTracer`'s `|+|` is multiple consumers of one *typed event*, each extracting what it
  needs. A metrics sink is just another leaf: `ContraTracer[IO, BlockEvent](e => counters.record(e.blockNum))`,
  fanned in with `|+|` — exactly what `ContraTracer`'s own doc cites ("metrics/telemetry by changing the
  `emit` effect ... combine ... via the semi-group instance").

**Conclusion:** keep `ContraTracer` as the bus permanently; adopt scribe as the log leaf. The migration
carries zero risk to the test event-bus or a future telemetry/multi-sink story, because those never
depended on the logging backend.

## Go/no-go findings (verified against scribe 3.19.0 source)

1. **Level introspection with no `org.slf4j` — the decisive criterion: GO.**
   `scribe.Logger.get(name): Option[Logger]` + `Logger.includes(level: Level): Boolean` is the exact
   runtime, per-call analogue of `isXEnabled` (consults cached level + modifiers, reflects config
   changes). So `isEnabled(name, level): IO[Boolean]` reimplements directly and the `org.slf4j` import
   disappears. Levels map 1:1 (`scribe.Level.{Trace,Debug,Info,Warn,Error}`).
   cats-effect module: `"com.outr" %% "scribe-cats" % "3.19.0"` (returns `F[Unit]` via `Sync`).
   Note: `log4cats-scribe` is a dead end (Scala 2.12 / scribe 2.7.1 only) — use `scribe-cats` directly.

2. **Throughput / losslessness — GO (measured — see PoC results above).**
   The sink uses scribe's **synchronous** handle: lossless at ~397k ev/s. For the high-volume trace
   path where we want the write off the consensus fibers, wrap the sink in a bounded cats-effect
   `Queue` + consumer fiber (lossless, ~384k ev/s, backpressure not drop). **Do not** use scribe's
   `AsynchronousLogHandle`: its drain thread writes one record per `Thread.sleep(1)` (~1000 rec/s) and
   drops ~99.8% under a burst. Its overflow default is `DropOld` and its `Overflow.Block` mode merely
   converts the same ~1000/s ceiling into producer backpressure on a JVM thread — neither is what a
   cats-effect app should use; own the async at the CE layer instead.

3. **Custom Mermaid layout — GO, straightforward.**
   `Formatter { def format(record: LogRecord): LogOutput }` + a `Writer` (`FileWriter` with a path
   DSL) replaces the `MermaidSequenceDiagramLayout extends PatternLayout` (a 7-line class whose only
   real customization is `getFileHeader = "sequenceDiagram"`). Body is `formatter"$messages"`; the
   once-per-file header is written at handler-construction time or by a thin `Writer` wrapper.
   `LogRecord` exposes `level`/`messages`/`className`/`fileName`/`timeStamp` — everything it needs.

4. **Config: XML → programmatic Scala — GO, bounded migration.**
   The 5 logback profiles (`logback.xml`, `logback-docker.xml`, root `logback-test.xml`, integration
   `logback-test.xml` + `logback-ci.xml`) become 5 Scala config objects selected by env var, each a
   `configure(): Unit` that sets root level/handlers and folds a shared `Map[String, Level]` of
   per-logger levels via `Logger(name).withMinimumLevel(level)`. Upside: the CLAUDE.md "keep every
   logback.xml in sync" hazard collapses to one shared `val`. Caveats: `additivity="false"` (used by
   the `hydrozoa.trace` JSONL and the Mermaid logger) is reproduced with `.orphan().replace()` — easy
   to get subtly wrong; the synchronous, lossless `TRACE_FILE` maps to a `FileWriter` with the
   **synchronous** handle (no async, no drop); scribe needs an explicit `configure()` call at
   process/test startup (Logback auto-inits from the classpath).

## Proposed drop-in (`ScribeTracer`, same `ContraTracer[IO, LogEvent]` surface)

Only the `object` body of `Slf4jTracer.scala` changes; `LogEvent`/`LogEventTyped`/`Rendered`/`Level`/
`From` and `ContraTracer` (incl. `squelchUnlessM`, the gate hook) are untouched. The gate still forces
`ev.render.value` only past the level check; the predicate's backend changes from `org.slf4j` to
`scribe.Logger.get(name).includes(level)`:

```scala
val sink: ContraTracer[IO, LogEvent] = ContraTracer
    .emit((ev: LogEvent) => IO {
        val r = ev.render.value                          // forced only past the gate
        val lg = scribe.Logger(ev.routingKey.getOrElse("hydrozoa"))
        val msg = renderMsg(r)
        ev.level match
            case Level.Trace => lg.trace(msg); case Level.Debug => lg.debug(msg)
            case Level.Info  => lg.info(msg);  case Level.Warn  => lg.warn(msg)
            case Level.Error => r.cause.fold(lg.error(msg))(t => lg.error(msg, t))
    })
    .levelGated

private def isEnabled(name: String, level: Level): IO[Boolean] = IO {
    scribe.Logger.get(name).getOrElse(scribe.Logger.root).includes(toScribe(level))  // no org.slf4j
}
```

`From.ctx` could later move into scribe MDC (`$mdc` formatter) instead of the manual `renderMsg`
prefix — a refinement, not needed for parity.

## Validation status

1. **Level API, no `org.slf4j` — DONE, PASS.** `ScribeTracer` compiles and gates via
   `Logger.includes`; see PoC results.
2. **Throughput / losslessness under consensus load — DONE, PASS.** Synchronous sink: 0 drops, ~397k
   ev/s; CE-async (queue + consumer): 0 drops, ~384k ev/s. Only scribe's opt-in `AsynchronousLogHandle`
   drops — excluded from the migration.

Remaining, before a full migration:

3. **Third-party SLF4J floods.** `com.bloxbean`, `scalus`, `org.testcontainers`, dockerjava, scalacheck
   log through SLF4J and are muted today via logback levels. Dropping Logback needs `scribe-slf4j`
   (the SLF4J→scribe facade) so they route into scribe, then `Logger("com.bloxbean").withMinimumLevel(Warn)`.
   PoC must confirm the bridge captures and gates them, else the noise returns.
4. **Config-profile fidelity.** Diff actual output (`hydrozoa-trace.jsonl`, `stage4-peers-interaction.mmd`,
   `integration-tests.log`) against current Logback output; verify `additivity=false` / sync-vs-async
   choices reproduce.

## Parked follow-up: the `squelchUnlessM` gate duplicates the backend's own gating

Benchmarks on the log4cats sink (JMH, `-prof gc`) showed the level-gate is a targeted win — it drops
a disabled × expensive-render emit from ~1314 to ~818 B/op (the wasted-`toHex`-on-a-disabled-trace
path behind the OOM) — but adds ~480 B/op on the *enabled* path. Most of that is the `Eval`/`Rendered`
deferral wrapping (inherent to deferred render), but ~100–150 B/op is the gate itself: `squelchUnlessM`
runs a per-emit `isEnabled` IO and `Either` routing, then the sink forces the render and hands it to
log4cats, whose `contextLog` is literally `F.delay(if (isEnabledUnsafe()) logging())` — i.e. log4cats
*already* checks the level and only forces its by-name message when enabled. So the ContraTracer gate
duplicates the backend's own lazy gating (a double level-check on the enabled path).

**This carries into scribe unchanged:** `ScribeTracer.sink` has the same shape — force `ev.render.value`
eagerly, then `.squelchUnlessM(includes)`. During the migration, decide deliberately: either drop
`squelchUnlessM` and hand the render to scribe inside its by-name `trace` call (lean on scribe's own
gating — smaller, backend-coupled), or keep the gate as an intentional *backend-agnostic* laziness
guarantee (holds even for a leaf whose log call isn't lazy) and accept the ~100–150 B/op. Not a
blocker either way; parked so it isn't re-discovered. The benchmark itself was a throwaway spike and
was not committed.

## Files

- `lib/logging/Slf4jTracer.scala` — the sole backend touch point to replace (→ `ScribeTracer`).
- `lib/logging/ContraTracer.scala` — unchanged; `squelchUnlessM` is the gate hook.
- `integration/src/test/scala/hydrozoa/integration/MermaidLayout.scala` — reproduce as a scribe `Formatter`/`Writer`.
- `src/main/resources/logback*.xml`, `src/test/resources/logback-test.xml`, `integration/src/test/resources/logback-{test,ci}.xml` — port to Scala config objects.

Sources: scribe repo + wiki (Cats-Effect Support, Features), `Logger.scala`, `AsynchronousLogHandle.scala`/`Overflow.scala`, `Formatter.scala`/`LogRecord.scala`.
