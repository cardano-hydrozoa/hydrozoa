# Spike: replace SLF4J + Logback + log4cats with scribe

**Status:** decision spike, PoC run. **Recommendation: HOLD** — the level-API win is real and proven,
but a measured load test found scribe's built-in async handle drops ~99.9% of a burst (a ~1000 ev/s
drain ceiling). Adopt scribe only with a **custom async handle** (or if prod log volume is provably low).

## PoC results (measured, this repo)

Two artifacts landed to validate criteria 1 and 2 concretely (not just on paper):
- `lib/logging/ScribeTracer.scala` — a drop-in `ContraTracer[IO, LogEvent]` sink on scribe. **Compiles
  and works; the level gate reimplements via `scribe.Logger.get(name).includes(level)` with zero
  `org.slf4j`.** ✅ Criterion 1 (the decisive one) proven in-repo.
- `src/test/.../ScribeLoadTest.scala` (ignored by default) — 64 CE fibers × 20 000 `trace` emits =
  1.28M events through `ScribeTracer.sink` with `AsynchronousLogHandle(maxBuffer = 8192, DropNew)` and a
  counting no-op writer (measures the queue, not disk).

Measured (idiomatic-CE harness, no nested `unsafeRunSync`):

| metric | value |
|---|---|
| events produced | 1 280 000 |
| **events drained/written** | **~1 090** |
| **drops** | **~99.9%** |
| producer throughput | ~829 000 ev/s (producers are *not* blocked in aggregate) |
| worst single-emit | ~43 ms (likely a GC pause under the 1.28M-object burst, not a scribe block) |

**Interpretation.** scribe's `AsynchronousLogHandle` is a single daemon thread sleep-polling a
`ConcurrentLinkedQueue` (`sleep(1ms)` busy / `sleep(10ms)` idle), giving a **~1000 ev/s drain ceiling**.
Under a burst the 8192 buffer fills in ~10ms and `DropNew` then drops essentially everything. Logback's
`AsyncAppender` (disruptor-style) sustains 100k+/s, so it drops far less at the same load. This is a
**real gap on the exact "consensus-rate trace" scenario** the async path exists for — not a harness
artifact (confirmed across two harness variants).

Nuance: both back-ends *intentionally* drop under overload (that's `neverBlock`/`DropNew`); the
difference is the rate at which they saturate. At low volume (info/warn/error, moderate debug) scribe
is fine; the ceiling only bites high-frequency `trace`/`debug` bursts. A **synchronous** scribe handle
(for the lossless `hydrozoa.trace` JSONL) has no drain thread and no drop — same trade as Logback's sync
`TRACE_FILE`.

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

2. **Async fairness — NO-GO on the built-in handle (measured — see PoC results above).**
   `AsynchronousLogHandle(maxBuffer, Overflow.DropNew)` is the API analogue of Logback
   `AsyncAppender neverBlock=true`, but its single sleep-polling drain thread caps at **~1000 ev/s** and
   dropped ~99.9% of the burst. This is the gate, and scribe's built-in async fails it for the
   high-volume trace path. **Mitigation required:** a custom `LogHandle` (scribe lets you supply one)
   that batch-drains without the per-item sleep — i.e. we'd write the async plumbing scribe doesn't,
   which is exactly what Logback already gives us. Sync handles (lossless trace file) are unaffected.

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
2. **Async drainer under consensus load — DONE, FAIL.** Measured ~99.9% drops / ~1000 ev/s ceiling;
   see PoC results. Blocks a naive migration.

Remaining, only if scribe is pursued with a custom async handle:

3. **Third-party SLF4J floods.** `com.bloxbean`, `scalus`, `org.testcontainers`, dockerjava, scalacheck
   log through SLF4J and are muted today via logback levels. Dropping Logback needs `scribe-slf4j`
   (the SLF4J→scribe facade) so they route into scribe, then `Logger("com.bloxbean").withMinimumLevel(Warn)`.
   PoC must confirm the bridge captures and gates them, else the noise returns.
3. **Config-profile fidelity.** Diff actual output (`hydrozoa-trace.jsonl`, `stage4-peers-interaction.mmd`,
   `integration-tests.log`) against current Logback output; verify `additivity=false` / sync-vs-async
   choices reproduce.

## Files

- `lib/logging/Slf4jTracer.scala` — the sole backend touch point to replace (→ `ScribeTracer`).
- `lib/logging/ContraTracer.scala` — unchanged; `squelchUnlessM` is the gate hook.
- `integration/src/test/scala/hydrozoa/integration/MermaidLayout.scala` — reproduce as a scribe `Formatter`/`Writer`.
- `src/main/resources/logback*.xml`, `src/test/resources/logback-test.xml`, `integration/src/test/resources/logback-{test,ci}.xml` — port to Scala config objects.

Sources: scribe repo + wiki (Cats-Effect Support, Features), `Logger.scala`, `AsynchronousLogHandle.scala`/`Overflow.scala`, `Formatter.scala`/`LogRecord.scala`.
