# Spike: replace SLF4J + Logback + log4cats with scribe

**Status:** decision spike (go/no-go). **Recommendation: GO to a PoC**, gated on one load test.

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

## Go/no-go findings (verified against scribe 3.19.0 source)

1. **Level introspection with no `org.slf4j` — the decisive criterion: GO.**
   `scribe.Logger.get(name): Option[Logger]` + `Logger.includes(level: Level): Boolean` is the exact
   runtime, per-call analogue of `isXEnabled` (consults cached level + modifiers, reflects config
   changes). So `isEnabled(name, level): IO[Boolean]` reimplements directly and the `org.slf4j` import
   disappears. Levels map 1:1 (`scribe.Level.{Trace,Debug,Info,Warn,Error}`).
   cats-effect module: `"com.outr" %% "scribe-cats" % "3.19.0"` (returns `F[Unit]` via `Sync`).
   Note: `log4cats-scribe` is a dead end (Scala 2.12 / scribe 2.7.1 only) — use `scribe-cats` directly.

2. **Async fairness — GO on paper, MUST be PoC-measured (the gate).**
   scribe's `AsynchronousLogHandle(maxBuffer = 1000, overflow = Overflow.DropNew | DropOld | Block | Error)`
   is the analogue of Logback `AsyncAppender neverBlock=true`. `Overflow.DropNew` = the literal
   never-block-drop-incoming behaviour; `maxBuffer` bounds memory (match today's 8192). **Risk:** the
   queue is a single sleep-polling daemon thread over a `ConcurrentLinkedQueue` (`sleep(1ms)` busy /
   `sleep(10ms)` idle) — structurally different from Logback's appender. Throughput/latency under a
   consensus-rate burst is unknown and is the one place the "consensus fibers must not stall"
   invariant lives.

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

## What a PoC must prove (in priority order)

1. **Async drainer under consensus load (the go/no-go gate).** Burst `trace` from many simulated
   consensus fibers on the CE compute pool; measure p99 of the emit `IO` (must stay bounded, never
   block the producing fiber), confirm clean drops at `maxBuffer` (`Overflow.DropNew`) with no
   unbounded growth/GC pressure, and that the single drainer keeps up or drops gracefully. Consider a
   dedicated async handle per hot logger vs. a shared one.
2. **Third-party SLF4J floods.** `com.bloxbean`, `scalus`, `org.testcontainers`, dockerjava, scalacheck
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
