# Logging backend benchmark: SLF4J/Logback vs scribe

**Question.** scribe advertises itself as "the fastest logging framework on the JVM." Is there a
performance case for swapping hydrozoa's SLF4J/Logback backend for scribe? And separately: how much
does the `ContraTracer` typed-event-bus layer cost over calling a backend directly?

**Answer.** No performance case for scribe. It is slower and allocates more per call, ~2.6× slower
on the lossless file path, and its asynchronous path drops ~99.8% of records under burst. The
`ContraTracer` + `Eval` layer is not free (~600 B/op over a bare `IO`), but it is backend-independent
and small relative to the actual logging work.

The two benchmarks live in the `benchmark` subproject:

- `LoggingBackendBench.scala` — JMH microbenchmark: caller-side cost per log call, to a discarding
  counting sink (the same shape scribe uses in its own `PerformanceBenchmark`).
- `LoggingLoadTest.scala` — standalone burst-and-count harness: end-to-end **delivery** to a real
  file under load (models scribe's own `LoggingStressTest`).

scribe/scribe-cats/scribe-file are dependencies of the `benchmark` subproject only — not `core`.

## Methodology / fairness

- **Discarding sink, no disk I/O** for the microbench, so numbers isolate framework overhead, not
  storage. Both writers materialize the formatted line and sum its length (scribe formats eagerly in
  the handler, logback in the appender — making both produce the final `String` is the
  apples-to-apples point); the counter defeats dead-code elimination.
- **scribe uses its own recommended benchmark formatter** `formatter"$date $levelPaddedRight
  [$threadName] $messages"` (from scribe's repo), not `Formatter.default` (which adds source-position
  capture). One variant keeps the default so the formatter's cost is visible.
- **Loggers cached once; level = INFO.** Disabled scenarios call at TRACE.
- **Async buffers matched at 1000 slots** on both sides in the load test, with each framework's real
  default overflow policy.
- Single machine, single JVM per run, G1GC, `-Xmx16G` (the benchmark subproject's fork options).
  Scala 3.3.7, scribe 3.19.0, logback-classic 1.5.18. Absolute numbers are storage/JVM-dependent;
  the **ratios** are the takeaway.

## Result 1 — per-call microbenchmark (JMH, `-prof gc`)

`Mode.Throughput`, higher ops/s is better; `gc.alloc.rate.norm` is bytes allocated per call.

| Scenario | Backend / config | Throughput (ops/s) | Alloc (B/op) |
|---|---|---:|---:|
| **enabled, interpolated** | **slf4j/logback** | **7,185,000** | **844** |
| | scribe — default formatter | 5,940,000 | 1,234 |
| | scribe — message-only formatter | 5,633,000 | 1,265 |
| | scribe — its own "lean" formatter | 3,727,000 | 1,959 |
| | scribe — **async** (fire-and-forget) | 15,343,000 | 528 |
| | scribe-cats `.f[IO]` (per-op `unsafeRunSync`)† | 90,000 | 2,957 |
| **enabled, cheap constant** | **slf4j/logback** | **8,982,000** | **488** |
| | scribe | 4,426,000 | 1,584 |
| **disabled** (TRACE @ INFO) | **slf4j — guarded `if(isTraceEnabled)`** | **1,108,000,000** | **~0** |
| | slf4j — naive `trace(s"…")` | 31,453,000 | 309 |
| | scribe — macro (no guard) | 17,013,000 | 464 |
| | scribe — guarded `includes(…)` | 18,758,000 | 464 |

† The `unsafeRunSync`-per-op figure is a benchmark artifact (~11 µs runloop entry). See Result 2 for
the batched, honest effectful throughput.

Notes:

- **Enabled path: logback wins on every variant** — ~1.5–2× the throughput at roughly half the
  allocation. Squeezing scribe didn't help: its *own* "lean" formatter is the **worst** (1,959 B/op)
  because `$date`/`$threadName` cost more than `Formatter.default`'s cached blocks; message-only
  can't get under ~1,265 B/op. logback sits at 844.
- **Disabled path: guarded logback is untouchable** (~0 allocation). scribe's macro laziness is
  ergonomic (no guard needed) but still allocates **~464 B/op even when it decides not to log**, and
  a manual `includes` guard doesn't change that. For short messages, even naive logback beats
  disabled scribe on both axes.
- **scribe async's 15M ops/s is caller-side enqueue only** — see Result 3 for what actually gets
  delivered.

## Result 2 — ContraTracer overhead (batched)

Running one `unsafeRunSync` per log call costs ~11 µs and swamps everything, which is *not* how the
system runs (logging IO composes into the actor's already-running IO). Batching 1000 fresh events
through one `unsafeRunSync` (`@OperationsPerInvocation(1000)`) gives the honest per-log cost. The
`traverse_` plumbing is identical across the three, so their deltas are clean.

| Layer (batched, per-op) | Throughput (ops/s) | Alloc (B/op) |
|---|---:|---:|
| bare `IO { counter }` (floor) | 20,515,000 | 137 |
| + ContraTracer arrow + `Eval` + `LogEvent` build (no backend) | 6,249,000 | 736 |
| + log4cats + logback (full `Slf4jTracer.sink`) | 3,077,000 | 1,288 |

- The **typed-event-bus layer** (`ContraTracer` + `Eval.later` + `LogEvent`/`Rendered`) adds
  **~600 B/op and a ~3× throughput hit over a bare `IO`** — backend-independent, the cost of the
  abstraction itself.
- Routing a line through the full sink (**3.1M ops/s, 1,288 B/op**) vs a direct logback call
  (**7.2M ops/s, 844 B/op**) roughly **halves throughput and adds ~50% allocation**. At consensus log
  rates this is almost certainly negligible against real work, but it is not zero.

## Result 3 — file-writer delivery under load

Fire 500,000 INFO lines from 8 producer threads as fast as possible, let the async drains settle
(3 s), then count how many lines actually reached the file. `dropped = produced − delivered`.

| config | produced | delivered | dropped | drop % | producer lines/s |
|---|---:|---:|---:|---:|---:|
| **scribe sync** | 500,000 | 499,997 | 3 | ~0% | 770,000 |
| **scribe async** | 500,000 | **1,076** | 498,924 | **99.78%** | 5,100,000 |
| **logback sync** | 500,000 | 500,000 | 0 | 0% | **2,036,000** |
| **logback async** | 500,000 | 163,592 | 336,408 | 67.28% | 6,279,000 |

- **scribe async drops 99.78%.** Only ~1,000 lines (≈ the buffer) survive. The drain loop is
  `flushNext(); Thread.sleep(1)` → a **~1,000 rec/s** ceiling with `Overflow.DropOld`. **This is not a
  buffer-size problem** — the per-record 1 ms sleep caps sustained delivery no matter how large
  `maxBuffer` is. Its 5M-lines/s "producer rate" is a fiction; the records go into the void.
- **logback async also sheds load under this burst (67%) but delivers ~150× more** (163,592) because
  its worker drains at the `FileAppender`'s real speed (~2M/s), not an artificial 1/ms. At real
  consensus rates (well under 2M lines/s) it delivers ~100%; `neverBlock=true` chooses "drop" over
  stalling consensus fibers only when a burst genuinely outruns the disk.
- **On the lossless path we actually use, logback is ~2.6× faster to file** (2.04M vs 0.77M lines/s),
  both delivering ~100%. (scribe sync's 3-line tail loss is an `AsynchronousFlush`-on-dispose
  artifact — effectively lossless.)

Making scribe viable for lossless logging would require a hand-written drain handle (batch drain, no
per-record sleep) — i.e. reimplementing what logback's `AsyncAppender` already does well.

## Verdict

| Axis | Winner | Margin |
|---|---|---|
| Per-call enabled (micro) | logback | ~1.5–2× faster, ~½ alloc |
| Per-call disabled, guarded | logback | ~0 alloc vs scribe's ~464 B/op floor |
| Lossless file throughput | logback | ~2.6× faster |
| Async delivery under burst | logback | delivers ~150× more before dropping |
| scribe async producer rate | scribe | but drops 99.78% — meaningless |

No performance reason to migrate off SLF4J/Logback. Independently, scalus and bloxbean pull in SLF4J
regardless, so scribe would be an *added* backend, not a replacement. Migration stays parked.

## Reproduce

```bash
# Per-call microbenchmark + ContraTracer decomposition (GC profiler on):
sbt "benchmark/Jmh/run -prof gc -i 4 -wi 3 -f 1 -t 1 .*LoggingBackendBench.*"

# File-writer delivery under load (defaults: 500000 lines, 8 producers):
sbt "benchmark/runMain hydrozoa.benchmarks.LoggingLoadTest [total] [threads]"
```
