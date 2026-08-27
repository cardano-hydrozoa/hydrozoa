package hydrozoa.benchmarks

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{Level as LbLevel, LoggerContext, PatternLayout}
import ch.qos.logback.core.AppenderBase
import hydrozoa.lib.logging.{ContraTracer, Level as HLevel, LogEvent, Slf4jTracer}
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicLong
import org.openjdk.jmh.annotations.*
// Reaches logback's global context on purpose (raw-backend baseline + the ContraTracer sink path).
import org.slf4j.{Logger as Slf4jLogger, LoggerFactory} // scalafix:ok DisableSyntax
import scribe.cats.*
import scribe.format.*
import scribe.handler.AsynchronousLogHandle
import scribe.output.LogOutput
import scribe.output.format.OutputFormat
import scribe.writer.Writer
import scribe.{Level as SLevel, LogRecord, Logger as SLogger, Scribe}

/** Backend microbenchmark: SLF4J/Logback vs scribe, plus the cost the `ContraTracer` sink adds over
  * calling a backend directly. A real logging call from a cached logger through the framework's own
  * level check and formatter to a **discarding, counting sink** — the same shape scribe uses in its
  * own JMH suite (`PerformanceBenchmark`), so neither side pays disk I/O.
  *
  * Fairness / correctness (checked against scribe's repo):
  *   - scribe loggers use scribe's **own recommended benchmark formatter**
  *     `formatter"$$date $$levelPaddedRight [$$threadName] $$messages"` — not `Formatter.default`,
  *     which adds source-position capture and unfairly inflates scribe. One variant keeps the
  *     default so the formatter's cost is visible.
  *   - Both writers materialize the formatted line (`output.plainText` / `layout.doLayout`) and sum
  *     its length: scribe formats eagerly in the handler, logback formats in the appender, so
  *     making both produce the final String is the apples-to-apples point. The counter defeats DCE.
  *   - Loggers are cached once; level = INFO.
  *
  * Axes:
  *   - `disabled*`: a TRACE call at an INFO threshold. Logback's arg is strict (naive builds the
  *     string; `guarded` is the idiomatic `isTraceEnabled` fix). scribe's macro captures by-name,
  *     so bare `trace` is already lazy; `guarded` uses `includes` to also skip the closure alloc.
  *   - `enabled{Cheap,Expensive}`: constant vs interpolated message, both backends.
  *   - `scribeAsync`: scribe's fire-and-forget `AsynchronousLogHandle` — its advertised "fastest"
  *     path. NB: default maxBuffer=1000, drain loop flushes one record then `Thread.sleep(1)`
  *     (~1000 rec/s ceiling) with `Overflow.DropOld`, so under JMH's call rate it SILENTLY DROPS;
  *     this measures caller-side enqueue cost only, not delivery.
  *   - `scribeCats`: the effectful `.f[IO]` path (what a scribe `ContraTracer` sink would use).
  *   - `contra*`: `rawIO` (IO + unsafeRunSync, no tracer) → `contraEmit` (ContraTracer arrow + Eval
  *     + LogEvent build, no backend) → `contraSink` (full `Slf4jTracer.sink` through
  *     log4cats+logback). Deltas isolate what ContraTracer/Eval add and what the whole sink costs
  *     over a bare slf4j call.
  *
  * Run: `sbt "benchmark/Jmh/run -prof gc -i 4 -wi 3 -f 1 -t 1 .*LoggingBackendBench.*"` Read
  * `gc.alloc.rate.norm` (B/op) next to throughput (ops/s).
  */
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@State(Scope.Thread)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 4, time = 1)
@Fork(1)
class LoggingBackendBench:

    /** Discarding sink shared by every backend: format to a String, sum its length. */
    private val sink = new AtomicLong(0)

    // Per-invocation-varying payload so interpolated messages are not compile-time constants.
    private var n: Long = 0L
    private val payload = new Object:
        override def toString: String = "payload#" + (n & 0xff)

    private var slf4j: Slf4jLogger = scala.compiletime.uninitialized
    private var scribeSync: SLogger = scala.compiletime.uninitialized
    private var scribeMsgOnly: SLogger = scala.compiletime.uninitialized
    private var scribeDefaultFmt: SLogger = scala.compiletime.uninitialized
    private var scribeAsync: SLogger = scala.compiletime.uninitialized
    private var scribeCats: Scribe[IO] = scala.compiletime.uninitialized

    // scribe's own recommended benchmark formatter (no source-position capture).
    private val leanFmt: Formatter = formatter"$date $levelPaddedRight [$threadName] $messages"

    /** ContraTracer with no backend: forces the Eval render (as a real sink would) and counts. */
    private val contraEmitSink: ContraTracer[IO, LogEvent] =
        ContraTracer.emit((ev: LogEvent) =>
            IO { val _ = sink.addAndGet(ev.render.value.msg.length.toLong) }
        )

    /** An eager event: the same fields a [[LogEvent]] carries, but the message is built and stored
      * directly — no `Eval.later` deferral, no `Rendered` wrapper. Lets us isolate what that
      * machinery costs from the bare ContraTracer arrow + event allocation.
      */
    private final case class EagerEvent(
        level: HLevel,
        routingKey: Option[String],
        msg: String,
        ctx: Map[String, String] = Map.empty,
        cause: Option[Throwable] = None
    )

    /** ContraTracer over the eager event: reads `msg` directly (nothing to force) and counts. */
    private val contraEmitEagerSink: ContraTracer[IO, EagerEvent] =
        ContraTracer.emit((ev: EagerEvent) => IO { val _ = sink.addAndGet(ev.msg.length.toLong) })

    @Setup(Level.Trial)
    def setup(): Unit =
        configureLogback()
        scribeSync = scribeLogger(leanFmt, None)
        scribeMsgOnly = scribeLogger(formatter"$messages$newLine", None)
        scribeDefaultFmt = scribeLogger(Formatter.default, None)
        scribeAsync = scribeLogger(leanFmt, Some(AsynchronousLogHandle()))
        scribeCats = scribeLogger(leanFmt, None).f[IO]

    // --- Logback wiring -----------------------------------------------------

    private class CountingLogbackAppender(layout: PatternLayout)
        extends AppenderBase[ILoggingEvent]:
        override def append(e: ILoggingEvent): Unit =
            val _ = sink.addAndGet(layout.doLayout(e).length.toLong)

    private def configureLogback(): Unit =
        val ctx = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
        val layout = new PatternLayout()
        layout.setContext(ctx)
        layout.setPattern("%d{HH:mm:ss.SSS} %-5level %logger{20} - %msg%n")
        layout.start()
        val appender = new CountingLogbackAppender(layout)
        appender.setContext(ctx)
        appender.start()
        val lg = ctx.getLogger("bench.slf4j")
        lg.setAdditive(false)
        lg.detachAndStopAllAppenders()
        lg.addAppender(appender)
        lg.setLevel(LbLevel.INFO)
        slf4j = lg

    // --- scribe wiring ------------------------------------------------------

    private class CountingScribeWriter extends Writer:
        def write(record: LogRecord, output: LogOutput, outputFormat: OutputFormat): Unit =
            val _ = sink.addAndGet(output.plainText.length.toLong)

    private def scribeLogger(fmt: Formatter, handle: Option[AsynchronousLogHandle]): SLogger =
        val base = SLogger.empty.orphan()
        handle.fold(
          base.withHandler(
            formatter = fmt,
            writer = new CountingScribeWriter,
            minimumLevel = Some(SLevel.Info)
          )
        )(h =>
            base.withHandler(
              formatter = fmt,
              writer = new CountingScribeWriter,
              minimumLevel = Some(SLevel.Info),
              handle = h
            )
        )

    // --- Disabled level (TRACE call, INFO threshold) ------------------------

    @Benchmark def slf4j_disabled_naive(): Unit =
        n += 1
        slf4j.trace(s"disabled n=$n obj=$payload")

    @Benchmark def slf4j_disabled_guarded(): Unit =
        n += 1
        if slf4j.isTraceEnabled then slf4j.trace(s"disabled n=$n obj=$payload")

    @Benchmark def scribe_disabled_macro(): Unit =
        n += 1
        scribeSync.trace(s"disabled n=$n obj=$payload")

    @Benchmark def scribe_disabled_guarded(): Unit =
        n += 1
        if scribeSync.includes(SLevel.Trace) then scribeSync.trace(s"disabled n=$n obj=$payload")

    // --- Enabled, cheap constant --------------------------------------------

    @Benchmark def slf4j_enabledCheap(): Unit = slf4j.info("constant message")

    @Benchmark def scribe_enabledCheap(): Unit = scribeSync.info("constant message")

    // --- Enabled, interpolated ----------------------------------------------

    @Benchmark def slf4j_enabledExpensive(): Unit =
        n += 1
        slf4j.info(s"enabled n=$n obj=$payload")

    @Benchmark def scribe_enabledExpensive(): Unit =
        n += 1
        scribeSync.info(s"enabled n=$n obj=$payload")

    @Benchmark def scribe_enabledExpensive_msgOnly(): Unit =
        n += 1
        scribeMsgOnly.info(s"enabled n=$n obj=$payload")

    @Benchmark def scribe_enabledExpensive_defaultFmt(): Unit =
        n += 1
        scribeDefaultFmt.info(s"enabled n=$n obj=$payload")

    @Benchmark def scribe_enabledExpensive_async(): Unit =
        n += 1
        scribeAsync.info(s"enabled n=$n obj=$payload") // enqueue only; drops above ~1000/s

    @Benchmark def scribeCats_enabledExpensive(): Unit =
        n += 1
        scribeCats.info(s"enabled n=$n obj=$payload").unsafeRunSync()

    // --- ContraTracer overhead decomposition (enabled INFO) -----------------

    @Benchmark def contra_rawIO_baseline(): Unit =
        n += 1
        val m = s"enabled n=$n obj=$payload"
        IO { val _ = sink.addAndGet(m.length.toLong) }.unsafeRunSync()

    @Benchmark def contra_emit_overhead(): Unit =
        n += 1
        contraEmitSink
            .traceWith(LogEvent(HLevel.Info, s"enabled n=$n obj=$payload", routingKey = Some("x")))
            .unsafeRunSync()

    @Benchmark def contra_sink_full(): Unit =
        n += 1
        Slf4jTracer.sink
            .traceWith(
              LogEvent(HLevel.Info, s"enabled n=$n obj=$payload", routingKey = Some("bench.slf4j"))
            )
            .unsafeRunSync()

    // Batched: one `unsafeRunSync` amortized over `Batch` fresh events, so throughput reflects the
    // per-log cost of the sink (as it runs inside an actor's already-running IO), not the ~11µs
    // runloop-entry tax. The `traverse_` plumbing is identical across the three, so their deltas are
    // clean: (emit - rawIO) = ContraTracer arrow + Eval + LogEvent build; (sink - emit) = log4cats +
    // logback; (emit - emit_eager) = the Eval.later + Rendered deferral alone.
    private val Batch = 1000

    @Benchmark @OperationsPerInvocation(1000) def contra_rawIO_batch(): Unit =
        (0 until Batch).toList
            .traverse_(i => IO { val _ = sink.addAndGet(i.toLong & 0xff) })
            .unsafeRunSync()

    @Benchmark @OperationsPerInvocation(1000) def contra_emit_batch(): Unit =
        (0 until Batch).toList
            .traverse_(i =>
                contraEmitSink.traceWith(
                  LogEvent(HLevel.Info, s"enabled $i obj=$payload", routingKey = Some("x"))
                )
            )
            .unsafeRunSync()

    @Benchmark @OperationsPerInvocation(1000) def contra_emit_eager_batch(): Unit =
        (0 until Batch).toList
            .traverse_(i =>
                contraEmitEagerSink.traceWith(
                  EagerEvent(HLevel.Info, Some("x"), s"enabled $i obj=$payload")
                )
            )
            .unsafeRunSync()

    @Benchmark @OperationsPerInvocation(1000) def contra_sink_batch(): Unit =
        (0 until Batch).toList
            .traverse_(i =>
                Slf4jTracer.sink.traceWith(
                  LogEvent(
                    HLevel.Info,
                    s"enabled $i obj=$payload",
                    routingKey = Some("bench.slf4j")
                  )
                )
            )
            .unsafeRunSync()
