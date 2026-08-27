package hydrozoa.benchmarks

import ch.qos.logback.classic.encoder.PatternLayoutEncoder
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{AsyncAppender, Level as LbLevel, LoggerContext}
import ch.qos.logback.core.FileAppender
import java.nio.file.{Files, Path}
// Harness drives logback directly to a temp file; it needs the SLF4J-bound global context.
import org.slf4j.LoggerFactory // scalafix:ok DisableSyntax
import scribe.file.*
import scribe.format.*
import scribe.handler.AsynchronousLogHandle
import scribe.{Level as SLevel, Logger as SLogger}

/** End-to-end file-writer load test: SLF4J/Logback vs scribe, synchronous vs asynchronous, under a
  * burst that outruns the writers. Unlike the JMH microbench (`LoggingBackendBench`, which measures
  * caller-side cost to a discarding sink), this measures **delivery**: fire `total` INFO lines from
  * `threads` producers as fast as possible, let the async drains settle, then count how many lines
  * actually reached the file. `dropped = total - delivered`.
  *
  * The point is the async trade-off. Both async paths use a **bounded 1000-slot buffer** and drop
  * on overflow, matching their real defaults:
  *   - scribe `AsynchronousLogHandle`: maxBuffer=1000, `Overflow.DropOld`, drain loop flushes ONE
  *     record then `Thread.sleep(1)` (~1000 rec/s ceiling).
  *   - logback `AsyncAppender`: queueSize=1000, `neverBlock=true` (hydrozoa's config),
  *     discardingThreshold=0; its worker drains as fast as the `FileAppender` can write.
  * The sync variants are the lossless baselines — they deliver 100% and their produce time is the
  * real sustainable write throughput.
  *
  * Run: `sbt "benchmark/runMain hydrozoa.benchmarks.LoggingLoadTest [total] [threads]"` (defaults:
  * 500000 lines, 8 threads).
  */
object LoggingLoadTest:

    private val leanFmt: Formatter = formatter"$date $levelPaddedRight [$threadName] $messages"
    private val Buffer = 1000
    private val DrainWindowMs = 3000L // generous settle time for async drains before counting

    final case class Result(
        name: String,
        produced: Long,
        delivered: Long,
        produceMs: Double,
        drainMs: Double
    ):
        def dropped: Long = produced - delivered
        def dropPct: Double = 100.0 * dropped / produced
        def producerRate: Double = produced / (produceMs / 1000.0) // lines/s the producer sustained

    def main(args: Array[String]): Unit =
        val total = args.lift(0).map(_.toLong).getOrElse(500000L)
        val threads = args.lift(1).map(_.toInt).getOrElse(8)
        println(
          s"File-writer load test: total=$total lines, producers=$threads, async buffer=$Buffer, drain window=${DrainWindowMs}ms\n"
        )

        // Warm the JIT (discarded) so the first real config isn't penalised.
        val _ = runScribe("warm", async = false, 20000, threads)
        val _ = runLogback("warm", async = false, 20000, threads)

        val results = List(
          runScribe("scribe   sync ", async = false, total, threads),
          runScribe("scribe   async", async = true, total, threads),
          runLogback("logback  sync ", async = false, total, threads),
          runLogback("logback  async", async = true, total, threads)
        )
        printTable(results)

    // --- producers ----------------------------------------------------------

    private def timeProduce(threads: Int, total: Long)(emit: Long => Unit): Double =
        val per = total / threads
        val ths = (0 until threads).map { t =>
            new Thread(() =>
                var i = 0L
                val base = t.toLong * per
                while i < per do { emit(base + i); i += 1 }
            )
        }
        val start = System.nanoTime()
        ths.foreach(_.start())
        ths.foreach(_.join())
        (System.nanoTime() - start) / 1.0e6

    private def line(i: Long): String = s"consensus block $i soft-confirmed by peer ${i & 7}"

    private def countLines(path: Path): Long =
        val s = Files.lines(path)
        try s.count()
        finally s.close()

    private def measure(name: String, async: Boolean, total: Long, threads: Int, path: Path)(
        emit: Long => Unit,
        close: () => Unit
    ): Result =
        val produceMs = timeProduce(threads, total)(emit)
        val drainStart = System.nanoTime()
        if async then Thread.sleep(DrainWindowMs)
        close()
        val drainMs = (System.nanoTime() - drainStart) / 1.0e6
        val delivered = countLines(path)
        Files.deleteIfExists(path)
        Result(name, total, delivered, produceMs, drainMs)

    // --- scribe -------------------------------------------------------------

    private def runScribe(name: String, async: Boolean, total: Long, threads: Int): Result =
        val path = Files.createTempFile("loadtest-scribe-", ".log")
        val fw = FileWriter(path2PathBuilder(path))
        val base = SLogger.empty.orphan()
        val logger =
            if async then
                base.withHandler(
                  formatter = leanFmt,
                  writer = fw,
                  minimumLevel = Some(SLevel.Info),
                  handle = AsynchronousLogHandle(maxBuffer = Buffer)
                )
            else
                base.withHandler(formatter = leanFmt, writer = fw, minimumLevel = Some(SLevel.Info))
        measure(name, async, total, threads, path)(
          i => { logger.info(line(i)); () },
          () => fw.dispose()
        )

    // --- logback ------------------------------------------------------------

    private def runLogback(name: String, async: Boolean, total: Long, threads: Int): Result =
        val ctx = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
        val path = Files.createTempFile("loadtest-logback-", ".log")
        val enc = new PatternLayoutEncoder()
        enc.setContext(ctx)
        enc.setPattern("%d{HH:mm:ss.SSS} %-5level [%thread] %logger - %msg%n")
        enc.start()
        val fa = new FileAppender[ILoggingEvent]()
        fa.setContext(ctx)
        fa.setFile(path.toString)
        fa.setEncoder(enc)
        fa.setImmediateFlush(false) // buffered, comparable to scribe's async flush; stop() flushes
        fa.setAppend(false)
        fa.start()
        val appender =
            if async then
                val a = new AsyncAppender()
                a.setContext(ctx)
                a.setQueueSize(Buffer)
                a.setNeverBlock(true) // drop instead of block on a full queue (hydrozoa's config)
                a.setDiscardingThreshold(
                  0
                ) // don't discard by level; only neverBlock full-queue drops
                a.addAppender(fa)
                a.start()
                a
            else fa
        val lg = ctx.getLogger(s"loadtest.$name.${System.identityHashCode(path)}")
        lg.setAdditive(false)
        lg.detachAndStopAllAppenders()
        lg.addAppender(appender)
        lg.setLevel(LbLevel.INFO)
        val close = () =>
            appender
                .stop() // AsyncAppender drains its queue to the FileAppender within maxFlushTime
            if async then fa.stop() // then flush + close the file stream
        measure(name, async, total, threads, path)(i => lg.info(line(i)), close)

    // --- report -------------------------------------------------------------

    private def printTable(rs: List[Result]): Unit =
        println(
          f"${"config"}%-15s ${"produced"}%10s ${"delivered"}%10s ${"dropped"}%10s ${"drop%"}%7s ${"producer lines/s"}%18s ${"drainMs"}%9s"
        )
        println("-" * 90)
        rs.foreach { r =>
            println(
              f"${r.name}%-15s ${r.produced}%10d ${r.delivered}%10d ${r.dropped}%10d ${r.dropPct}%6.2f%% ${r.producerRate}%18.0f ${r.drainMs}%9.0f"
            )
        }
