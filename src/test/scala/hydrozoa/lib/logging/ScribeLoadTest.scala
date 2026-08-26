package hydrozoa.lib.logging

import cats.effect.IO
import cats.effect.std.{CountDownLatch, Queue}
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import java.util.concurrent.atomic.AtomicLong
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.*
import scribe.handler.{AsynchronousLogHandle, Overflow}
import scribe.output.LogOutput
import scribe.output.format.OutputFormat
import scribe.writer.Writer
import scribe.{Level as SLevel, LogRecord, Logger as SLogger}

/** Load test (spike — `design/logging-scribe.md`): under a burst from many fibers, how do the three
  * scribe wirings behave? A counting no-op [[Writer]] stands in for the appender so we measure the
  * framework/queue mechanism, not disk I/O.
  *
  *   - `synchronous` — scribe's default handle, and exactly what [[ScribeTracer.sink]] drives (`IO
  *     { logger.trace(msg) }`). Lossless; the actual migration path.
  *   - `ce-async` — that same sink behind a bounded cats-effect `Queue` drained by one consumer
  *     fiber. Lossless with backpressure, and keeps the write off the producing fibers — the
  *     idiomatic replacement for Logback's `AsyncAppender`.
  *   - `scribe-async` — scribe's own `AsynchronousLogHandle`. Its drain loop writes one record then
  *     `Thread.sleep(1)` (source-verified ~1000 rec/s ceiling), so under a burst it drops the vast
  *     majority with `DropNew`. Ignored by default; kept only to document why NOT to use it.
  */
class ScribeLoadTest extends AnyFunSuite:

    private val fibers = 32
    private val perFiber = 10000
    private val total = fibers.toLong * perFiber

    private def countingWriter(counter: AtomicLong): Writer = new Writer:
        def write(record: LogRecord, output: LogOutput, outputFormat: OutputFormat): Unit =
            val _ = counter.incrementAndGet()

    private def configure(
        name: String,
        counter: AtomicLong,
        handle: Option[AsynchronousLogHandle]
    ): Unit =
        val base = SLogger(name).orphan()
        val withHandle = handle
            .fold(
              base.withHandler(writer = countingWriter(counter), minimumLevel = Some(SLevel.Trace))
            )(h =>
                base.withHandler(
                  writer = countingWriter(counter),
                  minimumLevel = Some(SLevel.Trace),
                  handle = h
                )
            )
        val _ = withHandle.replace()

    private def evAt(name: String)(i: Int): LogEvent =
        LogEvent(Level.Trace, s"poc load event $i", routingKey = Some(name))

    private def burst(emit: Int => IO[Unit]): IO[Unit] =
        (0 until fibers).toList.parTraverse_(_ => (0 until perFiber).toList.traverse_(emit))

    test("synchronous handle (the sink's real path) never drops") {
        val written = new AtomicLong(0)
        configure("poc.sync", written, None)

        val start = System.nanoTime()
        burst(i => ScribeTracer.sink.traceWith(evAt("poc.sync")(i))).unsafeRunSync()
        val elapsedMs = (System.nanoTime() - start) / 1000000.0

        val wrote = written.get()
        info(
          f"sync:     total=$total wrote=$wrote drops=${total - wrote} " +
              f"throughput=${total / (elapsedMs / 1000)}%.0f ev/s"
        )
        assert(wrote == total, s"synchronous logging dropped ${total - wrote} of $total")
    }

    test("cats-effect async (bounded queue + one consumer) never drops, off the producer") {
        val written = new AtomicLong(0)
        configure("poc.ceasync", written, None)

        val prog =
            for
                q <- Queue.bounded[IO, LogEvent](8192)
                latch <- CountDownLatch[IO](total.toInt)
                consumer <- q.take
                    .flatMap(ev => ScribeTracer.sink.traceWith(ev) *> latch.release)
                    .foreverM
                    .start
                start = System.nanoTime()
                _ <- burst(i => q.offer(evAt("poc.ceasync")(i)))
                _ <- latch.await.timeout(60.seconds)
                elapsedMs = (System.nanoTime() - start) / 1000000.0
                _ <- consumer.cancel
            yield elapsedMs
        val elapsedMs = prog.unsafeRunSync()

        val wrote = written.get()
        info(
          f"ce-async: total=$total wrote=$wrote drops=${total - wrote} " +
              f"throughput=${total / (elapsedMs / 1000)}%.0f ev/s"
        )
        assert(wrote == total, s"ce-async dropped ${total - wrote} of $total")
    }

    ignore("scribe AsynchronousLogHandle drops the majority under a burst — do not use") {
        val written = new AtomicLong(0)
        configure(
          "poc.scribeasync",
          written,
          Some(AsynchronousLogHandle(maxBuffer = 8192, overflow = Overflow.DropNew))
        )

        val start = System.nanoTime()
        burst(i => ScribeTracer.sink.traceWith(evAt("poc.scribeasync")(i))).unsafeRunSync()
        val elapsedMs = (System.nanoTime() - start) / 1000000.0
        IO.sleep(500.millis).unsafeRunSync() // let the single drain thread flush its tail

        val wrote = written.get()
        // Report writes/drops, not a producer "throughput": with DropNew the producer never blocks,
        // so a rate here would just measure how fast events are discarded. The drain ceiling is
        // ~1000 rec/s (one record per `Thread.sleep(1)`), so `wrote` ≈ elapsed-in-seconds × 1000.
        info(
          f"scribe-async: total=$total wrote=$wrote drops=${total - wrote} " +
              f"(${(total - wrote) * 100.0 / total}%.1f%%) — drain ceiling ~1000 rec/s"
        )
        assert(wrote < total / 10, s"expected heavy drops, wrote $wrote of $total")
    }
