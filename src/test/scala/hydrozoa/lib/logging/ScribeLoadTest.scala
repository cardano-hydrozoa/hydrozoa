package hydrozoa.lib.logging

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import java.util.concurrent.atomic.AtomicLong
import org.scalatest.funsuite.AnyFunSuite
import scribe.handler.{AsynchronousLogHandle, Overflow}
import scribe.output.LogOutput
import scribe.output.format.OutputFormat
import scribe.writer.Writer
import scribe.{Level as SLevel, LogRecord, Logger as SLogger}

/** PoC load test (spike — `design/logging-scribe.md`): does scribe's single-drainer async handle
  * keep consensus-rate log traffic off the producing fibers? Ignored by default (heavy,
  * timing-based); un-ignore and run `sbt "testOnly *ScribeLoadTest"` to collect numbers.
  *
  * The go/no-go gate: with `Overflow.DropNew` + a bounded buffer, emitting through
  * [[ScribeTracer.sink]] from many CE fibers must (a) never block the producer (bounded worst-case
  * single-emit latency), (b) drop cleanly at the buffer bound rather than grow unboundedly.
  */
class ScribeLoadTest extends AnyFunSuite:

    private val fibers = 64
    private val perFiber = 20000
    private val total = fibers.toLong * perFiber
    private val maxBuffer = 8192

    ignore("scribe async handle: producers do not stall under a consensus-rate burst") {
        val written = new AtomicLong(0)
        val maxEmitNanos = new AtomicLong(0)

        // Count what the drain thread actually writes (so total - written = drops), and discard the
        // real output so we measure the queue mechanism, not disk I/O.
        val countingWriter = new Writer:
            def write(record: LogRecord, output: LogOutput, outputFormat: OutputFormat): Unit =
                val _ = written.incrementAndGet()

        SLogger("poc.load")
            .orphan()
            .withHandler(
              writer = countingWriter,
              minimumLevel = Some(SLevel.Trace),
              handle = AsynchronousLogHandle(maxBuffer = maxBuffer, overflow = Overflow.DropNew)
            )
            .replace()

        // Each event forces a small render (an interpolation) at an ENABLED level, so it flows
        // through the async queue — the realistic hot path.
        def emitOne(i: Int): IO[Unit] =
            val ev = LogEvent(Level.Trace, s"poc load event $i", routingKey = Some("poc.load"))
            ScribeTracer.sink.traceWith(ev).timed.flatMap { (d, _) =>
                IO {
                    val dt =
                        d.toNanos // keep the running max single-emit latency (never-block proof)
                    var prev = maxEmitNanos.get()
                    while dt > prev && !maxEmitNanos.compareAndSet(prev, dt) do
                        prev = maxEmitNanos.get()
                }
            }

        val start = System.nanoTime()
        val run = (0 until fibers).toList.parTraverse_ { _ =>
            (0 until perFiber).toList.traverse_(emitOne)
        }
        run.unsafeRunSync()
        val elapsedMs = (System.nanoTime() - start) / 1000000.0

        // Give the single drain thread a moment to flush the tail before reading counts.
        IO.sleep(scala.concurrent.duration.DurationInt(500).millis).unsafeRunSync()

        val wrote = written.get()
        val drops = total - wrote
        val worstEmitUs = maxEmitNanos.get() / 1000.0
        info(
          f"scribe async: total=$total wrote=$wrote drops=$drops (${drops * 100.0 / total}%.1f%%)"
        )
        info(
          f"scribe async: elapsed=${elapsedMs}%.0fms throughput=${total / (elapsedMs / 1000)}%.0f ev/s"
        )
        info(f"scribe async: worst single-emit=${worstEmitUs}%.1fus (buffer=$maxBuffer, DropNew)")

        // Producers must never stall (worst single emit bounded) and the buffer must bound memory
        // (drops are fine, unbounded growth is not).
        assert(
          worstEmitUs < 50000.0 && wrote <= total,
          s"worst single emit ${worstEmitUs}us / wrote $wrote of $total"
        )
    }
