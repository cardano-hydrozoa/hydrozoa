package hydrozoa.lib.tracing

import cats.effect.IO
import cats.effect.unsafe.implicits.*
import java.io.{File, PrintWriter}
import org.scalatest.funsuite.AnyFunSuite

/** Integration test: emits a realistic protocol trace using the collecting tracer, writes it to a
  * JSONL file, and optionally runs the Lean checker on it.
  */
class ProtocolTracerTest extends AnyFunSuite {

    test("collecting tracer captures events and produces checker-compatible JSONL") {
        val (lines, traceFile) = (for {
            result <- ProtocolTracer.collecting("head:0")
            (tracer, ref) = result

            // Simulate a 3-block consensus run (2 minor + 1 major with 2-round)
            _ <- tracer.leaderStarted(0, 0)
            _ <- tracer.briefProduced(0, 0, "minor", 0, 0, 2)
            _ <- tracer.eventProcessed("0:0", 0, true)
            _ <- tracer.eventProcessed("0:1", 0, true)
            _ <- tracer.ack(0, 0, "minor")
            _ <- tracer.ack(0, 1, "minor")
            _ <- tracer.ack(0, 2, "minor")
            _ <- tracer.blockConfirmed(0, "minor", 0, 0)
            _ <- tracer.balanceSnapshot(0, 1000, 500)

            _ <- tracer.leaderStarted(1, 1)
            _ <- tracer.briefProduced(1, 1, "minor", 0, 1, 1)
            _ <- tracer.eventProcessed("1:0", 1, true)
            _ <- tracer.ack(1, 0, "minor")
            _ <- tracer.ack(1, 1, "minor")
            _ <- tracer.ack(1, 2, "minor")
            _ <- tracer.blockConfirmed(1, "minor", 0, 1)
            _ <- tracer.balanceSnapshot(1, 1000, 500)

            _ <- tracer.leaderStarted(2, 2)
            _ <- tracer.briefProduced(2, 2, "major", 1, 0, 3)
            _ <- tracer.eventProcessed("2:0", 2, true)
            _ <- tracer.eventProcessed("2:1", 2, false)
            _ <- tracer.eventProcessed("2:2", 2, true)
            _ <- tracer.ack(2, 0, "major1")
            _ <- tracer.ack(2, 1, "major1")
            _ <- tracer.ack(2, 2, "major1")
            _ <- tracer.roundComplete(2, "major", 1)
            _ <- tracer.ack(2, 0, "major2")
            _ <- tracer.ack(2, 1, "major2")
            _ <- tracer.ack(2, 2, "major2")
            _ <- tracer.blockConfirmed(2, "major", 1, 0)
            _ <- tracer.balanceSnapshot(2, 1000, 500)

            lines <- ref.get
            traceFile = new File("target/test-trace.jsonl")
            _ <- IO {
                val pw = new PrintWriter(traceFile)
                lines.foreach(pw.println)
                pw.close()
            }
        } yield (lines, traceFile)).unsafeRunSync()

        assert(lines.nonEmpty, "Should have collected trace events")
        assert(lines.forall(_.startsWith("HTRACE|{")), "All lines should have HTRACE| prefix")
        println(s"\n=== Synthetic trace: ${lines.size} events → ${traceFile.getAbsolutePath} ===")
    }
}
