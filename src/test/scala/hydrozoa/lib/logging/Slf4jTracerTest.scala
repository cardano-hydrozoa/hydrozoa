package hydrozoa.lib.logging

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import java.util.concurrent.atomic.AtomicInteger
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.ListBuffer

/** The level gate: [[Slf4jTracer.sink]] forces a [[LogEvent]]'s deferred `render` — the message
  * interpolation and any `toString` inside it — only when the routing key's logger has the event's
  * level enabled. See `logback-test.xml` for the `hydrozoa.test.gated.*` fixtures.
  */
class Slf4jTracerTest extends AnyFunSuite:

    test("a disabled level forces no rendering; an enabled level does") {
        val renders = new AtomicInteger(0)
        def event(routingKey: String): LogEvent =
            LogEvent(
              Level.Debug,
              { val _ = renders.incrementAndGet(); "message" }, // by-name: runs only if forced
              routingKey = Some(routingKey)
            )

        // OFF in logback-test.xml -> isDebugEnabled == false -> the sink squelches, render not forced.
        Slf4jTracer.sink.traceWith(event("hydrozoa.test.gated.off")).unsafeRunSync()
        val afterOff = renders.get

        // DEBUG in logback-test.xml -> isDebugEnabled == true -> render forced exactly once.
        Slf4jTracer.sink.traceWith(event("hydrozoa.test.gated.on")).unsafeRunSync()
        val afterOn = renders.get

        assert(afterOff == 0 && afterOn == 1)
    }

    test("the gate wraps only the slf4j leg: a composed capture sink still fires") {
        val captured = ListBuffer.empty[Int]
        val capture: ContraTracer[IO, Int] = ContraTracer.emit(i => IO { val _ = captured += i })

        // The slf4j leg formats to a LogEvent at a disabled level; the capture leg is separate.
        val slf4jLeg: ContraTracer[IO, Int] =
            Slf4jTracer.sink.contramap(i =>
                LogEvent(Level.Debug, i.toString, routingKey = Some("hydrozoa.test.gated.off"))
            )

        (slf4jLeg |+| capture).traceWith(7).unsafeRunSync()
        assert(captured.toList == List(7)) // capture fired even though the slf4j leg was gated off
    }
