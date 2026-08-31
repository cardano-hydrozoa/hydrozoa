package hydrozoa.multisig.consensus.limiter

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.actor.ActorSystem
import hydrozoa.config.head.parameters.RateLimits
import hydrozoa.lib.logging.ContraTracer
import io.circe.parser.decode
import java.time.Instant
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.{DurationInt, DurationLong, FiniteDuration}

/** Every message on the test lane carries the SAME, ancient timestamp.
  *
  * That is the production defect in its strongest form. The block lane's `limiterTimestamp` is a
  * block's end time, quantized to a one-second Cardano slot, so every block produced inside one
  * second really did share a single gate — and the old limiter, which computed its gate as
  * `limiterTimestamp + minPeriod`, let all but the first through with no hold at all. Fixing that
  * timestamp to `EPOCH` here means the gate can only come from the limiter's memory of its own last
  * release. A limiter that consults the message cannot pass these tests.
  */
private sealed trait LaneMsg extends LimiterTimestamp:
    def id: Int
    override def limiterTimestamp: Instant = Instant.EPOCH
    override def minPeriod(using cfg: RateLimits.Section): FiniteDuration = cfg.softBlockMinPeriod

private final case class Paced(id: Int) extends LaneMsg

private final case class Exempt(id: Int) extends LaneMsg:
    override def limiterExempt: Boolean = true

/** Records what reached the downstream, and when, on the same monotonic clock the limiter uses. */
private final case class Recorder(seen: Ref[IO, Vector[(Int, Long)]]) extends Actor[IO, LaneMsg]:
    override def receive: Receive[IO, LaneMsg] = PartialFunction.fromFunction { m =>
        IO.monotonic.map(_.toMillis).flatMap(t => seen.update(_ :+ (m.id -> t)))
    }

/** The block lane's 100 ms period was inert in production: the head produced 30 blocks/s against a
  * 10 Hz config, and at low load exactly 1.00 requests per block — one block per request, which is
  * the degenerate case the shaper exists to prevent.
  *
  * Two defects caused it, and gating on the limiter's own last release fixes both at once. These
  * tests pin the fixed behaviour, and each rate assertion is paired with a control that must NOT
  * space, so a suite that silently stopped measuring anything would fail rather than pass.
  */
class LimiterSpacingTest extends AnyFunSuite:

    private val PeriodMs = 200L

    private def limits(
        softBlockMinPeriod: FiniteDuration = PeriodMs.millis,
        hardStackMinPeriod: FiniteDuration = 30.seconds
    ): RateLimits =
        RateLimits.default.copy(
          softBlockMinPeriod = softBlockMinPeriod,
          hardStackMinPeriod = hardStackMinPeriod
        )

    // ---- the spacing gate ---------------------------------------------------------------------

    test("messages sharing one timestamp are still released a full period apart") {
        val seen = runLane(limits(), gate = None, settle = 1500.millis) { lim =>
            (0 until 5).toList.traverse_(i => (lim ! Paced(i)) >> IO.sleep(10.millis))
        }
        val _ = assert(seen.size == 5, s"expected all 5 messages through, got ${seen.size}: $seen")
        assertSpacing(seen, atLeastMs = (PeriodMs * 0.8).toLong)
        assertOrdered(seen)
    }

    // Control for the test above. Same injection, same assertions machinery, but a period short
    // enough that spacing cannot show up -- so if the limiter ever stopped holding anything, the
    // test above would fail while this one still passes. Without this pair, "gaps are >= 160ms"
    // could be satisfied by a limiter that simply delivered slowly for some unrelated reason.
    test("control: with a negligible period the same injection is NOT spaced") {
        val seen = runLane(limits(softBlockMinPeriod = 1.milli), gate = None, settle = 500.millis) {
            lim => (0 until 5).toList.traverse_(i => (lim ! Paced(i)) >> IO.sleep(10.millis))
        }
        val _ = assert(seen.size == 5, s"expected all 5 through, got $seen")
        val span = seen.last._2 - seen.head._2
        assert(span < 150L, s"expected no spacing with a 1ms period, but the span was ${span}ms")
    }

    // ⛔ The regression that matters most for the shared implementation. The stack lane uses the
    // same Limiter class with NO gate, and nothing ever sends it a drain signal. If the gate's
    // counting were unconditional rather than opt-in, its residual would climb forever and stretch
    // hardStackMinPeriod without bound -- silently, and only in integration. Here the period must
    // stay put across many releases with no drain signal ever sent.
    test("a lane with no gate holds its exact period and never stretches") {
        val seen = runLane(limits(), gate = None, settle = 2500.millis) { lim =>
            (0 until 8).toList.traverse_(i => lim ! Paced(i))
        }
        val _ = assert(seen.size == 8, s"expected 8 through, got ${seen.size}")
        assertSpacing(seen, atLeastMs = (PeriodMs * 0.8).toLong)
        assertNoStretch(seen, atMostMs = (PeriodMs * 1.6).toLong)
    }

    test("a gated lane does not stretch either until a drain signal arrives") {
        val gate = Some(tightGate)
        val seen = runLane(limits(), gate = gate, settle = 2500.millis) { lim =>
            (0 until 8).toList.traverse_(i => lim ! Paced(i))
        }
        val _ = assert(seen.size == 8, s"expected 8 through, got ${seen.size}")
        // The multiplier is re-derived only on a drain signal, so with none sent it stays 1.0 even
        // though the backlog is well past this gate's hard limit. That constancy between signals is
        // what stops the controller sawtoothing at the downstream cadence.
        assertNoStretch(seen, atMostMs = (PeriodMs * 1.6).toLong)
    }

    // ---- the backlog gate ---------------------------------------------------------------------

    test("a drain signal carrying a large backlog stretches the period") {
        val seen = runLane(limits(), gate = Some(tightGate), settle = 3000.millis) { lim =>
            for {
                // Three releases with this gate's hard limit at 3 puts the filtered residual at the
                // bottom of the ramp, so the multiplier lands on the floor.
                _ <- (0 until 3).toList.traverse_(i =>
                    (lim ! Paced(i)) >> IO.sleep(PeriodMs.millis)
                )
                _ <- lim ! LimiterControl.DownstreamDrained
                _ <- lim ! Paced(100)
                _ <- lim ! Paced(101)
            } yield ()
        }
        val tail = seen.filter(_._1 >= 100)
        val _ = assert(tail.size == 2, s"expected both post-drain messages, got $seen")
        val gap = tail(1)._2 - tail(0)._2
        // floor 0.25 => the period is stretched 4x.
        val expected = (PeriodMs * 4 * 0.8).toLong
        assert(
          gap >= expected,
          s"expected the gate to stretch the period to >=${expected}ms, got ${gap}ms"
        )
    }

    // ---- exemption ----------------------------------------------------------------------------

    test("an exempt message passes straight through and does not restart the spacing clock") {
        val seen =
            runLane(limits(softBlockMinPeriod = 500.millis), gate = None, settle = 2.seconds) {
                lim => (lim ! Paced(0)) >> (lim ! Exempt(1)) >> (lim ! Paced(2))
            }
        val at = seen.toMap
        val _ = assert(at.keySet == Set(0, 1, 2), s"expected all three through, got $seen")
        val _ = assert(
          at(1) - at(0) < 100L,
          s"the exempt message was held for ${at(1) - at(0)}ms; it must not be paced"
        )
        // If the exempt release had reset `lastReleaseMs`, this gap would be measured from the
        // exempt message instead and the schedule would drift by one message every time.
        assert(
          at(2) - at(0) >= 400L,
          s"expected the paced message to stay 500ms behind its predecessor, got ${at(2) - at(0)}ms"
        )
    }

    // ---- failing loudly ------------------------------------------------------------------------

    // ⛔ Do not make the limiter "fail open" by overriding `preRestart` to forward whatever it is
    // holding. `MultisigRegimeManagerBase` maps every supervisor directive to `Escalate` ("normally
    // `Restart` but our actors can't do that yet"), so this actor never restarts and such an
    // override is unreachable — it would claim a safety property the system does not have.
    //
    // What protects the lane is the property below: a failure must NOT be absorbed. The lane is
    // self-clocked, so a limiter that swallowed an exception and carried on with a message stuck in
    // its queue would leave upstream waiting forever for a release that is never coming — a wedge
    // with nothing in the log. Escalating turns that into a loud crash that systemd restarts and
    // that recovers from persistence. If someone ever wraps this actor's body in `.attempt`, this
    // test is what should fail.
    test("a failure inside the limiter is escalated, not swallowed and carried on from") {
        val period = 500.millis
        val seen = ActorSystem[IO]("limiter-failure")
            .use(system =>
                for {
                    got <- Ref.of[IO, Vector[(Int, Long)]](Vector.empty)
                    sink <- system.actorOf(Recorder(got))
                    lim <- system.actorOf(
                      Limiter[LaneMsg](sink, limits(softBlockMinPeriod = period), throwOnHold)
                    )
                    _ <- lim ! Paced(0) // released immediately; nothing is held yet
                    _ <- lim ! Paced(1) // held -> the injected failure fires
                    _ <- IO.sleep(300.millis)
                    _ <- (lim ! Paced(2)).attempt // must not be served by a limiter that carried on
                    _ <- IO.sleep(1500.millis)
                    r <- got.get
                } yield r
            )
            .attempt
            .unsafeRunSync()
            .getOrElse(Vector.empty)
        val delivered = seen.map(_._1).toSet
        val _ = assert(
          delivered.contains(0),
          s"the pre-failure message should have been delivered: $seen"
        )
        assert(
          !delivered.contains(2),
          s"the limiter kept serving the lane after failing; the failure was swallowed: $seen"
        )
    }

    // ---- the gate's arithmetic (no actor system) ----------------------------------------------

    private val curve = LimiterGate(
      backlogSoftLimit = 0,
      backlogHardLimit = 1000,
      floor = 0.01,
      smoothing = 0.5,
      slice = 150.millis
    )

    test(
      "the multiplier is 1 at or below the soft limit and the floor at or above the hard limit"
    ) {
        val _ = assert(curve.multiplier(0.0) === 1.0)
        val _ = assert(curve.multiplier(-5.0) === 1.0)
        val _ = assert(curve.multiplier(1000.0) === curve.floor)
        assert(curve.multiplier(5000.0) === curve.floor)
    }

    test("the multiplier decreases monotonically across the ramp") {
        val ms = (0 to 1000 by 50).map(b => curve.multiplier(b.toDouble))
        assert(
          ms.sliding(2).forall(w => w(0) >= w(1)),
          s"multiplier is not monotonically decreasing: $ms"
        )
    }

    // The design decision this pins: gain belongs where the backlog is dangerous, not where the
    // system is fine. The loop's dead time is a whole stack cycle plus confirmation latency, and
    // high gain across a long dead time is how a controller ends up in a limit cycle. The obvious
    // alternative curve, h^2, has exactly the opposite curvature and would fail this test.
    test("the ramp is flat where healthy and steepens as headroom runs out") {
        val early = curve.multiplier(250.0) - curve.multiplier(500.0)
        val late = curve.multiplier(500.0) - curve.multiplier(750.0)
        assert(late > early, s"expected the ramp to steepen; early drop $early, late drop $late")
    }

    test("a drain signal folds the cycle's count into the filter and resets the live count") {
        import LimiterGate.observeDrain
        val one = curve.observeDrain(LimiterGate.Open.copy(released = 100L))
        val _ = assert(
          one.residual === 50.0,
          s"expected a half-weighted first sample, got ${one.residual}"
        )
        val _ = assert(one.released == 0L, "the live count must reset on a drain signal")
        val _ = assert(one.drains == 1L)
        val two = curve.observeDrain(one.copy(released = 100L))
        assert(two.residual === 75.0, s"expected the filter to converge, got ${two.residual}")
    }

    // ---- config ------------------------------------------------------------------------------

    // A node whose config fails to decode does not start at all, and every private.json already
    // deployed predates the gate fields. This is what makes the deploy a binary swap with no
    // config edit.
    test("a rateLimits block written before the gate existed still decodes, with gate defaults") {
        val legacy = """{"softBlockMinPeriod":100,"hardStackMinPeriod":30000}"""
        decode[RateLimits](legacy) match {
            case Left(e) => fail(s"a pre-gate rateLimits block must still decode: $e")
            case Right(r) =>
                val _ = assert(r.softBlockMinPeriod == 100.millis)
                val _ = assert(r.hardStackMinPeriod == 30.seconds)
                val _ = assert(r.blockBacklogSoftLimit == RateLimits.defaultBlockBacklogSoftLimit)
                val _ = assert(r.blockBacklogHardLimit == RateLimits.defaultBlockBacklogHardLimit)
                assert(r.blockGateFloor == RateLimits.defaultBlockGateFloor)
        }
    }

    // ---- harness -----------------------------------------------------------------------------

    /** Fails the limiter from inside a hold, and only from there, so the crash lands at the one
      * moment it is holding a message. `preRestart`'s own trace is deliberately `attempt`ed in the
      * limiter, so a tracer this hostile still cannot stop the flush.
      */
    private def throwOnHold: ContraTracer[IO, LimiterEvent] =
        ContraTracer[IO, LimiterEvent] {
            case _: LimiterEvent.HoldingMsg =>
                IO.raiseError(new RuntimeException("injected failure inside the hold"))
            case _ => IO.unit
        }

    /** A gate small enough to reach its floor after a handful of releases, so the dynamics are
      * testable in seconds rather than in a 30-second stack cycle. `smoothing = 1.0` takes the
      * newest cycle unfiltered, which makes the arithmetic exact.
      */
    private def tightGate: LimiterGate = LimiterGate(
      backlogSoftLimit = 1,
      backlogHardLimit = 3,
      floor = 0.25,
      smoothing = 1.0,
      slice = 50.millis
    )

    private def runLane(
        rateLimits: RateLimits,
        gate: Option[LimiterGate],
        settle: FiniteDuration
    )(send: ActorRef[IO, LaneMsg | LimiterControl] => IO[Unit]): Vector[(Int, Long)] =
        ActorSystem[IO]("limiter-test")
            .use(system =>
                for {
                    seen <- Ref.of[IO, Vector[(Int, Long)]](Vector.empty)
                    sink <- system.actorOf(Recorder(seen))
                    lim <- system.actorOf(
                      Limiter[LaneMsg](
                        sink,
                        rateLimits,
                        ContraTracer.nullTracer[IO, LimiterEvent],
                        gate
                      )
                    )
                    _ <- send(lim)
                    _ <- IO.sleep(settle)
                    r <- seen.get
                } yield r
            )
            .unsafeRunSync()

    private def assertSpacing(seen: Vector[(Int, Long)], atLeastMs: Long): Unit =
        val gaps = seen.map(_._2).sliding(2).map(w => w(1) - w(0)).toList
        val _ = assert(
          gaps.forall(_ >= atLeastMs),
          s"expected every gap >= ${atLeastMs}ms, got $gaps"
        )

    private def assertNoStretch(seen: Vector[(Int, Long)], atMostMs: Long): Unit =
        val gaps = seen.map(_._2).sliding(2).map(w => w(1) - w(0)).toList
        val _ = assert(
          gaps.forall(_ <= atMostMs),
          s"a gap exceeded ${atMostMs}ms, so the period is stretching: $gaps"
        )

    private def assertOrdered(seen: Vector[(Int, Long)]): Unit =
        val _ = assert(seen.map(_._1) == seen.map(_._1).sorted, s"messages were reordered: $seen")
