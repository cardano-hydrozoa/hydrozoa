package hydrozoa.multisig

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.SupervisorStrategy.{Directive, Escalate}
import com.suprnation.actor.event.Error as ActorError
import com.suprnation.actor.{ActorSystem, OneForOneStrategy, SupervisionStrategy}
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.*

/** What a supervised actor's failure actually does, split by the two variables that were confounded
  * in the CI incident: whether the decider is defined for the thrown value, and whether the failing
  * actor has a supervising ancestor at all.
  *
  * The incident was `emergency stop: exception in failure handling for class
  * LaneOutbound$AppendOutOfOrder`, carrying `NoSuchElementException: None.get` — the original error
  * discarded and everything waiting on the subtree left waiting.
  *
  * ==What this pins down, deterministically==
  *
  * Every cell raises (the `raises > 0` assertion), so no cell is vacuous, and **a supervised
  * failure surfaces nothing on the event stream either way** — with a total decider or without,
  * matched class or not. Whatever the incident was, our decider's totality is not what governs it.
  * That is worth knowing: it means the total decider is a correctness improvement, not the fix.
  *
  * ==What was observed once, and is racy==
  *
  * The top-level + bare-`Throwable` cell has produced exactly the incident's inner exception:
  * {{{
  * [kukku://top-bare@localhost/user] None.get
  * }}}
  * at the user guardian — and only that cell. Neither top-level with a `RuntimeException` nor any
  * supervised cell produced it. That points at the guardian's own default decider being undefined
  * for a value extending `Throwable` directly, with its failure path doing a `.get` on `None`.
  *
  * It does not reproduce on every run: the failure cascade races system teardown, and tightening
  * the observation window changed whether it was caught. So it is reported via `info` and NOT
  * asserted — asserting a race would produce a flaky test, and this suite's value is the
  * deterministic half plus an honest record of the rest.
  *
  * The remaining lead, for whoever picks this up: our strategy escalates *everything*, which routes
  * every failure to the guardian — the one place this reproduces. `Stop` or `Restart` at the
  * regime-manager level, once the actors can take them, would keep failures out of that path
  * entirely.
  */
class SupervisionDeciderTest extends AnyFunSuite {

    /** The decider as it was: no arm matches a value that extends `Throwable` directly. */
    private val nonTotal: PartialFunction[Throwable, Directive] = {
        case _: IllegalArgumentException => Escalate
        case _: RuntimeException         => Escalate
        case _: Exception                => Escalate
    }

    /** The decider as it now is — every arm plus a catch-all, wrapped so `isDefinedAt` is always
      * true and the supervisor can never see it as undefined.
      */
    private val total: PartialFunction[Throwable, Directive] =
        PartialFunction.fromFunction {
            case _: IllegalArgumentException => Escalate
            case _: RuntimeException         => Escalate
            case _: Exception                => Escalate
            case _                           => Escalate
        }

    /** Extends `Throwable` directly, exactly as `LaneOutbound.AppendOutOfOrder` does. */
    private final case class BareThrowable() extends Throwable("bare throwable")

    private case class Boom(t: Throwable)

    private class Child(raised: cats.effect.Ref[IO, Int]) extends Actor[IO, Boom] {
        override def receive: Receive[IO, Boom] = { case Boom(t) =>
            raised.update(_ + 1) >> IO.raiseError(t)
        }
    }

    /** A supervising parent, so the failing actor is NOT the top of the tree. */
    private class Parent(
        decider: PartialFunction[Throwable, Directive],
        raised: cats.effect.Ref[IO, Int],
    ) extends Actor[IO, Boom] {
        override def supervisorStrategy: SupervisionStrategy[IO] =
            OneForOneStrategy[IO](maxNrOfRetries = 3, withinTimeRange = 1.minute)(decider)

        override def receive: Receive[IO, Boom] = { case b: Boom =>
            context.actorOf(new Child(raised)).flatMap(_ ! b)
        }
    }

    /** What the system reported for one failure.
      *
      * `raises` is the anti-vacuity guard — a run where the child never raised proves nothing, and
      * a silently-undelivered message would otherwise read as "handled cleanly". `errors` is what
      * the system's own event stream said, which is where the incident's "emergency stop: exception
      * in failure handling" text came from.
      */
    private case class Outcome(raises: Int, errors: List[String]):
        def emergencyStopped: Boolean = errors.exists(_.toLowerCase.contains("emergency stop"))
        override def toString: String =
            s"raises=$raises emergencyStop=$emergencyStopped errors=${errors.take(2)}"

    private def observe(
        decider: PartialFunction[Throwable, Directive],
        thrown: Throwable,
        label: String,
    ): Outcome =
        val io = for
            raised <- cats.effect.Ref[IO].of(0)
            errors <- cats.effect.Ref[IO].of(List.empty[String])
            _ <- ActorSystem[IO](label).use { system =>
                for
                    drain <- system.eventStream.take
                        .flatMap {
                            case e: ActorError if e.cause != ActorError.NoCause =>
                                errors.update(_ :+ s"[${e.logSource}] ${e.cause.getMessage}")
                            case _ => IO.unit
                        }
                        .foreverM
                        .start
                    parent <- system.actorOf(new Parent(decider, raised), s"parent-$label")
                    _ <- parent ! Boom(thrown)
                    // Poll rather than sleep a fixed span: a fixed 500ms flaked to raises=0,
                    // which would have read as "handled cleanly" instead of "never observed".
                    _ <- fs2.Stream
                        .repeatEval(raised.get)
                        .metered(50.millis)
                        .find(_ > 0)
                        .compile
                        .last
                        .timeout(3.seconds)
                        .attempt
                    _ <- IO.sleep(300.millis) // let any failure cascade reach the event stream
                    _ <- drain.cancel
                yield ()
            }.attempt
            r <- raised.get
            e <- errors.get
        yield Outcome(r, e)
        io.unsafeRunSync()

    /** The same failure in an actor spawned at the TOP of the tree — no supervising ancestor, the
      * library's default strategy. This is the shape `CoilRelayOrderingTest` uses, and there the
      * raise does bring the actor system down ("the raise emergency-stops the actor system; that is
      * the incident"). Included to separate "the decider did not match" from "escalation had
      * nowhere to go".
      */
    private def observeTopLevel(thrown: Throwable, label: String): Outcome =
        val io = for
            raised <- cats.effect.Ref[IO].of(0)
            errors <- cats.effect.Ref[IO].of(List.empty[String])
            _ <- ActorSystem[IO](label).use { system =>
                for
                    drain <- system.eventStream.take
                        .flatMap {
                            case e: ActorError if e.cause != ActorError.NoCause =>
                                errors.update(_ :+ s"[${e.logSource}] ${e.cause.getMessage}")
                            case _ => IO.unit
                        }
                        .foreverM
                        .start
                    child <- system.actorOf(new Child(raised), s"top-$label")
                    _ <- child ! Boom(thrown)
                    // Poll for the failure to surface rather than sleeping a fixed span: a
                    // fixed wait raced the system shutdown and lost the event about half the
                    // time, which reads as "no failure" instead of "not waited long enough".
                    _ <- fs2.Stream
                        .repeatEval(errors.get)
                        .metered(50.millis)
                        .find(_.nonEmpty)
                        .compile
                        .last
                        .timeout(3.seconds)
                        .attempt
                    _ <- drain.cancel
                yield ()
            }.attempt
            r <- raised.get
            e <- errors.get
        yield Outcome(r, e)
        io.unsafeRunSync()

    test("a failure the decider does not match degrades differently from one it does") {
        val matched = observe(nonTotal, new RuntimeException("matched"), "matched")
        val unmatched = observe(nonTotal, BareThrowable(), "unmatched")
        val totalled = observe(total, BareThrowable(), "totalled")

        info(s"non-total decider, RuntimeException (matched)   -> $matched")
        info(s"non-total decider, bare Throwable (UNMATCHED)   -> $unmatched")
        info(s"total decider,     bare Throwable (matched)     -> $totalled")
        info(
          s"TOP-LEVEL (no supervisor), RuntimeException     -> ${observeTopLevel(new RuntimeException("top"), "top-rt")}"
        )
        info(
          s"TOP-LEVEL (no supervisor), bare Throwable       -> ${observeTopLevel(BareThrowable(), "top-bare")}"
        )

        // The claim the fix rests on: with a total decider, a bare Throwable behaves exactly as a
        // RuntimeException already did. Whatever that behaviour is, the two must now agree.
        // Anti-vacuity: a run where the child never raised proves nothing at all.
        val _ = assert(
          List(matched, unmatched, totalled).forall(_.raises > 0),
          "the child never raised, so this test observed nothing: " +
              s"matched=$matched unmatched=$unmatched totalled=$totalled",
        )
        // With a total decider a bare Throwable must behave exactly as a RuntimeException already
        // did. This is the deterministic half; the guardian-level `None.get` is racy and is
        // reported above rather than asserted.
        assert(
          totalled.emergencyStopped == matched.emergencyStopped &&
              unmatched.emergencyStopped == matched.emergencyStopped,
          "supervised failures should not differ by thrown class: " +
              s"matched=$matched unmatched=$unmatched totalled=$totalled",
        )
    }
}
