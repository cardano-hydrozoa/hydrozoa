package hydrozoa.multisig

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.{ActorRef, NoSendActorRef}
import com.suprnation.actor.event.Debug
import com.suprnation.actor.{DeadLetter, Envelope}
import hydrozoa.lib.actor.HydrozoaActorSystem
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.HeadMultisigRegimeManager.{Actors, HandoffToRuleBased}
import hydrozoa.multisig.metrics.PeerMetrics
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.DurationInt

/** At the handoff to the rule-based regime, a regime manager stops its multisig children, which
  * message each other. It stops them in order, so none of those messages is lost; the control stops
  * them outright and loses one.
  */
class HandoffStopTest extends AnyFunSuite {

    /** Passes a countdown back and forth with its peer, 10 ms a step: at any moment one of the pair
      * is in its handler, about to send to the other.
      */
    private final class Pinger(peer: Deferred[IO, ActorRef[IO, Int]]) extends Actor[IO, Int] {
        override def receive: Receive[IO, Int] = PartialFunction.fromFunction(n =>
            IO.whenA(n > 0)(IO.sleep(10.millis) >> peer.get.flatMap(_ ! (n - 1)))
        )
    }

    /** The shared base with two [[Pinger]] children mid-conversation. */
    private final class StubManager(
        inOrder: Boolean,
        events: Ref[IO, Vector[HeadRegimeManagerEvent]],
    ) extends MultisigRegimeManagerBase[HeadRegimeManagerEvent] {

        override protected val tracer: ContraTracer[IO, HeadRegimeManagerEvent] =
            ContraTracer[IO, HeadRegimeManagerEvent](e => events.update(_ :+ e))

        override protected lazy val tracers: MrmTracers = MrmTracers.fromRoot(tracer)

        override protected val metrics: PeerMetrics = PeerMetrics.create(0L, Vector.empty)

        private val children = Ref.unsafe[IO, List[NoSendActorRef[IO]]](Nil)

        override protected def preStartLocal: IO[Unit] =
            for {
                toA <- Deferred[IO, ActorRef[IO, Int]]
                toB <- Deferred[IO, ActorRef[IO, Int]]
                a <- context.actorOf(new Pinger(toB))
                b <- context.actorOf(new Pinger(toA))
                _ <- toA.complete(a) >> toB.complete(b)
                _ <- watchChildren(a -> Actors.BlockWeaver, b -> Actors.JointLedger)
                _ <- children.set(List(a, b))
                _ <- a ! 50
                _ <- tracer.traceWith(LifecycleEvent.WatchingActors)
            } yield ()

        override protected def onHandoffToRuleBased: IO[Unit] =
            children
                .getAndSet(Nil)
                .flatMap(refs =>
                    if inOrder then stopInOrder(refs) else refs.traverse_(context.stop)
                )
    }

    /** Hand off mid-conversation; return the manager's events and the countdown messages the system
      * dead-lettered.
      */
    private def handOff(inOrder: Boolean): (Vector[HeadRegimeManagerEvent], List[Any]) =
        (for {
            letters <- Ref[IO].of(List.empty[Any])
            events <- Ref[IO].of(Vector.empty[HeadRegimeManagerEvent])
            _ <- HydrozoaActorSystem(
              s"handoff-stop-$inOrder",
              {
                  case Debug(_, _, dl: DeadLetter[?]) =>
                      dl.message match {
                          case e: Envelope[?, ?] => letters.update(e.message :: _)
                          case _                 => IO.unit
                      }
                  case _ => IO.unit
              }
            ).use { actors =>
                for {
                    manager <- actors.actorOf(new StubManager(inOrder, events))
                    // The control loses messages on purpose: logged as expected, not as lost while
                    // the system runs.
                    _ <- IO.unlessA(inOrder)(actors.expectDeadLetters(manager))
                    _ <- await(events)(_.contains(LifecycleEvent.WatchingActors))
                    _ <- IO.sleep(55.millis)
                    _ <- manager ! HandoffToRuleBased
                    _ <- await(events)(_.count {
                        case _: LifecycleEvent.TerminatedActor => true
                        case _                                 => false
                    } == 2)
                    // Dead letters are published asynchronously; give one time to surface.
                    _ <- IO.sleep(500.millis)
                } yield ()
            }
            evs <- events.get
            dead <- letters.get
        } yield (evs, dead)).timeout(60.seconds).unsafeRunSync()

    private def await[A](seen: Ref[IO, Vector[A]])(p: Vector[A] => Boolean): IO[Unit] = {
        def go: IO[Unit] =
            seen.get.flatMap(v => if p(v) then IO.unit else IO.sleep(20.millis) >> go)
        go.timeoutTo(10.seconds, IO.raiseError(new AssertionError("the manager never got there")))
    }

    test("the handoff stops the children in order, losing none of their messages") {
        val (events, letters) = handOff(inOrder = true)
        val stopped = events.collect { case e: LifecycleEvent.ChildrenStopped => e }
        val _ = assert(stopped.size == 1 && stopped.head.outcome.complete, s"events: $events")
        val _ = assert(
          events.collect { case LifecycleEvent.TerminatedActor(_, why) => why }.toSet ==
              Set(Some(LifecycleEvent.Stopping.AtHandoff)),
          s"events: $events"
        )
        assert(letters.isEmpty, s"dead letters: $letters")
    }

    test("control: stopped outright, the children lose a message in flight") {
        val (_, letters) = handOff(inOrder = false)
        assert(letters.nonEmpty, "nothing was in flight, so the test above proves nothing")
    }
}
