package hydrozoa.lib.actor

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, FiberIO, IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.{ActorRef, NoSendActorRef}
import com.suprnation.actor.DeadLetter
import com.suprnation.actor.event.Debug
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** [[OrderlyShutdown]] stops actors that message each other, and one that messages itself on a
  * timer, without losing a message.
  */
class OrderlyShutdownTest extends AnyFunSuite {

    /** Passes a countdown back and forth with `peer`, taking `hop` over each step: at any moment
      * one of the pair is in its handler, about to send to the other.
      */
    private final class Pinger(peer: Deferred[IO, ActorRef[IO, Int]], hop: FiniteDuration)
        extends Actor[IO, Int] {
        override def receive: Receive[IO, Int] = PartialFunction.fromFunction(n =>
            IO.whenA(n > 0)(IO.sleep(hop) >> peer.get.flatMap(_ ! (n - 1)))
        )
    }

    private case object Tick

    /** Sends itself `Tick` from a fiber every few milliseconds, as the liaisons' resend timers do.
      */
    private final class Ticker(quiesced: Ref[IO, Boolean])
        extends Actor[IO, Tick.type | Quiesce.type],
          Quiescent {
        private val timer = Ref.unsafe[IO, Option[FiberIO[Unit]]](None)
        private def cancelTimer: IO[Unit] = timer.getAndSet(None).flatMap(_.traverse_(_.cancel))
        override def preStart: IO[Unit] =
            (IO.sleep(3.millis) >> (context.self ! Tick)).foreverM.void.start
                .flatMap(f => timer.set(Some(f)))
        override def postStop: IO[Unit] = cancelTimer
        override def quiesce: IO[Unit] = self ! Quiesce
        override def receive: Receive[IO, Tick.type | Quiesce.type] =
            PartialFunction.fromFunction {
                case Tick    => IO.unit
                case Quiesce => quiesced.set(true) >> cancelTimer
            }
    }

    /** Spawns a pair of [[Pinger]]s and a [[Ticker]] as its children, and starts the countdown. */
    private final class Parent(quiesced: Ref[IO, Boolean], countdown: Int) extends Actor[IO, Unit] {
        override def preStart: IO[Unit] =
            for {
                toA <- Deferred[IO, ActorRef[IO, Int]]
                toB <- Deferred[IO, ActorRef[IO, Int]]
                a <- context.actorOf(new Pinger(toB, 10.millis), "a")
                b <- context.actorOf(new Pinger(toA, 10.millis), "b")
                _ <- toA.complete(a) >> toB.complete(b)
                _ <- context.actorOf(new Ticker(quiesced), "ticker")
                _ <- a ! countdown
            } yield ()
        override def receive: Receive[IO, Unit] = PartialFunction.fromFunction(_ => IO.unit)
    }

    /** A system whose dead letters are collected: those of messages an actor was sent, not the
      * system messages cats-actors dead-letters on its own account.
      */
    private def withDeadLetters[A](body: (HydrozoaActorSystem, IO[List[Any]]) => IO[A]): A =
        (for {
            letters <- Ref[IO].of(List.empty[Any])
            result <- HydrozoaActorSystem(
              "orderly-shutdown",
              {
                  case Debug(_, _, dl: DeadLetter[?]) if !isSystemMessage(dl.message) =>
                      letters.update(dl.message :: _)
                  case _ => IO.unit
              }
            ).use(actors => body(actors, IO.sleep(500.millis) >> letters.get))
        } yield result).timeout(60.seconds).unsafeRunSync()

    private def isSystemMessage(message: Any): Boolean =
        message.getClass.getName.startsWith("com.suprnation.actor.dispatch.SystemMessage")

    private def subtree(ref: NoSendActorRef[IO]): IO[List[NoSendActorRef[IO]]] =
        SubtreeStop.childrenOf(ref).flatMap(_.flatTraverse(subtree)).map(ref :: _)

    test("actors mid-conversation quiesce, go idle and stop with no message lost") {
        val (outcome, letters, quiesced, running) = withDeadLetters { (actors, deadLetters) =>
            for {
                quiesced <- Ref[IO].of(false)
                parent <- actors.actorOf(new Parent(quiesced, countdown = 30))
                all <- subtree(parent)
                _ <- IO.sleep(55.millis)
                outcome <- OrderlyShutdown.run(
                  actors.system,
                  List(parent),
                  OrderlyShutdown.Bounds(1.second, 5.seconds, 5.seconds)
                )
                running <- all.filterA(ref => SubtreeStop.isTerminated(ref).map(!_))
                letters <- deadLetters
                q <- quiesced.get
            } yield (outcome, letters, q, running)
        }
        val _ = assert(outcome.complete, outcome.describe)
        val _ = assert(outcome.quiesced == 1, "only the ticker is Quiescent")
        val _ = assert(quiesced, "the ticker was never told to quiesce")
        val _ = assert(running.isEmpty, s"still running: $running")
        assert(letters.isEmpty, s"dead letters: $letters")
    }

    test("control: stopping the same actors outright loses the messages between them") {
        val letters = withDeadLetters { (actors, deadLetters) =>
            for {
                quiesced <- Ref[IO].of(false)
                parent <- actors.actorOf(new Parent(quiesced, countdown = 30))
                // Loses messages on purpose: logged as expected, not as lost while the system runs.
                _ <- actors.expectDeadLetters(parent)
                _ <- IO.sleep(55.millis)
                _ <- SubtreeStop.stopAndAwait(actors.system, parent, 5.seconds)
                letters <- deadLetters
            } yield letters
        }
        assert(letters.nonEmpty, "nothing was in flight, so the test above proves nothing")
    }

    test("a handler that never returns delays the stop by the bounds and no more") {
        val (outcome, took) = withDeadLetters { (actors, _) =>
            for {
                stuck <- actors.actorOf(new Actor[IO, Unit] {
                    override def receive: Receive[IO, Unit] =
                        PartialFunction.fromFunction(_ => IO.sleep(20.seconds))
                })
                _ <- stuck ! (())
                result <- OrderlyShutdown
                    .run(
                      actors.system,
                      List(stuck),
                      OrderlyShutdown.Bounds(100.millis, 300.millis, 300.millis)
                    )
                    .timed
                // Still stuck: end the system rather than have its release wait on the actor.
                _ <- actors.system.terminate(None)
            } yield result.swap
        }
        val _ = assert(!outcome.idleInTime && !outcome.stoppedInTime, outcome.describe)
        assert(took < 3.seconds, s"took $took")
    }
}
