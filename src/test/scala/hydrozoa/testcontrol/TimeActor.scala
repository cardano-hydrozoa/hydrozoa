package hydrozoa.testcontrol

import cats.effect.{Deferred, IO, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}
import java.time.Instant
import scala.concurrent.duration.*

// Messages
sealed trait TimeMsg
case class GetTime(replyTo: Deferred[IO, Instant]) extends TimeMsg
case class RecordTime(into: Ref[IO, Vector[Instant]]) extends TimeMsg
case class Wait(delay: FiniteDuration) extends TimeMsg

// Actor that uses IO.realTime
class TimeActor extends Actor[IO, TimeMsg] {

    override def receive: Receive[IO, TimeMsg] = PartialFunction.fromFunction(receiveTotal)

    private def receiveTotal(req: TimeMsg) = req match {
        case GetTime(replyTo) =>
            for {
                now <- IO.realTimeInstant
                _ <- replyTo.complete(now)
            } yield ()

        case RecordTime(into) =>
            for {
                now <- IO.realTimeInstant
                _ <- into.update(_ :+ now)
            } yield ()

        case Wait(delay) => IO.sleep(delay)
    }
}
