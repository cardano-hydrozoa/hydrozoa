package hydrozoa.testcontrol

import cats.effect.{Deferred, IO}
import com.suprnation.actor.Actor.{Actor, Receive}
import java.time.Instant
import scala.concurrent.duration.*

// Messages
sealed trait TimeMsg
case class GetTime(replyTo: Deferred[IO, Instant]) extends TimeMsg
case class PrintTime() extends TimeMsg
case class WaitAndPrint(delay: FiniteDuration) extends TimeMsg

// Actor that uses IO.realTime
class TimeActor extends Actor[IO, TimeMsg] {

    override def receive: Receive[IO, TimeMsg] = {
        case GetTime(replyTo) =>
            for {
                now <- IO.realTimeInstant
                _ <- replyTo.complete(now)
            } yield ()

        case PrintTime() =>
            for {
                now <- IO.realTimeInstant
                _ <- IO.println(s"Current time: $now")
            } yield ()

        case WaitAndPrint(delay) =>
            for {
                _ <- IO.println(s"Waiting for $delay...")
                _ <- IO.sleep(delay)
                now <- IO.realTimeInstant
                _ <- IO.println(s"After waiting: $now")
            } yield ()
    }
}
