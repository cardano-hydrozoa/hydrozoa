package hydrozoa.multisig.actors.pure

import cats.effect.IO
import com.suprnation.actor.Actor.ReplyingReceive
import com.suprnation.actor.ReplyingActor

import scala.concurrent.duration.FiniteDuration

object ClockActor {
    def create(): IO[ClockActor] =
        for {
          startRealTime <- IO.realTime
          startMonotonic <- IO.monotonic
        } yield ClockActor (startRealTime, startMonotonic)
}

case class ClockActor(startRealTime: FiniteDuration, startMonotonic: FiniteDuration)
    extends ReplyingActor[IO, ActorReqClock, ActorRespClock] {
    override def receive: ReplyingReceive[IO, ActorReqClock, ActorRespClock] =
        PartialFunction.fromFunction({
            case GetTime =>
                for {
                    currentMonotonic <- IO.monotonic
                } yield RespTime(currentMonotonic - startMonotonic + startRealTime)
        })
}