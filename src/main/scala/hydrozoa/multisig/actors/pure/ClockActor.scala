package hydrozoa.multisig.actors.pure

import cats.effect.IO
import com.suprnation.actor.Actor.ReplyingReceive
import com.suprnation.actor.ReplyingActor

import scala.concurrent.duration.FiniteDuration

/**
 * Clock actor provides time, monotonically increasing after each request.
 */
object ClockActor {
    def create(): IO[ClockActor] =
        for {
          startRealTime <- IO.realTime
          startMonotonic <- IO.monotonic
        } yield ClockActor (startRealTime, startMonotonic)
}

case class ClockActor(startRealTime: FiniteDuration, startMonotonic: FiniteDuration)
    extends ReplyingActor[IO, ClockActorReq, ClockActorResp] {
    override def receive: ReplyingReceive[IO, ClockActorReq, ClockActorResp] =
        PartialFunction.fromFunction({
            case GetTime =>
                for {
                    currentMonotonic <- IO.monotonic
                } yield GetTimeResp(currentMonotonic - startMonotonic + startRealTime)
        })
}
