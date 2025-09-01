package hydrozoa.multisig.actors.pure

import cats.effect.unsafe.implicits.global
import cats.effect.IO
import com.suprnation.actor.ActorSystem

import scala.concurrent.ExecutionContext.Implicits.global
import munit.ScalaCheckSuite

import scala.concurrent.duration.FiniteDuration

class ClockActorSpec extends ScalaCheckSuite {
    test("Clock actor provides monotonically increasing timestamps."):
        ActorSystem[IO]("clock-system").use { system =>
            for {
              clockActor <- system.replyingActorOf(ClockActor.create(), "clock-actor")
              a <- clockActor ? GetTime
              b <- clockActor ? GetTime
              c <- clockActor ? GetTime
            } yield (a,b,c)
        }.unsafeToFuture().map {
            case (RespTime(a),RespTime(b),RespTime(c)) =>
                assert(a < b)
                assert(b < c)
        }
}