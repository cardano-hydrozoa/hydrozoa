package hydrozoa.multisig.consensus.limiter

import cats.effect.{IO, IOLocal}
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.config.node.operation.multisig.RateLimits
import hydrozoa.lib.logging.Tracer
import scala.concurrent.duration.DurationLong

/** Stateless FIFO rate-limiter actor sitting between two actors on one lane.
  *
  * For each incoming message mixing in [[LimiterTimestamp]], the limiter checks whether
  * `msg.limiterTimestamp + msg.minPeriod` is still in the future; if so it holds the message for
  * exactly that difference, then forwards. Otherwise it forwards immediately. Messages without the
  * trait pass through with no delay.
  *
  * The limiter keeps no state about prior messages: every throttled message gates itself based on
  * the timestamp it carries. Strict FIFO is provided by the single mailbox — `IO.sleep` inside
  * `receive` blocks subsequent (including non-throttled) messages until the hold elapses.
  *
  * Using an actor (rather than a fiber wrapping `(msg) => sleep >> send`) guarantees no concurrent
  * delivery to `downstream` from this lane — out-of-order delivery would otherwise be possible.
  *
  * @param downstream
  *   the actor the throttled lane terminates at (e.g. `BlockWeaver`, `StackComposer`). The
  *   limiter's own [[ActorRef]] is what upstream actors are wired to in place of `downstream`.
  * @param config
  *   reads `minPeriod` from per-message-type knobs.
  */
final case class Limiter[Msg](
    downstream: ActorRef[IO, Msg],
    config: RateLimits.Section,
    tracerLocal: IOLocal[Tracer]
) extends Actor[IO, Msg] {

    given RateLimits.Section = config
    given IOLocal[Tracer] = tracerLocal

    override def preStart: IO[Unit] =
        Tracer.routeLocal("Limiter") *> Tracer.debug("Limiter started.")

    override def receive: Receive[IO, Msg] = PartialFunction.fromFunction { msg =>
        msg match {
            case throttled: LimiterTimestamp =>
                for {
                    now <- IO.realTimeInstant
                    gate = throttled.limiterTimestamp.plusMillis(throttled.minPeriod.toMillis)
                    waitMs = gate.toEpochMilli - now.toEpochMilli
                    _ <-
                        if waitMs > 0 then
                            Tracer.debug(
                              s"Limiter holding ${msg.getClass.getSimpleName} for ${waitMs}ms"
                            ) *> IO.sleep(waitMs.millis)
                        else IO.unit
                    _ <- downstream ! msg
                } yield ()
            case other =>
                downstream ! other
        }
    }
}

object Limiter {
    type Handle[Msg] = ActorRef[IO, Msg]
}
