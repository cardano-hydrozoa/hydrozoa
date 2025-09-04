package hydrozoa.multisig.actors.pure

import cats.effect.IO
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.NoSendActorRef
import com.suprnation.actor.SupervisorStrategy.Escalate
import com.suprnation.actor.{OneForOneStrategy, SupervisionStrategy}

import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

/**
 * Multisig boss actor starts-up and monitors all the actors of the multisig regime.
 */
object MultisigRegimeActor {
    def create(peerId: PeerId): IO[MultisigRegimeActor] =
        IO.pure(MultisigRegimeActor(peerId))
}

case class MultisigRegimeActor(peerId: PeerId)
    extends Actor[IO, MultisigBossActorReq]{

    override def supervisorStrategy: SupervisionStrategy[IO] =
        OneForOneStrategy[IO](maxNrOfRetries = 3, withinTimeRange = 1 minute) {
            case _: IllegalArgumentException => Escalate // Normally `Stop` but we can't handle stopped actors yet
            case _: RuntimeException         => Escalate // Normally `Restart` but our actors can't do that yet
            case _: Exception                => Escalate
        }

    override def preStart: IO[Unit] =
        for {
            clockActor <- context.replyingActorOf(ClockActor.create(), "clock")
            _ <- context.watch(clockActor, TerminatedClock(clockActor))
        } yield ()

    override def receive: Receive[IO,MultisigBossActorReq] =
        PartialFunction.fromFunction({
            case x: TerminatedClock => IO.println("Clock actor has terminated.")
        })
}
