package hydrozoa.multisig.actors.pure

import cats.effect.IO
import com.suprnation.actor.Actor.{Actor, Receive}

/**
 * Event actor is the source of new L1 deposits and L2 transactions for the head.
 */
object EventActor {
    def create(peerId: PeerId): IO[EventActor] =
        IO.pure(EventActor(peerId))
}

case class EventActor(peerId: PeerId)
    extends Actor[IO, EventActorReq]{
    override def receive: Receive[IO, EventActorReq] =
        PartialFunction.fromFunction({
            case x: ConfirmBlockL2 => ???
        })
}
