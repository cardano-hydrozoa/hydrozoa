package hydrozoa.multisig.actors.pure

import cats.effect.IO
import com.suprnation.actor.Actor.{Actor, Receive}

object EventActor {
    def create(peerId: PeerId): IO[EventActor] =
        IO.pure(EventActor(peerId))
}

case class EventActor(peerId: PeerId)
    extends Actor[IO, ActorReqEvent]{
    override def receive: Receive[IO, ActorReqEvent] =
        PartialFunction.fromFunction({
            case ConfirmBlockL2() =>
                ???
        })
}