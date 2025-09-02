package hydrozoa.multisig.actors.pure

import cats.effect.IO
import com.suprnation.actor.Actor.{Actor, Receive}

object CardanoActor {
    def create(peerId: PeerId): IO[CardanoActor] =
        IO.pure(CardanoActor())
}

case class CardanoActor()
    extends Actor[IO, ActorReqCardano]{
    override def receive: Receive[IO, ActorReqCardano] =
        PartialFunction.fromFunction({
            case ConfirmBlockL2() =>
                ???
        })
}