package hydrozoa.multisig.actors.pure

import cats.effect.IO
import com.suprnation.actor.Actor.{Actor, Receive}

object CardanoBlockchain {
    def create(): IO[CardanoBlockchain] =
        IO.pure(CardanoBlockchain())
}

case class CardanoBlockchain()
    extends Actor[IO, ActorReqCardanoBlockchain]{
    override def receive: Receive[IO, ActorReqCardanoBlockchain] =
        PartialFunction.fromFunction({
            case SubmitL1Effects() =>
                ???
            case GetHeadStateOnCardano() =>
                ???
        })
}