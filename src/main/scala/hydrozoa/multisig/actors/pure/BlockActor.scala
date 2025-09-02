package hydrozoa.multisig.actors.pure

import cats.effect.IO
import com.suprnation.actor.Actor.{Actor, Receive}

object BlockActor {
    def create(peerId: PeerId): IO[BlockActor] =
        IO.pure(BlockActor(peerId))
}

case class BlockActor(peerId: PeerId)
    extends Actor[IO, ActorReqBlock]{
    override def receive: Receive[IO, ActorReqBlock] =
        PartialFunction.fromFunction({
            case NewDepositL1() =>
                ???
            case NewDepositL1Info() =>
                ???
            case NewTxL2() =>
                ???
            case NewTxL2Info() =>
                ???
            case NewBlockL2() =>
                ???
            case AckBlockL2() =>
                ???
        })
}