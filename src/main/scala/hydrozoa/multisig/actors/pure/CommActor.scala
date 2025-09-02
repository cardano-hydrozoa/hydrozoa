package hydrozoa.multisig.actors.pure

import cats.effect.IO
import com.suprnation.actor.Actor.ReplyingReceive
import com.suprnation.actor.ReplyingActor

object CommActor {
    def create(peerId: PeerId): IO[CommActor] =
        IO.pure(CommActor(peerId))
}

case class CommActor(peerId: PeerId)
    extends ReplyingActor[IO, ActorReqComm, ActorRespComm]{
    override def receive: ReplyingReceive[IO, ActorReqComm, ActorRespComm] =
        PartialFunction.fromFunction({
            case NewDepositL1() =>
                ???
            case NewTxL2() =>
                ???
            case NewBlockL2() =>
                ???
            case AckBlockL2() =>
                ???
            case ReqCommBatch() =>
                ???
            case RespCommBatch() =>
                ???
            case SyncLeaderComm() =>
                ???
            case SyncFollowerComm() =>
                ???
        })
}