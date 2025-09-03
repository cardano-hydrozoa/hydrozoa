package hydrozoa.multisig.actors.pure

import cats.effect.IO
import com.suprnation.actor.Actor.ReplyingReceive
import com.suprnation.actor.ReplyingActor

/**
 * Communication actor is connected to its counterpart at another peer:
 *
 *   - Requests communication batches from the counterpart.
 *   - Responds to the counterpart's requests for communication batches.
 */
object CommActor {
    def create(peerId: PeerId): IO[CommActor] =
        IO.pure(CommActor(peerId))
}

case class CommActor(peerId: PeerId)
    extends ReplyingActor[IO, CommActorReq, CommActorResp]{
    override def receive: ReplyingReceive[IO, CommActorReq, CommActorResp] =
        PartialFunction.fromFunction({
            case x: NewDepositL1 => ???
            case x: NewTxL2 => ???
            case x: NewBlockL2 => ???
            case x: AckBlockL2 => ???
            case x: ReqCommBatch => ???
            case x: RespCommBatch => ???
            case x: SyncLeaderComm => ???
            case x: SyncFollowerComm => ???
        })
}
