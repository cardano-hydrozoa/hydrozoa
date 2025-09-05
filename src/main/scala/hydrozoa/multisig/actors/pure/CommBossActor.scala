package hydrozoa.multisig.actors.pure

import cats.effect.{IO, Ref}
import com.suprnation.actor.Actor.ReplyingReceive
import com.suprnation.actor.ReplyingActor

/**
 * Communication-boss actor synchronizes and broadcasts requests to communication actors.
 */
object CommBossActor {
    def create(peerId: PeerId): IO[CommBossActor] =
        IO.pure(CommBossActor(peerId))
}

case class CommBossActor(peerId: PeerId)
    extends ReplyingActor[IO, CommBossActorReq, CommBossActorResp] {
    override def receive: ReplyingReceive[IO, CommBossActorReq, CommBossActorResp] =
        PartialFunction.fromFunction({
            case x: NewLedgerEvent => ???
            case x: NewBlock => ???
            case x: AckBlock => ???
            case x: SyncLeaderBossComm => ???
            case x: SyncFollowerBossComm => ???
        })
}
