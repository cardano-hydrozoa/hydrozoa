package hydrozoa.multisig.actors.pure

import cats.effect.IO
import com.suprnation.actor.Actor.ReplyingReceive
import com.suprnation.actor.ReplyingActor

object CommBossActor {
    def create(peerId: PeerId): IO[CommBossActor] =
        IO.pure(CommBossActor(peerId))
}

case class CommBossActor(peerId: PeerId)
    extends ReplyingActor[IO, ActorReqCommBoss, ActorRespCommBoss] {
    override def receive: ReplyingReceive[IO, ActorReqCommBoss, ActorRespCommBoss] =
        PartialFunction.fromFunction({
            case NewDepositL1() =>
                ???
            case NewTxL2() =>
                ???
            case NewBlockL2() =>
                ???
            case AckBlockL2() =>
                ???
            case ConfirmBlockL2() =>
                ???
            case SyncLeaderBossComm() =>
                ???
            case SyncFollowerBossComm() =>
                ???
        })
}