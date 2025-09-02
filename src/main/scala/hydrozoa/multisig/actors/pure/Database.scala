package hydrozoa.multisig.actors.pure

import cats.effect.IO
import com.suprnation.actor.Actor.ReplyingReceive
import com.suprnation.actor.ReplyingActor

object Database {
    def create(): IO[Database] =
        IO.pure(Database())
}

case class Database()
    extends ReplyingActor[IO, ActorReqDatabase, ActorRespDatabase]{
    override def receive: ReplyingReceive[IO, ActorReqDatabase, ActorRespDatabase] =
        PartialFunction.fromFunction({
            case PutNewDepositL1() =>
                ???
            case PutNewTxL2() =>
                ???
            case PutNewBlockL2() =>
                ???
            case PutAckBlockL2() =>
                ???
            case PutConfirmBlockL2() =>
                ???
            case PutCommBatch() =>
                ???
            case PutL1Effects() =>
                ???
            case PutHeadStateOnCardano() =>
                ???
            case GetL2BlockData() =>
                ???
            case GetConfirmedLocalEvents() =>
                ???
            case GetConfirmedL1Effects() =>
                ???
        })
}