package hydrozoa.multisig.actors.pure

import cats.effect.{IO, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}

/**
 * Communication-boss actor synchronizes and broadcasts requests to communication actors.
 */
object CommBossActor {
    def create(peerId: PeerId): IO[CommBossActor] =
        IO.pure(CommBossActor(peerId))
}

case class CommBossActor(peerId: PeerId)
    extends Actor[IO, CommBossActorReq] {
    override def receive: Receive[IO, CommBossActorReq] =
        PartialFunction.fromFunction({
            case x: NewLedgerEvent => ???
            case x: NewBlock => ???
            case x: AckBlock => ???
        })
}
