package hydrozoa.multisig.actors.pure

import cats.effect.IO
import com.suprnation.actor.Actor.{Actor, Receive}

/**
 * Event actor is the source of new L1 deposits and L2 transactions for the head.
 */
object LedgerEventActor {
    def create(peerId: PeerId): IO[LedgerEventActor] =
        IO.pure(LedgerEventActor(peerId))
}

case class LedgerEventActor(peerId: PeerId)
    extends Actor[IO, LedgerEventActorReq]{
    override def receive: Receive[IO, LedgerEventActorReq] =
        PartialFunction.fromFunction({
            case x: ConfirmBlock => ???
        })
}
