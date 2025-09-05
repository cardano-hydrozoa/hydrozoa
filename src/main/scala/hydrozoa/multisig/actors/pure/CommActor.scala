package hydrozoa.multisig.actors.pure

import cats.effect.IO
import com.suprnation.actor.Actor.{Actor, Receive}

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
    extends Actor[IO, CommActorReq]{
    override def receive: Receive[IO, CommActorReq] =
        PartialFunction.fromFunction({
            case x: NewLedgerEvent => ???
            case x: NewBlock => ???
            case x: AckBlock => ???
            case x: GetCommBatch => ???
            case x: NewCommBatch => ???
        })
}
