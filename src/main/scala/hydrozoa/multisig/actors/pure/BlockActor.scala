package hydrozoa.multisig.actors.pure

import cats.effect.IO
import com.suprnation.actor.Actor.{Actor, Receive}

/**
 * Block actor:
 *
 *   - When leader, receives L1 deposits + L2 txs and packages them into a new block.
 *   - When follower, receives L2 blocks and broadcasts L2 block acks for valid blocks.
 *   - When leader or follower, collects L2 block acks to confirm block effects and trigger leader/follower switch.
 */
object BlockActor {
    def create(peerId: PeerId): IO[BlockActor] =
        IO.pure(BlockActor(peerId))
}

case class BlockActor(peerId: PeerId)
    extends Actor[IO, BlockActorReq]{
    override def receive: Receive[IO, BlockActorReq] =
        PartialFunction.fromFunction({
            case x: NewLedgerEvent => ???
            case x: NewEventId => ???
            case x: NewBlock => ???
            case x: AckBlock => ???
        })
}
