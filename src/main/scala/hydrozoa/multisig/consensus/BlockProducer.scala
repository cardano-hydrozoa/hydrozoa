package hydrozoa.multisig.consensus

import hydrozoa.multisig.protocol.ConsensusProtocol.BlockProducer._
import hydrozoa.multisig.protocol.ConsensusProtocol._
import hydrozoa.multisig.protocol.PersistenceProtocol._
import hydrozoa.multisig.protocol.types.{AckBlock, Block, Peer}

import com.suprnation.actor.Actor.{Actor, Receive}

import cats.effect.{Deferred, IO, Ref}
import cats.implicits._

import BlockProducer.{Config, ConnectionsPending}

/** Block actor:
  *
  *   - When leader, receives L1 deposits + L2 txs and packages them into a new block.
  *   - When follower, receives L2 blocks and broadcasts L2 block acks for valid blocks.
  *   - When leader or follower, collects L2 block acks to confirm block effects and trigger
  *     leader/follower switch.
  */
object BlockProducer {
    final case class Config(peerId: Peer.Number, persistence: Persistence.Ref)

    final case class ConnectionsPending(
        cardanoLiaison: Deferred[IO, CardanoLiaison.Ref],
        peerLiaisons: Deferred[IO, List[PeerLiaison.Ref]],
        transactionSequencer: Deferred[IO, TransactionSequencer.Ref]
    )

    def apply(config: Config, connections: ConnectionsPending): IO[BlockProducer] = {
        IO(new BlockProducer(config = config, connections = connections) {})
    }
}

trait BlockProducer(config: Config, connections: ConnectionsPending) extends Actor[IO, Request] {
    private val subscribers = Ref.unsafe[IO, Option[Subscribers]](None)
    State()

    private final case class Subscribers(
        ackBlock: List[AckBlock.Subscriber],
        newBlock: List[Block.Subscriber],
        confirmBlock: List[ConfirmBlock.Subscriber]
    )

    override def preStart: IO[Unit] =
        for {
            cardanoLiaison <- connections.cardanoLiaison.get
            peerLiaisons <- connections.peerLiaisons.get
            transactionSequencer <- connections.transactionSequencer.get
            _ <- subscribers.set(
              Some(
                Subscribers(
                  ackBlock = peerLiaisons,
                  newBlock = peerLiaisons,
                  confirmBlock = List(cardanoLiaison, transactionSequencer)
                )
              )
            )
        } yield ()

    override def receive: Receive[IO, Request] =
        PartialFunction.fromFunction(req =>
            subscribers.get.flatMap({
                case Some(subs) =>
                    this.receiveTotal(req, subs)
                case _ =>
                    Error(
                      "Impossible: Block actor is receiving before its preStart provided subscribers."
                    ).raiseError
            })
        )

    private def receiveTotal(req: Request, subs: Subscribers): IO[Unit] =
        req match {
            case x: NewLedgerEvent =>
                ???
            case x: Block =>
                ???
            case x: AckBlock =>
                ???
        }

    private final class State {
        Ref.unsafe[IO, Block.Number](Block.Number(0))
    }
}
