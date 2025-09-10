package hydrozoa.multisig.actors

import cats.effect.Deferred
import cats.effect.IO
import cats.effect.Ref
import cats.implicits._
import com.suprnation.actor.Actor.Actor
import com.suprnation.actor.Actor.Receive

import BlockProducer.{Config, State, ConnectionsPending, Subscribers}
import hydrozoa.multisig.persistence.PersistenceActorRef

final case class BlockProducer(config: Config)(
    private val connections: ConnectionsPending
)(
    private val subscribers: Ref[IO, Option[Subscribers]],
    private val state: State
) extends Actor[IO, BlockProducerReq] {
    override def preStart: IO[Unit] =
        for {
            cardanoLiaison <- connections.cardanoLiaison.get
            peerLiaisons <- connections.peerLiaisons.get
            transactionSequencer <- connections.transactionSequencer.get
            persistence <- connections.persistence.get
            _ <- subscribers.set(
              Some(
                Subscribers(
                  ackBlock = peerLiaisons,
                  newBlock = peerLiaisons,
                  confirmBlock = List(cardanoLiaison, transactionSequencer),
                  persistence = persistence
                )
              )
            )
        } yield ()

    override def receive: Receive[IO, BlockProducerReq] =
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

    private def receiveTotal(req: BlockProducerReq, subs: Subscribers): IO[Unit] =
        req match {
            case x: NewLedgerEvent =>
                ???
            case x: NewBlock =>
                ???
            case x: AckBlock =>
                ???
        }
}

/** Block actor:
  *
  *   - When leader, receives L1 deposits + L2 txs and packages them into a new block.
  *   - When follower, receives L2 blocks and broadcasts L2 block acks for valid blocks.
  *   - When leader or follower, collects L2 block acks to confirm block effects and trigger
  *     leader/follower switch.
  */
object BlockProducer {
    final case class Config(peerId: PeerId)

    final case class ConnectionsPending(
        cardanoLiaison: Deferred[IO, CardanoLiaisonRef],
        peerLiaisons: Deferred[IO, List[PeerLiaisonRef]],
        transactionSequencer: Deferred[IO, TransactionSequencerRef],
        persistence: Deferred[IO, PersistenceActorRef]
    )

    final case class Subscribers(
        ackBlock: List[AckBlockSubscriber],
        newBlock: List[NewBlockSubscriber],
        confirmBlock: List[ConfirmBlockSubscriber],
        persistence: PersistenceActorRef
    )

    def create(config: Config, connections: ConnectionsPending): IO[BlockProducer] = {
        for {
            subscribers <- Ref.of[IO, Option[Subscribers]](None)
            state <- State.create
        } yield BlockProducer(config)(connections)(subscribers, state)
    }

    final case class State(nBlock: Ref[IO, BlockNum])

    object State {
        def create: IO[State] =
            for {
                nBlock <- Ref.of[IO, BlockNum](BlockNum(0))
            } yield State(
              nBlock = nBlock
            )
    }
}
