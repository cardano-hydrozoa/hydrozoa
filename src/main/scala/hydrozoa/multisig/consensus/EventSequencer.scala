package hydrozoa.multisig.consensus

import cats.effect.{Deferred, IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastSyntax.*
import hydrozoa.multisig.consensus.EventSequencer.Request.*
import hydrozoa.multisig.consensus.EventSequencer.{Config, ConnectionsPending, Request}
import hydrozoa.multisig.ledger.dapp.tx.RefundTx
import hydrozoa.multisig.protocol.*
import hydrozoa.multisig.protocol.types.{Block, LedgerEvent, LedgerEventId, Peer}
import scala.collection.immutable.Queue

trait EventSequencer(config: Config, connections: ConnectionsPending) extends Actor[IO, Request] {
    private val subscribers = Ref.unsafe[IO, Option[Subscribers]](None)
    private val state = State()

    private final case class Subscribers(
        newLedgerEvent: List[LedgerEvent.Subscriber]
    )

    override def preStart: IO[Unit] =
        for {
            blockWeaver <- connections.blockWeaver.get
            peerLiaisons <- connections.peerLiaisons.get
            _ <- subscribers.set(
              Some(
                Subscribers(
                  newLedgerEvent = blockWeaver :: peerLiaisons
                )
              )
            )
        } yield ()

    override def receive: Receive[IO, Request] =
        PartialFunction.fromFunction(req =>
            subscribers.get.flatMap {
                case Some(subs) =>
                    this.receiveTotal(req, subs)
                case _ =>
                    Error(
                      "Impossible: Ledger event actor is receiving before its preStart provided subscribers."
                    ).raiseError
            }
        )

    private def receiveTotal(req: Request, subs: Subscribers): IO[Unit] =
        req match {
            case x: LedgerEvent =>
                for {
                    newNum <- state.nextLedgerEventNum()
                    newId = LedgerEventId(config.peerId.peerNum, newNum)
                    newEvent: LedgerEvent = x match {
                        case y: LedgerEvent.TxL2Event       => y.copy(eventId = newId)
                        case y: LedgerEvent.RegisterDeposit => y.copy(eventId = newId)
                    }
                    _ <- (subs.newLedgerEvent ! newEvent).parallel
                } yield ()
            case x: BlockConfirmed =>
                // Complete the deferred ledger event outcomes confirmed by the block and remove them from queue
                ???
        }

    private final class State {
        private val nLedgerEvent = Ref.unsafe[IO, LedgerEventId.Number](LedgerEventId.Number(0))
        private val localRequests =
            Ref.unsafe[IO, Queue[(LedgerEventId.Number, Deferred[IO, Unit])]](
              Queue()
            )

        def nextLedgerEventNum(): IO[LedgerEventId.Number] =
            for {
                newNum <- nLedgerEvent.updateAndGet(x => x.increment)
                // FIXME:
//                _ <- localRequests.update(q => q :+ (newNum -> eventOutcome))
            } yield newNum

        def completeDeferredEventOutcomes(
            eventOutcomes: List[(LedgerEventId.Number, Unit)]
        ): IO[Unit] =
            ???
    }
}

/** Event sequencer receives local submissions of new ledger events and emits them sequentially into
  * the consensus system.
  */
object EventSequencer {
    def apply(config: Config, connections: ConnectionsPending): IO[EventSequencer] =
        IO(new EventSequencer(config, connections) {})

    final case class Config(peerId: Peer.Id)

    final case class ConnectionsPending(
        blockWeaver: Deferred[IO, BlockWeaver.Handle],
        peerLiaisons: Deferred[IO, List[PeerLiaison.Handle]]
    )

    type Handle = ActorRef[IO, Request]

    type Request = LedgerEvent | BlockConfirmed

    object Request {
        final case class BlockConfirmed(
            block: Block.Next,
            mbPostDatedRefundsSigned: List[RefundTx.PostDated]
        )
    }
}
