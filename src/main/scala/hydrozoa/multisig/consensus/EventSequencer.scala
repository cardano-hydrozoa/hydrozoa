package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastSyntax.*
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.OwnHeadPeer
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.EventSequencer.*
import hydrozoa.multisig.consensus.PeerLiaison.Handle
import hydrozoa.multisig.ledger.block.{BlockBody, BlockEffects, BlockStatus}
import hydrozoa.multisig.ledger.dapp.tx.RefundTx
import hydrozoa.multisig.ledger.event.LedgerEventId.ValidityFlag
import hydrozoa.multisig.ledger.event.{LedgerEvent, LedgerEventId, LedgerEventNumber}

trait EventSequencer(
    config: Config,
    pendingConnections: MultisigRegimeManager.PendingConnections | EventSequencer.Connections
) extends Actor[IO, Request] {
    private val connections = Ref.unsafe[IO, Option[EventSequencer.Connections]](None)
    private val state = State()

    private def getConnections: IO[Connections] = for {
        mConn <- this.connections.get
        conn <- mConn.fold(
          IO.raiseError(
            java.lang.Error(
              "Event sequencer is missing its connections to other actors."
            )
          )
        )(IO.pure)
    } yield conn

    private def initializeConnections: IO[Unit] = pendingConnections match {
        case x: MultisigRegimeManager.PendingConnections =>
            for {
                _connections <- x.get
                _ <- connections.set(
                  Some(
                    Connections(
                      blockWeaver = _connections.blockWeaver,
                      peerLiaisons = _connections.peerLiaisons
                    )
                  )
                )
            } yield ()
        case x: EventSequencer.Connections => connections.set(Some(x))
    }

    override def preStart: IO[Unit] = initializeConnections

    override def receive: Receive[IO, Request] =
        PartialFunction.fromFunction(req => getConnections.flatMap(receiveTotal(req, _)))

    private def receiveTotal(req: Request, conn: Connections): IO[Unit] =
        req match {
            case x: LedgerEvent =>
                for {
                    newNum <- state.nextLedgerEventNum()
                    newId = LedgerEventId(config.ownHeadPeerId.peerNum, newNum)
                    newEvent: LedgerEvent = x match {
                        case y: LedgerEvent.TxL2Event       => y.copy(eventId = newId)
                        case y: LedgerEvent.RegisterDeposit => y.copy(eventId = newId)
                    }
                    _ <- conn.blockWeaver ! newEvent
                    _ <- (conn.peerLiaisons ! newEvent).parallel
                } yield ()
            case x: BlockConfirmed =>
                // Complete the deferred ledger event outcomes confirmed by the block and remove them from queue
                ???
        }

    private final class State {
        private val nLedgerEvent = Ref.unsafe[IO, LedgerEventNumber](LedgerEventNumber(0))
//        private val localRequests =
//            Ref.unsafe[IO, Queue[(LedgerEventId.Number, Deferred[IO, Unit])]](
//              Queue()
//            )

        def nextLedgerEventNum(): IO[LedgerEventNumber] =
            for {
                newNum <- nLedgerEvent.updateAndGet(x => x.increment)
                // FIXME:
//                _ <- localRequests.update(q => q :+ (newNum -> eventOutcome))
            } yield newNum

        def completeDeferredEventOutcomes(
            eventOutcomes: List[(LedgerEventNumber, Unit)]
        ): IO[Unit] =
            ???
    }
}

/** Event sequencer receives local submissions of new ledger events and emits them sequentially into
  * the consensus system.
  */
object EventSequencer {
    def apply(
        config: Config,
        pendingConnections: MultisigRegimeManager.PendingConnections
    ): IO[EventSequencer] =
        IO(new EventSequencer(config, pendingConnections) {})

    type Config = OwnHeadPeer.Section & HeadPeers.Section

    final case class Connections(
        blockWeaver: BlockWeaver.Handle,
        peerLiaisons: List[PeerLiaison.Handle]
    )

    type Handle = ActorRef[IO, Request]

    type BlockConfirmed = BlockBody.Section & BlockEffects.Fields.HasPostDatedRefundTxs &
        BlockStatus.MultiSigned

    object BlockConfirmed {

        /** For unit/property testing. */
        final case class Minimal(
            override val body: BlockBody.Next,
            // FIXME: How do we ensure these are signed?
            override val postDatedRefundTxs: List[RefundTx.PostDated],
        ) extends BlockBody.Section,
              BlockEffects.Fields.HasPostDatedRefundTxs,
              BlockStatus.MultiSigned {
            override def events: List[(LedgerEventId, ValidityFlag)] = body.events
            override def depositsAbsorbed: List[LedgerEventId] = body.depositsAbsorbed
            override def depositsRefunded: List[LedgerEventId] = body.depositsAbsorbed
        }
    }

    type Request = LedgerEvent | BlockConfirmed
}
