package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastSyntax.*
import hydrozoa.config.node.owninfo.OwnHeadPeerPublic
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.EventSequencer.*
import hydrozoa.multisig.consensus.PeerLiaison.Handle
import hydrozoa.multisig.ledger.block.{BlockBody, BlockEffects, BlockStatus}
import hydrozoa.multisig.ledger.event.LedgerEventId.ValidityFlag
import hydrozoa.multisig.ledger.event.{LedgerEventId, LedgerEventNumber, UserEvent}
import hydrozoa.multisig.ledger.l1.tx.RefundTx

// TODO: move around
final case class L2TxRequest(
    tx: Array[Byte]
)

final case class DepositRequest(
    depositTxBytes: Array[Byte],
    refundTxBytes: Array[Byte],
    l2Payload: Array[Byte],
    depositFee: Long,
)

// TODO: update
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

    override def preStart: IO[Unit] = context.self ! EventSequencer.PreStart

    override def receive: Receive[IO, Request] =
        PartialFunction.fromFunction {
            case EventSequencer.PreStart => preStartLocal
            case req                     => getConnections.flatMap(receiveTotal(req, _))
        }

    private def receiveTotal(req: Request, conn: Connections): IO[Unit] =
        req match {
            case EventSequencer.PreStart =>
                // Should never reach here since PreStart is handled in receive
                IO.raiseError(new IllegalStateException("PreStart handled in receive"))
            case x: UserEvent =>
                for {
                    newNum <- state.nextLedgerEventNum()
                    newId = LedgerEventId(config.ownHeadPeerId.peerNum, newNum)
                    newEvent: UserEvent = x match {
                        case y: UserEvent.L2Event      => y.copy(eventId = newId)
                        case y: UserEvent.DepositEvent => y.copy(eventId = newId)
                    }
                    _ <- conn.blockWeaver ! newEvent
                    _ <- (conn.peerLiaisons ! newEvent).parallel
                } yield ()
            case x: BlockConfirmed =>
                // Complete the deferred ledger event outcomes confirmed by the block and remove them from queue
                ???
        }

    private def preStartLocal: IO[Unit] = initializeConnections

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

    type Config = OwnHeadPeerPublic.Section

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

    type Request = PreStart.type | UserEvent | BlockConfirmed

    case object PreStart
}
