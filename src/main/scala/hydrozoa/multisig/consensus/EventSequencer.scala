package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastSyntax.*
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.EventSequencer.*
import hydrozoa.multisig.consensus.EventSequencer.Request.*
import hydrozoa.multisig.consensus.PeerLiaison.Handle
import hydrozoa.multisig.consensus.peer.{PeerId, PeerNumber}
import hydrozoa.multisig.ledger.block.BlockBrief
import hydrozoa.multisig.ledger.dapp.tx.RefundTx
import hydrozoa.multisig.protocol.*
import hydrozoa.multisig.protocol.types.{LedgerEvent, LedgerEventId}

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
                    newId = LedgerEventId(config.peerId.peerNum, newNum)
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
        private val nLedgerEvent = Ref.unsafe[IO, LedgerEventId.Number](LedgerEventId.Number(0))
//        private val localRequests =
//            Ref.unsafe[IO, Queue[(LedgerEventId.Number, Deferred[IO, Unit])]](
//              Queue()
//            )

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
    def apply(
        config: Config,
        pendingConnections: MultisigRegimeManager.PendingConnections
    ): IO[EventSequencer] =
        IO(new EventSequencer(config, pendingConnections) {})

    final case class Config(peerId: PeerId)

    final case class Connections(
        blockWeaver: BlockWeaver.Handle,
        peerLiaisons: List[PeerLiaison.Handle]
    )

    type Handle = ActorRef[IO, Request]

    type Request = LedgerEvent | BlockConfirmed

    object Request {
        final case class BlockConfirmed(
            blockBrief: BlockBrief.Next,
            mbPostDatedRefundsSigned: List[RefundTx.PostDated]
        )
    }
}
