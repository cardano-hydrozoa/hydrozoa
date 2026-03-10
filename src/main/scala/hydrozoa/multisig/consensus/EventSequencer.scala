package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastSyntax.*
import hydrozoa.config.node.owninfo.OwnHeadPeerPublic
import hydrozoa.lib.actor.SyncRequest
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.EventSequencer.*
import hydrozoa.multisig.consensus.PeerLiaison.Handle
import hydrozoa.multisig.ledger.block.{BlockBody, BlockEffects, BlockStatus}
import hydrozoa.multisig.ledger.event.LedgerEventId.ValidityFlag
import hydrozoa.multisig.ledger.event.{LedgerEventId, LedgerEventNumber, UserEvent}
import hydrozoa.multisig.ledger.l1.tx.RefundTx
import scalus.cardano.ledger.{Coin, Value}

/** The first actor responsible for processing events from end-users, as received by the
  * [[HydrozoaServer]]. Only one event sequencer is running per node, specifically to handle _only_
  * the events that will be tagged with this Peer's [[HeadPeerNumber]] and sequential
  * [[LedgerEventId]]s.
  *
  * The messages are subsequently passed to the [[BlockWeaver]] and [[PeerLiaison]]s.
  */
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
            case req: SyncRequest.Any =>
                req.request match {
                    case r: L2TxRequest =>
                        r.handleSync(
                          req,
                          l2TxReq =>
                              for {
                                  newNum <- state.nextLedgerEventNum()
                                  newId = LedgerEventId(config.ownHeadPeerId.peerNum, newNum)
                                  newEvent = UserEvent.L2Event(
                                    eventId = newId,
                                    l2Payload = l2TxReq.tx
                                  )
                                  _ <- conn.blockWeaver ! newEvent
                                  _ <- (conn.peerLiaisons ! newEvent).parallel
                              } yield newId
                        )
                    case r: DepositRequest =>
                        r.handleSync(
                          req,
                          depositReq =>
                              for {
                                  newNum <- state.nextLedgerEventNum()
                                  newId = LedgerEventId(config.ownHeadPeerId.peerNum, newNum)
                                  newEvent = UserEvent.DepositEvent(
                                    eventId = newId,
                                    depositTxBytes = depositReq.depositTxBytes,
                                    refundTxBytes = depositReq.refundTxBytes,
                                    l2Payload = depositReq.l2Payload,
                                    l2Value = depositReq.l2Value,
                                    depositFee = depositReq.depositFee
                                  )
                                  _ <- conn.blockWeaver ! newEvent
                                  _ <- (conn.peerLiaisons ! newEvent).parallel
                              } yield newId
                        )
                }
        }

    private final class State {
        private val nLedgerEvent = Ref.unsafe[IO, LedgerEventNumber](LedgerEventNumber(0))

        def nextLedgerEventNum(): IO[LedgerEventNumber] =
            nLedgerEvent.updateAndGet(x => x.increment)
    }
}

/** Event sequencer receives local submissions of users' requests (via an http server), assigns
  * ledger event ids and emits them sequentially into the consensus system.
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

    /** Request to submit an L2 transaction. Can be used synchronously to get the assigned
      * LedgerEventId.
      */
    final case class L2TxRequest(
        tx: Array[Byte]
    ) extends SyncRequest[IO, L2TxRequest, LedgerEventId] {
        export L2TxRequest.Sync
        def ?: : this.Send = SyncRequest.send(_, this)
    }

    object L2TxRequest {
        type Sync = SyncRequest.Envelope[IO, L2TxRequest, LedgerEventId]
    }

    /** Request to register a deposit. Can be used synchronously to get the assigned LedgerEventId.
      */
    final case class DepositRequest(
        depositTxBytes: Array[Byte],
        refundTxBytes: Array[Byte],
        l2Payload: Array[Byte],
        depositFee: Coin,
        l2Value: Value
    ) extends SyncRequest[IO, DepositRequest, LedgerEventId] {
        export DepositRequest.Sync
        def ?: : this.Send = SyncRequest.send(_, this)
    }

    object DepositRequest {
        type Sync = SyncRequest.Envelope[IO, DepositRequest, LedgerEventId]
    }

    type Request = L2TxRequest.Sync | DepositRequest.Sync
}
