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
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber, UserRequest}
import hydrozoa.multisig.ledger.l1.tx.RefundTx
import hydrozoa.multisig.server.UserRequest
import scalus.cardano.ledger.{Coin, Value}

/** The first actor responsible for processing events from end-users, as received by the
  * [[HydrozoaServer]]. Only one event sequencer is running per node, specifically to handle _only_
  * the events that will be tagged with this Peer's [[HeadPeerNumber]] and sequential
  * [[RequestId]]s.
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
            case req: SyncRequest.Any =>
                req.request match {
                    case r: L2TxRequest =>
                        r.handleSync(
                          req,
                          l2TxReq =>
                              for {
                                  newNum <- state.nextLedgerEventNum()
                                  newId = RequestId(config.ownHeadPeerId.peerNum, newNum)
                                  newEvent = L2Event(
                                    requestId = newId,
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
                                  newId = RequestId(config.ownHeadPeerId.peerNum, newNum)
                                  newEvent = UserRequest.DepositEvent(
                                    requestId = newId,
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

    private def preStartLocal: IO[Unit] = initializeConnections

    private final class State {
        private val nLedgerEvent = Ref.unsafe[IO, RequestNumber](RequestNumber(0))

        def nextLedgerEventNum(): IO[RequestNumber] =
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
            override def events: List[(RequestId, ValidityFlag)] = body.events
            override def depositsAbsorbed: List[RequestId] = body.depositsAbsorbed
            override def depositsRefunded: List[RequestId] = body.depositsAbsorbed
        }
    }

    /** Request to submit an L2 transaction. Can be used synchronously to get the assigned
      * RequestId.
      */
    final case class L2TxRequest(
        tx: Array[Byte]
    ) extends SyncRequest[IO, L2TxRequest, RequestId] {
        export L2TxRequest.Sync
        def ?: : this.Send = SyncRequest.send(_, this)
    }

    object L2TxRequest {
        type Sync = SyncRequest.Envelope[IO, L2TxRequest, RequestId]
    }

    /** Request to register a deposit. Can be used synchronously to get the assigned RequestId.
      */
    final case class DepositRequest(
        depositTxBytes: Array[Byte],
        // TODO: remove
        refundTxBytes: Array[Byte],
        l2Payload: Array[Byte],
        depositFee: Coin,
        // TODO: remove
        l2Value: Value
    ) extends SyncRequest[IO, DepositRequest, RequestId] {
        export DepositRequest.Sync
        def ?: : this.Send = SyncRequest.send(_, this)
    }

    object DepositRequest {
        type Sync = SyncRequest.Envelope[IO, DepositRequest, RequestId]
    }

    case object PreStart

    type Request = PreStart.type | L2TxRequest.Sync | DepositRequest.Sync
}
