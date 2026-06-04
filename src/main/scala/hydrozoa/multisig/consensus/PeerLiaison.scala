package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
import hydrozoa.config.node.owninfo.OwnPeerPublic
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.PeerLiaison.*
import hydrozoa.multisig.consensus.PeerLiaison.Request.*
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckNumber, HubCoilAck, HubCoilAckNumber, SoftAck, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.RemotePeer.*
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId, RemotePeer}
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockNumber, BlockStatus, BlockType}
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.ledger.stack.{StackBrief, StackNumber}

/** The head-mesh liaison: one per other head peer. Full duplex — it sends this peer's own artifacts
  * and routes the remote head's artifacts to every local consensus actor.
  */
abstract class PeerLiaison(
    config: Config,
    remotePeer: RemotePeer,
    pendingConnections: MultisigRegimeManager.PendingConnections | PeerLiaison.Connections,
) extends BatchProtocolLiaison(config, remotePeer) {
    private val connections = Ref.unsafe[IO, Option[Connections]](None)

    private def getConnections: IO[Connections] = for {
        mConn <- this.connections.get
        conn <- mConn.fold(
          IO.raiseError(
            java.lang.Error(
              "Peer liaison is missing its connections to other actors."
            )
          )
        )(IO.pure)
    } yield conn

    override protected def initializeConnections: IO[Unit] = pendingConnections match {
        case x: MultisigRegimeManager.PendingConnections =>
            for {
                _connections <- x.get
                _ <- connections.set(
                  Some(
                    Connections(
                      blockWeaver = _connections.blockWeaver,
                      consensusActor = _connections.consensusActor,
                      stackComposer = _connections.stackComposer,
                      slowConsensusActor = _connections.slowConsensusActor,
                      remotePeerLiaison = _connections.remotePeerLiaisons(remotePeer.peerId)
                    )
                  )
                )
            } yield ()
        case x: PeerLiaison.Connections => connections.set(Some(x))
    }

    override protected def sendToRemoteLiaison(msg: Request): IO[Unit] =
        getConnections.flatMap(_.remotePeerLiaison ! msg)

    override protected def dispatchVerifiedBatch(batch: NewMsgBatch): IO[Unit] =
        for {
            conn <- getConnections
            _ <- batch.softAck.traverse_(conn.consensusActor ! _)
            _ <- batch.blockBrief.traverse_(conn.blockWeaver ! _)
            _ <- batch.stackBrief.traverse_(conn.stackComposer ! _)
            _ <- batch.hardAck.traverse_(conn.slowConsensusActor ! _)
            // Relayed coil hard-acks: unwrap and hand the raw, signed coil ack to the local
            // SlowConsensusActor (verified end-to-end there).
            _ <- batch.hubCoilAck.traverse_(hc => conn.slowConsensusActor ! hc.ack)
            _ <- batch.requests.traverse_(conn.blockWeaver ! _)
        } yield ()
}

/** A communication actor that is connected to its counterpart at another peer:
  *
  *   - Requests communication batches from the counterpart.
  *   - Responds to the counterpart's requests for communication batches.
  */
object PeerLiaison {
    def apply(
        config: Config,
        remotePeer: RemotePeer,
        pendingLocalConnections: MultisigRegimeManager.PendingConnections,
    ): IO[PeerLiaison] =
        IO(new PeerLiaison(config, remotePeer, pendingLocalConnections) {})

    type Config = OwnPeerPublic.Section & NodeOperationMultisigConfig.Section

    final case class Connections(
        blockWeaver: BlockWeaver.Handle,
        consensusActor: FastConsensusActor.Handle,
        stackComposer: StackComposer.Handle,
        slowConsensusActor: SlowConsensusActor.Handle,
        remotePeerLiaison: PeerLiaison.Handle
    )

    type Handle = ActorRef[IO, Request]

    type BlockConfirmed = BlockBrief.Section & BlockType.Next & BlockStatus.SoftConfirmed

    object BlockConfirmed {

        /** For testing purposes */
        object Minimal {
            def apply(x: BlockBrief.Next): BlockConfirmed = x.asSoftConfirmed
        }
    }

    type Request =
        PreStart.type | ResendCurrent.type | RemoteBroadcast | GetMsgBatch | NewMsgBatch |
            BlockConfirmed

    /** Outcome of verifying a [[NewMsgBatch]] against the receiver's expected [[GetMsgBatch]]
      * state.
      *
      * The two-arm split decouples verification (a pure check) from state mutation: only
      * [[VerifyOutcome.Advance]] should trigger a write of `currentlyRequesting` and the downstream
      * dispatch of the batch's payload.
      */
    enum VerifyOutcome:
        /** The received batch matches the expected next-batch contract. `next` is the new value the
          * caller should write to `currentlyRequesting` and use as the outgoing [[GetMsgBatch]].
          */
        case Advance(next: GetMsgBatch)

        /** The received batch was rejected. The protocol stays at the current [[GetMsgBatch]].
          * `reason` carries the specific verification predicate that failed, for logging and
          * triage.
          */
        case Reject(reason: VerifyOutcome.Rejection)

    object VerifyOutcome:
        /** Why a [[NewMsgBatch]] was rejected during verification. Each case carries the expected
          * vs. received pair so the rejection can be diagnosed from a single log line.
          */
        enum Rejection:
            case BatchNumMismatch(expected: Batch.Number, received: Batch.Number)
            case AckPeerMismatch(expected: PeerId, received: HeadPeerNumber)
            case AckNumMismatch(expected: SoftAckNumber, received: SoftAckNumber)
            case BlockNumMismatch(expected: BlockNumber, received: BlockNumber)
            case StackBriefNumMismatch(expected: StackNumber, received: StackNumber)
            case HardAckPeerMismatch(expected: PeerId, received: PeerId)
            case HardAckNumMismatch(expected: HardAckNumber, received: HardAckNumber)
            case HubCoilAckNumMismatch(expected: HubCoilAckNumber, received: HubCoilAckNumber)
            case RequestsHeadMismatch(expected: RequestNumber, received: RequestNumber)
            case RequestsNotConsecutive(prev: RequestId, next: RequestId)

            override def toString: String = this match
                case BatchNumMismatch(e, r) =>
                    s"BatchNumMismatch(expected=$e, received=$r)"
                case AckPeerMismatch(e, r) =>
                    s"AckPeerMismatch(expected=$e, received=$r)"
                case AckNumMismatch(e, r) =>
                    s"AckNumMismatch(expected=$e, received=$r)"
                case BlockNumMismatch(e, r) =>
                    s"BlockNumMismatch(expected=$e, received=$r)"
                case StackBriefNumMismatch(e, r) =>
                    s"StackBriefNumMismatch(expected=$e, received=$r)"
                case HardAckPeerMismatch(e, r) =>
                    s"HardAckPeerMismatch(expected=$e, received=$r)"
                case HardAckNumMismatch(e, r) =>
                    s"HardAckNumMismatch(expected=$e, received=$r)"
                case HubCoilAckNumMismatch(e, r) =>
                    s"HubCoilAckNumMismatch(expected=$e, received=$r)"
                case RequestsHeadMismatch(e, r) =>
                    s"RequestsHeadMismatch(expected=$e, received=$r)"
                case RequestsNotConsecutive(p, n) =>
                    s"RequestsNotConsecutive(prev=(${p.peerNum}:${p.requestNum}), " +
                        s"next=(${n.peerNum}:${n.requestNum}))"

    case object PreStart

    /** Local-only self-tick that asks the liaison to re-send its currently outstanding
      * [[GetMsgBatch]] to the remote peer. Never put on the wire — filtered out by
      * [[hydrozoa.multisig.consensus.transport.Frame.fromWire]].
      *
      * ## Design note: keeping the chain alive at the protocol layer
      *
      * The `GetMsgBatch` / `NewMsgBatch` exchange is a single-outstanding-request chain: each side
      * only sends the next `GetMsgBatch` after processing the previous `NewMsgBatch` reply. Message
      * content itself is idempotent — sender queues are only drained on `BlockConfirmed`, and
      * `buildNewMsgBatch` reads via `dropWhile` rather than dequeueing — so no data is lost when a
      * frame goes missing. What is at risk is the chain's "next pull": if a `GetMsgBatch` or its
      * reply does not land, neither side fires again on its own.
      *
      * Two places this responsibility could live:
      *
      *   - '''(A) The transport.''' A "link came back up" callback would let `PeerLiaison` re-emit
      *     `currentlyRequesting` at the moment a reconnect completes. Couples the liaison to a
      *     specific transport's notion of "connection event" and covers only the failures that
      *     transport can observe.
      *   - '''(B) The protocol itself.''' `PeerLiaison` owns a slow periodic resend of its
      *     outstanding `GetMsgBatch`. Transport-agnostic. Equally applicable to clean reconnects,
      *     half-open TCP, paused GC on the peer, frame-corrupting NAT timeouts, or any other path
      *     that leaves the chain stalled without a visible "reconnect" signal.
      *
      * We chose '''(B)'''. The protocol's liveness is a protocol-level concern, not the
      * transport's. It is also cheap to keep healthy: when the chain is already moving, a resend
      * either harmlessly overwrites the remote's stashed `sendBatchImmediately` value, or it
      * triggers a duplicate `NewMsgBatch` that the `advanced` check in the `NewMsgBatch` handler
      * discards without bouncing a `GetMsgBatch` back. Worst-case steady-state cost is one extra
      * round-trip per peer per `peerLiaisonResendInterval`.
      */
    case object ResendCurrent

    object Request {
        type RemoteBroadcast =
            SoftAck | BlockBrief.Next | UserRequestWithId | StackBrief | HardAck | HubCoilAck

        /** Request by a comm actor to its remote comm-actor counterpart for a batch of acks,
          * blocks, stack briefs, hard-acks, and user requests originating from the remote peer.
          *
          * ====Cursor protocol====
          *
          * (See the per-lane summary table just above the `final case class GetMsgBatch` below for
          * a one-glance reference of all five lanes.)
          *
          * All five lanes share ONE '''next-expected''' cursor contract. They differ only in
          * per-link sequence SHAPE, which changes the successor function — not the contract:
          *
          *   - Three are CONTIGUOUS per-peer (every peer produces every element): `ackNum`,
          *     `hardAckNum`, `requestNum`. Successor is `+1`.
          *   - Two are SPARSE per-link (only the round-robin leader produces the element):
          *     `blockNum` and `stackBriefNum`. Successor is the remote's leader schedule
          *     (`nextLeaderBlock` / `nextSlowLeaderStack`); the cursor is precomputed to the next
          *     leader item, so its verify check is the same exact match as the contiguous lanes.
          *
          * The shared next-expected contract:
          *
          *   - The cursor names the NEXT number the requester wants. The responder
          *     (`buildNewMsgBatch`) prunes each queue with `dropWhile(_ < cursor)` and sends the
          *     head. It RETAINS the head until the requester's cursor moves PAST it, so a lost
          *     batch is just re-sent on the next request (retransmit-safe).
          *   - The verifier (`verifyAgainst`) requires the received element's number to equal the
          *     cursor exactly (`received == current`), then advances the cursor to its successor
          *     (`+1` for contiguous lanes; the leader-schedule successor for sparse lanes).
          *   - The initial cursor is each lane's FIRST emitted number, so the very first element
          *     validates against `GetMsgBatch.initial`:
          *     - `requestNum` = 0 — user requests are 0-indexed per peer.
          *     - `hardAckNum` = 0 — the per-peer hard-ack counter is 0-based; it tracks the slow
          *       cycle from stack 0, so the initial stack (once injected) takes 0 = round-1, 1 =
          *       round-2.
          *     - `ackNum` = 1 — `SoftAck.ackNum == blockNum`; block 0 is the config-bootstrapped
          *       block and is never acked over the wire, so the first soft-ack is for block 1.
          *     - `blockNum` = `remotePeerId.nextLeaderBlock(0)` — the first block this remote
          *       leads, skipping block 0 (config-bootstrap, never sent on the wire).
          *     - `stackBriefNum` = `remotePeerId.nextSlowLeaderStack(0)` — the first stack this
          *       remote slow-leads, skipping stack 0 (bootstrap stack, never sent on the wire).
          *   - The producer side (`appendToOutbox`) independently enforces gap-free monotonic
          *     numbering: contiguous lanes use `+1` per emission; sparse lanes use
          *     `ownHeadPeerId.nextLeader…(last)` (this peer only emits items it leads). The wire
          *     stream is therefore always at the cursor's next-expected value — the precondition
          *     for `received == current`.
          *
          * Every lane additionally carries an `OutOfBounds` sanity guard in `buildNewMsgBatch`: the
          * remote's cursor may never exceed the next-expected number after what we've produced
          * (contiguous ⇒ `<= produced + 1`; sparse ⇒ `<= ownId.nextLeader…(produced)`). It never
          * false-fires in correct operation; tripping it means protocol desync/corruption and
          * raises rather than silently stalling.
          *
          * @param batchNum
          *   Batch number that increases by one for every consecutive batch.
          * @param softAckNumber
          *   Next-expected soft-ack number (== block number) from the remote peer.
          * @param blockNum
          *   Next-expected block number from the remote peer — the next leader block of this
          *   remote, computed via `nextLeaderBlock`.
          * @param stackNum
          *   Next-expected stack-brief number from the remote peer — the next slow-leader stack of
          *   this remote, computed via `nextSlowLeaderStack`.
          * @param hardAckNum
          *   Next-expected hard-ack number from the remote peer.
          * @param requestNum
          *   Next-expected user-request number from the remote peer (inclusive).
          */
        // format: off
        // ===== Per-lane batch-protocol summary =====
        //
        // All five lanes are NEXT-EXPECTED. The differences are only in:
        //   1. who produces (every peer vs only the lane's leader), and
        //   2. the successor function (contiguous `+1` vs leader-schedule `f`).
        //
        // Symbols: nX  = highest number ever appended to this outbox (per-lane).
        //          cur = the remote's `GetMsgBatch.X` for this lane.
        //          f   = the lane's leader-schedule function: `nextLeaderBlock` for `blockNum`,
        //                `nextSlowLeaderStack` for `stackBriefNum`.
        //
        //   lane           producer          first emitted #               init cur (per-remote)
        //   ackNum         every peer        1                             1
        //   blockNum       only fast-leader  ownId.nextLeaderBlock(0)      remote.nextLeaderBlock(0)
        //   stackBriefNum  only slow-leader  ownId.nextSlowLeaderStack(0)  remote.nextSlowLeaderStack(0)
        //   hardAckNum     every peer        0                             0
        //   requestNum     every peer        0                             0
        //
        //   family       append (raise iff)   prune    verify (reject iff)  advance        OOB iff
        //   contiguous   nY != nX + 1 *       < cur    recv != cur          recv+1         nX+1 < cur
        //   sparse       nY != ownId.f(nX)    < cur    recv != cur          remote.f(recv) ownId.f(nX) < cur
        //
        // * Bootstrap escape on contiguous lanes: the very first append may carry the lane's
        //   initial number unconditionally (`nY.convert != 0 && …`).
        // format: on
        final case class GetMsgBatch(
            batchNum: Batch.Number,
            softAckNumber: SoftAckNumber,
            blockNum: BlockNumber,
            stackNum: StackNumber,
            hardAckNum: HardAckNumber,
            hubCoilAckNum: HubCoilAckNumber,
            requestNum: RequestNumber
        )

        object GetMsgBatch {
            // Initial cursor values (see the cursor-protocol note above). All lanes are
            // next-expected:
            //   - Contiguous lanes (ack, hardAck, request): each lane's FIRST emitted number —
            //     requests and hard-acks are 0-based; soft-acks (== blockNum) are 1-based
            //     because block 0 is the config-bootstrap block never acked.
            //   - Sparse lanes (block, stackBrief): the remote's first wire-eligible item per its
            //     leader schedule, since `remotePeerId.nextLeader…(0)` skips the out-of-band
            //     bootstrap item 0.
            //
            // Per-remote because the sparse-lane initial cursor depends on which remote peer the
            // liaison is talking to. Tests can use any [[HeadPeerId]] fixture.
            def initial(remotePeer: RemotePeer): GetMsgBatch = GetMsgBatch(
              batchNum = Batch.Number(0),
              softAckNumber = SoftAckNumber.zero.increment,
              blockNum = remotePeer.nextLeaderBlock(BlockNumber.zero).getOrElse(BlockNumber.zero),
              stackNum =
                  remotePeer.nextSlowLeaderStack(StackNumber.zero).getOrElse(StackNumber.zero),
              hardAckNum = HardAckNumber.zero,
              hubCoilAckNum = HubCoilAckNumber.zero,
              requestNum = RequestNumber.zero
            )
        }

        /** Comm actor provides a batch in response to its remote comm-actor counterpart's request.
          *
          * @param batchNum
          *   Batch number matching the one from the request.
          * @param softAck
          *   If provided, a soft block acknowledgment originating from the responder after the
          *   requested [[SoftAckNumber]].
          * @param blockBrief
          *   If provided, a block originating from the responder after the requested
          *   [[BlockNumber]]. The initial block is never sent in a message batch, instead it's
          *   derived from the head config locally.
          * @stackBrief
          *   If provided, the next stack after requested [[StackNumber]]. The initial stack (#0) is
          *   never sent in a message batch, instead it's derived f rom the head config locally.
          * @param hardAck
          *   If provided, a hard stack acknowledgment originating from the responder after the
          *   requested * [[HardAckNum]].
          * @param requests
          *   A possibly empty list of events originating from the responder after the requested
          *   [[RequestNumber]].
          */
        final case class NewMsgBatch(
            batchNum: Batch.Number,
            softAck: Option[SoftAck],
            blockBrief: Option[BlockBrief.Next],
            stackBrief: Option[StackBrief],
            hardAck: Option[HardAck],
            hubCoilAck: Option[HubCoilAck],
            requests: List[UserRequestWithId]
        )
    }

    object Batch {
        type Number = Number.Number

        object Number {
            opaque type Number = Int

            def apply(i: Int): Number = i

            given Conversion[Number, Int] = identity

            given Ordering[Number] with {
                override def compare(x: Number, y: Number): Int =
                    x.compare(y)
            }

            extension (self: Number) def increment: Number = Number(self + 1)
        }
    }
}
