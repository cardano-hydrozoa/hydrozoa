package hydrozoa.multisig.actors.pure
import cats.effect.Deferred
import cats.effect.IO
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.actor.ActorRef.NoSendActorRef
import hydrozoa.multisig.ledger.multi.trivial.LedgerEvent
import hydrozoa.multisig.ledger.multi.trivial.LedgerEventOutcome

import scala.concurrent.duration.FiniteDuration

/** =Multisig regime protocol= */

/**
 * Multisig regime's protocol for actor requests and responses.
 * See diagram: [[https://app.excalidraw.com/s/9N3iw9j24UW/9eRJ7Dwu42X]]
 */
sealed trait MultisigRegimeProtocol

/** ==Actors receiving requests== */

/** Requests received by the multisig regime manager. */
sealed trait MultisigRegimeManagerReq extends MultisigRegimeProtocol

/** Requests received by actors in the multisig regime. */
sealed trait MultisigRegimeActorReq extends MultisigRegimeProtocol

/** Requests that must be persisted. */
sealed trait PersistedReq extends MultisigRegimeProtocol

/** Requests received by the block actor. */
sealed trait BlockActorReq extends MultisigRegimeActorReq

/** Requests received by the Cardano actor. */
sealed trait CardanoEventActorReq extends MultisigRegimeActorReq

/** Requests received by the comm actor. */
sealed trait CommActorReq extends MultisigRegimeActorReq

/** Requests received by the event actor. */
sealed trait LedgerEventActorReq extends MultisigRegimeActorReq

/** ==Async requests== */

/** Requests that comm actors should collectively rebroadcast to the other head peers via [[NewMsgBatch]]. */
sealed trait RemoteBroadcastReq extends CommActorReq

/** Submit a new ledger event to the head via a peer's ledger event actor. */
final case class SubmitLedgerEvent(
    event: LedgerEvent,
    eventOutcome: Deferred[IO, LedgerEventOutcome]
    ) extends LedgerEventActorReq

/** Convenience method to create a SubmitLedgerEvent from a LedgerEvent. */
object SubmitLedgerEvent {
    def create(event: LedgerEvent): IO[SubmitLedgerEvent] =
        for {
            eventOutcome <- Deferred[IO, LedgerEventOutcome]
        } yield SubmitLedgerEvent(event, eventOutcome)
}

/**
 * The ledger event actor announces a new multi-ledger ledger event, timestamped and assigned a LedgerEventId. */
final case class NewLedgerEvent(
    id: LedgerEventId,
    time: FiniteDuration,
    event: LedgerEvent
    ) extends BlockActorReq, RemoteBroadcastReq, PersistedReq

/**
 * The block actor announces a new block.
 *
 * @param id                      The block ID, increasing by one for every consecutive new block.
 * @param time                    The creation time of the block.
 * @param blockType               The block's type: initial, minor, major, or final.
 * @param ledgerEventIdsRequired  The event number for each peer that the block creator had processed at the moment the
 *                                block was created. Every follower must reach the same event numbers for each peer
 *                                before attempting to verify the block.
 * @param ledgerEventsValid       The sequence of valid events that the block creator has applied to transition the
 *                                previous block's multi-ledger state. Every follower must apply these events in the
 *                                same order when verifying the block.
 * @param ledgerEventsInvalid     The sequence of events that must be invalid when applied after [[ledgerEventsValid]].
 * @param ledgerCallbacksAccepted The set of mature deposits that are absorbed by this block into the head's treasury.
 * @param ledgerCallbacksRejected The set of deposits that will never be absorbed into the head's treasury because they
 *                                do not fulfill the absorption criteria.
 */
final case class NewBlock(
    id: BlockId,
    time: FiniteDuration,
    blockType: BlockType,
    ledgerEventIdsRequired: Map[PeerId, LedgerEventNum],
    ledgerEventsValid: List[LedgerEventId],
    ledgerEventsInvalid: List[LedgerEventId],
    ledgerCallbacksAccepted: List[LedgerCallbackId],
    ledgerCallbacksRejected: List[LedgerCallbackId]
    ) extends BlockActorReq, RemoteBroadcastReq, PersistedReq

/**
 * A peer's block actor announces its acknowledgement of an L2 block.
 * When a peer's block actor acknowledges a block and receives all other peers' acknowledgement of the block,
 * then the peer can consider the block to be confirmed by multisig consensus.
 * Note that for major and final blocks, two rounds of acknowledgements are needed to confirm.
 */
final case class AckBlock(
    id: AckId,
    time: FiniteDuration
    ) extends BlockActorReq, RemoteBroadcastReq, PersistedReq

/** L2 block confirmations (local-only signal) */
final case class ConfirmBlock(
    id: BlockId
    ) extends CardanoEventActorReq, LedgerEventActorReq, PersistedReq

/**
 * Request by a comm actor to its remote comm-actor counterpart for a batch of events, blocks,
 * or block acknowledgements originating from the remote peer.
 * @param id Batch number that increases by one for every consecutive batch.
 * @param ackNum The requester's last seen block acknowledgement from the remote peer.
 * @param blockNum The requester's last seen block from the remote peer.
 * @param eventNum The requester's last seen event number from the remote peer.
 */
final case class GetMsgBatch(
    id: BatchId,
    ackNum: AckNum,
    blockNum: BlockNum,
    eventNum: LedgerEventNum
    ) extends CommActorReq

/**
 * Comm actor provides a batch in response to its remote comm-actor counterpart's request.
 *
 * @param id       Batch number matching the one from the request.
 * @param ackNum   The latest acknowledgement number originating from the responder.
 * @param blockNum The latest block number originating from the responder.
 * @param eventNum The latest event number originating from the responder.
 * @param ack      If provided, a block acknowledgment originating from the responder after the requested [[AckNum]].
 * @param block    If provided, a block originating from the responder after the requested [[BlockNum]].
 * @param events   A possibly empty list of events originating from the responder after the requested
 *                 [[LedgerEventNum]].
 */
final case class NewMsgBatch(
    id: BatchId,
    ackNum: AckNum,
    blockNum: BlockNum,
    eventNum: LedgerEventNum,
    ack: Option[AckBlock],
    block: Option[NewBlock],
    events: List[NewLedgerEvent]
    ) extends CommActorReq, PersistedReq {
    def nextGetMsgBatch = GetMsgBatch(
        id,
        ackNum,
        blockNum,
        eventNum
    )
}

/** ==Multisig regime manager's messages== */

final case class TerminatedBlockActor(ref: NoSendActorRef[IO]) extends MultisigRegimeManagerReq

final case class TerminatedCardanoEventActor(ref: NoSendActorRef[IO]) extends MultisigRegimeManagerReq

final case class TerminatedCommActor(ref: NoSendActorRef[IO]) extends MultisigRegimeManagerReq

final case class TerminatedLedgerEventActor(ref: NoSendActorRef[IO]) extends MultisigRegimeManagerReq

final case class TerminatedCardanoBackend(ref: NoSendActorRef[IO]) extends MultisigRegimeManagerReq

final case class TerminatedPersistenceActor(ref: NoSendActorRef[IO]) extends MultisigRegimeManagerReq

/** ==Entity identifiers== */

object AckNum:
    opaque type AckNum = Int
    def apply(i: Int): AckNum = i
    given Conversion[AckNum, Int] = identity
    given Ordering[AckNum] with {
        override def compare(x : AckNum, y : AckNum) : Int =
            x.compare(y)
    }
    extension (self: AckNum) def increment: AckNum = AckNum(self + 1)

type AckNum = AckNum.AckNum

object BlockNum:
    opaque type BlockNum = Int
    def apply(i: Int): BlockNum = i

    given Conversion[BlockNum, Int] = identity

    given Ordering[BlockNum] with {
        override def compare(x: BlockNum, y: BlockNum): Int =
            x.compare(y)
    }

    extension (self: BlockNum) def increment: BlockNum = BlockNum(self + 1)

type BlockNum = BlockNum.BlockNum

object BatchNum:
    opaque type BatchNum = Int
    def apply(i: Int): BatchNum = i

    given Conversion[BatchNum, Int] = identity

    given Ordering[BatchNum] with {
        override def compare(x: BatchNum, y: BatchNum): Int =
            x.compare(y)
    }

    extension (self: BatchNum) def increment: BatchNum = BatchNum(self + 1)

type BatchNum = BatchNum.BatchNum

object LedgerEventNum:
    opaque type LedgerEventNum = Int
    def apply(i: Int): LedgerEventNum = i

    given Conversion[LedgerEventNum, Int] = identity

    given Ordering[LedgerEventNum] with {
        override def compare(x: LedgerEventNum, y: LedgerEventNum): Int =
            x.compare(y)
    }

    extension (self: LedgerEventNum) def increment: LedgerEventNum = LedgerEventNum(self + 1)

type LedgerEventNum = LedgerEventNum.LedgerEventNum

object LedgerCallbackNum:
    opaque type LedgerCallbackNum = Int
    def apply(i: Int): LedgerCallbackNum = i

    given Conversion[LedgerCallbackNum, Int] = identity

    given Ordering[LedgerCallbackNum] with {
        override def compare(x: LedgerCallbackNum, y: LedgerCallbackNum): Int =
            x.compare(y)
    }

    extension (self: LedgerCallbackNum) def increment: LedgerCallbackNum = LedgerCallbackNum(self + 1)

type LedgerCallbackNum = LedgerCallbackNum.LedgerCallbackNum

object PeerNum:
    opaque type PeerNum = Int
    def apply(i: Int): PeerNum = i

    given Ordering[PeerNum] with {
        override def compare(x: PeerNum, y: PeerNum): Int =
            x.compare(y)
    }

    given Conversion[PeerNum, Int] = identity

    extension (self: PeerNum) def increment: PeerNum = PeerNum(self + 1)

type PeerNum = PeerNum.PeerNum

type BlockId = BlockNum
type PeerId = PeerNum
type AckId = (PeerId, AckNum)
type BatchId = (PeerId, BatchNum)
type LedgerEventId = (PeerId, LedgerEventNum)
type LedgerCallbackId = (BlockId, LedgerCallbackNum)

object BlockVersionMajor:
    opaque type BlockVersionMajor = Int
    def apply(i: Int): BlockVersionMajor = i

    given Conversion[BlockVersionMajor, Int] = identity

type BlockVersionMajor = BlockVersionMajor.BlockVersionMajor

object BlockVersionMinor:
    opaque type BlockVersionMinor = Int
    def apply(i: Int): BlockVersionMinor = i

    given Conversion[BlockVersionMinor, Int] = identity

type BlockVersionMinor = BlockVersionMinor.BlockVersionMinor

enum BlockType:
    case BlockInit
    case BlockMinor
    case BlockMajor
    case BlockFinal

type BlockActorRef = ActorRef[IO, BlockActorReq]
type CommActorRef = ActorRef[IO, CommActorReq]
type CardanoEventActorRef = ActorRef[IO, CardanoEventActorReq]
type LedgerEventActorRef = ActorRef[IO, LedgerEventActorReq]

type NewLedgerEventSubscriber = ActorRef[IO, NewLedgerEvent]
type NewBlockSubscriber = ActorRef[IO, NewBlock]
type AckBlockSubscriber = ActorRef[IO, AckBlock]
type ConfirmBlockSubscriber = ActorRef[IO, ConfirmBlock]
