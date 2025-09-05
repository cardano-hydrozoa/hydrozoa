package hydrozoa.multisig.actors.pure

import cats.effect.{Deferred, IO}
// import com.suprnation.actor.ActorRef.NoSendActorRef
import hydrozoa.multisig.ledger.multi.trivial.LedgerEvent

import scala.concurrent.duration.FiniteDuration

/** =Multisig regime protocol= */

/**
 * Multisig regime's protocol for actor requests and responses.
 * See diagram: [[https://app.excalidraw.com/s/9N3iw9j24UW/9eRJ7Dwu42X]]
 */
sealed trait MultisigProtocol

/** ==Actors receiving requests== */

/** Requests received by the multisig boss actor. */
sealed trait MultisigBossActorReq extends MultisigProtocol

/** Requests received by actors in the multisig regime. */
sealed trait MultisigActorReq extends MultisigProtocol

/** Responses issued by actors for synchronous requests in the multisig regime. */
sealed trait MultisigActorResp extends MultisigProtocol

/** Requests received by the block actor. */
sealed trait BlockActorReq extends MultisigActorReq

/** Requests received by the Cardano actor. */
sealed trait CardanoActorReq extends MultisigActorReq

/** Requests received by the clock actor. */
sealed trait ClockActorReq extends MultisigActorReq

/** Requests received by the comm actor. */
sealed trait CommActorReq extends MultisigActorReq

/** Requests received by the comm-boss actor. */
sealed trait CommBossActorReq extends MultisigActorReq

/** Requests received by the event actor. */
sealed trait LedgerEventActorReq extends MultisigActorReq

/** ==Async requests== */

/** A new multi-ledger ledger event, including all details about the deposit. */
case class NewLedgerEvent(
    id: LedgerEventId,
    time: FiniteDuration,
    event: LedgerEvent
    ) extends BlockActorReq, CommActorReq, CommBossActorReq

/**
 * A new L2 block.
 *
 * @param id                     The block ID, increasing by one for every consecutive new block.
 * @param time                   The creation time of the block.
 * @param blockType              The block's type: initial, minor, major, or final.
 * @param ledgerEventIdsRequired The event number for each peer that the block creator had processed at the moment the block
 *                               was created. Every follower must reach the same event numbers for each peer before attempting
 *                               to verify the block.
 * @param ledgerEventsValid      The sequence of valid events that the block creator has applied to transition the previous block's
 *                               multi-ledger state. Every follower must apply these events in the same order when verifying the
 *                               block.
 * @param ledgerEventsInvalid          The sequence of events that must be invalid when applied after the [[ledgerEventsValid]].
 */
// * @param absorbedDeposits The set of mature deposits that are absorbed by this block into the head's treasury.
// * @param rejectedDeposits The set of deposits that will never be absorbed into the head's treasury because they
// *                         do not fulfill the absorption criteria.
case class NewBlock(
    id: BlockId,
    time: FiniteDuration,
    blockType: BlockType,
    ledgerEventIdsRequired: Map[PeerId, LedgerEventNum],
    ledgerEventsValid: List[LedgerEventId],
    ledgerEventsInvalid: List[LedgerEventId],
    ledgerCallbacksAccepted: List[LedgerCallbackId],
    ledgerCallbacksRejected: List[LedgerCallbackId]
    ) extends BlockActorReq, CommActorReq, CommBossActorReq

/**
 * A peer's acknowledgement of an L2 block.
 * When a peer's block actor acknowledges a block and receives all other peers' acknowledgement of the block,
 * then the peer can consider the block to be confirmed by multisig consensus. */
case class AckBlock(
    id: AckId,
    time: FiniteDuration
    ) extends BlockActorReq, CommActorReq, CommBossActorReq

/** L2 block confirmations (local-only signal) */
case class ConfirmBlock(
    id: BlockId
    ) extends CardanoActorReq, LedgerEventActorReq

/**
 * Request by a comm actor to its remote comm-actor counterpart for a batch of events, blocks,
 * or block acknowledgements originating from the remote peer.
 * @param id Batch number that increases by one for every consecutive batch.
 * @param ackNum The requester's last seen block acknowledgement from the remote peer.
 * @param blockNum The requester's last seen block from the remote peer.
 * @param eventNum The requester's last seen event number from the remote peer.
 */
case class GetCommBatch(
    id: BatchNum,
    ackNum: AckNum,
    blockNum: BlockNum,
    eventNum: LedgerEventNum
    ) extends CommActorReq

/**
 * Comm actor provides a communication batch in response to its remote comm-actor counterpart's request.
 *
 * @param id       Batch number matching the one from the request.
 * @param eventNum The largest event num in this batch.
 * @param ack      A block acknowledgment originating from the responder after the requested [[AckNum]].
 * @param block    A block originating from the responder after the requested [[BlockNum]].
 * @param events   A list of events originating from the responder after the requested [[LedgerEventNum]].
 */
case class NewCommBatch(
    id: BatchNum,
    eventNum: LedgerEventNum,
    ack: Option[(AckNum, AckBlock)],
    block: Option[(BlockNum, NewBlock)],
    events: List[(LedgerEventNum, NewLedgerEvent)]
    ) extends CommActorReq

/** ==Multisig regime actor's messages== */

// /** Received by the multisig regime actor when its clock-actor child terminates. */
// case class TerminatedClock(actorRef: NoSendActorRef[IO]) extends MultisigBossActorReq

/** ==Entity identifiers== */

type AckNum = Int
type BlockNum = Int
type BatchNum = Int
type LedgerEventNum = Int
type LedgerCallbackNum = Int
type PeerNum = Int

type BlockId = BlockNum
type PeerId = PeerNum
type AckId = (PeerId, AckNum)
type BatchId = (PeerId, BatchNum)
type LedgerEventId = (PeerId, LedgerEventNum)
type LedgerCallbackId = (BlockId, LedgerCallbackNum)

type BlockVersionMajor = Int
type BlockVersionMinor = Int

enum BlockType:
    case BlockInit
    case BlockMinor
    case BlockMajor
    case BlockFinal
