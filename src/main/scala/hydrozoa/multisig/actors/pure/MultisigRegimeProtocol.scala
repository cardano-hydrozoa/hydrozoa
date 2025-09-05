package hydrozoa.multisig.actors.pure

import hydrozoa.multisig.ledger.multi.trivial.LedgerEvent

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

/** Responses issued by actors for synchronous requests in the multisig regime. */
sealed trait MultisigRegimeActorResp extends MultisigRegimeProtocol

/** Requests received by the block actor. */
sealed trait BlockActorReq extends MultisigRegimeActorReq

/** Requests received by the Cardano actor. */
sealed trait CardanoActorReq extends MultisigRegimeActorReq

/** Requests received by the clock actor. */
sealed trait ClockActorReq extends MultisigRegimeActorReq

/** Requests received by the comm actor. */
sealed trait CommActorReq extends MultisigRegimeActorReq

/** Requests received by the event actor. */
sealed trait LedgerEventActorReq extends MultisigRegimeActorReq

/** ==Async requests== */

/** Submit a new ledger event to the head via a peer's ledger event actor. */
case class SubmitLedgerEvent(
    event: LedgerEvent
    ) extends LedgerEventActorReq

/**
 * The ledger event actor announces a new multi-ledger ledger event, timestamped and assigned a LedgerEventId. */
case class NewLedgerEvent(
    id: LedgerEventId,
    time: FiniteDuration,
    event: LedgerEvent
    ) extends BlockActorReq, CommActorReq

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
case class NewBlock(
    id: BlockId,
    time: FiniteDuration,
    blockType: BlockType,
    ledgerEventIdsRequired: Map[PeerId, LedgerEventNum],
    ledgerEventsValid: List[LedgerEventId],
    ledgerEventsInvalid: List[LedgerEventId],
    ledgerCallbacksAccepted: List[LedgerCallbackId],
    ledgerCallbacksRejected: List[LedgerCallbackId]
    ) extends BlockActorReq, CommActorReq

/**
 * A peer's block actor announces its acknowledgement of an L2 block.
 * When a peer's block actor acknowledges a block and receives all other peers' acknowledgement of the block,
 * then the peer can consider the block to be confirmed by multisig consensus.
 * Note that for major and final blocks, two rounds of acknowledgements are needed to confirm.
 */
case class AckBlock(
    id: AckId,
    time: FiniteDuration
    ) extends BlockActorReq, CommActorReq

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

/** ==Multisig regime manager's messages== */

// Mostly these will be concerned with messages about watched actors terminating.

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
