package hydrozoa.multisig.actors.pure

import cats.effect.{Deferred, IO}
import com.suprnation.actor.ActorRef.NoSendActorRef
import hydrozoa.multisig.ledger.multi.trivial.MultiLedgerEvent

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
sealed trait EventActorReq extends MultisigActorReq

/** ==Actors' responses to synchronous requests== */

/** Clock actor's responses to synchronous requests. */
sealed trait ClockActorResp extends MultisigActorResp

/** Comm actor's responses to synchronous requests. */
sealed trait CommActorResp extends MultisigActorResp

/** Comm-boss actor's responses to synchronous requests. */
sealed trait CommBossActorResp extends MultisigActorResp

/** ==Async requests== */

/** A new multi-ledger ledger event, including all details about the deposit. */
case class NewEvent(
    id: EventId,
    time: FiniteDuration,
    event: MultiLedgerEvent
    ) extends BlockActorReq, CommActorReq, CommBossActorReq

/** An abbreviated notification about a new multi-ledger event, omitting any details other than the event key. */
case class NewEventId(
    id: EventId
    ) extends BlockActorReq

/**
 * A new L2 block.
 * @param id The block ID, increasing by one for every consecutive new block.
 * @param time The creation time of the block.
 * @param blockType The block's type: initial, minor, major, or final.
 * @param requiredEventIds The event number for each peer that the block creator had processed at the moment the block
 *                         was created. Every follower must reach the same event numbers for each peer before attempting
 *                         to verify the block.
 * @param validEvents The sequence of valid events that the block creator has applied to transition the previous block's
 *                    multi-ledger state. Every follower must apply these events in the same order when verifying the
 *                    block.
 * @param invalidEvents The sequence of events that must be invalid when applied after the [[validEvents]].
 */
// * @param absorbedDeposits The set of mature deposits that are absorbed by this block into the head's treasury.
// * @param rejectedDeposits The set of deposits that will never be absorbed into the head's treasury because they
// *                         do not fulfill the absorption criteria.
case class NewBlock(
    id: BlockId,
    time: FiniteDuration,
    blockType: BlockType,
    requiredEventIds: Map[PeerId, EventNum],
    validEvents: List[EventId],
    invalidEvents: List[EventId],
//    absorbedDeposits: List[???],
//    rejectedDeposits: List[???]
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
    id: BlockId,
    time: FiniteDuration
    ) extends CardanoActorReq, EventActorReq

/**
 * Request by a comm actor to its remote comm-actor counterpart for a batch of events, blocks,
 * or block acknowledgements originating from the remote peer.
 * @param id Batch number that increases by one for every consecutive batch.
 * @param ackNum The requester's last seen block acknowledgement from the remote peer.
 * @param blockNum The requester's last seen block from the remote peer.
 * @param eventNum The requester's last seen event number from the remote peer.
 */
case class ReqCommBatch (
    id: BatchNum,
    ackNum: AckNum,
    blockNum: BlockNum,
    eventNum: EventNum
    ) extends CommActorReq

/**
 * Comm actor provides a communication batch in response to its remote comm-actor counterpart's request.
 * @param id Batch number matching the one from the request.
 * @param eventNum The largest event num in this batch.
 * @param ack A block acknowledgment originating from the responder after the requested [[AckNum]].
 * @param block A block originating from the responder after the requested [[BlockNum]].
 * @param events A list of events originating from the responder after the requested [[EventNum]].
 */
case class RespCommBatch (
    id: BatchNum,
    eventNum: EventNum,
    ack: Option[(AckNum, AckBlock)],
    block: Option[(BlockNum, NewBlock)],
    events: List[(EventNum, NewEvent)]
    ) extends CommActorReq

/**
 * ==Synchronous requests and responses==
 * Block actor's synchronization about its leader/follower status with the comm-boss actor.
 */

/** ===Leader mode synchronization=== */

/**
 * Block actor synchronously requests that the comm-boss actor send it events in '''leader''' mode,
 * and to re-broadcast the same synchronous request as [[SyncLeaderComm]] to all the comm actors.
 * @param resumeSignal a [[Deferred]] value that the block actor will complete when
 *                     he's ready to receive new event messages in leader mode.
 */
case class SyncLeaderBossComm (
    resumeSignal: Deferred[IO, Unit]
    ) extends CommBossActorReq

/**
 * Comm-boss actor synchronously requests that the recipient comm actor send events to the block actor
 *  in '''leader''' mode. Sent upon receiving [[SyncLeaderBossComm]] from the block actor.
 * @param resumeSignal a [[Deferred]] value that the block actor will complete when
 *                     he's ready to receive new event messages in leader mode.
 */
case class SyncLeaderComm (
    resumeSignal: Deferred[IO, Unit]
    ) extends CommActorReq

/**
 * Comm actor responds to comm-boss actor that it is ready to send events to the block actor in '''leader''' mode,
 * as soon as the block actor completes the [[Deferred]] value provided in the request.
 * @param remoteEventId the last event ID sent by the comm actor to the block actor before leader mode.
 */
case class SyncLeaderCommResp(
    remoteEventId: EventId
    ) extends CommActorResp

/**
 * Boss-comm actor responds to the block actor that it and all the comm actors are ready to send events to
 * the block actor in '''leader''' mode, as soon as the block actor completes the [[Deferred]] value provided in
 * the request. Sent upon receiving [[SyncLeaderCommResp]] from all the comm actors.
 * @param localEventId the last event ID sent by the boss-comm actor to the block actor before leader mode.
 * @param remoteEventIds the last event IDs sent by the comm actors to the block actor before leader mode.
 */
case class SyncLeaderBossCommResp(
    localEventId: EventId,
    remoteEventIds: Map[PeerId, EventNum]
    ) extends CommBossActorResp

/** ===Follower mode synchronization=== */

/**
 * Block actor synchronously requests that the comm-boss actor send it events in '''follower''' mode,
 * and to re-broadcast the same synchronous request as [[SyncFollowerComm]] to all the comm actors.
 * @param resumeSignal a [[Deferred]] value that the block actor will complete when
 *                     he's ready to receive new event messages in follower mode.
 */
case class SyncFollowerBossComm (
    resumeSignal: Deferred[IO, Unit]
    ) extends CommBossActorReq

/**
 * Comm-boss actor synchronously requests that the recipient comm actor send events to the block actor
 *  in '''follower''' mode. Sent upon receiving [[SyncFollowerBossComm]] from the block actor.
 * @param resumeSignal a [[Deferred]] value that the block actor will complete when
 *                     he's ready to receive new event messages in follower mode.
 */
case class SyncFollowerComm (
    resumeSignal: Deferred[IO, Unit]
    ) extends CommActorReq

/**
 * Comm actor responds to comm-boss actor that it is ready to send events to the block actor in '''follower''' mode,
 * as soon as the block actor completes the [[Deferred]] value provided in the request.
 * @param remoteEventId the last event ID sent by the comm actor to the block actor before follower mode.
 */
case class SyncFollowerCommResp(
    remoteEventId: EventId
    ) extends CommActorResp

/**
 * Boss-comm actor responds to the block actor that it and all the comm actors are ready to send events to
 * the block actor in '''follower''' mode, as soon as the block actor completes the [[Deferred]] value provided in
 * the request. Sent upon receiving [[SyncFollowerCommResp]] from all the comm actors.
 * @param localEventId the last event ID sent by the boss-comm actor to the block actor before follower mode.
 * @param remoteEventIds the last event IDs sent by the comm actors to the block actor before follower mode.
 */
case class SyncFollowerBossCommResp(
    localEventId: EventId,
    remoteEventIds: Map[PeerId, EventNum]
    ) extends CommBossActorResp

/**
 * Request the current timestamp from the clock actor.
 * Guaranteed to be monotonically increasing with each request.
 */
case object GetTime extends ClockActorReq

/**
 * The current timestamp, provided by the clock actor in response to a synchronous request.
 * Guaranteed to be monotonically increasing with each request.
 */
case class GetTimeResp(
    time: FiniteDuration
    ) extends ClockActorResp

/** ==Multisig regime actor's messages== */

/** Received by the multisig regime actor when its clock-actor child terminates. */
case class TerminatedClock(actorRef: NoSendActorRef[IO]) extends MultisigBossActorReq

/** ==Entity identifiers== */

type AckNum = Int
type BlockNum = Int
type BatchNum = Int
type EventNum = Int
type PeerNum = Int

type BlockId = BlockNum
type PeerId = PeerNum
type AckId = (PeerId, AckNum)
type BatchId = (PeerId, BatchNum)
type EventId = (PeerId, EventNum)

type BlockVersionMajor = Int
type BlockVersionMinor = Int

enum BlockType:
    case BlockInit
    case BlockMinor
    case BlockMajor
    case BlockFinal
