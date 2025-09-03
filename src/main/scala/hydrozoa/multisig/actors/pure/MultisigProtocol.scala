package hydrozoa.multisig.actors.pure

import cats.effect.{Deferred, IO}
import com.suprnation.actor.ActorRef.NoSendActorRef

import scala.concurrent.duration.FiniteDuration

type PeerId = Int

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

/** A new L1 deposit, including all details about the deposit. */
case class NewDepositL1 (
    ) extends BlockActorReq, CommActorReq, CommBossActorReq

/** An abbreviated notification about a new L1 deposit, omitting any details other than the event key. */
case class NewDepositL1Info (
    ) extends BlockActorReq

/** A new L2 transaction, including all details about the transaction. */
case class NewTxL2 (
    ) extends BlockActorReq, CommActorReq, CommBossActorReq

/** An abbreviated notification about a new L2 transaction, omitting any details other than the event key. */
case class NewTxL2Info (
    ) extends BlockActorReq

/** A new L2 block. */
case class NewBlockL2 (
    ) extends BlockActorReq, CommActorReq, CommBossActorReq

/**
 * A peer's acknowledgement of an L2 block.
 * When a peer's block actor acknowledges a block and receives all other peers' acknowledgement of the block,
 * then the peer can consider the block to be confirmed by multisig consensus. */
case class AckBlockL2 (
    ) extends BlockActorReq, CommActorReq, CommBossActorReq

/** L2 block confirmations (local-only signal) */
case class ConfirmBlockL2 (
    ) extends CardanoActorReq, CommBossActorReq, EventActorReq

/** Request by a comm actor for a communication batch from its remote comm-actor counterpart. */
case class ReqCommBatch (
    ) extends CommActorReq

/** Comm actor provides a communication batch in response to its remote comm-actor counterpart's request. */
case class RespCommBatch (
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
 */
case class SyncLeaderCommResp(
    ) extends CommActorResp

/**
 * Boss-comm actor responds to the block actor that it and all the comm actors are ready to send events to
 * the block actor in '''leader''' mode, as soon as the block actor completes the [[Deferred]] value provided in
 * the request. Sent upon receiving [[SyncLeaderCommResp]] from all the comm actors.
 */
case class SyncLeaderBossCommResp(
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
 */
case class SyncFollowerCommResp(
    ) extends CommActorResp

/**
 * Boss-comm actor responds to the block actor that it and all the comm actors are ready to send events to
 * the block actor in '''follower''' mode, as soon as the block actor completes the [[Deferred]] value provided in
 * the request. Sent upon receiving [[SyncFollowerCommResp]] from all the comm actors.
 */
case class SyncFollowerBossCommResp(
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
