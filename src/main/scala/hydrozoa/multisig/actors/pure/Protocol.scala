package hydrozoa.multisig.actors.pure

import cats.effect.IO
import com.suprnation.actor.ActorRef.NoSendActorRef

import scala.concurrent.duration.FiniteDuration

type PeerId = Int

/**
 * # Multisig regime protocol requests and responses
 * See diagram: https://app.excalidraw.com/s/9N3iw9j24UW/9eRJ7Dwu42X
 */
sealed trait Protocol

sealed trait ActorReq extends Protocol
sealed trait ActorResp extends Protocol

/** ## Actors receiving requests */
/**
 * Multisig regime actor starts-up and monitors all the actors of the multisig regime.
 *
 */
sealed trait ActorReqMultisigRegime extends ActorReq
case class TerminatedClock(actorRef: NoSendActorRef[IO]) extends ActorReqMultisigRegime

/**
 * Block actor:
 *
 *   - When leader, receives L1 deposits + L2 txs and packages them into a new block.
 *   - When follower, receives L2 blocks and broadcasts L2 block acks for valid blocks.
 *   - When leader or follower, collects L2 block acks to confirm block effects and trigger leader/follower switch.
 */
sealed trait ActorReqBlock extends ActorReq
/**
 * Cardano actor:
 *
 *   - Keeps track of confirmed L1 effects of L2 blocks.
 *   - Periodically polls the Cardano blockchain for the head's utxo state.
 *   - Submits whichever L1 effects are not yet reflected in the Cardano blockchain.
 */
sealed trait ActorReqCardano extends ActorReq
/**
 * Cardano blockchain actor is a mock interface to the Cardano blockchain:
 *
 *   - Receives L1 effects
 *   - Responds to queries about utxo state.
 */
sealed trait ActorReqCardanoBlockchain extends ActorReq
/**
 * Clock actor provides time, monotonically increasing after each request.
 */
sealed trait ActorReqClock extends ActorReq
/**
 * Communication actor is connected to its counterpart at another peer:
 *
 *   - Requests communication batches from the counterpart.
 *   - Responds to the counterpart's requests for communication batches.
 */
sealed trait ActorReqComm extends ActorReq

/**
 * Communication-boss actor synchronizes and broadcasts requests to communication actors.
 */
sealed trait ActorReqCommBoss extends ActorReq

/**
 * Database actor is a mock interface to a key-value store (e.g. RocksDB):
 *
 *   - Puts data in (i.e. write/persist)
 *   - Gets data that was put in (i.e. read/retrieve)
 */
sealed trait ActorReqDatabase extends ActorReq
/**
 * Event actor is the source of new L1 deposits and L2 transactions for the head.
 */
sealed trait ActorReqEvent extends ActorReq

/** ## Actors responding to synchronous requests */
sealed trait ActorRespClock extends ActorResp
sealed trait ActorRespCardanoBlockchain extends ActorResp
sealed trait ActorRespComm extends ActorResp
sealed trait ActorRespCommBoss extends ActorResp
sealed trait ActorRespDatabase extends ActorResp

/** ## Async requests */

/** L1 deposits */
case class NewDepositL1 (
    ) extends ActorReqBlock, ActorReqComm, ActorReqCommBoss

case class NewDepositL1Info (
    ) extends ActorReqBlock

/** L2 transactions */
case class NewTxL2 (
    ) extends ActorReqBlock, ActorReqComm, ActorReqCommBoss

case class NewTxL2Info (
    ) extends ActorReqBlock

/** L2 blocks */
case class NewBlockL2 (
    ) extends ActorReqBlock, ActorReqComm, ActorReqCommBoss

case class AckBlockL2 (
    ) extends ActorReqBlock, ActorReqComm, ActorReqCommBoss

/** L2 block confirmations (local-only signal) */
case class ConfirmBlockL2 (
    ) extends ActorReqCardano, ActorReqCommBoss, ActorReqEvent

/**
 * Communication batches requested and delivered asynchronously between pairs of local/remote comm actors
 */
case class ReqCommBatch (
    ) extends ActorReqComm

case class RespCommBatch (
    ) extends ActorReqComm

/**
 * Submit L1 effect to the Cardano blockchain
 * The response is ignored, so we model this as an async request in the pure model.
 */
case class SubmitL1Effects (
    ) extends ActorReqCardanoBlockchain

/** ## Sync requests and responses */

/** Block actor's synchronization about its leader/follower status with the comm-boss actor */
case class SyncLeaderBossComm (
    ) extends ActorReqCommBoss

case class RespSyncLeaderBossComm (
    ) extends ActorRespCommBoss

case class SyncFollowerBossComm (
    ) extends ActorReqCommBoss

case class RespSyncFollowerBossComm (
    ) extends ActorRespCommBoss

/** Comm-boss actor's synchronization about block actor's leader/follower status with the local comm actors */
case class SyncLeaderComm (
    ) extends ActorReqComm

case class RespSyncLeaderComm (
    ) extends ActorRespComm

case class SyncFollowerComm (
    ) extends ActorReqComm

case class RespSyncFollowerComm (
    ) extends ActorRespComm

/** Time */
case object GetTime extends ActorReqClock

case class RespTime (
      time: FiniteDuration
    ) extends ActorRespClock

/** Get the head's current utxo state in Cardano */
case class GetHeadStateOnCardano (
    ) extends ActorReqCardanoBlockchain

case class RespHeadStateOnCardano (
    ) extends ActorRespCardanoBlockchain

/** ## Database puts (i.e. writing/persistence) */

/** Generic response to all Put operations */
case class RespPut (
    ) extends ActorRespDatabase

/** Persist L1 deposits */
case class PutNewDepositL1 (
    ) extends ActorReqDatabase

/** Persist L2 transactions */
case class PutNewTxL2 (
    ) extends ActorReqDatabase

/** Persist L2 blocks */
case class PutNewBlockL2 (
    ) extends ActorReqDatabase

case class PutAckBlockL2 (
    ) extends ActorReqDatabase

/** Persist L2 block confirmations (local-only signal) */
case class PutConfirmBlockL2 (
    ) extends ActorReqDatabase

/** Persist communication batches received from remote communication actors */
case class PutCommBatch (
    ) extends ActorReqDatabase

/** Persist L1 effects of L2 blocks */
case class PutL1Effects (
    ) extends ActorReqDatabase

/** Persist the head's latest utxo state in Cardano  */
case class PutHeadStateOnCardano (
    ) extends ActorReqDatabase

/** ## Database gets (i.e. reading/retrieval) */

/** Retrieve data referenced by an L2 block (e.g. L1 deposits and transactions) */
case class GetL2BlockData(
    ) extends ActorReqDatabase

case class RespL2BlockData(
    ) extends ActorRespDatabase

/**
 * Retrieve local events confirmed by L2 blocks:
 *
 *   - L1 deposits' multi-signed post-dated refund transactions for Cardano.
 *   - L2 transaction IDs
 */
case class GetConfirmedLocalEvents (
    ) extends ActorReqDatabase

case class RespConfirmedLocalEvents (
    ) extends ActorRespDatabase

/** Retrieve L1 effects of confirmed L2 blocks. */
case class GetConfirmedL1Effects (
    ) extends ActorReqDatabase

case class RespConfirmedL1Effects (
    ) extends ActorRespDatabase