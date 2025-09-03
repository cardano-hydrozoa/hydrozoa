package hydrozoa.multisig.persistence.pure

import cats.effect.IO
import com.suprnation.actor.Actor.ReplyingReceive
import com.suprnation.actor.ReplyingActor

/**
 * Persistence actor is a mock interface to a key-value store (e.g. RocksDB):
 *
 *   - Puts data in (i.e. write/persist)
 *   - Gets data that was put in (i.e. read/retrieve)
 */
object PersistenceActor {
    def create(): IO[PersistenceActor] =
        IO.pure(PersistenceActor())
}

case class PersistenceActor()
    extends ReplyingActor[IO, PersistenceReq, PersistenceResp]{
    override def receive: ReplyingReceive[IO, PersistenceReq, PersistenceResp] =
        PartialFunction.fromFunction({
            case x: PutNewDepositL1 => ???
            case x: PutNewTxL2 => ???
            case x: PutNewBlockL2 => ???
            case x: PutAckBlockL2 => ???
            case x: PutConfirmBlockL2 => ???
            case x: PutCommBatch => ???
            case x: PutL1Effects => ???
            case x: PutCardanoHeadState => ???
            case x: GetL2BlockData => ???
            case x: GetConfirmedLocalEvents => ???
            case x: GetConfirmedL1Effects => ???
        })
}

/**
 * Persistence protocol
 * See diagram: https://app.excalidraw.com/s/9N3iw9j24UW/9eRJ7Dwu42X
 */
sealed trait PersistenceProtocol

/** Requests received by the persistence actor. */
sealed trait PersistenceReq extends PersistenceProtocol

/** Responses sent by the persistence actor. */
sealed trait PersistenceResp extends PersistenceProtocol

/** ==Put/write data into the persistence system== */

/** Generic response to all Put requests. */
case class PutResp(
    ) extends PersistenceResp

/** Persist L1 deposits */
case class PutNewDepositL1 (
    ) extends PersistenceReq

/** Persist L2 transactions */
case class PutNewTxL2 (
    ) extends PersistenceReq

/** Persist L2 blocks */
case class PutNewBlockL2 (
    ) extends PersistenceReq

case class PutAckBlockL2 (
    ) extends PersistenceReq

/** Persist L2 block confirmations (local-only signal) */
case class PutConfirmBlockL2 (
    ) extends PersistenceReq

/** Persist communication batches received from remote communication actors */
case class PutCommBatch (
    ) extends PersistenceReq

/** Persist L1 effects of L2 blocks */
case class PutL1Effects (
    ) extends PersistenceReq

/** Persist the head's latest utxo state in Cardano  */
case class PutCardanoHeadState(
    ) extends PersistenceReq

/** ==Get/read data from the persistence system== */

/** Request data referenced by an L2 block (e.g. L1 deposits and transactions). */
case class GetL2BlockData(
    ) extends PersistenceReq

/** Response to [[GetL2BlockData]]. */
case class GetL2BlockDataResp(
    ) extends PersistenceResp

/**
 * Retrieve local events confirmed by L2 blocks:
 *
 *   - L1 deposits' multi-signed post-dated refund transactions for Cardano.
 *   - L2 transaction IDs
 */
case class GetConfirmedLocalEvents (
    ) extends PersistenceReq

/** Response to [[GetConfirmedLocalEvents]]. */
case class GetConfirmedLocalEventsResp(
    ) extends PersistenceResp

/** Retrieve L1 effects of confirmed L2 blocks. */
case class GetConfirmedL1Effects (
    ) extends PersistenceReq

/** Response to [[GetConfirmedL1Effects]]. */
case class GetConfirmedL1EffectsResp(
    ) extends PersistenceResp
