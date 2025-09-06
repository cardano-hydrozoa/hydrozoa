package hydrozoa.multisig.persistence.pure

import cats.implicits._
import cats.effect.{IO, Ref}
import com.suprnation.actor.Actor.ReplyingReceive
import com.suprnation.actor.ReplyingActor
import hydrozoa.multisig.actors.pure.{AckBlock, AckId, BatchId, BlockId, ConfirmBlock, GetMsgBatch, LedgerEventId, NewBlock, NewLedgerEvent, NewMsgBatch, PersistedReq}

import scala.collection.immutable

/**
 * Persistence actor is a mock interface to a key-value store (e.g. RocksDB):
 *
 *   - Puts data into the store (i.e. write/persist)
 *   - Gets data that was put into the store (i.e. read/retrieve)
 */
object PersistenceActor {
    def create(): IO[PersistenceActor] = {
        for {
            acks <- Ref[IO].of(immutable.TreeMap[AckId, AckBlock]())
            batches <- Ref[IO].of(immutable.TreeMap[BatchId, GetMsgBatch]())
            blocks <- Ref[IO].of(immutable.TreeMap[BlockId, NewBlock]())
            events <- Ref[IO].of(immutable.TreeMap[LedgerEventId, NewLedgerEvent]())
            confirmedBlock <- Ref[IO].of(Option.empty)
        } yield PersistenceActor()(acks, batches, blocks, events, confirmedBlock)
    }
}

final case class PersistenceActor()(
    private val acks: Ref[IO, immutable.TreeMap[AckId, AckBlock]],
    private val batches: Ref[IO, immutable.TreeMap[BatchId, GetMsgBatch]],
    private val blocks: Ref[IO, immutable.TreeMap[BlockId, NewBlock]],
    private val events: Ref[IO, immutable.TreeMap[LedgerEventId, NewLedgerEvent]],
    private val confirmedBlock: Ref[IO, Option[BlockId]]
    ) extends ReplyingActor[IO, PersistenceReq, PersistenceResp]{
    override def receive: ReplyingReceive[IO, PersistenceReq, PersistenceResp] =
        PartialFunction.fromFunction({
            case PutActorReq(data) =>
                data match {
                    case x: NewLedgerEvent =>
                        events.update(m => m + (x.id -> x)) >>
                            PutSucceeded.pure
                    case x: NewBlock =>
                        blocks.update(m => m + (x.id -> x)) >>
                            PutSucceeded.pure
                    case x: AckBlock =>
                        acks.update(m => m + (x.id -> x)) >>
                            PutSucceeded.pure
                    case x: ConfirmBlock =>
                        confirmedBlock.update(_ => Some(x.id)) >>
                            PutSucceeded.pure
                    case x: NewMsgBatch =>
                        batches.update(m => m + (x.id -> x.nextGetMsgBatch)) >>
                            x.ack.traverse_(y => acks.update(m => m + (y.id -> y))) >>
                            x.block.traverse_(y => blocks.update(m => m + (y.id -> y))) >>
                            x.events.traverse_(y => events.update(m => m ++ y.map(y => y.id -> y))) >>
                            PutSucceeded.pure
                }
            case x: PutL1Effects => ???
            case x: PutCardanoHeadState => ???
            case x: GetBlockData => ???
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

final case class PutActorReq(
    data: PersistedReq
    ) extends PersistenceReq

/** Generic response to all Put requests. */
sealed trait PutResp extends PersistenceResp

/** Successfully persisted the data. */
case object PutSucceeded extends PersistenceResp
//final case class PutFailed(reason: String) extends PersistenceResp

/** Persist L1 effects of L2 blocks */
final case class PutL1Effects (
    ) extends PersistenceReq

/** Persist the head's latest utxo state in Cardano  */
final case class PutCardanoHeadState(
    ) extends PersistenceReq

/** ==Get/read data from the persistence system== */

/** Request data referenced by a block (e.g. multi-ledger events and absorbed/rejected L1 deposits). */
final case class GetBlockData(
    ) extends PersistenceReq

/** Response to [[GetBlockData]]. */
final case class GetBlockDataResp(
    ) extends PersistenceResp

/**
 * Retrieve local events referenced by a confirmed block:
 *
 *   - Event IDs for the L2 transactions and withdrawals referenced by the block.
 *   - Multi-signed deposit and post-dated refund transactions for the deposit events referenced by the block.
 */
final case class GetConfirmedLocalEvents (
    ) extends PersistenceReq

/** Response to [[GetConfirmedLocalEvents]]. */
final case class GetConfirmedLocalEventsResp(
    ) extends PersistenceResp

/** Retrieve L1 effects of confirmed L2 blocks. */
final case class GetConfirmedL1Effects (
    ) extends PersistenceReq

/** Response to [[GetConfirmedL1Effects]]. */
final case class GetConfirmedL1EffectsResp(
    ) extends PersistenceResp
