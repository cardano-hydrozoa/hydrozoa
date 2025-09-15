package hydrozoa.multisig.persistence

import cats.effect.IO
import cats.effect.Ref
import cats.implicits._
import com.suprnation.actor.Actor.ReplyingReceive
import com.suprnation.actor.ReplyingActor
import hydrozoa.multisig.protocol.*
import hydrozoa.multisig.protocol.Identifiers.*
import hydrozoa.multisig.protocol.ConsensusProtocol.*
import hydrozoa.multisig.protocol.PersistenceProtocol.Persistence.*
import hydrozoa.multisig.protocol.PersistenceProtocol.Persistence.PutResponse.*

import scala.collection.immutable.TreeMap

/** Persistence actor is a mock interface to a key-value store (e.g. RocksDB):
  *
  *   - Puts data into the store (i.e. write/persist)
  *   - Gets data that was put into the store (i.e. read/retrieve)
  */
object Persistence {
    def apply(): IO[Persistence] =
        IO(new Persistence {})
}

trait Persistence extends ReplyingActor[IO, Request, Response] {
    private val acks = Ref.unsafe[IO, TreeMap[AckId, AckBlock]](TreeMap())
    private val batches = Ref.unsafe[IO, TreeMap[BatchId, GetMsgBatch]](TreeMap())
    private val blocks = Ref.unsafe[IO, TreeMap[BlockId, NewBlock]](TreeMap())
    private val events = Ref.unsafe[IO, TreeMap[LedgerEventId, NewLedgerEvent]](TreeMap())
    private val confirmedBlock = Ref.unsafe[IO, Option[BlockId]](None)
    
    override def receive: ReplyingReceive[IO, Request, Response] =
        PartialFunction.fromFunction({
            case PersistRequest(data) =>
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
                            events.update(m => m ++ x.events.map(y => y.id -> y)) >>
                            PutSucceeded.pure
                }
            case x: PutL1Effects            => ???
            case x: PutCardanoHeadState     => ???
            case x: GetBlockData            => ???
            case x: GetConfirmedLocalEvents => ???
            case x: GetConfirmedL1Effects   => ???
        })
}
