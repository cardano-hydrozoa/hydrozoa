package hydrozoa.multisig.persistence

import cats.effect.IO
import cats.effect.Ref
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ReplyingActor
import hydrozoa.multisig.consensus.ack.Ack
import hydrozoa.multisig.consensus.batch.Batch
import hydrozoa.multisig.consensus.block.Block
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

trait Persistence extends Actor[IO, Request] {
    private val acks = Ref.unsafe[IO, TreeMap[Ack.Id, AckBlock]](TreeMap())
    private val batches = Ref.unsafe[IO, TreeMap[Batch.Id, GetMsgBatch]](TreeMap())
    private val blocks = Ref.unsafe[IO, TreeMap[Block.Number, NewBlock]](TreeMap())
    private val events = Ref.unsafe[IO, TreeMap[LedgerEventId, NewLedgerEvent]](TreeMap())
    private val confirmedBlock = Ref.unsafe[IO, Option[Block.Number]](None)

    override def receive: Receive[IO, Request] =
        PartialFunction.fromFunction({
            case x: PersistRequest =>
                x.handleRequest({ case PersistRequest(data, dResp) =>
                    for {
                        _ <- data match {
                            case x: NewLedgerEvent =>
                                events.update(m => m + (x.id -> x))
                            case x: NewBlock =>
                                blocks.update(m => m + (x.id -> x))
                            case x: AckBlock =>
                                acks.update(m => m + (x.id -> x))
                            case x: ConfirmBlock =>
                                confirmedBlock.update(_ => Some(x.id))
                            case x: NewMsgBatch =>
                                batches.update(m => m + (x.id -> x.nextGetMsgBatch)) >>
                                    x.ack.traverse_(y => acks.update(m => m + (y.id -> y))) >>
                                    x.block.traverse_(y => blocks.update(m => m + (y.id -> y))) >>
                                    events.update(m => m ++ x.events.map(y => y.id -> y))
                        }
                    } yield PutSucceeded

                })
            case x: PutL1Effects        => x.handleRequest(_ => IO.pure(PutSucceeded))
            case x: PutCardanoHeadState => x.handleRequest(_ => IO.pure(PutSucceeded))
            case x: GetBlockData        => x.handleRequest(_ => IO.pure(GetBlockDataResp()))
            case x: GetConfirmedLocalEvents =>
                x.handleRequest(_ => IO.pure(GetConfirmedLocalEventsResp()))
            case x: GetConfirmedL1Effects =>
                x.handleRequest(_ => IO.pure(GetConfirmedL1EffectsResp()))
        })
}
