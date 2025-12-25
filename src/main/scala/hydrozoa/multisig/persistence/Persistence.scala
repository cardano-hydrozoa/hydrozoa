package hydrozoa.multisig.persistence

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ReplyingActor
import hydrozoa.lib.actor.SyncRequest
import hydrozoa.multisig.protocol.*
import hydrozoa.multisig.protocol.ConsensusProtocol.*
import hydrozoa.multisig.protocol.PersistenceProtocol.Persistence.*
import hydrozoa.multisig.protocol.PersistenceProtocol.Persistence.PutResponse.*
import hydrozoa.multisig.protocol.types.{AckBlock, Batch, Block, LedgerEvent}
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
    private val acks = Ref.unsafe[IO, TreeMap[AckBlock.Id, AckBlock]](TreeMap())
    private val batches = Ref.unsafe[IO, TreeMap[Batch.Id, GetMsgBatch]](TreeMap())
    private val blocks = Ref.unsafe[IO, TreeMap[Block.Number, Block]](TreeMap())
    private val events = Ref.unsafe[IO, TreeMap[LedgerEvent.Id, NewLedgerEvent]](TreeMap())
    private val confirmedBlock = Ref.unsafe[IO, Option[Block.Number]](None)

    override def receive: Receive[IO, Request] = PartialFunction.fromFunction(receiveTotal)

    private def receiveTotal(req: Request): IO[Unit] = req match {
        case req: SyncRequest.Any =>
            req.request match {
                case r: PersistRequest =>
                    r.handleSync(req, handlePersistRequest)
                case r: PutL1Effects.type =>
                    r.handleSync(req, _ => IO.pure(PutSucceeded))

                case r: PutCardanoHeadState.type =>
                    r.handleSync(req, _ => IO.pure(PutSucceeded))

                case r: GetBlockData.type =>
                    r.handleSync(req, _ => IO.pure(GetBlockDataResp()))

                case r: GetConfirmedL1Effects.type =>
                    r.handleSync(req, _ => IO.pure(GetConfirmedL1EffectsResp()))

                case r: GetConfirmedLocalEvents.type =>
                    r.handleSync(req, _ => IO.pure(GetConfirmedLocalEventsResp()))
            }
    }

    def handlePersistRequest(req: PersistRequest): IO[PutResponse] = for {
        _ <- req.data match {
            case x: NewLedgerEvent =>
                events.update(m => m + (x.event.eventId -> x))
            case x: Block =>
                blocks.update(m => m + (x.id -> x))
            case x: AckBlock =>
                acks.update(m => m + (x.id -> x))
            case x: ConfirmBlock =>
                confirmedBlock.update(_ => Some(x.id))
            case x: NewMsgBatch =>
                batches.update(m => m + (x.id -> x.nextGetMsgBatch)) >>
                    x.ack.traverse_(y => acks.update(m => m + (y.id -> y))) >>
                    x.block.traverse_(y => blocks.update(m => m + (y.id -> y))) >>
                    events.update(m => m ++ x.events.map(y => y.event.eventId -> y))
        }
    } yield PutSucceeded
}
