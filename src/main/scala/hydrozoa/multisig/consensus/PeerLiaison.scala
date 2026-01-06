package hydrozoa.multisig.consensus

import cats.effect.{Deferred, IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import hydrozoa.multisig.consensus.PeerLiaison.{Config, ConnectionsPending, MaxEvents}
import hydrozoa.multisig.protocol.ConsensusProtocol.*
import hydrozoa.multisig.protocol.ConsensusProtocol.PeerLiaison.*
import hydrozoa.multisig.protocol.PersistenceProtocol.*
import hydrozoa.multisig.protocol.types.*
import scala.annotation.targetName
import scala.collection.immutable.Queue

/** Communication actor is connected to its counterpart at another peer:
  *
  *   - Requests communication batches from the counterpart.
  *   - Responds to the counterpart's requests for communication batches.
  */
object PeerLiaison {
    type MaxEvents = Int

    final case class Config(
        peerId: Peer.Number,
        remotePeerId: Peer.Number,
        maxLedgerEventsPerBatch: MaxEvents = 25,
        persistence: Persistence.Ref
    )

    final case class ConnectionsPending(
        blockWeaver: Deferred[IO, BlockWeaver.Ref],
        remotePeerLiaison: Deferred[IO, PeerLiaisonRef]
    )

    def apply(config: Config, connections: ConnectionsPending): IO[PeerLiaison] =
        IO(new PeerLiaison(config, connections) {})
}

trait PeerLiaison(config: Config, connections: ConnectionsPending) extends Actor[IO, Request] {
    private val subscribers = Ref.unsafe[IO, Option[Subscribers]](None)
    private val state = State()

    private final case class Subscribers(
        ackBlock: AckBlock.Subscriber,
        newBlock: Block.Subscriber,
        newLedgerEvent: LedgerEvent.Subscriber,
        remotePeerLiaison: PeerLiaisonRef
    )

    override def preStart: IO[Unit] =
        for {
            blockProducer <- connections.blockWeaver.get
            // This means that the comm actor will not start receiving until it is connected to its
            // remote counterpart:
            remotePeerLiaison <- connections.remotePeerLiaison.get
            _ <- subscribers.set(
              Some(
                Subscribers(
                  /*
                    AckBlock -> Consensus
                    Block -> Weaver
                    LedgerEvent -> Weaver
                   */
                  ackBlock = ???, // blockProducer,
                  newBlock = blockProducer,
                  newLedgerEvent = blockProducer,
                  remotePeerLiaison = remotePeerLiaison
                )
              )
            )
        } yield ()

    override def receive: Receive[IO, Request] = PartialFunction.fromFunction(req =>
        subscribers.get.flatMap {
            case Some(subs) =>
                this.receiveTotal(req, subs)
            case _ =>
                IO.raiseError(
                  Error(
                    "Impossible: Comm actor is receiving before its preStart provided subscribers."
                  )
                )
        }
    )

    private def receiveTotal(req: Request, subs: Subscribers): IO[Unit] =
        req match {
            case x: RemoteBroadcast.Request =>
                for {
                    // Check whether the next batch must be sent immediately, and then turn off the flag regardless.
                    mBatchId <- state.dischargeSendNextBatchImmediately
                    _ <- mBatchId match {
                        case None =>
                            // Queue up the message for a future message batch
                            state.:+(x)
                        case Some(batchId) =>
                            // Immediately send a batch containing only this message
                            state
                                .immediateNewMsgBatch(batchId, x)
                                .flatMap(subs.remotePeerLiaison ! _)
                    }
                } yield ()
            case x: GetMsgBatch =>
                for {
                    eNewBatch <- state.extractNewMsgBatch(x, config.maxLedgerEventsPerBatch)
                    _ <- eNewBatch match {
                        case Right(newBatch) =>
                            subs.remotePeerLiaison ! newBatch
                        case Left(state.EmptyNewMsgBatch) =>
                            state.sendNextBatchImmediatelyUponNewMsg(x.id)
                        case Left(state.OutOfBoundsGetMsgBatch) =>
                            IO.raiseError(Error("Incorrect bounds in GetMsgBatch."))
                    }
                } yield ()
            case x: NewMsgBatch =>
                for {
                    _ <- config.persistence ?: Persistence.PersistRequest(x)
                    _ <- subs.remotePeerLiaison ! x.nextGetMsgBatch
                    _ <- x.ack.traverse_(subs.ackBlock ! _)
                    _ <- x.block.traverse_(subs.newBlock ! _)
                    _ <- x.events.traverse_(subs.newLedgerEvent ! _)
                } yield ()
        }

    private final class State {
        private val nAck = Ref.unsafe[IO, AckBlock.Number](AckBlock.Number(0))
        private val nBlock = Ref.unsafe[IO, Block.Number](Block.Number(0))
        private val nEvent = Ref.unsafe[IO, LedgerEventId.Number](LedgerEventId.Number(0))
        private val qAck = Ref.unsafe[IO, Queue[AckBlock]](Queue())
        private val qBlock = Ref.unsafe[IO, Queue[Block]](Queue())
        private val qEvent = Ref.unsafe[IO, Queue[LedgerEvent]](Queue())
        private val sendBatchImmediately = Ref.unsafe[IO, Option[Batch.Id]](None)

        /** Check whether there are no acks, blocks, or events queued-up to be sent out. */
        private def areEmptyQueues: IO[Boolean] =
            for {
                ack <- this.qAck.get.map(_.isEmpty)
                block <- this.qBlock.get.map(_.isEmpty)
                event <- this.qEvent.get.map(_.isEmpty)
            } yield ack && block && event

        @targetName("append")
        infix def :+(x: RemoteBroadcast.Request): IO[Unit] =
            x match {
                case y: LedgerEvent =>
                    for {
                        _ <- this.nEvent.update(_.increment)
                        _ <- this.qEvent.update(_ :+ y)
                    } yield ()
                // FIXME:
                // case y: AckBlock =>
                //    for {
                //        _ <- this.nAck.update(_.increment)
                //        _ <- this.qAck.update(_ :+ y)
                //    } yield ()
                case y: Block =>
                    for {
                        _ <- this.nBlock.update(_.increment)
                        _ <- this.qBlock.update(_ :+ y)
                    } yield ()
            }

        /** Make sure a batch is sent to the counterparty as soon as another local ack/block/event
          * arrives.
          */
        def sendNextBatchImmediatelyUponNewMsg(batchId: Batch.Id): IO[Unit] =
            this.sendBatchImmediately.set(Some(batchId))

        /** Check whether a new batch must be immediately sent, deactivating the flag in the
          * process.
          */
        def dischargeSendNextBatchImmediately: IO[Option[Batch.Id]] =
            this.sendBatchImmediately.getAndSet(None)

        sealed trait ExtractNewMsgBatchError extends Throwable

        case object EmptyNewMsgBatch extends ExtractNewMsgBatchError

        case object OutOfBoundsGetMsgBatch extends ExtractNewMsgBatchError

        /** Given a locally-sourced ack, block, or event that just arrived, assuming that the next
          * message batch must be sent immediately, construct that [[NewMsgBatch]]. The state's
          * queues message queues must be empty, and the corresponding counter is incremented
          * depending on the arrived message's type.
          */
        def immediateNewMsgBatch(batchId: Batch.Id, x: RemoteBroadcast.Request): IO[NewMsgBatch] =
            for {
                nAck <- this.nAck.get
                nBlock <- this.nBlock.get
                nEvents <- this.nEvent.get
                newBatch <- x match {
                    case y: LedgerEvent =>
                        for {
                            nEventsNew <- this.nEvent.updateAndGet(_.increment)
                        } yield NewMsgBatch(batchId, nAck, nBlock, nEventsNew, None, None, List(y))
                    // FIXME:
                    // case y: AckBlock =>
                    //    for {
                    //        nAckNew <- this.nAck.updateAndGet(_.increment)
                    //    } yield NewMsgBatch(
                    //      batchId,
                    //      nAckNew,
                    //      nBlock,
                    //      nEvents,
                    //      Some(y),
                    //      None,
                    //      List()
                    //    )
                    case y: Block =>
                        for {
                            nBlockNew <- this.nBlock.updateAndGet(_.increment)
                        } yield NewMsgBatch(
                          batchId,
                          nAck,
                          nBlockNew,
                          nEvents,
                          None,
                          Some(y),
                          List()
                        )
                }
            } yield newBatch

        /** Construct a [[NewMsgBatch]] containing acks, blocks, and events starting '''after''' the
          * numbers indicated in a [[GetMsgBatch]] request, up to the configured limits. The state
          * is modified to keep only acks, blocks, and events with numbers '''after''' those
          * provided in the returned [[NewMsgBatch]].
          *
          * @param batchReq
          *   the [[GetMsgBatch]] request
          * @param maxEvents
          *   the maximum number of ledger events to include in the [[NewMsgBatch]].
          */
        // TODO: if the [[GetMsgBatch]] bounds are lower than the queued messages, then the comm actor will need to
        // query the database to properly respond. Currently, we just respond as if no messages are available to send.
        def extractNewMsgBatch(
            batchReq: GetMsgBatch,
            maxEvents: MaxEvents
        ): IO[Either[ExtractNewMsgBatchError, NewMsgBatch]] =
            (for {
                nAck <- this.nAck.get
                nBlock <- this.nBlock.get
                nEvents <- this.nEvent.get

                _ <-
                    if nAck < batchReq.ackNum || nBlock < batchReq.blockNum || nEvents < batchReq.eventNum
                    then {
                        IO.raiseError(OutOfBoundsGetMsgBatch)
                    } else {
                        IO.pure(())
                    }

                _ <- this.areEmptyQueues.ifM(IO.raiseError(EmptyNewMsgBatch), IO.pure(()))

                mAck <- this.qAck.modify(q =>
                    val dropped = q.dropWhile(_.id._2 <= batchReq.ackNum)
                    dropped.dequeueOption.fold((dropped, None))((x, xs) => (xs, Some(x)))
                )
                mBlock <- this.qBlock.modify(q =>
                    val dropped = q.dropWhile(_.id <= batchReq.blockNum)
                    dropped.dequeueOption.fold((dropped, None))((x, xs) => (xs, Some(x)))
                )
                events <- this.qEvent.modify(q =>
                    val dropped = q.dropWhile(_.eventId._2 <= batchReq.eventNum)
                    dropped.splitAt(maxEvents).swap
                )

                ackNum = mAck.fold(nAck)(_.id.ackNum)
                blockNum = mBlock.fold(nBlock)(_.id)
                eventNum = events.lastOption.fold(nEvents)(_.eventId.eventNum)
            } yield NewMsgBatch(
              id = batchReq.id,
              ackNum = ackNum,
              blockNum = blockNum,
              eventNum = eventNum,
              ack = mAck,
              block = mBlock,
              events = events.toList
            )).attemptNarrow
    }
}
