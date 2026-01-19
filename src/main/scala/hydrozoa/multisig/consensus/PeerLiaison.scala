package hydrozoa.multisig.consensus

import cats.effect.{Deferred, IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.multisig.consensus.PeerLiaison.{Config, ConnectionsPending, MaxEvents}
import hydrozoa.multisig.protocol.types.*
import scala.annotation.targetName
import scala.collection.immutable.Queue

import PeerLiaison.{Request, Batch}
import PeerLiaison.Request.*

trait PeerLiaison(config: Config, connections: ConnectionsPending) extends Actor[IO, Request] {
    private val subscribers = Ref.unsafe[IO, Option[Subscribers]](None)
    private val state = State()

    private final case class Subscribers(
        ackBlock: AckBlock.Subscriber,
        newBlock: Block.Subscriber,
        newLedgerEvent: LedgerEvent.Subscriber,
        remotePeerLiaison: PeerLiaison.Handle
    )

    override def preStart: IO[Unit] =
        for {
            blockWeaver <- connections.blockWeaver.get
            consensusActor <- connections.consensusActor.get
            // This means that the comm actor will not start receiving until it is connected to its
            // remote counterpart:
            remotePeerLiaison <- connections.remotePeerLiaison.get
            _ <- subscribers.set(
              Some(
                Subscribers(
                  ackBlock = consensusActor,
                  newBlock = blockWeaver,
                  newLedgerEvent = blockWeaver,
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
            case x: RemoteBroadcast =>
                for {
                    // Check whether the next batch must be sent immediately, and then turn off the flag regardless.
                    mBatchNum <- state.dischargeSendNextBatchImmediately
                    _ <- mBatchNum match {
                        case None =>
                            // Queue up the message for a future message batch
                            state.:+(x)
                        case Some(batchNum) =>
                            // Immediately send a batch containing only this message
                            state
                                .immediateNewMsgBatch(batchNum, x)
                                .flatMap(subs.remotePeerLiaison ! _)
                    }
                } yield ()
            case x: GetMsgBatch =>
                for {
                    eNewBatch <- state.buildNewMsgBatch(x, config.maxLedgerEventsPerBatch)
                    _ <- eNewBatch match {
                        case Right(newBatch) =>
                            subs.remotePeerLiaison ! newBatch
                        case Left(state.EmptyNewMsgBatch) =>
                            state.sendNextBatchImmediatelyUponNewMsg(x.batchNum)
                        case Left(state.OutOfBoundsGetMsgBatch) =>
                            IO.raiseError(Error("Incorrect bounds in GetMsgBatch."))
                    }
                } yield ()
            // TODO: check the new batch against the GetMsgBatch that we're requesting:
            //   - its ack/blocks/events must immediately follow the GetMsgBatch's indices
            //   - its events list must not contain gaps
            //   - for blocks, "immediately follows" means the next block number for which the counterparty is leader
            //     (block number mod N + remotePeerId)
            case x: NewMsgBatch =>
                for {
                    _ <- subs.remotePeerLiaison ! ??? // x.nextGetMsgBatch
                    _ <- x.ack.traverse_(subs.ackBlock ! _)
                    _ <- x.block.traverse_(subs.newBlock ! _)
                    _ <- x.events.traverse_(subs.newLedgerEvent ! _)
                } yield ()
        }

    // TODO: store the current GetMsgBatch that we're requesting
    private final class State {
        private val nAck = Ref.unsafe[IO, AckBlock.Number](AckBlock.Number(0))
        private val nBlock = Ref.unsafe[IO, Block.Number](Block.Number(0))
        private val nEvent = Ref.unsafe[IO, LedgerEventId.Number](LedgerEventId.Number(0))
        private val qAck = Ref.unsafe[IO, Queue[AckBlock]](Queue())
        private val qBlock = Ref.unsafe[IO, Queue[Block.Next]](Queue())
        private val qEvent = Ref.unsafe[IO, Queue[LedgerEvent]](Queue())
        private val sendBatchImmediately = Ref.unsafe[IO, Option[Batch.Number]](None)

        /** Check whether there are no acks, blocks, or events queued-up to be sent out. */
        private def areEmptyQueues: IO[Boolean] =
            for {
                ack <- this.qAck.get.map(_.isEmpty)
                block <- this.qBlock.get.map(_.isEmpty)
                event <- this.qEvent.get.map(_.isEmpty)
            } yield ack && block && event

        @targetName("append")
        infix def :+(x: RemoteBroadcast): IO[Unit] =
            x match {
                case y: LedgerEvent =>
                    for {
                        nEvent <- this.nEvent.get
                        nY = y.eventId.eventNum
                        _ <-
                            if nEvent.increment == nY then { IO.pure(()) }
                            else { IO.raiseError(Error("Bad LedgerEvent increment.")) }
                        _ <- this.nEvent.set(nY)
                        _ <- this.qEvent.update(_ :+ y)
                    } yield ()
                case y: AckBlock =>
                    for {
                        nAck <- this.nAck.get
                        nY = y.id.ackNum
                        _ <-
                            if nAck.increment == nY then { IO.pure(()) }
                            else { IO.raiseError(Error("Bad AckBlock increment.")) }
                        _ <- this.nAck.set(nY)
                        _ <- this.qAck.update(_ :+ y)
                    } yield ()
                case y: Block.Next =>
                    for {
                        nBlock <- this.nBlock.get
                        nY = y.blockNum
                        _ <-
                            if config.ownPeerId.nextLeaderBlock(nBlock) == nY then { IO.pure(()) }
                            else { IO.raiseError(Error("Bad Block.Next increment.")) }
                        _ <- this.nBlock.set(nY)
                        _ <- this.qBlock.update(_ :+ y)
                    } yield ()
            }

        /** Make sure a batch is sent to the counterparty as soon as another local ack/block/event
          * arrives.
          */
        def sendNextBatchImmediatelyUponNewMsg(batchNum: Batch.Number): IO[Unit] =
            this.sendBatchImmediately.set(Some(batchNum))

        /** Check whether a new batch must be immediately sent, deactivating the flag in the
          * process.
          */
        def dischargeSendNextBatchImmediately: IO[Option[Batch.Number]] =
            this.sendBatchImmediately.getAndSet(None)

        sealed trait ExtractNewMsgBatchError extends Throwable

        case object EmptyNewMsgBatch extends ExtractNewMsgBatchError

        case object OutOfBoundsGetMsgBatch extends ExtractNewMsgBatchError

        /** Given a locally-sourced ack, block, or event that just arrived, assuming that the next
          * message batch must be sent immediately, construct that [[NewMsgBatch]]. The state's
          * queues message queues must be empty, and the corresponding counter is incremented
          * depending on the arrived message's type.
          */
        def immediateNewMsgBatch(batchNum: Batch.Number, x: RemoteBroadcast): IO[NewMsgBatch] =
            for {
                nAck <- this.nAck.get
                nBlock <- this.nBlock.get
                nEvents <- this.nEvent.get
                newBatch <- x match {
                    case y: LedgerEvent =>
                        for {
                            nEventsNew <- this.nEvent.updateAndGet(_.increment)
                        } yield NewMsgBatch(batchNum, None, None, List(y))
                    case y: AckBlock =>
                        for {
                            nAckNew <- this.nAck.updateAndGet(_.increment)
                        } yield NewMsgBatch(
                          batchNum,
                          Some(y),
                          None,
                          List()
                        )
                    case y: Block.Next =>
                        for {
                            nBlockNew <- this.nBlock.updateAndGet(_.increment)
                        } yield NewMsgBatch(
                          batchNum,
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
        // FIXME: if the [[GetMsgBatch]] bounds are lower than the queued messages, then the comm actor will need to
        //   query the database to properly respond. Currently, we just respond as if no messages are available to send.
        def buildNewMsgBatch(
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
              batchNum = batchReq.batchNum,
              ack = mAck,
              block = mBlock,
              events = events.toList
            )).attemptNarrow
    }
}

/** A communication actor that is connected to its counterpart at another peer:
  *
  *   - Requests communication batches from the counterpart.
  *   - Responds to the counterpart's requests for communication batches.
  */
object PeerLiaison {
    def apply(config: Config, connections: ConnectionsPending): IO[PeerLiaison] =
        IO(new PeerLiaison(config, connections) {})

    type MaxEvents = Int

    final case class Config(
        ownPeerId: Peer.Id,
        remotePeerId: Peer.Id,
        maxLedgerEventsPerBatch: MaxEvents = 25
    )

    final case class ConnectionsPending(
        blockWeaver: Deferred[IO, BlockWeaver.Handle],
        consensusActor: Deferred[IO, ConsensusActor.Handle],
        remotePeerLiaison: Deferred[IO, PeerLiaison.Handle]
    )

    type Handle = ActorRef[IO, Request]

    type Request = RemoteBroadcast | GetMsgBatch | NewMsgBatch

    object Request {
        type RemoteBroadcast = AckBlock | Block.Next | LedgerEvent

        /** Request by a comm actor to its remote comm-actor counterpart for a batch of events,
          * blocks, or block acknowledgements originating from the remote peer.
          *
          * @param batchNum
          *   Batch number that increases by one for every consecutive batch.
          * @param ackNum
          *   The requester's last seen block acknowledgement from the remote peer.
          * @param blockNum
          *   The requester's last seen block from the remote peer.
          * @param eventNum
          *   The requester's last seen event number from the remote peer.
          */
        final case class GetMsgBatch(
            batchNum: Batch.Number,
            ackNum: AckBlock.Number,
            blockNum: Block.Number,
            eventNum: LedgerEventId.Number
        )

        /** Comm actor provides a batch in response to its remote comm-actor counterpart's request.
          *
          * @param batchNum
          *   Batch number matching the one from the request.
          * @param ack
          *   If provided, a block acknowledgment originating from the responder after the requested
          *   [[AckNum]].
          * @param block
          *   If provided, a block originating from the responder after the requested [[Number]].
          *   The initial block is never sent in a message batch because it's already pre-confirmed
          *   in the head config, which every peer already has locally.
          * @param events
          *   A possibly empty list of events originating from the responder after the requested
          *   [[LedgerEventNum]].
          */
        final case class NewMsgBatch(
            batchNum: Batch.Number,
            ack: Option[AckBlock],
            block: Option[Block.Next],
            events: List[LedgerEvent]
        )
    }

    object Batch {
        type Number = Number.Number

        object Number {
            opaque type Number = Int

            def apply(i: Int): Number = i

            given Conversion[Number, Int] = identity

            given Ordering[Number] with {
                override def compare(x: Number, y: Number): Int =
                    x.compare(y)
            }

            extension (self: Number) def increment: Number = Number(self + 1)
        }
    }
}
