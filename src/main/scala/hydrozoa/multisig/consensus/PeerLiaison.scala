package hydrozoa.multisig.consensus

import cats.effect.{Deferred, IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.multisig.consensus.PeerLiaison.*
import hydrozoa.multisig.consensus.PeerLiaison.Request.*
import hydrozoa.multisig.protocol.types.*
import scala.collection.immutable.Queue

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
                    // Append the event to the corresponding queue in the state
                    _ <- state.appendToOutbox(x)
                    // Check whether the next batch must be sent immediately, and then turn off the flag regardless.
                    mbBatchReq <- state.dischargeSendNextBatchImmediately
                    // Pretend we just received the cached batch request that we need to send immediately (if any)
                    _ <- mbBatchReq.fold(IO.unit)(batchReq => receiveTotal(batchReq, subs))
                } yield ()
            case x: GetMsgBatch =>
                for {
                    eNewBatch <- state.buildNewMsgBatch(x, config.maxLedgerEventsPerBatch)
                    _ <- eNewBatch match {
                        case Right(newBatch) =>
                            subs.remotePeerLiaison ! newBatch
                        case Left(state.EmptyNewMsgBatch) =>
                            state.sendNextBatchImmediatelyUponNewMsg(x)
                        case Left(state.OutOfBoundsGetMsgBatch) =>
                            IO.raiseError(Error("Incorrect bounds in GetMsgBatch."))
                    }
                } yield ()

            case x: NewMsgBatch =>
                for {
                    next: GetMsgBatch <- state.verifyBatchAndGetNext(x)
                    _ <- subs.remotePeerLiaison ! next
                    _ <- x.ack.traverse_(subs.ackBlock ! _)
                    _ <- x.block.traverse_(subs.newBlock ! _)
                    _ <- x.events.traverse_(subs.newLedgerEvent ! _)
                } yield ()
        }

    private final class State {
        private val currentlyRequesting: Ref[IO, GetMsgBatch] =
            Ref.unsafe[IO, GetMsgBatch](GetMsgBatch.initial)
        private val nAck = Ref.unsafe[IO, AckBlock.Number](AckBlock.Number(0))
        private val nBlock = Ref.unsafe[IO, Block.Number](Block.Number(0))
        private val nEvent = Ref.unsafe[IO, LedgerEventId.Number](LedgerEventId.Number(0))
        private val qAck = Ref.unsafe[IO, Queue[AckBlock]](Queue())
        private val qBlock = Ref.unsafe[IO, Queue[Block.Next]](Queue())
        private val qEvent = Ref.unsafe[IO, Queue[LedgerEvent]](Queue())
        private val sendBatchImmediately = Ref.unsafe[IO, Option[GetMsgBatch]](None)

        /** Check whether there are no acks, blocks, or events queued-up to be sent out. */
        private def queuesAreEmpty: IO[Boolean] =
            for {
                ack <- this.qAck.get.map(_.isEmpty)
                block <- this.qBlock.get.map(_.isEmpty)
                event <- this.qEvent.get.map(_.isEmpty)
            } yield ack && block && event

        infix def appendToOutbox(x: RemoteBroadcast): IO[Unit] =
            x match {
                case y: LedgerEvent =>
                    for {
                        nEvent <- this.nEvent.get
                        nY = y.eventId.eventNum
                        _ <- IO.raiseWhen(nEvent.increment != nY)(
                          Error("Bad LedgerEvent increment.")
                        )
                        _ <- this.nEvent.set(nY)
                        _ <- this.qEvent.update(_ :+ y)
                    } yield ()
                case y: AckBlock =>
                    for {
                        nAck <- this.nAck.get
                        nY = y.id.ackNum
                        _ <- IO.raiseWhen(nAck.increment != nY)(
                          Error("Bad AckBlock increment.")
                        )
                        _ <- this.nAck.set(nY)
                        _ <- this.qAck.update(_ :+ y)
                    } yield ()
                case y: Block.Next =>
                    for {
                        nBlock <- this.nBlock.get
                        nY = y.blockNum
                        _ <- IO.raiseWhen(config.ownPeerId.nextLeaderBlock(nBlock) != nY)(
                          Error("Bad Block.Next increment.")
                        )
                        _ <- this.nBlock.set(nY)
                        _ <- this.qBlock.update(_ :+ y)
                    } yield ()
            }

        /** Make sure a batch is sent to the counterparty as soon as another local ack/block/event
          * arrives.
          */
        def sendNextBatchImmediatelyUponNewMsg(batchReq: GetMsgBatch): IO[Unit] =
            this.sendBatchImmediately.set(Some(batchReq))

        /** Check whether a new batch must be immediately sent, deactivating the flag in the
          * process.
          */
        def dischargeSendNextBatchImmediately: IO[Option[GetMsgBatch]] =
            this.sendBatchImmediately.getAndSet(None)

        sealed trait ExtractNewMsgBatchError extends Throwable

        case object EmptyNewMsgBatch extends ExtractNewMsgBatchError

        case object OutOfBoundsGetMsgBatch extends ExtractNewMsgBatchError

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

                _ <- IO.raiseWhen(
                  nAck < batchReq.ackNum ||
                      nBlock < batchReq.blockNum ||
                      nEvents < batchReq.eventNum
                )(OutOfBoundsGetMsgBatch)

                _ <- this.queuesAreEmpty.ifM(IO.raiseError(EmptyNewMsgBatch), IO.unit)

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
            } yield NewMsgBatch(
              batchNum = batchReq.batchNum,
              ack = mAck,
              block = mBlock,
              events = events.toList
            )).attemptNarrow

        def verifyBatchAndGetNext(receivedBatch: NewMsgBatch): IO[GetMsgBatch] = {
            import receivedBatch.{ack, block, events}
            for {
                current <- this.currentlyRequesting.get
                nextBatchNum = current.batchNum.increment
                nextAckNum = current.ackNum.increment
                nextBlockNum = config.remotePeerId.nextLeaderBlock(current.blockNum)
                nextEventNum = current.eventNum.increment

                correctBatchNum = current.batchNum == receivedBatch.batchNum
                
                // Received ack num (if any) is the increment of the requested ack num
                correctAckNum = ack.forall(x =>
                    x.id.peerNum == config.remotePeerId.peerNum &&
                        x.id.ackNum == nextAckNum
                )

                // Received block num (if any) is the next leader block from the remote peer
                // after the requested block num
                correctBlockNum = block.forall(_.blockNum == nextBlockNum)

                // First received event ID is the increment of the requested event ID
                // and all subsequent received event IDs are consecutive from it.
                correctReceivedEventIds =
                    events.headOption.forall(_.eventId.eventNum == nextEventNum) &&
                        events
                            .map(_.eventId)
                            .iterator
                            .sliding(2)
                            .withPartial(false)
                            .forall(x => x.head.precedes(x(1)))
            } yield
                if correctBatchNum && correctAckNum && correctBlockNum && correctReceivedEventIds then
                    GetMsgBatch(
                      batchNum = nextBatchNum,
                      ackNum = ack.fold(current.ackNum)(_.id.ackNum),
                      blockNum = block.fold(current.blockNum)(_.blockNum),
                      eventNum = events.lastOption.fold(current.eventNum)(_.eventId.eventNum)
                    )
                else current
        }
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

        object GetMsgBatch {
            def initial: GetMsgBatch = GetMsgBatch(
              batchNum = Batch.Number(0),
              ackNum = AckBlock.Number(0),
              blockNum = Block.Number(0),
              eventNum = LedgerEventId.Number(0)
            )
        }

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
