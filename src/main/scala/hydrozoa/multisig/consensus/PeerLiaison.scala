package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.PeerLiaison.*
import hydrozoa.multisig.consensus.PeerLiaison.Request.*
import hydrozoa.multisig.protocol.types.*
import scala.collection.immutable.Queue

trait PeerLiaison(
    config: Config,
    pendingConnections: MultisigRegimeManager.PendingConnections | PeerLiaison.Connections,
) extends Actor[IO, Request] {
    private val connections = Ref.unsafe[IO, Option[Connections]](None)
    private val state = State()

    private def getConnections: IO[Connections] = for {
        mConn <- this.connections.get
        conn <- mConn.fold(
          IO.raiseError(
            java.lang.Error(
              "Peer liaison is missing its connections to other actors."
            )
          )
        )(IO.pure)
    } yield conn

    private def initializeConnections: IO[Unit] = pendingConnections match {
        case x: MultisigRegimeManager.PendingConnections =>
            for {
                _connections <- x.get
                _ <- connections.set(
                  Some(
                    Connections(
                      blockWeaver = _connections.blockWeaver,
                      consensusActor = _connections.consensusActor,
                      remotePeerLiaison = _connections.remotePeerLiaisons(config.remotePeerId)
                    )
                  )
                )
            } yield ()
        case x: PeerLiaison.Connections => connections.set(Some(x))
    }

    override def preStart: IO[Unit] = initializeConnections

    override def receive: Receive[IO, Request] =
        PartialFunction.fromFunction(req => getConnections.flatMap(receiveTotal(req, _)))

    private def receiveTotal(req: Request, conn: Connections): IO[Unit] =
        req match {
            case x: RemoteBroadcast =>
                for {
                    // Append the event to the corresponding queue in the state
                    _ <- state.appendToOutbox(x)
                    // Check whether the next batch must be sent immediately, and then turn off the flag regardless.
                    mbBatchReq <- state.dischargeSendNextBatchImmediately
                    // Pretend we just received the cached batch request that we need to send immediately (if any)
                    _ <- mbBatchReq.fold(IO.unit)(batchReq => receiveTotal(batchReq, conn))
                } yield ()
            case x: GetMsgBatch =>
                for {
                    eNewBatch <- state.buildNewMsgBatch(x, config.maxLedgerEventsPerBatch)
                    _ <- eNewBatch match {
                        case Right(newBatch) =>
                            conn.remotePeerLiaison ! newBatch
                        case Left(state.EmptyNewMsgBatch) =>
                            state.sendNextBatchImmediatelyUponNewMsg(x)
                        case Left(state.OutOfBoundsGetMsgBatch) =>
                            IO.raiseError(Error("Incorrect bounds in GetMsgBatch."))
                    }
                } yield ()

            case x: NewMsgBatch =>
                for {
                    next: GetMsgBatch <- state.verifyBatchAndGetNext(x)
                    _ <- conn.remotePeerLiaison ! next
                    _ <- x.ack.traverse_(conn.consensusActor ! _)
                    _ <- x.block.traverse_(conn.blockWeaver ! _)
                    _ <- x.events.traverse_(conn.blockWeaver ! _)
                } yield ()

            // TODO: when the final block is confirmed, inform the counterpart that
            //   we no longer need to receive any blocks, acks, or events from them.
            //   When both sides have received the final confirmed block, the connection can be closed and
            //   the peer liaison can terminate.
            case x: BlockConfirmed => state.dequeueConfirmed(x)
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
          * numbers indicated in a [[GetMsgBatch]] request, up to the configured limits.
          *
          * @param batchReq
          *   the [[GetMsgBatch]] request
          * @param maxEvents
          *   the maximum number of ledger events to include in the [[NewMsgBatch]].
          *
          * Note: if the [[GetMsgBatch]] bounds are lower than the queued messages, then the comm
          * actor would need to query the database to properly respond. Currently, we just respond
          * as if no messages are available to send. However, this should can only happen in rare
          * recovery scenarios because we don't remove elements from the outbox queues until they
          * are confirmed in blocks. Until the persistence system is implemented, such
          * [[GetMsgBatch]] requests will be ignored.
          */

        def buildNewMsgBatch(
            batchReq: GetMsgBatch,
            maxEvents: Int
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

                mAck <- this.qAck.get.map(_.dropWhile(_.id._2 <= batchReq.ackNum).headOption)
                mBlock <- this.qBlock.get.map(_.dropWhile(_.id <= batchReq.blockNum).headOption)
                events <- this.qEvent.get.map(
                  _.dropWhile(_.eventId._2 <= batchReq.eventNum).take(maxEvents)
                )
            } yield NewMsgBatch(
              batchNum = batchReq.batchNum,
              ack = mAck,
              block = mBlock,
              events = events.toList
            )).attemptNarrow

        def dequeueConfirmed(x: BlockConfirmed): IO[Unit] = {
            import x.*
            val blockNum: Block.Number = block.blockNum
            val ackNum: AckBlock.Number = AckBlock.Number.neededToConfirm(block)
            val eventNum: LedgerEventId.Number = block.body.events.collect {
                case x if x._1.peerNum == config.ownPeerId.peerNum => x._1.eventNum
            }.max
            for {
                _ <- this.qAck.update(q => q.dropWhile(_.id._2 <= ackNum))
                _ <- this.qBlock.update(q => q.dropWhile(_.id <= blockNum))
                _ <- this.qEvent.update(q => q.dropWhile(_.eventId.eventNum <= eventNum))
            } yield ()
        }

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
                if correctBatchNum && correctAckNum && correctBlockNum && correctReceivedEventIds
                then
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
    def apply(
        config: Config,
        pendingLocalConnections: MultisigRegimeManager.PendingConnections,
    ): IO[PeerLiaison] =
        IO(new PeerLiaison(config, pendingLocalConnections) {})

    final case class Config(
        ownPeerId: Peer.Id,
        remotePeerId: Peer.Id,
        maxLedgerEventsPerBatch: Int = 25
    )

    final case class Connections(
        blockWeaver: BlockWeaver.Handle,
        consensusActor: ConsensusActor.Handle,
        remotePeerLiaison: PeerLiaison.Handle
    ) extends MultisigRegimeManager.Connections.BlockWeaver,
          MultisigRegimeManager.Connections.ConsensusActor

    type Handle = ActorRef[IO, Request]

    type Request = RemoteBroadcast | GetMsgBatch | NewMsgBatch | BlockConfirmed

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

        final case class BlockConfirmed(
            block: Block.Next
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
