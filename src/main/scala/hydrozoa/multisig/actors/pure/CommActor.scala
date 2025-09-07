package hydrozoa.multisig.actors.pure

import cats.implicits.*
import cats.effect.{Deferred, IO, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}
import hydrozoa.multisig.persistence.pure.{PersistenceActorRef, PutActorReq}

import scala.annotation.targetName
import scala.collection.immutable.Queue

// Not sure why this is needed, but otherwise Scala doesn't allow the companion object's nested classes
// to be used directly in the case class, and it also wrongly says that Subscribers can be private.
import CommActor.{Config, State, ConnectionsPending, Subscribers}

final case class CommActor(config: Config)(
    private val connections: ConnectionsPending
    ) (
    private val subscribers: Ref[IO, Option[Subscribers]],
    private val state: State
    ) extends Actor[IO, CommActorReq]{
    override def preStart: IO[Unit] =
        for {
            blockActor <- connections.blockActor.get
            persistence <- connections.persistence.get
            // This means that the comm actor will not start receiving until it is connected to its
            // remote counterpart:
            remoteCommActor <- connections.remoteCommActor.get
            _ <- subscribers.set(Some(Subscribers(
                ackBlock = blockActor,
                newBlock = blockActor,
                newLedgerEvent = blockActor,
                persistence = persistence,
                remoteCommActor = remoteCommActor
            )))
        } yield ()
    
    override def receive: Receive[IO, CommActorReq] = PartialFunction.fromFunction(req =>
        subscribers.get.flatMap({
            case Some(subs) =>
                this.receiveTotal(req, subs)
            case _ =>
                IO.raiseError(Error("Impossible: Comm actor is receiving before its connections are live."))
        }))

    private def receiveTotal(req: CommActorReq, subs: Subscribers): IO[Unit] =
        req match {
            case x: NewLedgerEvent =>
                state.:+(x)
            case x: NewBlock =>
                state.:+(x)
            case x: AckBlock =>
                state.:+(x)
            case x: GetMsgBatch =>
                for {
                    eNewBatch <- state.extractNewMsgBatch(x, config.maxLedgerEventsPerBatch)
                    _ <- eNewBatch match {
                        case Right(newBatch) =>
                            subs.remoteCommActor ! newBatch
                        case Left(state.EmptyNewMsgBatch) =>
                            state.sendNextBatchImmediatelyUponNewMsg()
                        case Left(state.OutOfBoundsGetMsgBatch) =>
                            IO.raiseError(Error("Incorrect bounds in GetMsgBatch."))
                    }
                } yield ()
            case x: NewMsgBatch =>
                for {
                    _ <- subs.persistence ? PutActorReq(x)
                    _ <- subs.remoteCommActor ! x.nextGetMsgBatch
                    _ <- x.ack.traverse_(subs.ackBlock ! _)
                    _ <- x.block.traverse_(subs.newBlock ! _)
                    _ <- x.events.traverse_(subs.newLedgerEvent ! _)
                } yield ()
        }
}

/**
 * Communication actor is connected to its counterpart at another peer:
 *
 *   - Requests communication batches from the counterpart.
 *   - Responds to the counterpart's requests for communication batches.
 */
object CommActor {
    private type maxEvents = Int

    final case class Config(
        peerId: PeerId,
        remotePeerId: PeerId,
        maxLedgerEventsPerBatch: maxEvents = 25
        )

    final case class ConnectionsPending(
        blockActor: Deferred[IO, BlockActorRef],
        persistence: Deferred[IO, PersistenceActorRef],
        remoteCommActor: Deferred[IO, CommActorRef]
        )

    final case class Subscribers(
        ackBlock: AckBlockSubscriber,
        newBlock: NewBlockSubscriber,
        newLedgerEvent: NewLedgerEventSubscriber,
        persistence: PersistenceActorRef,
        remoteCommActor: CommActorRef
        )

    def create(config: Config, connections: ConnectionsPending): IO[CommActor] =
        for {
            subscribers <- Ref.of[IO, Option[Subscribers]](None)
            state <- State.create
        } yield CommActor(config)(connections)(subscribers, state)

    object State {
        def create: IO[State] =
            for {
                nBatch <- Ref.of[IO, BatchNum](0)
                nAck <- Ref.of[IO, AckNum](0)
                nBlock <- Ref.of[IO, BlockNum](0)
                nEvent <- Ref.of[IO, LedgerEventNum](0)
                qAck <- Ref.of[IO, Queue[AckBlock]](Queue())
                qBlock <- Ref.of[IO, Queue[NewBlock]](Queue())
                qEvent <- Ref.of[IO, Queue[NewLedgerEvent]](Queue())
                sendBatchImmediately <- Ref.of[IO, Boolean](false)
            } yield State(
                nBatch = nBatch,
                nAck = nAck,
                nBlock = nBlock,
                nEvent = nEvent,
                qAck = qAck,
                qBlock = qBlock,
                qEvent = qEvent,
                sendBatchImmediately = sendBatchImmediately
            )
    }

    final case class State(
        private val nBatch: Ref[IO, BatchNum],
        private val nAck: Ref[IO, AckNum],
        private val nBlock: Ref[IO, BlockNum],
        private val nEvent: Ref[IO, LedgerEventNum],
        private val qAck: Ref[IO, Queue[AckBlock]],
        private val qBlock: Ref[IO, Queue[NewBlock]],
        private val qEvent: Ref[IO, Queue[NewLedgerEvent]],
        private val sendBatchImmediately: Ref[IO, Boolean],
        ) {
        /** Check whether there are no acks, blocks, or events queued-up to be sent out. */
        private def areEmptyQueues(): IO[Boolean] =
            for {
                ack <- this.qAck.get.map(_.isEmpty)
                block <- this.qBlock.get.map(_.isEmpty)
                event <- this.qEvent.get.map(_.isEmpty)
            } yield ack && block && event

        @targetName("append")
        infix def :+(x: AckBlock): IO[Unit] =
            for {
                _ <- this.nAck.update(_ + 1)
                _ <- this.qAck.update(_ :+ x)
            } yield ()

        infix def :+(x: NewBlock): IO[Unit] =
            for {
                _ <- this.nBlock.update(_ + 1)
                _ <- this.qBlock.update(_ :+ x)
            } yield ()

        infix def :+(x: NewLedgerEvent): IO[Unit] =
            for {
                _ <- this.nEvent.update(_ + 1)
                _ <- this.qEvent.update(_ :+ x)
            } yield ()

        sealed trait ErrorNewMsgBatch extends Throwable
        case object EmptyNewMsgBatch extends ErrorNewMsgBatch
        case object OutOfBoundsGetMsgBatch extends ErrorNewMsgBatch

        def sendNextBatchImmediatelyUponNewMsg(): IO[Unit] =
            this.sendBatchImmediately.set(true)

        def deactivateSendBatchImmediately(): IO[Unit] =
            this.sendBatchImmediately.set(false)

        /** Construct a [[NewMsgBatch]] containing acks, blocks, and events starting '''after''' the numbers indicated
         * in a [[GetMsgBatch]] request, up to the configured limits. The state is modified to keep only acks, blocks,
         * and events with numbers '''after''' those provided in the returned [[NewMsgBatch]].
         *
         * @param batchReq  the [[GetMsgBatch]] request
         * @param maxEvents the maximum number of ledger events to include in the [[NewMsgBatch]].
         */
        def extractNewMsgBatch(batchReq: GetMsgBatch, maxEvents: maxEvents): IO[Either[ErrorNewMsgBatch, NewMsgBatch]] =
            (for {
                nAck <- this.nAck.get
                nBlock <- this.nBlock.get
                nEvents <- this.nEvent.get

                _ <- if (nAck < batchReq.ackNum || nBlock < batchReq.blockNum || nEvents < batchReq.eventNum) {
                        IO.raiseError(OutOfBoundsGetMsgBatch)
                    } else {
                        IO.pure(())
                    }

                _ <- this.areEmptyQueues().ifM(IO.raiseError(EmptyNewMsgBatch), IO.pure(()))

                mAck <- this.qAck.modify(q =>
                    val dropped = q.dropWhile(_.id._2 <= batchReq.ackNum)
                    dropped.dequeueOption.fold((dropped, None))((x, xs) => (xs, Some(x)))
                )
                mBlock <- this.qBlock.modify(q =>
                    val dropped = q.dropWhile(_.id <= batchReq.blockNum)
                    dropped.dequeueOption.fold((dropped, None))((x, xs) => (xs, Some(x)))
                )
                events <- this.qEvent.modify(q =>
                    val dropped = q.dropWhile(_.id._2 <= batchReq.eventNum)
                    dropped.splitAt(maxEvents).swap
                )

                ackNum = mAck.fold(nAck)(_.id._2)
                blockNum = mBlock.fold(nBlock)(_.id)
                eventNum = events.lastOption.fold(nEvents)(_.id._2)
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