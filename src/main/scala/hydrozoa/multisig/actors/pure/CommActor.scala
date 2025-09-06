package hydrozoa.multisig.actors.pure

import cats.implicits.*
import cats.effect.{Deferred, IO, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}
import hydrozoa.multisig.persistence.pure.PersistenceActorRef

import scala.collection.immutable.Queue

/**
 * Communication actor is connected to its counterpart at another peer:
 *
 *   - Requests communication batches from the counterpart.
 *   - Responds to the counterpart's requests for communication batches.
 */
object CommActor {
    final case class Config(peerId: PeerId, remotePeerId: PeerId)

    object State {
        def create: IO[State] =
            for {
                nBatch <- Ref.of[IO, BatchNum](0)
                qEvent <- Ref.of[IO, Queue[NewLedgerEvent]](Queue())
                qBlock <- Ref.of[IO, Queue[NewBlock]](Queue())
                qAck <- Ref.of[IO, Queue[AckBlock]](Queue())
                mDeferredMsgBatch <- Ref.of[IO, Option[Deferred[IO, NewMsgBatch]]](None)
            } yield State(nBatch, qEvent, qBlock, qAck, mDeferredMsgBatch)
    }
    final case class State(
        private val nBatch: Ref[IO, BatchNum],
        private val qEvent: Ref[IO, Queue[NewLedgerEvent]],
        private val qBlock: Ref[IO, Queue[NewBlock]],
        private val qAck: Ref[IO, Queue[AckBlock]],
        private val mDeferredMsgBatch: Ref[IO, Option[Deferred[IO, NewMsgBatch]]],
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
}

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
                Error("Impossible: Comm actor is receiving before its connections are live.").raiseError
        }))

    private def receiveTotal(req: CommActorReq, subs: Subscribers): IO[Unit] =
        req match {
            case x: NewLedgerEvent =>
                ???
            case x: NewBlock =>
                ???
            case x: AckBlock =>
                ???
            case x: GetMsgBatch =>
                ???
            case x: NewMsgBatch =>
                ???
        }
}
