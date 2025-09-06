package hydrozoa.multisig.actors.pure

import cats.implicits._
import cats.effect.{Deferred, IO, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}

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

    sealed trait Connections
    final case class ConnectionsLive(
        blockActor: BlockActorRef,
        persistence: PersistenceRef,
        remoteCommActor: CommActorRef
        ) extends Connections
    final case class ConnectionsPending(
        blockActor: Deferred[IO, BlockActorRef],
        persistence: Deferred[IO, PersistenceRef],
        remoteCommActor: Deferred[IO, CommActorRef]
        ) extends Connections

    def create(config: Config, conn0: Connections): IO[CommActor] =
        for {
            conn <- Ref.of[IO, Connections](conn0)
            state <- State.create
        } yield CommActor(config)(conn, state)
}

final case class CommActor(config: CommActor.Config)(
    private val connections: Ref[IO, CommActor.Connections],
    private val state: CommActor.State
    ) extends Actor[IO, CommActorReq]{
    override def preStart: IO[Unit] =
        connections.get.flatMap({
            case x: CommActor.ConnectionsPending =>
                for {
                    bla <- x.blockActor.get
                    per <- x.persistence.get
                    // This means that the comm actor will not start receiving until it is connected to its
                    // remote counterpart:
                    rca <- x.remoteCommActor.get
                    _ <- connections.set(CommActor.ConnectionsLive(bla, per, rca))
                } yield ()
            case x: CommActor.ConnectionsLive =>
                ().pure
        })
    
    override def receive: Receive[IO, CommActorReq] = PartialFunction.fromFunction(req =>
        connections.get.flatMap({
            case conn: CommActor.ConnectionsLive =>
                this.receiveTotal(req, conn)
            case _ =>
                Error("Impossible: Comm actor is receiving before its connections are live.").raiseError
        }))

    private def receiveTotal(req: CommActorReq, conn: CommActor.ConnectionsLive): IO[Unit] =
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
