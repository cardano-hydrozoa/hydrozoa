package hydrozoa.multisig.actors.pure

import cats.effect.{Deferred, IO, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.typelevel.actors.syntax.BroadcastSyntax.*
import hydrozoa.multisig.persistence.pure.PutNewLedgerEvent

/**
 * Event actor is the source of new L1 deposits and L2 transactions for the head.
 */
object LedgerEventActor {
    final case class Config(peerId: PeerId)

    object State {
        def create: IO[State] =
          for {
             nBlock <- Ref.of[IO, LedgerEventNum](0)
          } yield State(nBlock)
    }
    final case class State(nBlock: Ref[IO, LedgerEventNum])

    sealed trait Connections
    final case class ConnectionsLive(
        blockActor: BlockActorRef,
        commActors: List[CommActorRef],
        persistence: PersistenceRef,
        ) extends Connections
    final case class ConnectionsPending(
        blockActor: Deferred[IO, BlockActorRef],
        commActors: Deferred[IO, List[CommActorRef]],
        persistence: Deferred[IO, PersistenceRef],
        ) extends Connections

    def create(config: Config, conn0: Connections): IO[LedgerEventActor] =
        for {
            conn <- Ref.of[IO, Connections](conn0)
            state <- State.create
        } yield LedgerEventActor(config)(conn, state)
}

final case class LedgerEventActor(config: LedgerEventActor.Config)(
    private val connections: Ref[IO, LedgerEventActor.Connections],
    private val state: LedgerEventActor.State
    ) extends Actor[IO, LedgerEventActorReq]{

    override def preStart: IO[Unit] =
        connections.get.flatMap({
            case x: LedgerEventActor.ConnectionsPending =>
                for {
                    bla <- x.blockActor.get
                    cas <- x.commActors.get
                    per <- x.persistence.get
                    _ <- connections.set(LedgerEventActor.ConnectionsLive(bla, cas, per))
                } yield ()
            case x: LedgerEventActor.ConnectionsLive =>
                IO.pure(())
        })
    
    override def receive: Receive[IO, LedgerEventActorReq] =
        PartialFunction.fromFunction(req =>
            connections.get.flatMap({
                case conn: LedgerEventActor.ConnectionsLive =>
                    this.receiveTotal(req, conn)
                case _ =>
                    IO.raiseError(Error("Impossible: Ledger event actor is receiving before its connections are live."))
            }))

    private def receiveTotal(req: LedgerEventActorReq, conn: LedgerEventActor.ConnectionsLive): IO[Unit] =
        req match {
            case x: SubmitLedgerEvent =>
                for {
                    newNum <- state.nBlock.updateAndGet(x => x + 1)
                    t <- IO.monotonic
                    newId = (config.peerId, newNum)
                    newEvent = NewLedgerEvent(newId, t, x.event)
                    _ <- conn.persistence ? PutNewLedgerEvent(newId, newEvent)
                    _ <- conn.blockActor ! newEvent
                    _ <- (conn.commActors ! newEvent).parallel
                } yield ()
            case x: ConfirmBlock =>
                ???
        }
}
