package hydrozoa.multisig.actors.pure

import cats.implicits.*
import cats.effect.{Deferred, IO, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastSyntax.*
import hydrozoa.multisig.ledger.multi.trivial.LedgerEventOutcome
import hydrozoa.multisig.persistence.pure.{PersistenceActorRef, PutActorReq}

import scala.collection.immutable.Queue

// Not sure why this is needed, but otherwise Scala doesn't allow the companion object's nested classes
// to be used directly in the case class, and it also wrongly says that Subscribers can be private.
import LedgerEventActor.{Config, State, ConnectionsPending, Subscribers}

final case class LedgerEventActor(config: Config)(
    private val connections: ConnectionsPending
    ) (
    private val subscribers: Ref[IO, Option[Subscribers]],
    private val state: State
    ) extends Actor[IO, LedgerEventActorReq]{
    override def preStart: IO[Unit] =
        for {
            blockActor <- connections.blockActor.get
            commActors <- connections.commActors.get
            persistence <- connections.persistence.get
            _ <- subscribers.set(Some(Subscribers(
                newLedgerEvent = blockActor :: commActors,
                persistence = persistence
            )))
        } yield ()

    override def receive: Receive[IO, LedgerEventActorReq] =
        PartialFunction.fromFunction(req =>
            subscribers.get.flatMap({
                case Some(subs) =>
                    this.receiveTotal(req, subs)
                case _ =>
                    Error("Impossible: Ledger event actor is receiving before its connections are live.").raiseError
            }))

    private def receiveTotal(req: LedgerEventActorReq, subs: Subscribers): IO[Unit] =
        req match {
            case x: SubmitLedgerEvent =>
                for {
                    newNum <- state.nLedgerEvent.updateAndGet(x => x + 1)
                    t <- IO.monotonic
                    newId = (config.peerId, newNum)
                    newEvent = NewLedgerEvent(newId, t, x.event)
                    _ <- state.localRequests.update(q => q :+ (newNum -> x.eventOutcome))
                    _ <- subs.persistence ? PutActorReq(newEvent)
                    _ <- (subs.newLedgerEvent ! newEvent).parallel
                } yield ()
            case x: ConfirmBlock =>
                // Complete the deferred ledger event outcomes confirmed by the block and remove them from queue
                ???
        }
}

/**
 * Event actor is the source of new L1 deposits and L2 transactions for the head.
 */
object LedgerEventActor {
    final case class Config(peerId: PeerId)

    final case class ConnectionsPending(
        blockActor: Deferred[IO, BlockActorRef],
        commActors: Deferred[IO, List[CommActorRef]],
        persistence: Deferred[IO, PersistenceActorRef],
        )

    final case class Subscribers(
        newLedgerEvent: List[NewLedgerEventSubscriber],
        persistence: PersistenceActorRef,
        )

    def create(config: Config, connections: ConnectionsPending): IO[LedgerEventActor] =
        for {
            subscribers <- Ref.of[IO, Option[Subscribers]](None)
            state <- State.create
        } yield LedgerEventActor(config)(connections)(subscribers, state)

    object State {
        def create: IO[State] =
            for {
                nLedgerEvent <- Ref.of[IO, LedgerEventNum](0)
                localRequests <- Ref.of[IO, Queue[(LedgerEventNum, Deferred[IO, LedgerEventOutcome])]](Queue())
            } yield State(
                nLedgerEvent = nLedgerEvent,
                localRequests = localRequests
            )
    }

    final case class State(
        nLedgerEvent: Ref[IO, LedgerEventNum],
        localRequests: Ref[IO, Queue[(LedgerEventNum, Deferred[IO, LedgerEventOutcome])]]
        )

}
