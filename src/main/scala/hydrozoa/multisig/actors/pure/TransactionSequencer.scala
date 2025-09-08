package hydrozoa.multisig.actors.pure

import cats.effect.Deferred
import cats.effect.IO
import cats.effect.Ref
import cats.implicits._
import com.suprnation.actor.Actor.Actor
import com.suprnation.actor.Actor.Receive
import com.suprnation.typelevel.actors.syntax.BroadcastSyntax._
import hydrozoa.multisig.persistence.pure.PersistenceActorRef
import hydrozoa.multisig.persistence.pure.PutActorReq

import scala.collection.immutable.Queue

// Not sure why this is needed, but otherwise Scala doesn't allow the companion object's nested classes
// to be used directly in the case class, and it also wrongly says that Subscribers can be private.
import TransactionSequencer.{Config, State, ConnectionsPending, Subscribers}

final case class TransactionSequencer(config: Config)(
    private val connections: ConnectionsPending
    )(
    private val subscribers: Ref[IO, Option[Subscribers]],
    private val state: State
    ) extends Actor[IO, TransactionSequencerReq]{
    override def preStart: IO[Unit] =
        for {
            blockProducer <- connections.blockProducer.get
            peerLiaisons <- connections.peerLiaisons.get
            persistence <- connections.persistence.get
            _ <- subscribers.set(Some(Subscribers(
                newLedgerEvent = blockProducer :: peerLiaisons,
                persistence = persistence
            )))
        } yield ()

    override def receive: Receive[IO, TransactionSequencerReq] =
        PartialFunction.fromFunction(req =>
            subscribers.get.flatMap({
                case Some(subs) =>
                    this.receiveTotal(req, subs)
                case _ =>
                    Error("Impossible: Ledger event actor is receiving before its preStart provided subscribers.").raiseError
            }))

    private def receiveTotal(req: TransactionSequencerReq, subs: Subscribers): IO[Unit] =
        req match {
            case x: SubmitLedgerEvent =>
                for {
                    newNum <- state.nLedgerEvent.updateAndGet(x => x.increment)
                    newId = (config.peerId, newNum)
                    newEvent = NewLedgerEvent(newId, x.time, x.event)
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
object TransactionSequencer {
    final case class Config(peerId: PeerId)

    final case class ConnectionsPending(
        blockProducer: Deferred[IO, BlockProducerRef],
        peerLiaisons: Deferred[IO, List[PeerLiaisonRef]],
        persistence: Deferred[IO, PersistenceActorRef],
        )

    final case class Subscribers(
        newLedgerEvent: List[NewLedgerEventSubscriber],
        persistence: PersistenceActorRef,
        )

    def create(config: Config, connections: ConnectionsPending): IO[TransactionSequencer] =
        for {
            subscribers <- Ref.of[IO, Option[Subscribers]](None)
            state <- State.create
        } yield TransactionSequencer(config)(connections)(subscribers, state)

    object State {
        def create: IO[State] =
            for {
                nLedgerEvent <- Ref.of[IO, LedgerEventNum](LedgerEventNum(0))
                localRequests <- Ref.of[IO, Queue[(LedgerEventNum, Deferred[IO, Unit /*LedgerEventOutcome*/])]](Queue())
            } yield State(
                nLedgerEvent = nLedgerEvent,
                localRequests = localRequests
            )
    }

    final case class State(
        nLedgerEvent: Ref[IO, LedgerEventNum],
        localRequests: Ref[IO, Queue[(LedgerEventNum, Deferred[IO, Unit /*LedgerEventOutcome*/])]]
        )

}
