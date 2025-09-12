package hydrozoa.multisig.consensus

import cats.effect.Deferred
import cats.effect.IO
import cats.effect.Ref
import cats.implicits.*
import com.suprnation.actor.Actor.Actor
import com.suprnation.actor.Actor.Receive
import com.suprnation.typelevel.actors.syntax.BroadcastSyntax.*

import scala.collection.immutable.Queue
import TransactionSequencer.{Config, ConnectionsPending, State, Subscribers}
import hydrozoa.multisig.protocol.*
import hydrozoa.multisig.protocol.Identifiers.*
import hydrozoa.multisig.protocol.ConsensusProtocol.*
import hydrozoa.multisig.protocol.PersistenceProtocol.*
import hydrozoa.multisig.protocol.ConsensusProtocol.TransactionSequencer.*

final case class TransactionSequencer(config: Config)(
    private val connections: ConnectionsPending
)(
    private val subscribers: Ref[IO, Option[Subscribers]],
    private val state: State
) extends Actor[IO, Request] {
    override def preStart: IO[Unit] =
        for {
            blockProducer <- connections.blockProducer.get
            peerLiaisons <- connections.peerLiaisons.get
            persistence <- connections.persistence.get
            _ <- subscribers.set(
              Some(
                Subscribers(
                  newLedgerEvent = blockProducer :: peerLiaisons,
                  persistence = persistence
                )
              )
            )
        } yield ()

    override def receive: Receive[IO, Request] =
        PartialFunction.fromFunction(req =>
            subscribers.get.flatMap({
                case Some(subs) =>
                    this.receiveTotal(req, subs)
                case _ =>
                    Error(
                      "Impossible: Ledger event actor is receiving before its preStart provided subscribers."
                    ).raiseError
            })
        )

    private def receiveTotal(req: Request, subs: Subscribers): IO[Unit] =
        req match {
            case x: SubmitLedgerEvent =>
                for {
                    newNum <- state.nLedgerEvent.updateAndGet(x => x.increment)
                    newId = (config.peerId, newNum)
                    newEvent = NewLedgerEvent(newId, x.time, x.event)
                    _ <- state.localRequests.update(q => q :+ (newNum -> x.eventOutcome))
                    _ <- subs.persistence ? Persistence.PersistRequest(newEvent)
                    _ <- (subs.newLedgerEvent ! newEvent).parallel
                } yield ()
            case x: ConfirmBlock =>
                // Complete the deferred ledger event outcomes confirmed by the block and remove them from queue
                ???
        }
}

/** Event actor is the source of new L1 deposits and L2 transactions for the head.
  */
object TransactionSequencer {
    final case class Config(peerId: PeerId)

    final case class ConnectionsPending(
        blockProducer: Deferred[IO, BlockProducer.Ref],
        peerLiaisons: Deferred[IO, List[PeerLiaison.Ref]],
        persistence: Deferred[IO, Persistence.Ref]
    )

    final case class Subscribers(
        newLedgerEvent: List[NewLedgerEvent.Subscriber],
        persistence: Persistence.Ref
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
                localRequests <- Ref
                    .of[IO, Queue[(LedgerEventNum, Deferred[IO, Unit /*LedgerEventOutcome*/ ])]](
                      Queue()
                    )
            } yield State(
              nLedgerEvent = nLedgerEvent,
              localRequests = localRequests
            )
    }

    final case class State(
        nLedgerEvent: Ref[IO, LedgerEventNum],
        localRequests: Ref[IO, Queue[(LedgerEventNum, Deferred[IO, Unit /*LedgerEventOutcome*/ ])]]
    )

}
