package hydrozoa.multisig.consensus

import cats.effect.Deferred
import cats.effect.IO
import cats.effect.Ref
import cats.implicits.*
import com.suprnation.actor.Actor.Actor
import com.suprnation.actor.Actor.Receive
import com.suprnation.typelevel.actors.syntax.BroadcastSyntax.*

import scala.collection.immutable.Queue
import TransactionSequencer.{Config, ConnectionsPending}
import hydrozoa.multisig.protocol.*
import hydrozoa.multisig.protocol.Identifiers.*
import hydrozoa.multisig.protocol.ConsensusProtocol.*
import hydrozoa.multisig.protocol.PersistenceProtocol.*
import hydrozoa.multisig.protocol.ConsensusProtocol.TransactionSequencer.*

/** Transaction sequencer receives local submissions of new ledger events and emits them
  * sequentially into the consensus system.
  */
object TransactionSequencer {
    final case class Config(peerId: PeerId)

    final case class ConnectionsPending(
        blockProducer: Deferred[IO, BlockProducer.Ref],
        peerLiaisons: Deferred[IO, List[PeerLiaison.Ref]],
        persistence: Deferred[IO, Persistence.Ref]
    )

    def create(config: Config, connections: ConnectionsPending): IO[TransactionSequencer] =
        IO(TransactionSequencer(config, connections))
}

final class TransactionSequencer private (
    config: Config,
    private val connections: ConnectionsPending
) extends Actor[IO, Request] {
    private val subscribers = Ref.unsafe[IO, Option[Subscribers]](None)
    private val state = State()

    private final case class Subscribers(
        newLedgerEvent: List[NewLedgerEvent.Subscriber],
        persistence: Persistence.Ref
    )

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
                    newNum <- state.enqueueDeferredEventOutcome(x.deferredEventOutcome)
                    newId = (config.peerId, newNum)
                    newEvent = NewLedgerEvent(newId, x.time, x.event)
                    _ <- subs.persistence ? Persistence.PersistRequest(newEvent)
                    _ <- (subs.newLedgerEvent ! newEvent).parallel
                } yield ()
            case x: ConfirmBlock =>
                // Complete the deferred ledger event outcomes confirmed by the block and remove them from queue
                ???
        }

    private final class State {
        private val nLedgerEvent = Ref.unsafe[IO, LedgerEventNum](LedgerEventNum(0))
        private val localRequests =
            Ref.unsafe[IO, Queue[(LedgerEventNum, Deferred[IO, Unit])]](
              Queue()
            )

        def enqueueDeferredEventOutcome(eventOutcome: Deferred[IO, Unit]): IO[LedgerEventNum] =
            for {
                newNum <- nLedgerEvent.updateAndGet(x => x.increment)
                _ <- localRequests.update(q => q :+ (newNum -> eventOutcome))
            } yield newNum

        def completeDeferredEventOutcomes(eventOutcomes: List[(LedgerEventNum, Unit)]): IO[Unit] =
            ???
    }
}
