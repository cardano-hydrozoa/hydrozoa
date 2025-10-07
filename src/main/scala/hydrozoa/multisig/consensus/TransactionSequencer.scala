package hydrozoa.multisig.consensus

import cats.effect.{Deferred, IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.typelevel.actors.syntax.BroadcastSyntax.*
import hydrozoa.multisig.protocol.*
import hydrozoa.multisig.protocol.ConsensusProtocol.*
import hydrozoa.multisig.protocol.ConsensusProtocol.TransactionSequencer.*
import hydrozoa.multisig.protocol.PersistenceProtocol.*
import hydrozoa.multisig.protocol.types.{LedgerEvent, Peer}
import scala.collection.immutable.Queue

import TransactionSequencer.{Config, ConnectionsPending}

/** Transaction sequencer receives local submissions of new ledger events and emits them
  * sequentially into the consensus system.
  */
object TransactionSequencer {
    final case class Config(peerId: Peer.Number, persistence: Persistence.Ref)

    final case class ConnectionsPending(
        blockProducer: Deferred[IO, BlockProducer.Ref],
        peerLiaisons: Deferred[IO, List[PeerLiaison.Ref]]
    )

    def apply(config: Config, connections: ConnectionsPending): IO[TransactionSequencer] =
        IO(new TransactionSequencer(config, connections) {})
}

trait TransactionSequencer(config: Config, connections: ConnectionsPending)
    extends Actor[IO, Request] {
    private val subscribers = Ref.unsafe[IO, Option[Subscribers]](None)
    private val state = State()

    private final case class Subscribers(
        newLedgerEvent: List[NewLedgerEvent.Subscriber]
    )

    override def preStart: IO[Unit] =
        for {
            blockProducer <- connections.blockProducer.get
            peerLiaisons <- connections.peerLiaisons.get
            _ <- subscribers.set(
              Some(
                Subscribers(
                  newLedgerEvent = blockProducer :: peerLiaisons
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
                    newId = LedgerEvent.Id(config.peerId, newNum)
                    newEvent = NewLedgerEvent(newId, x.time, x.event)
                    persistenceRequest <- Persistence.PersistRequest(newEvent)
                    _ <- config.persistence ?: persistenceRequest
                    _ <- (subs.newLedgerEvent ! newEvent).parallel
                } yield ()
            case x: ConfirmBlock =>
                // Complete the deferred ledger event outcomes confirmed by the block and remove them from queue
                ???
        }

    private final class State {
        private val nLedgerEvent = Ref.unsafe[IO, LedgerEvent.Number](LedgerEvent.Number(0))
        private val localRequests =
            Ref.unsafe[IO, Queue[(LedgerEvent.Number, Deferred[IO, Unit])]](
              Queue()
            )

        def enqueueDeferredEventOutcome(eventOutcome: Deferred[IO, Unit]): IO[LedgerEvent.Number] =
            for {
                newNum <- nLedgerEvent.updateAndGet(x => x.increment)
                _ <- localRequests.update(q => q :+ (newNum -> eventOutcome))
            } yield newNum

        def completeDeferredEventOutcomes(
            eventOutcomes: List[(LedgerEvent.Number, Unit)]
        ): IO[Unit] =
            ???
    }
}
