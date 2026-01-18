package hydrozoa.multisig.consensus

import cats.effect.{Deferred, IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.typelevel.actors.syntax.BroadcastSyntax.*
import hydrozoa.multisig.consensus.EventSequencer.{Config, ConnectionsPending}
import hydrozoa.multisig.protocol.*
import hydrozoa.multisig.protocol.ConsensusProtocol.*
import hydrozoa.multisig.protocol.ConsensusProtocol.EventSequencer.*
import hydrozoa.multisig.protocol.types.{LedgerEvent, LedgerEventId, Peer}
import scala.collection.immutable.Queue

/** Event sequencer receives local submissions of new ledger events and emits them sequentially into
  * the consensus system.
  */
object EventSequencer {
    final case class Config(peerId: Peer.Id)

    final case class ConnectionsPending(
        blockWeaver: Deferred[IO, BlockWeaver.Handle],
        peerLiaisons: Deferred[IO, List[PeerLiaison.Handle]]
    )

    def apply(config: Config, connections: ConnectionsPending): IO[EventSequencer] =
        IO(new EventSequencer(config, connections) {})
}

trait EventSequencer(config: Config, connections: ConnectionsPending) extends Actor[IO, Request] {
    private val subscribers = Ref.unsafe[IO, Option[Subscribers]](None)
    private val state = State()

    private final case class Subscribers(
        newLedgerEvent: List[LedgerEvent.Subscriber]
    )

    override def preStart: IO[Unit] =
        for {
            blockWeaver <- connections.blockWeaver.get
            peerLiaisons <- connections.peerLiaisons.get
            _ <- subscribers.set(
              Some(
                Subscribers(
                  newLedgerEvent = blockWeaver :: peerLiaisons
                )
              )
            )
        } yield ()

    override def receive: Receive[IO, Request] =
        PartialFunction.fromFunction(req =>
            subscribers.get.flatMap {
                case Some(subs) =>
                    this.receiveTotal(req, subs)
                case _ =>
                    Error(
                      "Impossible: Ledger event actor is receiving before its preStart provided subscribers."
                    ).raiseError
            }
        )

    private def receiveTotal(req: Request, subs: Subscribers): IO[Unit] =
        req match {
            case x: SubmitLedgerEvent =>
                for {
                    newNum <- state.enqueueDeferredEventOutcome(x.deferredEventOutcome)
                    newId = LedgerEventId(config.peerId.peerNum, newNum)
                    // FIXME: fill in
                    newEvent: LedgerEvent = ???
                    _ <- (subs.newLedgerEvent ! newEvent).parallel
                } yield ()
            case x: ConfirmBlock =>
                // Complete the deferred ledger event outcomes confirmed by the block and remove them from queue
                ???
        }

    private final class State {
        private val nLedgerEvent = Ref.unsafe[IO, LedgerEventId.Number](LedgerEventId.Number(0))
        private val localRequests =
            Ref.unsafe[IO, Queue[(LedgerEventId.Number, Deferred[IO, Unit])]](
              Queue()
            )

        def enqueueDeferredEventOutcome(
            eventOutcome: Deferred[IO, Unit]
        ): IO[LedgerEventId.Number] =
            for {
                newNum <- nLedgerEvent.updateAndGet(x => x.increment)
                _ <- localRequests.update(q => q :+ (newNum -> eventOutcome))
            } yield newNum

        def completeDeferredEventOutcomes(
            eventOutcomes: List[(LedgerEventId.Number, Unit)]
        ): IO[Unit] =
            ???
    }
}
