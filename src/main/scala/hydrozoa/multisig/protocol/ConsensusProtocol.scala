package hydrozoa.multisig.protocol

import cats.effect.{Deferred, IO}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.multisig.protocol.types.Block.*
import scala.concurrent.duration.FiniteDuration

object ConsensusProtocol {

    /** =Multisig regime protocol= */

    /** Multisig regime's protocol for actor requests and responses. See diagram:
      * [[https://app.excalidraw.com/s/9N3iw9j24UW/9eRJ7Dwu42X]]
      */
    enum Actors:
        case BlockWeaver, CardanoLiaison, Consensus, JointLedger, PeerLiaison, EventSequencer

    object EventSequencer {
        type EventSequencerRef = Ref
        type Ref = ActorRef[IO, Request]
        type Request =
            SubmitLedgerEvent | ConfirmBlock

        type ConfirmBlock = Void
    }

    /** Submit a new ledger event to the head via a peer's ledger event actor. */
    final case class SubmitLedgerEvent(
        time: FiniteDuration,
        event: Unit, // FIXME
        deferredEventOutcome: Deferred[IO, Unit] // FIXME: LedgerEventOutcome]
    )

    /** A ledger event submission is constructed by taking a ledger event, timestamping it, and
      * creating a deferred cell where the outcome of the event can later be placed. The intention
      * is for the fiber handling the user's event submission to construct this
      * [[SubmitLedgerEvent]] and send it to the ledger event actor.
      */
    // TODO: for deposit txs, the tx time bounds and deposit utxo datum will need to be adjusted based on the timestamp.
    object SubmitLedgerEvent {
        def create(event: Unit): // FIXME
        IO[SubmitLedgerEvent] =
            for {
                time <- IO.monotonic
                eventOutcome <- Deferred[IO, Unit] // FIXME: LedgerEventOutcome]
            } yield SubmitLedgerEvent(time, event, eventOutcome)
    }
}
