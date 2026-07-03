package hydrozoa.rulebased

import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.ledger.l1.tx.EnrichedTx

sealed trait RuleBasedActorEvent

object RuleBasedActorEvent:

    object Lifecycle:
        case object RegimeManagerPreStartEntered extends RuleBasedActorEvent
        final case class RegimeManagerPreStartFailed(errorClass: String, message: String)
            extends RuleBasedActorEvent
        case object ActorPreStartEntered extends RuleBasedActorEvent
        case object TickReceived extends RuleBasedActorEvent

    object Backend:
        final case class ErrorDisputeUtxos(e: CardanoBackend.Error) extends RuleBasedActorEvent
        final case class ErrorTreasuryUtxos(e: CardanoBackend.Error) extends RuleBasedActorEvent
        final case class ErrorPeerUtxos(e: CardanoBackend.Error) extends RuleBasedActorEvent
        final case class ErrorFeeUtxos(e: CardanoBackend.Error) extends RuleBasedActorEvent
        final case class ErrorContinuingTxs(e: CardanoBackend.Error) extends RuleBasedActorEvent
        final case class ErrorSubmittingTx(e: CardanoBackend.Error) extends RuleBasedActorEvent

    object Treasury:
        case object Querying extends RuleBasedActorEvent
        final case class Found(value: String) extends RuleBasedActorEvent
        case object NotFound extends RuleBasedActorEvent
        case object Parsing extends RuleBasedActorEvent
        case object ParsedUnresolved extends RuleBasedActorEvent
        case object ParsedResolved extends RuleBasedActorEvent

    object Collateral:
        final case class Querying(addr: String) extends RuleBasedActorEvent
        case object Found extends RuleBasedActorEvent
        final case class NotFound(peerLabel: String) extends RuleBasedActorEvent
        case object NoFeeCollateralUtxo extends RuleBasedActorEvent
        // TEMPORARY (2026-07-02): diagnostic events for a `Collateral.NotFound` misfire while
        // integrating RRM into the vote-version-mismatch test. Delete `QueryResult` and
        // `DiagnosticMismatch` (and their emit sites in `RuleBasedActor.getCollateral` and their
        // renderers in `RuleBasedActorEventFormat`) once the pattern-match failure is diagnosed
        // and the test is green.
        final case class QueryResult(peerLabel: String, count: Int, sample: String)
            extends RuleBasedActorEvent
        final case class DiagnosticMismatch(peerLabel: String, reason: String, detail: String)
            extends RuleBasedActorEvent

    object Dispute:
        case object Querying extends RuleBasedActorEvent
        case object Parsing extends RuleBasedActorEvent
        case object ParsedCastVote extends RuleBasedActorEvent
        case object ParsedTally extends RuleBasedActorEvent
        case object ParsedResolve extends RuleBasedActorEvent
        case object ParsedEmptyVotes extends RuleBasedActorEvent

    object Tx:
        final case class Building(family: String) extends RuleBasedActorEvent
        final case class Submitting(tx: EnrichedTx[?]) extends RuleBasedActorEvent
        final case class SubmitSuccess(tx: EnrichedTx[?]) extends RuleBasedActorEvent
        case object Tallying extends RuleBasedActorEvent

    object Evacuation:
        case object NoMore extends RuleBasedActorEvent
        final case class PayoutsLeft(n: Int) extends RuleBasedActorEvent
