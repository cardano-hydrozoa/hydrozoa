package hydrozoa.rulebased

import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.ledger.l1.tx.EnrichedTx

sealed trait RuleBasedActorEvent

object RuleBasedActorEvent:

    object Backend:
        final case class ErrorDisputeUtxos(e: CardanoBackend.Error) extends RuleBasedActorEvent
        final case class ErrorTreasuryUtxos(e: CardanoBackend.Error) extends RuleBasedActorEvent
        final case class ErrorPeerUtxos(e: CardanoBackend.Error) extends RuleBasedActorEvent
        final case class ErrorFeeUtxos(e: CardanoBackend.Error) extends RuleBasedActorEvent
        final case class ErrorContinuingTxs(e: CardanoBackend.Error) extends RuleBasedActorEvent
        final case class ErrorSubmittingTx(e: CardanoBackend.Error) extends RuleBasedActorEvent

    object Treasury:
        case object Parsing extends RuleBasedActorEvent
        case object Unresolved extends RuleBasedActorEvent
        case object Resolved extends RuleBasedActorEvent
        final case class Found(value: String) extends RuleBasedActorEvent
        case object NotYetResolved extends RuleBasedActorEvent

    object Collateral:
        final case class Looking(addr: String) extends RuleBasedActorEvent
        case object Found extends RuleBasedActorEvent
        final case class NotFound(peerLabel: String) extends RuleBasedActorEvent
        case object NoFeeCollateralUtxo extends RuleBasedActorEvent

    object Tx:
        final case class Building(family: String) extends RuleBasedActorEvent
        final case class Submitting(tx: EnrichedTx[?]) extends RuleBasedActorEvent
        final case class SubmitSuccess(tx: EnrichedTx[?]) extends RuleBasedActorEvent
        case object Tallying extends RuleBasedActorEvent

    object Evacuation:
        case object NoMore extends RuleBasedActorEvent
        final case class PayoutsLeft(n: Int) extends RuleBasedActorEvent
