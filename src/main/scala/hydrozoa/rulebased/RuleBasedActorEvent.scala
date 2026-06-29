package hydrozoa.rulebased

import hydrozoa.multisig.backend.cardano.CardanoBackend

sealed trait RuleBasedActorEvent

object RuleBasedActorEvent:
    // Shared
    final case class CardanoBackendError(e: CardanoBackend.Error) extends RuleBasedActorEvent

    // Dispute side
    final case class BuildingTx(label: String) extends RuleBasedActorEvent
    final case class SubmittingTxLabel(label: String) extends RuleBasedActorEvent
    final case class SubmittingTxFamily(family: String, txId: String) extends RuleBasedActorEvent
    final case class TxCbor(pretty: String, cbor: String) extends RuleBasedActorEvent
    final case class TxSubmitSuccess(family: String, txId: String) extends RuleBasedActorEvent
    final case class LookingForCollateral(addr: String) extends RuleBasedActorEvent
    case object CollateralFound extends RuleBasedActorEvent
    final case class NoCollateralFound(peerLabel: String) extends RuleBasedActorEvent
    case object Tallying extends RuleBasedActorEvent
    case object ParsingTreasury extends RuleBasedActorEvent
    case object TreasuryIsUnresolved extends RuleBasedActorEvent
    case object TreasuryIsResolved extends RuleBasedActorEvent
    final case class TreasuryFound(value: String) extends RuleBasedActorEvent

    // Evacuation side
    final case class BackendErrorContinuingTxs(e: CardanoBackend.Error) extends RuleBasedActorEvent
    final case class BackendErrorTreasuryUtxos(e: CardanoBackend.Error) extends RuleBasedActorEvent
    final case class BackendErrorFeeUtxos(e: CardanoBackend.Error) extends RuleBasedActorEvent
    final case class BackendErrorSubmittingEvacTx(e: CardanoBackend.Error)
        extends RuleBasedActorEvent
    case object TreasuryNotYetResolved extends RuleBasedActorEvent
    case object NoMoreEvacuations extends RuleBasedActorEvent
    final case class PayoutObligationsLeft(n: Int) extends RuleBasedActorEvent
    case object NoFeeCollateralUtxo extends RuleBasedActorEvent
    final case class BuildingEvacTx(treasuryValue: String, evacuateeCount: Int, totalValue: String)
        extends RuleBasedActorEvent
    final case class SubmittingEvacTx(evacuatedOutputs: Int, cbor: String)
        extends RuleBasedActorEvent
    case object EvacTxSubmitted extends RuleBasedActorEvent
