package hydrozoa.rulebased

import hydrozoa.multisig.backend.cardano.CardanoBackend

sealed trait EvacuationActorEvent

object EvacuationActorEvent:
    final case class BackendErrorContinuingTxs(e: CardanoBackend.Error) extends EvacuationActorEvent
    final case class BackendErrorTreasuryUtxos(e: CardanoBackend.Error) extends EvacuationActorEvent
    final case class BackendErrorFeeUtxos(e: CardanoBackend.Error) extends EvacuationActorEvent
    final case class BackendErrorSubmittingEvacTx(e: CardanoBackend.Error)
        extends EvacuationActorEvent
    case object TreasuryNotYetResolved extends EvacuationActorEvent
    case object NoMoreEvacuations extends EvacuationActorEvent
    final case class PayoutObligationsLeft(n: Int) extends EvacuationActorEvent
    case object NoFeeCollateralUtxo extends EvacuationActorEvent
    final case class BuildingEvacTx(treasuryValue: String, evacuateeCount: Int, totalValue: String)
        extends EvacuationActorEvent
    final case class SubmittingEvacTx(evacuatedOutputs: Int, cbor: String)
        extends EvacuationActorEvent
    case object EvacTxSubmitted extends EvacuationActorEvent
