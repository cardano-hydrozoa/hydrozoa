package hydrozoa.rulebased

import hydrozoa.multisig.backend.cardano.CardanoBackend

sealed trait DisputeActorEvent

object DisputeActorEvent:
    final case class CardanoBackendError(e: CardanoBackend.Error) extends DisputeActorEvent
    final case class BuildingTx(label: String) extends DisputeActorEvent
    final case class SubmittingTxLabel(label: String) extends DisputeActorEvent
    final case class SubmittingTxFamily(family: String, txId: String) extends DisputeActorEvent
    final case class TxCbor(pretty: String, cbor: String) extends DisputeActorEvent
    final case class TxSubmitSuccess(family: String, txId: String) extends DisputeActorEvent
    final case class LookingForCollateral(addr: String) extends DisputeActorEvent
    case object CollateralFound extends DisputeActorEvent
    final case class NoCollateralFound(peerLabel: String) extends DisputeActorEvent
    case object Tallying extends DisputeActorEvent
    case object ParsingTreasury extends DisputeActorEvent
    case object TreasuryIsUnresolved extends DisputeActorEvent
    case object TreasuryIsResolved extends DisputeActorEvent
    final case class TreasuryFound(value: String) extends DisputeActorEvent
