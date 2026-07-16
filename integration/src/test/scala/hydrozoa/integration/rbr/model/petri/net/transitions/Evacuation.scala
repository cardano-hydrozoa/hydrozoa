package hydrozoa.integration.rbr.model.petri.net.transitions

import hydrozoa.integration.rbr.model.petri.net.*
import hydrozoa.integration.rbr.model.petri.net.RBRPlaceId.*
import hydrozoa.integration.rbr.model.petri.net.Transitions.RBRTransitionId
import hydrozoa.lib.number.PositiveInt

/** Arcs for [[RBRTransitionId.EvacuationId]] (EvacuationTx).
  *
  * Consumes `payoutBatchSize` obligations from `PayoutObligationsPlace`, produces the same number
  * of outputs on `EvacuationOutputPlace`, and reads the resolved treasury / regime witness / one
  * setup-ladder rung / one collateral utxo. Also draws 2 tokens from `AmbientPlace` per firing — a
  * stand-in for wallet-ADA fee/change flow. The seeded Ambient marking + `payoutBatchSize` jointly
  * determine how many times this transition can fire on the way to terminal.
  */
object Evacuation {
    val id: RBRTransitionId = RBRTransitionId.EvacuationId

    def arcs(payoutBatchSize: PositiveInt): List[(RBRArcId, RBRArc)] = List(
      RBRArcId.EvacuationReadTreasuryRef ->
          RBRReadArc(TreasuryRefPlaceId, id, PositiveInt.unsafeApply(1)),
      RBRArcId.EvacuationReadResolved ->
          RBRReadArc(ResolvedTreasuryPlaceId, id, PositiveInt.unsafeApply(1)),
      RBRArcId.EvacuationReadSetupLadder ->
          RBRReadArc(SetupLadderRefPlaceId, id, PositiveInt.unsafeApply(1)),
      RBRArcId.EvacuationReadCollateral ->
          RBRReadArc(CollateralPlaceId, id, PositiveInt.unsafeApply(1)),
      RBRArcId.EvacuationSpendAmbient ->
          RBRPTArc(AmbientPlaceId, id, PositiveInt.unsafeApply(2)),
      RBRArcId.EvacuationSpendPayoutObligations ->
          RBRPTArc(PayoutObligationsPlaceId, id, payoutBatchSize),
      RBRArcId.EvacuationProduceEvacuationOutput ->
          RBRTPArc(EvacuationOutputPlaceId, id, payoutBatchSize),
    )
}
