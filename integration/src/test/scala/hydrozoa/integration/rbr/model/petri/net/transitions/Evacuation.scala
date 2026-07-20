package hydrozoa.integration.rbr.model.petri.net.transitions

import hydrozoa.integration.rbr.model.petri.net.*
import hydrozoa.integration.rbr.model.petri.net.RBRPlaceId.*
import hydrozoa.integration.rbr.model.petri.net.Transitions.RBRTransitionId
import hydrozoa.integration.rbr.model.petri.net.Transitions.RBRTransitionId.EvacuationId
import hydrozoa.lib.number.PositiveInt
import hydrozoa.lib.petri.MapNet
import hydrozoa.lib.petri.net.components.Arc.Flow
import hydrozoa.lib.petri.net.components.{Arc, Transition}
import spire.math.Natural

// =============================================================================
// Arc value (annotation only — direction lives in the Flow key)
// =============================================================================

case class EvacuationArc(override val weight: PositiveInt) extends Arc.Syntax.Weighted

// =============================================================================
// Transition (no presentation)
// =============================================================================

type RBRTransition = Transition.Topology & Transition.Syntax & Transition.Semantics

private object EvacuatingTransition
    extends Transition.Topology,
      Transition.Syntax,
      Transition.Semantics

// =============================================================================
// Net type alias and builder
// =============================================================================

type EvacuationNet =
    MapNet[
      RBRPlaceId,
      RBRTransitionId,
      PositiveInt,
      Natural,
      EvacuationArc,
      RBRPlace,
      RBRTransition
    ]

object EvacuationNet {

    private val ops: MapNet.BuilderMOps[
      RBRPlaceId,
      RBRTransitionId,
      PositiveInt,
      Natural,
      EvacuationArc,
      RBRPlace,
      RBRTransition,
    ] = MapNet.BuilderMOps()

    import ops.*

    private def weighted(weight: Int): EvacuationArc =
        EvacuationArc(PositiveInt.unsafeApply(weight))

    /** Build a typed evacuation net parameterised on the ambient UTxO count, payout obligations,
      * head-peer count (for collateral and final markings), and payout batch size.
      *
      * Reference reads (treasury, resolved treasury, setup ladder, collateral) are self-loops: a
      * `Pt` + `Tp` flow pair on the same place with equal weight.
      */
    def apply(
        numAmbientUtxos: Natural,
        numPayoutObligations: Natural,
        nHeadPeers: Int,
        payoutBatchSize: Int = 63,
    ): EvacuationNet = {
        val builder = for {
            // ---- Places ----
            _ <- addPlace(
              TreasuryRefPlaceId,
              TreasuryRefPlace(
                marking = Natural.one,
                finalMarking = Some(Natural.one),
              ),
            )
            _ <- addPlace(
              ResolvedTreasuryPlaceId,
              ResolvedPlace(
                marking = Natural.one,
                finalMarking = Some(Natural.one),
              ),
            )
            _ <- addPlace(SetupLadderRefPlaceId, SetupLadderRefPlace())
            _ <- addPlace(AmbientPlaceId, AmbientPlace(marking = numAmbientUtxos))
            _ <- addPlace(
              PayoutObligationsPlaceId,
              PayoutObligationsPlace(marking = numPayoutObligations),
            )
            _ <- addPlace(
              EvacuationOutputPlaceId,
              EvacuationOutputPlace(
                marking = Natural.zero,
                finalMarking = Some(Natural((nHeadPeers * payoutBatchSize).toLong)),
              ),
            )
            _ <- addPlace(
              CollateralPlaceId,
              CollateralPlace(
                marking = Natural(nHeadPeers.toLong),
                expectedCount = nHeadPeers,
                finalMarking = Some(Natural(nHeadPeers.toLong)),
              ),
            )
            // ---- Transitions ----
            _ <- addTransition(EvacuationId, EvacuatingTransition)
            // ---- Arcs ----
            _ <- addArc(Flow.Pt(TreasuryRefPlaceId, EvacuationId), weighted(1))
            _ <- addArc(Flow.Tp(EvacuationId, TreasuryRefPlaceId), weighted(1))
            _ <- addArc(Flow.Pt(ResolvedTreasuryPlaceId, EvacuationId), weighted(1))
            _ <- addArc(Flow.Tp(EvacuationId, ResolvedTreasuryPlaceId), weighted(1))
            // Each evacuation reads exactly one setup-ladder rung
            _ <- addArc(Flow.Pt(SetupLadderRefPlaceId, EvacuationId), weighted(1))
            _ <- addArc(Flow.Tp(EvacuationId, SetupLadderRefPlaceId), weighted(1))
            // PT(3)+TP(1) on Ambient collapses to net -2
            _ <- addArc(Flow.Pt(AmbientPlaceId, EvacuationId), weighted(2))
            _ <- addArc(Flow.Pt(PayoutObligationsPlaceId, EvacuationId), weighted(payoutBatchSize))
            _ <- addArc(Flow.Tp(EvacuationId, EvacuationOutputPlaceId), weighted(payoutBatchSize))
            _ <- addArc(Flow.Pt(CollateralPlaceId, EvacuationId), weighted(1))
            _ <- addArc(Flow.Tp(EvacuationId, CollateralPlaceId), weighted(1))
        } yield ()

        val Right((net, _)) = builder.runEmpty: @unchecked
        net
    }
}
