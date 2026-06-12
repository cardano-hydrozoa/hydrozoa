package hydrozoa.integration.rbr.model.petri.net.transitions

import hydrozoa.integration.rbr.model.petri.net.Transitions.RBRTransitionId
import hydrozoa.integration.rbr.model.petri.net.Transitions.RBRTransitionId.EvacuationId
import hydrozoa.integration.rbr.model.petri.net.RBRPlaceId.*
import hydrozoa.integration.rbr.model.petri.net.*
import hydrozoa.lib.number.{NonNegativeInt, PositiveInt}
import hydrozoa.lib.petri.MapNet
import hydrozoa.lib.petri.net.components.{Arc, Transition}

// =============================================================================
// Arc ID
// =============================================================================

enum RBRArcId:
    case ReadTreasury
    case ReadResolved
    case SpendAmbient
    case FulfillPayoutObligation
    case SendEvacuationOutput
    case UseCollateral

object RBRArcId {
    given Ordering[RBRArcId] = Ordering.by(_.ordinal)
}

// =============================================================================
// Arc sealed hierarchy (no presentation)
// =============================================================================

sealed trait EvacuationArc
    extends Arc.Topology[RBRPlaceId, RBRTransitionId],
      Arc.Syntax,
      Arc.Semantics[RBRPlace]

case class EvacuationPTArc(
    override val arcPlaceId: RBRPlaceId,
    override val arcTransitionId: RBRTransitionId,
    override val weight: PositiveInt,
) extends EvacuationArc,
      Arc.Semantics.PT[RBRPlace]

case class EvacuationTPArc(
    override val arcPlaceId: RBRPlaceId,
    override val arcTransitionId: RBRTransitionId,
    override val weight: PositiveInt,
) extends EvacuationArc,
      Arc.Semantics.TP[RBRPlace]

case class EvacuationReadArc(
    override val arcPlaceId: RBRPlaceId,
    override val arcTransitionId: RBRTransitionId,
    override val weight: PositiveInt,
) extends EvacuationArc,
      Arc.Semantics.Read[RBRPlace]

// =============================================================================
// Transition (no presentation)
// =============================================================================

type RBRTransition = Transition.Topology & Transition.Syntax & Transition.Semantics

private object EvacuatingTransition extends Transition.Topology, Transition.Syntax, Transition.Semantics

// =============================================================================
// Net type alias and builder
// =============================================================================

type EvacuationNet =
    MapNet[RBRArcId, RBRPlaceId, RBRTransitionId, EvacuationArc, RBRPlace, RBRTransition]

object EvacuationNet {

    private val ops: MapNet.BuilderMOps[
      RBRArcId,
      RBRPlaceId,
      RBRTransitionId,
      EvacuationArc,
      RBRPlace,
      RBRTransition,
    ] = MapNet.BuilderMOps()

    import ops.*

    /** Build a typed evacuation net parameterised on the ambient UTxO count, payout obligations,
      * head-peer count (for collateral and final markings), and payout batch size.
      *
      * The topology mirrors the [[hydrozoa.lib.petri.Demo]] net exactly; the difference is that all
      * IDs are typed enums and the place type carries domain-specific marking validation.
      */
    def apply(
        numAmbientUtxos: NonNegativeInt,
        numPayoutObligations: NonNegativeInt,
        nHeadPeers: Int,
        payoutBatchSize: Int = 63,
    ): EvacuationNet = {
        val builder = for {
            // ---- Places ----
            _ <- addPlace(
              TreasuryRefPlaceId,
              TreasuryRefPlace(
                marking = NonNegativeInt.unsafeApply(1),
                finalMarking = Some(NonNegativeInt.unsafeApply(1)),
              ),
            )
            _ <- addPlace(
              ResolvedTreasuryPlaceId,
              ResolvedPlace(
                marking = NonNegativeInt.unsafeApply(1),
                finalMarking = Some(NonNegativeInt.unsafeApply(1)),
              ),
            )
            _ <- addPlace(AmbientPlaceId, AmbientPlace(marking = numAmbientUtxos))
            _ <- addPlace(
              PayoutObligationsPlaceId,
              PayoutObligationsPlace(marking = numPayoutObligations),
            )
            _ <- addPlace(
              EvacuationOutputPlaceId,
              EvacuationOutputPlace(
                marking = NonNegativeInt.unsafeApply(0),
                finalMarking = Some(NonNegativeInt.unsafeApply(nHeadPeers * payoutBatchSize)),
              ),
            )
            _ <- addPlace(
              CollateralPlaceId,
              CollateralPlace(
                marking = NonNegativeInt.unsafeApply(nHeadPeers),
                expectedCount = nHeadPeers,
                finalMarking = Some(NonNegativeInt.unsafeApply(nHeadPeers)),
              ),
            )
            // ---- Transitions ----
            _ <- addTransition(EvacuationId, EvacuatingTransition)
            // ---- Arcs ----
            _ <- addArc(
              RBRArcId.ReadTreasury,
              EvacuationReadArc(TreasuryRefPlaceId, EvacuationId, PositiveInt.unsafeApply(1)),
            )
            _ <- addArc(
              RBRArcId.ReadResolved,
              EvacuationReadArc(ResolvedTreasuryPlaceId, EvacuationId, PositiveInt.unsafeApply(1)),
            )
            // PT(3)+TP(1) on Ambient collapses to net -2
            _ <- addArc(
              RBRArcId.SpendAmbient,
              EvacuationPTArc(AmbientPlaceId, EvacuationId, PositiveInt.unsafeApply(2)),
            )
            _ <- addArc(
              RBRArcId.FulfillPayoutObligation,
              EvacuationPTArc(
                PayoutObligationsPlaceId,
                EvacuationId,
                PositiveInt.unsafeApply(payoutBatchSize),
              ),
            )
            _ <- addArc(
              RBRArcId.SendEvacuationOutput,
              EvacuationTPArc(
                EvacuationOutputPlaceId,
                EvacuationId,
                PositiveInt.unsafeApply(payoutBatchSize),
              ),
            )
            _ <- addArc(
              RBRArcId.UseCollateral,
              EvacuationReadArc(CollateralPlaceId, EvacuationId, PositiveInt.unsafeApply(1)),
            )
        } yield ()

        val Right((net, _)) = builder.runEmpty: @unchecked
        net
    }
}
