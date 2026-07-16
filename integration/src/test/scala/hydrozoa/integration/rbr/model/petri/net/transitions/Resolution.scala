package hydrozoa.integration.rbr.model.petri.net.transitions

import hydrozoa.integration.rbr.model.petri.net.*
import hydrozoa.integration.rbr.model.petri.net.RBRPlaceId.*
import hydrozoa.integration.rbr.model.petri.net.Transitions.RBRTransitionId
import hydrozoa.lib.number.{NonNegativeInt, PositiveInt}

/** Arcs for [[RBRTransitionId.Resolution]] (ResolutionTx).
  *
  * Consumes the unresolved treasury plus one `Voted` ballot box; produces the resolved treasury
  * plus `nEvacs` tokens on `PayoutObligationsPlace`. The `nEvacs` weight is fixed at net
  * construction (the winning kzg commitment's evacuation-map size on-chain).
  *
  * When `nEvacs == 0` the `ProducePayoutObligations` arc is omitted — a `PositiveInt(0)` is not
  * constructible — so Resolution still fires but seeds no evacuation work.
  */
object Resolution {
    val id: RBRTransitionId = RBRTransitionId.Resolution

    def arcs(nEvacs: NonNegativeInt): List[(RBRArcId, RBRArc)] = {
        val w1 = PositiveInt.unsafeApply(1)
        val core = List(
          RBRArcId.ResolutionSpendUnresolved -> RBRPTArc(UnresolvedTreasuryPlaceId, id, w1),
          RBRArcId.ResolutionSpendVoted -> RBRPTArc(VotedPlaceId, id, w1),
          RBRArcId.ResolutionProduceResolved -> RBRTPArc(ResolvedTreasuryPlaceId, id, w1),
        )
        val payoutArc = PositiveInt.apply(nEvacs.toInt).map { w =>
            RBRArcId.ResolutionProducePayoutObligations ->
                RBRTPArc(PayoutObligationsPlaceId, id, w)
        }
        core ++ payoutArc.toList
    }
}
