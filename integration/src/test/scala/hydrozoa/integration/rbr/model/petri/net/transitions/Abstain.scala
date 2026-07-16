package hydrozoa.integration.rbr.model.petri.net.transitions

import hydrozoa.integration.rbr.model.petri.net.*
import hydrozoa.integration.rbr.model.petri.net.RBRPlaceId.*
import hydrozoa.integration.rbr.model.petri.net.Transitions.RBRTransitionId
import hydrozoa.lib.number.PositiveInt

/** Arcs for [[RBRTransitionId.Abstain]] (AbstainTx).
  *
  * Consumes one `AwaitingVote` ballot box, produces one `Abstain` ballot box. Unlike VoteTx,
  * AbstainTx has no treasury reference — the on-chain script only checks the reserved peer's
  * signature and that value / key / link are preserved. So there is no `Read` arc binding it to
  * `UnresolvedTreasuryPlaceId`; Abstain could in principle fire after Resolution too, but the
  * canonical firing order in [[hydrozoa.integration.rbr.model.RBRModel]] ensures Vote preempts it
  * while `UnvotedPlaceId` still has tokens.
  */
object Abstain {
    val id: RBRTransitionId = RBRTransitionId.Abstain

    val arcs: List[(RBRArcId, RBRArc)] = {
        val w1 = PositiveInt.unsafeApply(1)
        List(
          RBRArcId.AbstainSpendUnvoted -> RBRPTArc(UnvotedPlaceId, id, w1),
          RBRArcId.AbstainProduceAbstain -> RBRTPArc(AbstainPlaceId, id, w1),
        )
    }
}
