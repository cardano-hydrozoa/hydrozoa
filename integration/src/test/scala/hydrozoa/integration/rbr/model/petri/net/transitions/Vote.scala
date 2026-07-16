package hydrozoa.integration.rbr.model.petri.net.transitions

import hydrozoa.integration.rbr.model.petri.net.*
import hydrozoa.integration.rbr.model.petri.net.RBRPlaceId.*
import hydrozoa.integration.rbr.model.petri.net.Transitions.RBRTransitionId
import hydrozoa.lib.number.PositiveInt

/** Arcs for [[RBRTransitionId.Vote]] (VoteTx).
  *
  * Consumes one `AwaitingVote` ballot box, produces one `Voted` ballot box. The `Read` on
  * `UnresolvedTreasuryPlaceId` encodes the on-chain reference to the treasury utxo and, at marking
  * level, forbids Vote from firing once [[RBRTransitionId.Resolution]] has consumed the treasury.
  * Peer identity is not distinguished at the marking level.
  */
object Vote {
    val id: RBRTransitionId = RBRTransitionId.Vote

    val arcs: List[(RBRArcId, RBRArc)] = {
        val w1 = PositiveInt.unsafeApply(1)
        List(
          RBRArcId.VoteReadTreasury -> RBRReadArc(UnresolvedTreasuryPlaceId, id, w1),
          RBRArcId.VoteSpendUnvoted -> RBRPTArc(UnvotedPlaceId, id, w1),
          RBRArcId.VoteProduceVoted -> RBRTPArc(VotedPlaceId, id, w1),
        )
    }
}
