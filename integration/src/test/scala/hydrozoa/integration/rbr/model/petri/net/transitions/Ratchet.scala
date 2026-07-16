package hydrozoa.integration.rbr.model.petri.net.transitions

import hydrozoa.integration.rbr.model.petri.net.*
import hydrozoa.integration.rbr.model.petri.net.RBRPlaceId.*
import hydrozoa.integration.rbr.model.petri.net.Transitions.RBRTransitionId
import hydrozoa.lib.number.PositiveInt

/** Arcs for [[RBRTransitionId.Ratchet]] (RatchetVoteTx).
  *
  * RatchetVoteTx moves an already-Open ballot box forward using a newer SEC. On-chain it accepts
  * both `Voted` and `Abstain` inputs and produces a `Voted` output. In our marking-only model the
  * `Voted → Voted` case is a self-loop (versionMinor bump is invisible without data-aware arcs), so
  * only the `Abstain → Voted` variant is expressed — the only variant that changes the marking.
  */
object Ratchet {
    val id: RBRTransitionId = RBRTransitionId.Ratchet

    val arcs: List[(RBRArcId, RBRArc)] = {
        val w1 = PositiveInt.unsafeApply(1)
        List(
          RBRArcId.RatchetReadTreasury -> RBRReadArc(UnresolvedTreasuryPlaceId, id, w1),
          RBRArcId.RatchetSpendAbstain -> RBRPTArc(AbstainPlaceId, id, w1),
          RBRArcId.RatchetProduceVoted -> RBRTPArc(VotedPlaceId, id, w1),
        )
    }
}
