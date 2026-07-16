package hydrozoa.integration.rbr.model.petri.net.transitions

import hydrozoa.integration.rbr.model.petri.net.*
import hydrozoa.integration.rbr.model.petri.net.RBRPlaceId.*
import hydrozoa.integration.rbr.model.petri.net.Transitions.RBRTransitionId
import hydrozoa.lib.number.PositiveInt

/** Arcs for the six [[RBRTransitionId]] `Tally*` variants (TallyTx).
  *
  * TallyTx is one on-chain tx family that consumes two linked ballot boxes and produces one whose
  * status is `maxVote(a, b)` with `Voted > Abstain > AwaitingVote`. This file enumerates one
  * transition per unordered pair of input statuses because the marking-only arc semantics cannot
  * express "consume any two of {U, A, V}" in one transition. See [[RBRTransitionId]] for the ISO
  * 15909-1 HLPN note on collapsing these.
  *
  * ## Same-status flavors (UU, AA, VV)
  *
  * These consume 2 tokens of X and produce 1 token of X (net X -= 1). Encoded via a two-step detour
  * through a synthetic `TallyBufferXPlaceId`:
  *   - `TallyXX` — PT(2) X, TP(1) buffer, Read(1) UnresolvedTreasury.
  *   - `TallyRestoreX` — PT(1) buffer, TP(1) X. No treasury read; it just returns the "winner"
  *     token to X so the pair completes.
  *
  * The detour exists because the library's `SingleArc` topology constraint forbids putting both a
  * PT and a TP arc on the same (place, transition) pair — the simulator would then apply both
  * against the same pre-snapshot and merge via last-write-wins on the TreeMap, giving a wrong net
  * effect.
  *
  * `RBRTransitionId` orders the restore transitions before the consume ones so the model driver's
  * canonical-order firing completes each pair before starting another.
  *
  * ## Mixed flavors (UA, UV, AV)
  *
  * These consume 1 loser + 1 winner and produce 1 winner (net loser -= 1, winner unchanged).
  * Encoded atomically: PT(1) on the loser status + Read(1) on the winner status. Different places,
  * so no SingleArc conflict.
  */
object Tally {

    private val w1 = PositiveInt.unsafeApply(1)
    private val w2 = PositiveInt.unsafeApply(2)

    // ---- Same-status: two-step consume/restore -----------------------------

    val uuArcs: List[(RBRArcId, RBRArc)] = {
        val id = RBRTransitionId.TallyUU
        List(
          RBRArcId.TallyUUReadTreasury -> RBRReadArc(UnresolvedTreasuryPlaceId, id, w1),
          RBRArcId.TallyUUSpendUnvoted -> RBRPTArc(UnvotedPlaceId, id, w2),
          RBRArcId.TallyUUProduceBuffer -> RBRTPArc(TallyBufferUnvotedPlaceId, id, w1),
        )
    }

    val restoreUArcs: List[(RBRArcId, RBRArc)] = {
        val id = RBRTransitionId.TallyRestoreU
        List(
          RBRArcId.TallyRestoreUSpendBuffer -> RBRPTArc(TallyBufferUnvotedPlaceId, id, w1),
          RBRArcId.TallyRestoreUProduceUnvoted -> RBRTPArc(UnvotedPlaceId, id, w1),
        )
    }

    val aaArcs: List[(RBRArcId, RBRArc)] = {
        val id = RBRTransitionId.TallyAA
        List(
          RBRArcId.TallyAAReadTreasury -> RBRReadArc(UnresolvedTreasuryPlaceId, id, w1),
          RBRArcId.TallyAASpendAbstain -> RBRPTArc(AbstainPlaceId, id, w2),
          RBRArcId.TallyAAProduceBuffer -> RBRTPArc(TallyBufferAbstainPlaceId, id, w1),
        )
    }

    val restoreAArcs: List[(RBRArcId, RBRArc)] = {
        val id = RBRTransitionId.TallyRestoreA
        List(
          RBRArcId.TallyRestoreASpendBuffer -> RBRPTArc(TallyBufferAbstainPlaceId, id, w1),
          RBRArcId.TallyRestoreAProduceAbstain -> RBRTPArc(AbstainPlaceId, id, w1),
        )
    }

    val vvArcs: List[(RBRArcId, RBRArc)] = {
        val id = RBRTransitionId.TallyVV
        List(
          RBRArcId.TallyVVReadTreasury -> RBRReadArc(UnresolvedTreasuryPlaceId, id, w1),
          RBRArcId.TallyVVSpendVoted -> RBRPTArc(VotedPlaceId, id, w2),
          RBRArcId.TallyVVProduceBuffer -> RBRTPArc(TallyBufferVotedPlaceId, id, w1),
        )
    }

    val restoreVArcs: List[(RBRArcId, RBRArc)] = {
        val id = RBRTransitionId.TallyRestoreV
        List(
          RBRArcId.TallyRestoreVSpendBuffer -> RBRPTArc(TallyBufferVotedPlaceId, id, w1),
          RBRArcId.TallyRestoreVProduceVoted -> RBRTPArc(VotedPlaceId, id, w1),
        )
    }

    // ---- Mixed status: atomic PT(loser) + Read(winner) ---------------------

    val uaArcs: List[(RBRArcId, RBRArc)] = {
        val id = RBRTransitionId.TallyUA
        List(
          RBRArcId.TallyUAReadTreasury -> RBRReadArc(UnresolvedTreasuryPlaceId, id, w1),
          RBRArcId.TallyUASpendUnvoted -> RBRPTArc(UnvotedPlaceId, id, w1),
          RBRArcId.TallyUAReadAbstain -> RBRReadArc(AbstainPlaceId, id, w1),
        )
    }

    val uvArcs: List[(RBRArcId, RBRArc)] = {
        val id = RBRTransitionId.TallyUV
        List(
          RBRArcId.TallyUVReadTreasury -> RBRReadArc(UnresolvedTreasuryPlaceId, id, w1),
          RBRArcId.TallyUVSpendUnvoted -> RBRPTArc(UnvotedPlaceId, id, w1),
          RBRArcId.TallyUVReadVoted -> RBRReadArc(VotedPlaceId, id, w1),
        )
    }

    val avArcs: List[(RBRArcId, RBRArc)] = {
        val id = RBRTransitionId.TallyAV
        List(
          RBRArcId.TallyAVReadTreasury -> RBRReadArc(UnresolvedTreasuryPlaceId, id, w1),
          RBRArcId.TallyAVSpendAbstain -> RBRPTArc(AbstainPlaceId, id, w1),
          RBRArcId.TallyAVReadVoted -> RBRReadArc(VotedPlaceId, id, w1),
        )
    }

    val allArcs: List[(RBRArcId, RBRArc)] =
        uuArcs ++ restoreUArcs ++
            aaArcs ++ restoreAArcs ++
            vvArcs ++ restoreVArcs ++
            uaArcs ++ uvArcs ++ avArcs
}
