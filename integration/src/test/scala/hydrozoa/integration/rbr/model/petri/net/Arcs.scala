package hydrozoa.integration.rbr.model.petri.net

import hydrozoa.integration.rbr.model.petri.net.Transitions.RBRTransitionId
import hydrozoa.lib.number.PositiveInt
import hydrozoa.lib.petri.net.components.Arc

/** Generic arc case classes for the RBR net, spanning every transition.
  *
  * Every arc carries a place id, a transition id, and a weight. Which arc semantic (PT, TP, Read,
  * Inhibitor) applies is determined by the case class. The files under `transitions/` instantiate
  * these directly.
  */
sealed trait RBRArc
    extends Arc.Topology[RBRPlaceId, RBRTransitionId],
      Arc.Syntax,
      Arc.Semantics[RBRPlace]

/** A `Place → Transition` arc: removes `weight` tokens from the place on firing. */
case class RBRPTArc(
    override val arcPlaceId: RBRPlaceId,
    override val arcTransitionId: RBRTransitionId,
    override val weight: PositiveInt,
) extends RBRArc,
      Arc.Semantics.PT[RBRPlace]

/** A `Transition → Place` arc: adds `weight` tokens to the place on firing. */
case class RBRTPArc(
    override val arcPlaceId: RBRPlaceId,
    override val arcTransitionId: RBRTransitionId,
    override val weight: PositiveInt,
) extends RBRArc,
      Arc.Semantics.TP[RBRPlace]

/** A read arc: enables the transition only if the place has ≥ `weight` tokens; does not modify. */
case class RBRReadArc(
    override val arcPlaceId: RBRPlaceId,
    override val arcTransitionId: RBRTransitionId,
    override val weight: PositiveInt,
) extends RBRArc,
      Arc.Semantics.Read[RBRPlace]

/** An inhibitor arc: enables the transition only if the place is empty. Weight is unused by
  * semantics but retained for `PositiveInt` uniformity across the arc hierarchy.
  */
case class RBRInhibitorArc(
    override val arcPlaceId: RBRPlaceId,
    override val arcTransitionId: RBRTransitionId,
    weight: PositiveInt = PositiveInt.unsafeApply(1),
) extends RBRArc,
      Arc.Semantics.Inhibitor[RBRPlace]

/** Arc identities for the full RBR net.
  *
  * Names are prefixed with their owning transition. The `Read*Treasury` arcs to
  * [[RBRPlaceId.UnresolvedTreasuryPlaceId]] encode a causal ordering — once
  * [[RBRTransitionId.Resolution]] consumes the treasury, any dispute-side transition that reads it
  * becomes disabled. Reads to places that never deplete (ref-script utxos, regime witness,
  * collateral) are omitted: at marking level they're always-enabled and only add bookkeeping.
  *
  * The [[Ordering]] derived from `.ordinal` produces a canonical enumeration order, which drives
  * deterministic firing in [[hydrozoa.integration.rbr.model.RBRModel]].
  */
enum RBRArcId:
    // --- Vote (VoteTx) --------------------------------------------------------
    case VoteReadTreasury
    case VoteSpendUnvoted
    case VoteProduceVoted

    // --- Abstain (AbstainTx) --------------------------------------------------
    // No treasury Read: AbstainTx has no treasury reference (only a per-peer signature check).
    case AbstainSpendUnvoted
    case AbstainProduceAbstain

    // --- Ratchet (RatchetVoteTx) ----------------------------------------------
    // Marking-only model: only the Abstain → Voted variant is visible. The Voted → Voted
    // (versionMinor bump) case is a self-loop at marking level and is not modeled.
    case RatchetReadTreasury
    case RatchetSpendAbstain
    case RatchetProduceVoted

    // --- Tally (TallyTx) ------------------------------------------------------
    // Six flavors for the unordered pairs {U, A, V}. The library's SingleArc topology
    // constraint forbids putting both a PT and a TP arc on the same (place, transition) pair,
    // so the same-status flavors (UU/AA/VV) cannot be written as PT(2)+TP(1). Instead they go
    // through a synthetic TallyBuffer* place in two steps: TallyXX consumes 2 X and produces 1
    // buffer token, then TallyRestoreX consumes the buffer token and produces 1 X. Net effect
    // per pair: X -= 1.
    //
    // Mixed flavors (UA/UV/AV) stay atomic: PT on the "loser" status + Read on the "winner"
    // status. Different places, so no SingleArc conflict.

    case TallyUUReadTreasury
    case TallyUUSpendUnvoted // weight = 2
    case TallyUUProduceBuffer

    case TallyRestoreUSpendBuffer
    case TallyRestoreUProduceUnvoted

    case TallyUAReadTreasury
    case TallyUASpendUnvoted
    case TallyUAReadAbstain

    case TallyUVReadTreasury
    case TallyUVSpendUnvoted
    case TallyUVReadVoted

    case TallyAAReadTreasury
    case TallyAASpendAbstain // weight = 2
    case TallyAAProduceBuffer

    case TallyRestoreASpendBuffer
    case TallyRestoreAProduceAbstain

    case TallyAVReadTreasury
    case TallyAVSpendAbstain
    case TallyAVReadVoted

    case TallyVVReadTreasury
    case TallyVVSpendVoted // weight = 2
    case TallyVVProduceBuffer

    case TallyRestoreVSpendBuffer
    case TallyRestoreVProduceVoted

    // --- Resolution (ResolutionTx) -------------------------------------------
    // Resolution spends the Unresolved treasury and the final Voted ballot; produces the Resolved
    // treasury and `nEvacs` PayoutObligations tokens.
    case ResolutionSpendUnresolved
    case ResolutionSpendVoted
    case ResolutionProduceResolved
    case ResolutionProducePayoutObligations // weight = nEvacs (set at net construction)

    // --- Evacuation (EvacuationTx) -------------------------------------------
    // Preserved verbatim from the previous transition-scoped Evacuation net so its firing
    // semantics remain unchanged.
    case EvacuationReadTreasuryRef
    case EvacuationReadResolved
    case EvacuationReadSetupLadder
    case EvacuationReadCollateral
    case EvacuationSpendAmbient
    case EvacuationSpendPayoutObligations
    case EvacuationProduceEvacuationOutput

object RBRArcId {
    given Ordering[RBRArcId] = Ordering.by(_.ordinal)
}
