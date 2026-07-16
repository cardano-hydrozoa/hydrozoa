package hydrozoa.integration.rbr.model.petri.net

import hydrozoa.lib.petri.net.components.Transition

/** The RBR net attaches no data or behavior to a transition beyond its identity, so a single shared
  * marker satisfies `Transition.Topology & Transition.Syntax & Transition.Semantics` for every
  * `RBRTransitionId`.
  */
type RBRTransition = Transition.Topology & Transition.Syntax & Transition.Semantics

object RBRTransition {
    val singleton: RBRTransition =
        new Transition.Topology with Transition.Syntax with Transition.Semantics {}
}

object Transitions {

    /** Transition ids covering the RBR lifecycle from post-fallback to post-evacuation.
      *
      * `DeinitId` is deliberately omitted for PR1a: the tech-lead decision is that peers submit
      * DeinitTx manually (consensus-broken paths cannot safely automate it), so the RBA's terminal
      * matches the model's terminal at `ResolvedTreasury = 1`, `EvacuationOutput = nEvacs`.
      *
      * The six `Tally*` cases correspond to the unordered pairs of ballot-box statuses that TallyTx
      * can consume: U=Unvoted (AwaitingVote), A=Abstain, V=Voted. TallyTx itself is one on-chain tx
      * family (`TallyTx.scala`) — the split is a modeling artefact because our marking-only arcs
      * cannot express "consume any two of {Unvoted, Abstain, Voted}" in one transition.
      *
      * Note: ISO 15909-1 "High-Level Petri Nets" (HLPN) allows the six variants to collapse into a
      * single transition via typed arc inscriptions (colored tokens + guards). Once the library
      * supports data-aware / colored arcs, these can be folded.
      */
    enum RBRTransitionId:
        case Vote
        case Abstain
        case Ratchet
        // Restore transitions come before their Consume counterparts in canonical firing order.
        // See transitions/Tally.scala for why the same-status Tally variants are split into
        // Consume + Restore across a synthetic TallyBufferPlace.
        case TallyRestoreU
        case TallyRestoreA
        case TallyRestoreV
        case TallyUU
        case TallyUA
        case TallyUV
        case TallyAA
        case TallyAV
        case TallyVV
        case Resolution
        case EvacuationId

    object RBRTransitionId {
        given Ordering[RBRTransitionId] = Ordering.by(_.ordinal)
    }
}
