package hydrozoa.integration.rbr.model.petri.net

import cats.syntax.all.*
import hydrozoa.integration.rbr.model.petri.net.RBRPlaceId.*
import hydrozoa.integration.rbr.model.petri.net.Transitions.RBRTransitionId
import hydrozoa.integration.rbr.model.petri.net.transitions.{Abstain, Evacuation, Ratchet, Resolution, Tally, Vote }
import hydrozoa.lib.number.{NonNegativeInt, PositiveInt}
import hydrozoa.lib.petri.MapNet

/** The combined RBR petri net covering the post-fallback lifecycle: Vote / Abstain / Ratchet /
  * Tally (six flavors) / Resolution / Evacuation.
  *
  * Initial marking reflects the post-fallback on-chain state: 1 unresolved treasury, 1 public
  * `Voted` ballot box (from FallbackTx), `nHeadPeers` `AwaitingVote` ballot boxes, `nHeadPeers`
  * collateral utxos, and the four ref-type utxos (TreasuryRef, DisputeRef, RegimeRef, and 7
  * SetupLadder rungs). Terminal marking (per tech-lead decision: no Deinit) leaves the resolved
  * treasury and the evacuated outputs in place.
  *
  * `AmbientPlaceId` is seeded at `2 * (nEvacs / payoutBatchSize)` — the exact amount
  * [[Evacuation]]'s PT(2) arc will drain across all its firings. This is a modeling stand-in for
  * the wallet-ADA fee/change flow the real RBA performs; the marking-only library cannot express
  * variable-weight arcs so we constrain `nEvacs` to be a multiple of `payoutBatchSize`.
  */
object RBRNet {

    type Type = MapNet[RBRArcId, RBRPlaceId, RBRTransitionId, RBRArc, RBRPlace, RBRTransition]

    /** Net-construction parameters. `nHeadPeers` sizes the AwaitingVote and collateral markings;
      * `nEvacs` sizes the ResolutionProducePayoutObligations arc weight and the evacuation terminal
      * count.
      */
    case class Params(
        nHeadPeers: Int,
        nEvacs: Int,
        payoutBatchSize: Int,
    ) {
        require(nHeadPeers >= 1, s"nHeadPeers must be >= 1, got $nHeadPeers")
        require(nEvacs >= 0, s"nEvacs must be >= 0, got $nEvacs")
        require(payoutBatchSize >= 1, s"payoutBatchSize must be >= 1, got $payoutBatchSize")
        require(
          nEvacs % payoutBatchSize == 0,
          s"nEvacs ($nEvacs) must be a multiple of payoutBatchSize ($payoutBatchSize)"
        )

        /** How many times [[RBRTransitionId.EvacuationId]] will fire on the way to terminal. */
        def nEvacFirings: Int = nEvacs / payoutBatchSize

        /** How many `AmbientPlaceId` tokens must be seeded so every Evacuation firing has its PT(2)
          * input available.
          */
        def ambientSeed: Int = 2 * nEvacFirings
    }

    private val ops: MapNet.BuilderMOps[
      RBRArcId,
      RBRPlaceId,
      RBRTransitionId,
      RBRArc,
      RBRPlace,
      RBRTransition,
    ] = MapNet.BuilderMOps()

    import ops.*

    def apply(params: Params): Type = {
        val builder = for {
            _ <- addPlaces(params)
            _ <- addTransitions()
            _ <- addArcs(params)
        } yield ()

        val Right((net, _)) = builder.runEmpty: @unchecked
        net
    }

    // -----------------------------------------------------------------------
    // Places
    // -----------------------------------------------------------------------

    private def addPlaces(params: Params) = {
        val n = NonNegativeInt.unsafeApply
        val one = n(1)
        val zero = n(0)
        val seven = n(7)
        val places: List[(RBRPlaceId, RBRPlace)] = List(
          TreasuryRefPlaceId ->
              TreasuryRefPlace(marking = one, finalMarking = Some(one)),
          DisputeRefPlaceId ->
              DisputeRefPlace(marking = one, finalMarking = Some(one)),
          RegimeRefPlaceId ->
              RegimeRefPlace(marking = one, finalMarking = Some(one)),
          SetupLadderRefPlaceId ->
              SetupLadderRefPlace(marking = seven, finalMarking = Some(seven)),
          UnresolvedTreasuryPlaceId ->
              UnresolvedTreasuryPlace(marking = one, finalMarking = Some(zero)),
          ResolvedTreasuryPlaceId ->
              ResolvedPlace(marking = zero, finalMarking = Some(one)),
          UnvotedPlaceId ->
              UnvotedPlace(marking = n(params.nHeadPeers), finalMarking = Some(zero)),
          VotedPlaceId ->
              VotedPlace(marking = one, finalMarking = Some(zero)),
          AbstainPlaceId ->
              AbstainPlace(marking = zero, finalMarking = Some(zero)),
          TallyBufferUnvotedPlaceId ->
              TallyBufferPlace(marking = zero, finalMarking = Some(zero)),
          TallyBufferAbstainPlaceId ->
              TallyBufferPlace(marking = zero, finalMarking = Some(zero)),
          TallyBufferVotedPlaceId ->
              TallyBufferPlace(marking = zero, finalMarking = Some(zero)),
          PayoutObligationsPlaceId ->
              PayoutObligationsPlace(marking = zero, finalMarking = Some(zero)),
          EvacuationOutputPlaceId ->
              EvacuationOutputPlace(marking = zero, finalMarking = Some(n(params.nEvacs))),
          CollateralPlaceId -> CollateralPlace(
            marking = n(params.nHeadPeers),
            expectedCount = params.nHeadPeers,
            finalMarking = Some(n(params.nHeadPeers)),
          ),
          AmbientPlaceId ->
              AmbientPlace(marking = n(params.ambientSeed), finalMarking = Some(zero)),
        )
        places.traverse_((id, p) => addPlace(id, p))
    }

    // -----------------------------------------------------------------------
    // Transitions
    // -----------------------------------------------------------------------

    private def addTransitions() =
        RBRTransitionId.values.toList
            .traverse_(id => addTransition(id, RBRTransition.singleton))

    // -----------------------------------------------------------------------
    // Arcs
    // -----------------------------------------------------------------------

    private def addArcs(params: Params) = {
        val allArcs: List[(RBRArcId, RBRArc)] =
            Vote.arcs ++
                Abstain.arcs ++
                Ratchet.arcs ++
                Tally.allArcs ++
                Resolution.arcs(NonNegativeInt.unsafeApply(params.nEvacs)) ++
                Evacuation.arcs(PositiveInt.unsafeApply(params.payoutBatchSize))
        allArcs.traverse_((id, a) => addArc(id, a))
    }
}
