package hydrozoa.integration.rbr.model.petri.hlpn

import cats.data.{NonEmptySet, ValidatedNel}
import cats.implicits.catsKernelOrderingForOrder
import hydrozoa.lib.collection.Multiset
import hydrozoa.lib.number.PositiveInt
import hydrozoa.lib.petri.hlpn.*
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import scala.collection.immutable.SortedMap
import spire.algebra.Order
import spire.math.SafeLong

/** The RBR dispute/evacuation model as an HLPN. Each transition mirrors the corresponding L1 tx
  * builder step-for-step (spent/produced utxos are input/output arcs; reference inputs and
  * spend-and-recreate utxos are `Pt`+`Tp` self-loop pairs). Time bounds (`ValidityEndSlot`) and
  * signatures are not modeled: a required signer is captured by consuming that peer's own token.
  *
  * Colors are the domain's own types: a peer is its [[HeadPeerNumber]] (the on-chain
  * `AwaitingVote(peer: PubKeyHash)` datum is abstracted to the peer's number) and a version is the
  * `versionMinor: BigInt` of `Voted`. The kzg commitment is left out of the vote payload for now.
  */
object RBRHlNet {

    enum RBRPlaceId:
        case AwaitingVote
        case Voted
        case Abstained
        case UnresolvedTreasury
        case ResolvedTreasury
        case RegimeRef
        case DisputeScriptRef
        case Collateral
        case SetupLadder
        case Ambient
        case PayoutObligations
        case EvacuationOutput

    object RBRPlaceId {
        given Ordering[RBRPlaceId] = Ordering.by(_.ordinal)
    }

    enum RBRTransitionId:
        case Vote
        case Abstain
        case RatchetVoted
        case RatchetAbstained
        case Tally
        case Resolution
        case Evacuation
        case Deinit

    object RBRTransitionId {
        given Ordering[RBRTransitionId] = Ordering.by(_.ordinal)
    }

    /** A plain bag place: marking + declared color domain, no extra invariants (first draft). */
    final case class RBRPlace[C](marking: MultiSet[C], colorDomain: Sort[C])
        extends ColoredPlace[C] {
        def mark(m: MultiSet[C]): RBRPlace[C] = copy(marking = m)
    }

    /** Build the RBR net for the given head-peer count and minor-version bound.
      *
      * Color classes: `Peer` (unordered — peer numbers `0..nHeadPeers-1`) and `Version` (linear
      * `1..maxVersionMinor` — whose order carries ratchet monotonicity). Ballot-box *status*
      * (`AwaitingVote | Voted | Abstain`) is which place a token sits in; the payload is the color.
      */
    def apply(
        nHeadPeers: Int,
        maxVersionMinor: Int,
    ): ValidatedNel[NetBuilder.Error, HlNet[RBRPlaceId, RBRTransitionId, Any]] = {
        import RBRPlaceId.*

        val peers: NonEmptySet[HeadPeerNumber] =
            NonEmptySet.of(
              HeadPeerNumber.zero,
              (1 until nHeadPeers).map(HeadPeerNumber(_))*
            )
        val peerClass =
            Sort.Class("Peer", peers, Sort.Discipline.Unordered, Map.empty)
        val versionClass = Sort.Class(
          "Version",
          NonEmptySet.of(BigInt(1), (2 to maxVersionMinor).map(BigInt(_))*),
          Sort.Discipline.Linear,
          Map.empty
        )
        val peerVersion: Sort[(HeadPeerNumber, BigInt)] = Sort.Prod(peerClass, versionClass)

        given Order[(HeadPeerNumber, BigInt)] = peerVersion.order

        // ---- Variables ----
        val p = Var("p", peerClass)  // the ballot box's peer
        val q = Var("q", peerClass)  // the acting peer supplying collateral (ratchets)
        val v = Var("v", versionClass)
        val vOld = Var("vOld", versionClass)
        val vNew = Var("vNew", versionClass)

        // ---- Inscriptions ----
        def one[C](term: ColorTerm[C]): Inscription[C] =
            Inscription.Weighted(PositiveInt.unsafeApply(1), term)
        val peerToken = one(ColorTerm.Ref(p)) // ⟨p⟩
        val actorToken = one(ColorTerm.Ref(q)) // ⟨q⟩
        val ballotToken = one(ColorTerm.Tuple(ColorTerm.Ref(p), ColorTerm.Ref(v))) // ⟨(p, v)⟩
        val ballotOld = one(ColorTerm.Tuple(ColorTerm.Ref(p), ColorTerm.Ref(vOld))) // ⟨(p, vOld)⟩
        val ballotNew = one(ColorTerm.Tuple(ColorTerm.Ref(p), ColorTerm.Ref(vNew))) // ⟨(p, vNew)⟩
        val dotToken = one(ColorTerm.Const((), Sort.Dot)) // ⟨•⟩

        // ---- Initial markings ----
        def bagOf[C](entries: (C, Int)*)(using Order[C]): MultiSet[C] =
            Multiset(entries.map((c, n) => c -> SafeLong(n)).to(SortedMap))
        val allPeers: MultiSet[HeadPeerNumber] = bagOf(peers.toSortedSet.toSeq.map(_ -> 1)*)
        val oneDot: MultiSet[Unit] = bagOf(() -> 1)
        val noDots: MultiSet[Unit] = bagOf[Unit]()

        val b = NetBuilder[RBRPlaceId, RBRTransitionId]()

        val program = for {
            // ---- Places ----
            awaitingVote <- b.place(AwaitingVote, RBRPlace(allPeers, peerClass))
            voted <- b.place(Voted, RBRPlace(bagOf[(HeadPeerNumber, BigInt)](), peerVersion))
            abstained <- b.place(Abstained, RBRPlace(bagOf[HeadPeerNumber](), peerClass))
            unresolvedTreasury <- b.place(UnresolvedTreasury, RBRPlace(oneDot, Sort.Dot))
            _ <- b.place(ResolvedTreasury, RBRPlace(noDots, Sort.Dot))
            regimeRef <- b.place(RegimeRef, RBRPlace(oneDot, Sort.Dot))
            disputeScriptRef <- b.place(DisputeScriptRef, RBRPlace(oneDot, Sort.Dot))
            collateral <- b.place(Collateral, RBRPlace(allPeers, peerClass))

            // ---- Vote (mirrors VoteTx.Build.buildVoteTx) ----
            vote <- b.transition(RBRTransitionId.Vote, List(p, v), Guard.True)
            // uncastBallotBox.votingSpend / votedOutput.send: AwaitingVote(p) → Voted(p, v)
            _ <- b.input(awaitingVote, vote, peerToken)
            _ <- b.output(vote, voted, ballotToken)
            // collateralUtxo.spend / collateralOutput.send: the peer's collateral, recreated
            _ <- b.input(collateral, vote, peerToken)
            _ <- b.output(vote, collateral, peerToken)
            // treasuryUtxo.referenceOutput (treasury is unresolved during voting)
            _ <- b.input(unresolvedTreasury, vote, dotToken)
            _ <- b.output(vote, unresolvedTreasury, dotToken)
            // regimeUtxo.referenceOutput
            _ <- b.input(regimeRef, vote, dotToken)
            _ <- b.output(vote, regimeRef, dotToken)
            // config.referenceDispute (the deployed dispute-validator reference script)
            _ <- b.input(disputeScriptRef, vote, dotToken)
            _ <- b.output(vote, disputeScriptRef, dotToken)

            // ---- Abstain (mirrors AbstainTx.Build.buildAbstainTx) ----
            // Leaner than Vote: no treasury/regime references, no validity window, no version.
            abstain <- b.transition(RBRTransitionId.Abstain, List(p), Guard.True)
            // uncastBallotBox.votingSpend(Abstain) / abstainOutput.send: AwaitingVote(p) → Abstain
            _ <- b.input(awaitingVote, abstain, peerToken)
            _ <- b.output(abstain, abstained, peerToken)
            // collateralUtxo.spend / collateralOutput.send
            _ <- b.input(collateral, abstain, peerToken)
            _ <- b.output(abstain, collateral, peerToken)
            // config.referenceDispute
            _ <- b.input(disputeScriptRef, abstain, dotToken)
            _ <- b.output(abstain, disputeScriptRef, dotToken)

            // ---- RatchetVoted (mirrors RatchetVoteTx.Build; spent box is Voted) ----
            // The on-chain script skips the tx-signer check: any peer q ratchets with a
            // multisigned SEC, supplying its own collateral. Monotonicity (the on-chain
            // `VoteRatchetNotMonotonic` check) is the Lt guard on the linear Version class.
            ratchetVoted <- b.transition(
              RBRTransitionId.RatchetVoted,
              List(p, vOld, vNew, q),
              Guard.Lt(ColorTerm.Ref(vOld), ColorTerm.Ref(vNew))
            )
            // openBallotBox.spend / votedOutput.send: Voted(p, vOld) → Voted(p, vNew)
            _ <- b.input(voted, ratchetVoted, ballotOld)
            _ <- b.output(ratchetVoted, voted, ballotNew)
            // collateralUtxo.spend / collateralOutput.send: the acting peer's collateral
            _ <- b.input(collateral, ratchetVoted, actorToken)
            _ <- b.output(ratchetVoted, collateral, actorToken)
            // treasuryUtxo.referenceOutput / regimeUtxo.referenceOutput / config.referenceDispute
            _ <- b.input(unresolvedTreasury, ratchetVoted, dotToken)
            _ <- b.output(ratchetVoted, unresolvedTreasury, dotToken)
            _ <- b.input(regimeRef, ratchetVoted, dotToken)
            _ <- b.output(ratchetVoted, regimeRef, dotToken)
            _ <- b.input(disputeScriptRef, ratchetVoted, dotToken)
            _ <- b.output(ratchetVoted, disputeScriptRef, dotToken)

            // ---- RatchetAbstained (mirrors RatchetVoteTx.Build; spent box is Abstain) ----
            // Abstain ratchets treat the previous versionMinor as 0, so any vNew ≥ 1 is monotonic.
            ratchetAbstained <- b.transition(
              RBRTransitionId.RatchetAbstained,
              List(p, vNew, q),
              Guard.True
            )
            // openBallotBox.spend / votedOutput.send: Abstain(p) → Voted(p, vNew)
            _ <- b.input(abstained, ratchetAbstained, peerToken)
            _ <- b.output(ratchetAbstained, voted, ballotNew)
            // collateralUtxo.spend / collateralOutput.send: the acting peer's collateral
            _ <- b.input(collateral, ratchetAbstained, actorToken)
            _ <- b.output(ratchetAbstained, collateral, actorToken)
            // treasuryUtxo.referenceOutput / regimeUtxo.referenceOutput / config.referenceDispute
            _ <- b.input(unresolvedTreasury, ratchetAbstained, dotToken)
            _ <- b.output(ratchetAbstained, unresolvedTreasury, dotToken)
            _ <- b.input(regimeRef, ratchetAbstained, dotToken)
            _ <- b.output(ratchetAbstained, regimeRef, dotToken)
            _ <- b.input(disputeScriptRef, ratchetAbstained, dotToken)
            _ <- b.output(ratchetAbstained, disputeScriptRef, dotToken)
        } yield ()

        b.build(program)
    }
}
