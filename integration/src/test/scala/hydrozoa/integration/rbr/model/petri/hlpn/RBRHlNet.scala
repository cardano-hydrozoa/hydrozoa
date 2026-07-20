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
  * spend-and-recreate utxos are `Pt`+`Tp` self-loop pairs; a required signer is captured by
  * consuming/reading that peer's own token).
  *
  * A ballot box is one token colored `(key, (link, (status, versionMinor)))`, mirroring the
  * on-chain `VoteDatum(key, link, voteStatus)` with the status sum flattened: `versionMinor` is
  * meaningful only for `Voted` boxes and 0 otherwise (0 is on-chain-real — the public box seeds
  * `Voted(versionMinor = 0)`). The `Owner` place is the static peer↔key relation ("peer i owns box
  * key i+1"); box ownership checks are token presence, not arithmetic. The kzg commitment is left
  * out of the vote payload for now.
  *
  * The voting deadline is the untimed phase pair `VotingOpen`/`VotingClosed`: `VotingDeadline` is
  * the untimed projection of an ISO 15909-1 Clause-10 `[D,D]` transition — adding Clause-10 time
  * later changes only *when* it fires. `Vote`/`RatchetVote` read `VotingOpen`
  * (`ValidityEndSlot(deadline)`); the tallies read `VotingClosed` (`ValidityStartSlot(deadline+1)`);
  * `Abstain` reads neither (`AbstainTx` has no validity window).
  */
object RBRHlNet {

    /** Ballot-box status, ordered by `maxVote` precedence: `Voted > AwaitingVote > Abstain` (see
      * `DisputeResolutionScript.maxVote`). The linear order lets tally winner selection be
      * `Lt`/`Eq` guards.
      */
    enum BallotStatus:
        case Abstained
        case Awaiting
        case Voted

    object BallotStatus {
        given Order[BallotStatus] = Order.by(_.ordinal)
    }

    enum RBRPlaceId:
        case Ballots
        case Owner
        case VotingOpen
        case VotingClosed
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
        case RatchetVote
        case TallyContinuingWins
        case TallyRemovedWins
        case VotingDeadline
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

    /** A ballot-box color value: `(key, (link, (status, versionMinor)))`. */
    type Ballot = (BigInt, (BigInt, (BallotStatus, BigInt)))

    /** Build the RBR net for the given head-peer count and minor-version bound.
      *
      * Color classes: `Peer` (unordered peer numbers `0..nHeadPeers-1`), `Key` (unordered BigInts
      * `0..nHeadPeers` — box keys and links; link 0 is the end-of-list sentinel), `Status`
      * ([[BallotStatus]], linear), and `Version` (linear BigInts `0..maxVersionMinor`, whose order
      * carries ratchet monotonicity and tally winner selection).
      *
      * Initial ballots mirror `FallbackTx`: the public box `(0, (1, (Voted, 0)))` plus peer boxes
      * `(i+1, (i+2 | 0 for the last, (Awaiting, 0)))`.
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
        val keyClass = Sort.Class(
          "Key",
          NonEmptySet.of(BigInt(0), (1 to nHeadPeers).map(BigInt(_))*),
          Sort.Discipline.Unordered,
          Map.empty
        )
        val statusClass = Sort.Class(
          "Status",
          NonEmptySet.of(BallotStatus.Abstained, BallotStatus.Awaiting, BallotStatus.Voted),
          Sort.Discipline.Linear,
          Map.empty
        )
        val versionClass = Sort.Class(
          "Version",
          NonEmptySet.of(BigInt(0), (1 to maxVersionMinor).map(BigInt(_))*),
          Sort.Discipline.Linear,
          Map.empty
        )
        val ballotSort: Sort[Ballot] =
            Sort.Prod(keyClass, Sort.Prod(keyClass, Sort.Prod(statusClass, versionClass)))
        val ownerSort: Sort[(HeadPeerNumber, BigInt)] = Sort.Prod(peerClass, keyClass)

        given Order[Ballot] = ballotSort.order
        given Order[(HeadPeerNumber, BigInt)] = ownerSort.order

        // ---- Variables ----
        val p = Var("p", peerClass) // the box owner (Vote / Abstain signer)
        val q = Var("q", peerClass) // the acting peer supplying collateral (ratchet / tally)
        val k = Var("k", keyClass)
        val l = Var("l", keyClass)
        val s = Var("s", statusClass)
        val v = Var("v", versionClass)
        val vOld = Var("vOld", versionClass)
        val vNew = Var("vNew", versionClass)
        // Tally operands: the continuing box (1) and the removed box (2)
        val k1 = Var("k1", keyClass)
        val l1 = Var("l1", keyClass)
        val s1 = Var("s1", statusClass)
        val v1 = Var("v1", versionClass)
        val k2 = Var("k2", keyClass)
        val l2 = Var("l2", keyClass)
        val s2 = Var("s2", statusClass)
        val v2 = Var("v2", versionClass)

        // ---- Terms ----
        import ColorTerm.{Const, Ref, Tuple}
        def one[C](term: ColorTerm[C]): Inscription[C] =
            Inscription.Weighted(PositiveInt.unsafeApply(1), term)
        def ballot(
            key: ColorTerm[BigInt],
            link: ColorTerm[BigInt],
            status: ColorTerm[BallotStatus],
            version: ColorTerm[BigInt],
        ): ColorTerm[Ballot] = Tuple(key, Tuple(link, Tuple(status, version)))

        val awaiting = Const(BallotStatus.Awaiting, statusClass)
        val voted = Const(BallotStatus.Voted, statusClass)
        val abstainedStatus = Const(BallotStatus.Abstained, statusClass)
        val version0 = Const(BigInt(0), versionClass)

        val peerToken = one(Ref(p)) // ⟨p⟩
        val actorToken = one(Ref(q)) // ⟨q⟩
        val ownerToken = one(Tuple(Ref(p), Ref(k))) // ⟨(p, k)⟩
        val dotToken = one(Const((), Sort.Dot)) // ⟨•⟩

        // ---- Initial markings ----
        def bagOf[C](entries: (C, Int)*)(using Order[C]): MultiSet[C] =
            Multiset(entries.map((c, n) => c -> SafeLong(n)).to(SortedMap))
        val allPeers: MultiSet[HeadPeerNumber] = bagOf(peers.toSortedSet.toSeq.map(_ -> 1)*)
        val oneDot: MultiSet[Unit] = bagOf(() -> 1)
        val noDots: MultiSet[Unit] = bagOf[Unit]()

        // FallbackTx seeds: the public box (key 0, already Voted at version 0) and one AwaitingVote
        // box per peer; the last box's link is the 0 sentinel.
        val initialBallots: MultiSet[Ballot] = bagOf(
          (
            (BigInt(0), (BigInt(1), (BallotStatus.Voted, BigInt(0)))): Ballot,
            1
          ) +: (0 until nHeadPeers).map { i =>
              val link = if i < nHeadPeers - 1 then BigInt(i + 2) else BigInt(0)
              ((BigInt(i + 1), (link, (BallotStatus.Awaiting, BigInt(0)))): Ballot, 1)
          }*
        )
        val ownership: MultiSet[(HeadPeerNumber, BigInt)] =
            bagOf((0 until nHeadPeers).map(i => (HeadPeerNumber(i), BigInt(i + 1)) -> 1)*)

        val b = NetBuilder[RBRPlaceId, RBRTransitionId]()

        val program = for {
            // ---- Places ----
            ballots <- b.place(Ballots, RBRPlace(initialBallots, ballotSort))
            owner <- b.place(Owner, RBRPlace(ownership, ownerSort))
            votingOpen <- b.place(VotingOpen, RBRPlace(oneDot, Sort.Dot))
            votingClosed <- b.place(VotingClosed, RBRPlace(noDots, Sort.Dot))
            unresolvedTreasury <- b.place(UnresolvedTreasury, RBRPlace(oneDot, Sort.Dot))
            _ <- b.place(ResolvedTreasury, RBRPlace(noDots, Sort.Dot))
            regimeRef <- b.place(RegimeRef, RBRPlace(oneDot, Sort.Dot))
            disputeScriptRef <- b.place(DisputeScriptRef, RBRPlace(oneDot, Sort.Dot))
            collateral <- b.place(Collateral, RBRPlace(allPeers, peerClass))

            // ---- Vote (mirrors VoteTx.Build.buildVoteTx) ----
            vote <- b.transition(RBRTransitionId.Vote, List(p, k, l, v), Guard.True)
            // uncastBallotBox.votingSpend / votedOutput.send: AwaitingVote → Voted(v)
            _ <- b.input(ballots, vote, one(ballot(Ref(k), Ref(l), awaiting, version0)))
            _ <- b.output(vote, ballots, one(ballot(Ref(k), Ref(l), voted, Ref(v))))
            // addRequiredSigners(votingSigners): the box's peer signs — presence in Owner
            _ <- b.input(owner, vote, ownerToken)
            _ <- b.output(vote, owner, ownerToken)
            // collateralUtxo.spend / collateralOutput.send: the peer's collateral, recreated
            _ <- b.input(collateral, vote, peerToken)
            _ <- b.output(vote, collateral, peerToken)
            // treasuryUtxo.referenceOutput / regimeUtxo.referenceOutput / config.referenceDispute
            _ <- b.input(unresolvedTreasury, vote, dotToken)
            _ <- b.output(vote, unresolvedTreasury, dotToken)
            _ <- b.input(regimeRef, vote, dotToken)
            _ <- b.output(vote, regimeRef, dotToken)
            _ <- b.input(disputeScriptRef, vote, dotToken)
            _ <- b.output(vote, disputeScriptRef, dotToken)
            // ValidityEndSlot(votingDeadline): only while voting is open
            _ <- b.input(votingOpen, vote, dotToken)
            _ <- b.output(vote, votingOpen, dotToken)

            // ---- Abstain (mirrors AbstainTx.Build.buildAbstainTx) ----
            // Leaner than Vote: no treasury/regime references, no validity window.
            abstain <- b.transition(RBRTransitionId.Abstain, List(p, k, l), Guard.True)
            // uncastBallotBox.votingSpend(Abstain) / abstainOutput.send: AwaitingVote → Abstain
            _ <- b.input(ballots, abstain, one(ballot(Ref(k), Ref(l), awaiting, version0)))
            _ <- b.output(abstain, ballots, one(ballot(Ref(k), Ref(l), abstainedStatus, version0)))
            // addRequiredSigners(votingSigners): the box's peer signs
            _ <- b.input(owner, abstain, ownerToken)
            _ <- b.output(abstain, owner, ownerToken)
            // collateralUtxo.spend / collateralOutput.send
            _ <- b.input(collateral, abstain, peerToken)
            _ <- b.output(abstain, collateral, peerToken)
            // config.referenceDispute
            _ <- b.input(disputeScriptRef, abstain, dotToken)
            _ <- b.output(abstain, disputeScriptRef, dotToken)

            // ---- RatchetVote (mirrors RatchetVoteTx.Build; spent box is Voted or Abstain) ----
            // The on-chain script skips the tx-signer check: any peer q ratchets with a
            // multisigned SEC, supplying its own collateral. Monotonicity (the on-chain
            // `VoteRatchetNotMonotonic` check) is the Lt guard on the linear Version class —
            // Abstained boxes carry version 0, so "Abstain ratchets as prev = 0" is the same guard.
            ratchet <- b.transition(
              RBRTransitionId.RatchetVote,
              List(q, k, l, s, vOld, vNew),
              Guard.And(
                Guard.Not(Guard.Eq(Ref(s), awaiting)),
                Guard.Lt(Ref(vOld), Ref(vNew))
              )
            )
            // openBallotBox.spend / votedOutput.send: (s, vOld) → Voted(vNew)
            _ <- b.input(ballots, ratchet, one(ballot(Ref(k), Ref(l), Ref(s), Ref(vOld))))
            _ <- b.output(ratchet, ballots, one(ballot(Ref(k), Ref(l), voted, Ref(vNew))))
            // collateralUtxo.spend / collateralOutput.send: the acting peer's collateral
            _ <- b.input(collateral, ratchet, actorToken)
            _ <- b.output(ratchet, collateral, actorToken)
            // treasuryUtxo.referenceOutput / regimeUtxo.referenceOutput / config.referenceDispute
            _ <- b.input(unresolvedTreasury, ratchet, dotToken)
            _ <- b.output(ratchet, unresolvedTreasury, dotToken)
            _ <- b.input(regimeRef, ratchet, dotToken)
            _ <- b.output(ratchet, regimeRef, dotToken)
            _ <- b.input(disputeScriptRef, ratchet, dotToken)
            _ <- b.output(ratchet, disputeScriptRef, dotToken)
            // ValidityEndSlot(votingDeadline)
            _ <- b.input(votingOpen, ratchet, dotToken)
            _ <- b.output(ratchet, votingOpen, dotToken)

            // ---- Tally (mirrors TallyTx.Build; split by the maxVote winner) ----
            // The continuing box (k1, l1) absorbs its successor (k2, l2): adjacency is the
            // Eq(l1, k2) guard, and the result keeps k1 and inherits l2. maxVote's ordering is the
            // Status/Version linear orders; ties go to the removed box (`else b` in maxVote).
            tallyContinuing <- b.transition(
              RBRTransitionId.TallyContinuingWins,
              List(q, k1, l1, s1, v1, k2, l2, s2, v2),
              Guard.And(
                Guard.Eq(Ref(l1), Ref(k2)),
                Guard.Or(
                  Guard.Lt(Ref(s2), Ref(s1)),
                  Guard.And(Guard.Eq(Ref(s1), Ref(s2)), Guard.Lt(Ref(v2), Ref(v1)))
                )
              )
            )
            // continuingBallotBox.spend + removedBallotBox.spend (one Union inscription — W is a
            // function on F, so both boxes ride a single input arc) / tallied.send
            _ <- b.input(
              ballots,
              tallyContinuing,
              Inscription.Union(
                one(ballot(Ref(k1), Ref(l1), Ref(s1), Ref(v1))),
                one(ballot(Ref(k2), Ref(l2), Ref(s2), Ref(v2)))
              )
            )
            _ <- b.output(tallyContinuing, ballots, one(ballot(Ref(k1), Ref(l2), Ref(s1), Ref(v1))))
            // collateralUtxo.add only — presence, never spent
            _ <- b.input(collateral, tallyContinuing, actorToken)
            _ <- b.output(tallyContinuing, collateral, actorToken)
            // treasuryUtxo.referenceOutput / regimeUtxo.referenceOutput / config.referenceDispute
            _ <- b.input(unresolvedTreasury, tallyContinuing, dotToken)
            _ <- b.output(tallyContinuing, unresolvedTreasury, dotToken)
            _ <- b.input(regimeRef, tallyContinuing, dotToken)
            _ <- b.output(tallyContinuing, regimeRef, dotToken)
            _ <- b.input(disputeScriptRef, tallyContinuing, dotToken)
            _ <- b.output(tallyContinuing, disputeScriptRef, dotToken)
            // ValidityStartSlot(votingDeadline + 1): only after the deadline
            _ <- b.input(votingClosed, tallyContinuing, dotToken)
            _ <- b.output(tallyContinuing, votingClosed, dotToken)

            tallyRemoved <- b.transition(
              RBRTransitionId.TallyRemovedWins,
              List(q, k1, l1, s1, v1, k2, l2, s2, v2),
              Guard.And(
                Guard.Eq(Ref(l1), Ref(k2)),
                Guard.Or(
                  Guard.Lt(Ref(s1), Ref(s2)),
                  Guard.And(
                    Guard.Eq(Ref(s1), Ref(s2)),
                    Guard.Or(Guard.Lt(Ref(v1), Ref(v2)), Guard.Eq(Ref(v1), Ref(v2)))
                  )
                )
              )
            )
            _ <- b.input(
              ballots,
              tallyRemoved,
              Inscription.Union(
                one(ballot(Ref(k1), Ref(l1), Ref(s1), Ref(v1))),
                one(ballot(Ref(k2), Ref(l2), Ref(s2), Ref(v2)))
              )
            )
            _ <- b.output(tallyRemoved, ballots, one(ballot(Ref(k1), Ref(l2), Ref(s2), Ref(v2))))
            _ <- b.input(collateral, tallyRemoved, actorToken)
            _ <- b.output(tallyRemoved, collateral, actorToken)
            _ <- b.input(unresolvedTreasury, tallyRemoved, dotToken)
            _ <- b.output(tallyRemoved, unresolvedTreasury, dotToken)
            _ <- b.input(regimeRef, tallyRemoved, dotToken)
            _ <- b.output(tallyRemoved, regimeRef, dotToken)
            _ <- b.input(disputeScriptRef, tallyRemoved, dotToken)
            _ <- b.output(tallyRemoved, disputeScriptRef, dotToken)
            _ <- b.input(votingClosed, tallyRemoved, dotToken)
            _ <- b.output(tallyRemoved, votingClosed, dotToken)

            // ---- VotingDeadline (untimed ISO Clause-10 [D,D] projection) ----
            deadline <- b.transition(RBRTransitionId.VotingDeadline, List(), Guard.True)
            _ <- b.input(votingOpen, deadline, dotToken)
            _ <- b.output(deadline, votingClosed, dotToken)
        } yield ()

        b.build(program)
    }
}
