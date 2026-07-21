package hydrozoa.integration.rbr.model.petri.hlpn

import cats.data.{NonEmptySet, ValidatedNel}
import cats.implicits.catsKernelOrderingForOrder
import hydrozoa.lib.collection.Multiset
import hydrozoa.lib.number.PositiveInt
import hydrozoa.lib.petri.hlpn.*
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.rulebased.ledger.l1.state.VoteState.{Key, Link}
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
        // Explicit `maxVote` precedence, not `_.ordinal`: reordering or inserting an enum case must
        // not silently invert the order that tally winner selection and `Lt` depend on.
        private def rank(status: BallotStatus): Int = status match
            case Abstained => 0
            case Awaiting  => 1
            case Voted     => 2

        given Order[BallotStatus] = Order.by(rank)
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

    /** A ballot-box color value: `(key, (link, (status, versionMinor)))`. `Key`/`Link` reuse the
      * on-chain `VoteState` aliases (both `BigInt`); the version minor stays a plain `BigInt`.
      */
    type Ballot = (Key, (Link, (BallotStatus, BigInt)))

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
        // TODO: harden. The product domain `Key × Key × Status × Version` over-approximates valid
        // ballots — it admits structurally-impossible colors like `key == link` (a box linking to
        // itself). The initial marking seeds only well-formed boxes, but nothing rejects an invalid
        // one arising during firing. Enforce via a `ColoredPlace.markingError` invariant on the
        // Ballots place (e.g. `key != link`) or a domain refinement.
        val ballotSort: Sort[Ballot] =
            Sort.Prod(keyClass, Sort.Prod(keyClass, Sort.Prod(statusClass, versionClass)))
        // TODO: harden. `Peer × Key` admits any (peer, key) pair; only the diagonal `(i, i+1)`
        // ("peer i owns box key i+1") is real. The initial `Owner` marking seeds the diagonal, but
        // the relation isn't enforced — a markingError invariant (or making Owner a derived
        // function rather than free tokens) would pin it.
        val ownerSort: Sort[(HeadPeerNumber, BigInt)] = Sort.Prod(peerClass, keyClass)

        given Order[Ballot] = ballotSort.order
        given Order[(HeadPeerNumber, BigInt)] = ownerSort.order

        // ---- Variables ----
        val peer = Var("peer", peerClass) // the box owner (Vote / Abstain signer)
        // the acting peer supplying collateral (ratchet / tally)
        val collateralPeer = Var("collateralPeer", peerClass)
        val key = Var("key", keyClass)
        val link = Var("link", keyClass)
        val status = Var("status", statusClass)
        val version = Var("version", versionClass)
        val versionOld = Var("versionOld", versionClass)
        val versionNew = Var("versionNew", versionClass)
        // Tally operands: the continuing box (1) and the removed box (2)
        val key1 = Var("key1", keyClass)
        val link1 = Var("link1", keyClass)
        val status1 = Var("status1", statusClass)
        val version1 = Var("version1", versionClass)
        val key2 = Var("key2", keyClass)
        val link2 = Var("link2", keyClass)
        val status2 = Var("status2", statusClass)
        val version2 = Var("version2", versionClass)

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

        val peerToken = one(Ref(peer)) // ⟨peer⟩
        val collateralPeerToken = one(Ref(collateralPeer)) // ⟨collateralPeer⟩
        val ownerToken = one(Tuple(Ref(peer), Ref(key))) // ⟨(peer, key)⟩
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
            vote <- b.transition(RBRTransitionId.Vote, List(peer, key, link, version), Guard.True)
            // uncastBallotBox.votingSpend / votedOutput.send: AwaitingVote → Voted(version)
            _ <- b.input(ballots, vote, one(ballot(Ref(key), Ref(link), awaiting, version0)))
            _ <- b.output(vote, ballots, one(ballot(Ref(key), Ref(link), voted, Ref(version))))
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
            abstain <- b.transition(RBRTransitionId.Abstain, List(peer, key, link), Guard.True)
            // uncastBallotBox.votingSpend(Abstain) / abstainOutput.send: AwaitingVote → Abstain
            _ <- b.input(ballots, abstain, one(ballot(Ref(key), Ref(link), awaiting, version0)))
            _ <- b.output(abstain, ballots, one(ballot(Ref(key), Ref(link), abstainedStatus, version0)))
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
              List(collateralPeer, key, link, status, versionOld, versionNew),
              Guard.And(
                Guard.Not(Guard.Eq(Ref(status), awaiting)),
                Guard.Lt(Ref(versionOld), Ref(versionNew))
              )
            )
            // openBallotBox.spend / votedOutput.send: (status, versionOld) → Voted(versionNew)
            _ <- b.input(ballots, ratchet, one(ballot(Ref(key), Ref(link), Ref(status), Ref(versionOld))))
            _ <- b.output(ratchet, ballots, one(ballot(Ref(key), Ref(link), voted, Ref(versionNew))))
            // collateralUtxo.spend / collateralOutput.send: the acting peer's collateral
            _ <- b.input(collateral, ratchet, collateralPeerToken)
            _ <- b.output(ratchet, collateral, collateralPeerToken)
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
              List(collateralPeer, key1, link1, status1, version1, key2, link2, status2, version2),
              Guard.And(
                Guard.Eq(Ref(link1), Ref(key2)),
                Guard.Or(
                  Guard.Lt(Ref(status2), Ref(status1)),
                  Guard.And(Guard.Eq(Ref(status1), Ref(status2)), Guard.Lt(Ref(version2), Ref(version1)))
                )
              )
            )
            // continuingBallotBox.spend + removedBallotBox.spend (one Union inscription — W is a
            // function on F, so both boxes ride a single input arc) / tallied.send
            _ <- b.input(
              ballots,
              tallyContinuing,
              Inscription.Union(
                one(ballot(Ref(key1), Ref(link1), Ref(status1), Ref(version1))),
                one(ballot(Ref(key2), Ref(link2), Ref(status2), Ref(version2)))
              )
            )
            _ <- b.output(
              tallyContinuing,
              ballots,
              one(ballot(Ref(key1), Ref(link2), Ref(status1), Ref(version1)))
            )
            // collateralUtxo.add only — presence, never spent
            _ <- b.input(collateral, tallyContinuing, collateralPeerToken)
            _ <- b.output(tallyContinuing, collateral, collateralPeerToken)
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
              List(collateralPeer, key1, link1, status1, version1, key2, link2, status2, version2),
              Guard.And(
                Guard.Eq(Ref(link1), Ref(key2)),
                Guard.Or(
                  Guard.Lt(Ref(status1), Ref(status2)),
                  Guard.And(
                    Guard.Eq(Ref(status1), Ref(status2)),
                    Guard.Or(Guard.Lt(Ref(version1), Ref(version2)), Guard.Eq(Ref(version1), Ref(version2)))
                  )
                )
              )
            )
            _ <- b.input(
              ballots,
              tallyRemoved,
              Inscription.Union(
                one(ballot(Ref(key1), Ref(link1), Ref(status1), Ref(version1))),
                one(ballot(Ref(key2), Ref(link2), Ref(status2), Ref(version2)))
              )
            )
            _ <- b.output(
              tallyRemoved,
              ballots,
              one(ballot(Ref(key1), Ref(link2), Ref(status2), Ref(version2)))
            )
            _ <- b.input(collateral, tallyRemoved, collateralPeerToken)
            _ <- b.output(tallyRemoved, collateral, collateralPeerToken)
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
