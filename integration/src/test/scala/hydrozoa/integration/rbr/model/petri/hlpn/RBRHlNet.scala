package hydrozoa.integration.rbr.model.petri.hlpn

import cats.data.{NonEmptySet, ReaderT, ValidatedNel}
import cats.implicits.catsKernelOrderingForOrder
import hydrozoa.lib.collection.Multiset
import hydrozoa.lib.petri.Positive
import hydrozoa.lib.petri.hlpn.*
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.rulebased.ledger.l1.script.plutus.SetupLadder as SetupLadderScript
import hydrozoa.rulebased.ledger.l1.state.VoteState.{Key, Link}
import hydrozoa.rulebased.ledger.l1.tx.EvacuationTx
import scala.collection.immutable.SortedMap
import scalus.cardano.ledger.TransactionOutput
import scalus.serialization.cbor.Cbor
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
  * (`ValidityEndSlot(deadline)`); the tallies read `VotingClosed`
  * (`ValidityStartSlot(deadline+1)`); `Abstain` reads neither (`AbstainTx` has no validity window).
  *
  * Payout obligations are the committed outputs of each candidate SEC, held in `PayoutObligations`
  * keyed by version. `Resolution` produces the `ResolvedVersion` selector; `Evacuation` drains only
  * the resolved version's obligations (the losing SECs' commitments stay put) — so the paid-out set
  * follows the resolution outcome, not a fixed "latest". The kzg update is emergent (the multiset
  * shrinks by firing); the treasury-value shrink is unmodeled (the treasury is `Dot`).
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
        case ResolvedVersion
        case RegimeRef
        case DisputeScriptRef
        case TreasuryScriptRef
        case Collateral
        case SetupLadder
        case Ambient
        case VotableVersions
        case PayoutObligations
        case EvacuationOutput
        case WithdrawalOutput

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

    /** A plain bag place: marking + declared color domain, no extra invariants. Invalid colors are
      * rejected by [[ColoredPlace.markingError]] when the domain is an enumerated [[Sort.Class]].
      */
    final case class RBRPlace[C](marking: MultiSet[C], colorDomain: Sort[C])
        extends ColoredPlace[C] {
        def mark(m: MultiSet[C]): RBRPlace[C] = copy(marking = m)
    }

    /** A ballot-box color value: key, link (both `VoteState` aliases over `BigInt`), status, and
      * minor version. Opaque over the underlying `(Key, (Link, (BallotStatus, BigInt)))` product:
      * the color algebra requires the runtime color to be that tuple (Concept 14, and the marking
      * is keyed by it), so this is a named product with a constructor and accessors rather than a
      * `case class` — whose instances would not match the tuple-keyed marking.
      */
    opaque type Ballot = (Key, (Link, (BallotStatus, BigInt)))

    object Ballot {
        def apply(key: Key, link: Link, status: BallotStatus, versionMinor: BigInt): Ballot =
            (key, (link, (status, versionMinor)))

        extension (ballot: Ballot)
            def key: Key = ballot._1
            def link: Link = ballot._2._1
            def status: BallotStatus = ballot._2._2._1
            def versionMinor: BigInt = ballot._2._2._2
    }

    /** The net's places, grouped so each transition helper references them by name. */
    final case class RBRPlaces(
        ballots: PlaceRef[RBRPlaceId, Ballot],
        owner: PlaceRef[RBRPlaceId, (HeadPeerNumber, Key)],
        votingOpen: PlaceRef[RBRPlaceId, Unit],
        votingClosed: PlaceRef[RBRPlaceId, Unit],
        unresolvedTreasury: PlaceRef[RBRPlaceId, Unit],
        resolvedTreasury: PlaceRef[RBRPlaceId, Unit],
        regimeRef: PlaceRef[RBRPlaceId, Unit],
        disputeScriptRef: PlaceRef[RBRPlaceId, Unit],
        treasuryScriptRef: PlaceRef[RBRPlaceId, Unit],
        setupLadder: PlaceRef[RBRPlaceId, BigInt],
        collateral: PlaceRef[RBRPlaceId, HeadPeerNumber],
        votableVersions: PlaceRef[RBRPlaceId, BigInt],
        payoutObligations: PlaceRef[RBRPlaceId, (BigInt, TransactionOutput)],
        resolvedVersion: PlaceRef[RBRPlaceId, BigInt],
        evacuationOutput: PlaceRef[RBRPlaceId, (BigInt, TransactionOutput)],
        // A static, inert tally of L2 outputs that withdrew to L1 *before* fallback (via
        // `RolloutTx`). They sit outside the dispute entirely — seeded once and never touched by a
        // transition — so the terminal marking reports exactly the withdrawal count. Distinct from
        // `evacuationOutput` (the resolved SEC's committed obligations, drained by `Evacuation`).
        withdrawalOutput: PlaceRef[RBRPlaceId, TransactionOutput],
    )

    /** The listing of structurally-valid ballot colors: version is meaningful only for `Voted`
      * (Awaiting/Abstained carry 0), and a box never links to itself except the fully-tallied
      * terminal box `(0, 0)`. The reachability invariant the Ballots place preserves — its
      * `Key × Key × Status × Version` domain over-approximates this.
      */
    def validBallots(nHeadPeers: Int, votableVersions: Set[BigInt]): Set[Ballot] =
        val validStatusVersion: Set[(BallotStatus, BigInt)] =
            Set(BallotStatus.Abstained -> BigInt(0), BallotStatus.Awaiting -> BigInt(0)) ++
                (votableVersions + BigInt(0)).map(BallotStatus.Voted -> _)
        val keys = 0 to nHeadPeers
        val validKeyLink: Set[(BigInt, BigInt)] =
            (for { k <- keys; l <- keys if k != l } yield BigInt(k) -> BigInt(l)).toSet +
                (BigInt(0) -> BigInt(0))
        for {
            (k, l) <- validKeyLink
            (s, v) <- validStatusVersion
        } yield Ballot(k, l, s, v)

    /** The static ownership diagonal: peer `i` owns box key `i + 1`. Seeds the `Owner` place's
      * initial marking within its `Peer × Key` domain.
      */
    def validOwners(nHeadPeers: Int): Set[(HeadPeerNumber, Key)] =
        (0 until nHeadPeers).map(i => HeadPeerNumber(i) -> BigInt(i + 1)).toSet

    /** Build the RBR net for the given head-peer count and committed obligations.
      *
      * Color domains: `Peer`/`Key`/`Version` are intensional [[Sort.Data]] — no sized carrier, so
      * the net graph is independent of `nHeadPeers` and the version count (only the marking varies,
      * so a property proven over the structure holds for all n). Peer/Key are unordered; `Version`
      * is `linear`, its order carrying ratchet monotonicity and tally winner selection. `Status`
      * ([[BallotStatus]]) stays a linear enumerated class. Keys `0..nHeadPeers` are box keys and
      * links; link 0 is the end-of-list sentinel.
      *
      * Initial ballots mirror `FallbackTx`: the public box `(0, (1, (Voted, 0)))` plus peer boxes
      * `(i+1, (i+2 | 0 for the last, (Awaiting, 0)))`.
      *
      * `committedObligations` seeds `PayoutObligations` with the `(version, output)` payout tokens
      * of each candidate SEC, and `VotableVersions` with their distinct versions. `Output` is
      * intensional, so an empty list is a valid, empty seed (a head with nothing to evacuate or
      * vote on).
      */
    def apply(
        // TODO: a strictly-positive count — retype to a positive Int type.
        nHeadPeers: Int,
        committedObligations: Map[BigInt, List[TransactionOutput]],
        // L2 outputs that withdrew to L1 before fallback — seeds the inert `WithdrawalOutput` place.
        // Defaults to empty: a run with no withdrawals (and every non-MBT caller) leaves it 0.
        withdrawnOutputs: List[TransactionOutput] = List.empty,
    ): ValidatedNel[NetBuilder.Error, HlNet[RBRPlaceId, RBRTransitionId, Any]] = {
        import RBRPlaceId.*

        // Peer/Key/Version are intensional domains: `nHeadPeers` and the committed versions size only
        // the initial marking, never a carrier. Peer is unordered (bound from tokens, never `<`);
        // Key and Version are `linear` for the tally `<` guards (Key: the on-chain
        // `removed.key > continuing.key` fold direction; Version: the ratchet/tally maxVote). Status
        // is a fixed enum.
        val peerClass = Sort.Data[HeadPeerNumber]("Peer")
        val keyClass = Sort.Data[BigInt]("Key", linear = true)
        val versionClass = Sort.Data[BigInt]("Version", linear = true)
        val statusClass = Sort.Class(
          "Status",
          NonEmptySet.of(BallotStatus.Abstained, BallotStatus.Awaiting, BallotStatus.Voted),
          Sort.Discipline.Linear
        )
        val peers: List[HeadPeerNumber] = (0 until nHeadPeers).map(HeadPeerNumber(_)).toList
        val votableVersions: Set[BigInt] = committedObligations.keySet
        // The G2 setup-ladder rungs, one reference utxo per rung (`SetupLadder.rungCount`); rung i
        // covers 2^i evacuations. Colored by rung index so the model holds one token per on-chain
        // rung; Evacuation references one. Fixed on-chain constant, so an enumerated class (like
        // Status), not a reified `Data` domain.
        val setupRungClass = Sort.Class(
          "SetupRung",
          NonEmptySet.of(BigInt(0), (1 until SetupLadderScript.rungCount).map(BigInt(_))*),
          Sort.Discipline.Unordered
        )
        // A payout obligation is a real `TransactionOutput` (`EvacuationTx` drains
        // `evacuatedOutputs: List[TransactionOutput]`). Outputs are large opaque data, bound from
        // present tokens and never enumerated, so `Output` is an intensional `Sort.Data` domain (all
        // outputs) rather than a carrier-listed color class — the committed set enters as the initial
        // marking, not the domain. Ordered by serialized bytes (spire `Order` extends cats `Order`,
        // satisfying the `SortedMap` marking key).
        given Order[TransactionOutput] = Order.from { (left, right) =>
            Cbor.encode(left).mkString(",").compareTo(Cbor.encode(right).mkString(","))
        }
        val outputClass = Sort.Data[TransactionOutput]("Output")
        // Ballot is the full Key × Key × Status × Version product. The structural constraints (no
        // self-link except the terminal (0,0), version meaningful only for Voted — see
        // [[validBallots]]) are not a domain narrowing but a reachability invariant the transition
        // inscriptions preserve from the valid M₀.
        val ballotSort: Sort[Ballot] =
            Sort.Prod(keyClass, Sort.Prod(keyClass, Sort.Prod(statusClass, versionClass)))
        given Order[Ballot] = ballotSort.order
        given Order[(HeadPeerNumber, Key)] = Sort.Prod(peerClass, keyClass).order
        // Owner is the full Peer × Key product; the diagonal seed lives in the marking (read-only place).
        val ownerSort: Sort[(HeadPeerNumber, Key)] = Sort.Prod(peerClass, keyClass)
        // A committed obligation is (version, output): PayoutObligations holds one per candidate SEC.
        val payoutSort: Sort[(BigInt, TransactionOutput)] = Sort.Prod(versionClass, outputClass)
        given Order[(BigInt, TransactionOutput)] = payoutSort.order

        // ---- Variables ----
        val peer = Var("peer", peerClass) // the box owner (Vote / Abstain signer)
        // the acting peer supplying collateral (ratchet / tally)
        val collateralPeer = Var("collateralPeer", peerClass)
        val key = Var("key", keyClass)
        val link = Var("link", keyClass)
        val status = Var("status", statusClass)
        val version = Var("version", versionClass)
        // the evacuated batch: all obligations of the resolved version, up to maxEvacuationsPerTx
        val batch = CollectVar("batch", payoutSort, EvacuationTx.Assumptions.maxEvacuationsPerTx)
        val rung = Var("rung", setupRungClass) // the setup-ladder rung Evacuation references
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
        import ColorTerm.{Const, Ref, Tuple, Wildcard}
        def one[C](term: ColorTerm[C]): Inscription[C] =
            Inscription.Weighted(Positive.unsafe(1), term)
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
        val key0 = Const(BigInt(0), keyClass) // the fully-tallied box's key and link (Resolution)

        val peerToken = one(Ref(peer)) // ⟨peer⟩
        val collateralPeerToken = one(Ref(collateralPeer)) // ⟨collateralPeer⟩
        val ownerToken = one(Tuple(Ref(peer), Ref(key))) // ⟨(peer, key)⟩
        val dotToken = one(Const((), Sort.Dot)) // ⟨•⟩

        // ---- Initial markings ----
        def bagOf[C](entries: (C, Int)*)(using Order[C]): MultiSet[C] =
            Multiset(entries.map((c, n) => c -> SafeLong(n)).to(SortedMap))
        val allPeers: MultiSet[HeadPeerNumber] = bagOf(peers.map(_ -> 1)*)
        val oneDot: MultiSet[Unit] = bagOf(() -> 1)
        val noDots: MultiSet[Unit] = bagOf[Unit]()
        // One token per setup-ladder rung (indices 0..rungCount-1).
        val setupRungs: MultiSet[BigInt] =
            bagOf((0 until SetupLadderScript.rungCount).map(i => BigInt(i) -> 1)*)

        // FallbackTx seeds: the public box (key 0, already Voted at version 0) and one AwaitingVote
        // box per peer; the last box's link is the 0 sentinel.
        val initialBallots: MultiSet[Ballot] = bagOf(
          (Ballot(BigInt(0), BigInt(1), BallotStatus.Voted, BigInt(0)), 1)
              +: (0 until nHeadPeers).map { i =>
                  val link: Link = if i < nHeadPeers - 1 then BigInt(i + 2) else BigInt(0)
                  (Ballot(BigInt(i + 1), link, BallotStatus.Awaiting, BigInt(0)), 1)
              }*
        )
        val ownership: MultiSet[(HeadPeerNumber, BigInt)] =
            bagOf(validOwners(nHeadPeers).toSeq.map(_ -> 1)*)
        // The votable candidate SEC versions — read (not consumed) by Vote/RatchetVote to bind the
        // new version, so those transitions bind from tokens instead of enumerating the Version domain.
        val votable: MultiSet[BigInt] = bagOf(votableVersions.toSeq.map(_ -> 1)*)
        // Every candidate SEC's committed obligation is present but inert (the kzg-hiding abstraction);
        // only the resolved version's is ever drained. ResolvedVersion / EvacuationOutput start empty.
        val initialObligations: MultiSet[(BigInt, TransactionOutput)] =
            bagOf(committedObligations.toSeq.flatMap { case (v, outs) =>
                outs.map(o => (v, o) -> 1)
            }*)
        val noVersions: MultiSet[BigInt] = bagOf[BigInt]()
        val noOutputs: MultiSet[(BigInt, TransactionOutput)] = bagOf[(BigInt, TransactionOutput)]()
        // The pre-fallback withdrawals, one inert token per L2 output that reached L1 (multiplicity
        // preserved so identical outputs still count separately).
        val withdrawnBag: MultiSet[TransactionOutput] = bagOf(withdrawnOutputs.map(_ -> 1)*)

        val b = NetBuilder[RBRPlaceId, RBRTransitionId]()

        // ---- Transition DSL ----
        // A `ReaderT` over `RBRPlaces`: transition builders reference places by selector (`_.owner`)
        // instead of threading `places` through every arc, and the recurring reads become named
        // helpers (`whileVotingOpen`, `unresolvedReferences`, …).
        type TxB[A] = ReaderT[[X] =>> Build[RBRPlaceId, RBRTransitionId, X], RBRPlaces, A]
        type Tref = TransitionRef[RBRTransitionId]
        def transition(id: RBRTransitionId, vars: List[Var[?]], guard: Guard): TxB[Tref] =
            ReaderT.liftF(b.transition(id, vars, guard))
        def input[C](
            p: RBRPlaces => PlaceRef[RBRPlaceId, C],
            t: Tref,
            ins: Inscription[C]
        ): TxB[Unit] =
            ReaderT(places => b.input(p(places), t, ins))
        def read[C](
            p: RBRPlaces => PlaceRef[RBRPlaceId, C],
            t: Tref,
            ins: Inscription[C]
        ): TxB[Unit] =
            ReaderT(places => b.read(p(places), t, ins))
        def output[C](
            t: Tref,
            p: RBRPlaces => PlaceRef[RBRPlaceId, C],
            ins: Inscription[C]
        ): TxB[Unit] =
            ReaderT(places => b.output(t, p(places), ins))
        // A dot (reference / phase) read.
        def readDot(p: RBRPlaces => PlaceRef[RBRPlaceId, Unit], t: Tref): TxB[Unit] =
            read(p, t, dotToken)
        // The acting peer's collateral (present, on L1 spent-and-recreated).
        def collateral(t: Tref, token: Inscription[HeadPeerNumber]): TxB[Unit] =
            read(_.collateral, t, token)
        // treasuryUtxo.referenceOutput / regimeUtxo.referenceOutput / config.referenceDispute.
        def unresolvedReferences(t: Tref): TxB[Unit] =
            for {
                _ <- readDot(_.unresolvedTreasury, t)
                _ <- readDot(_.regimeRef, t)
                _ <- readDot(_.disputeScriptRef, t)
            } yield ()
        // ValidityEndSlot(votingDeadline): only while voting is open.
        def whileVotingOpen(t: Tref): TxB[Unit] = readDot(_.votingOpen, t)
        // ValidityStartSlot(votingDeadline + 1): only after the deadline.
        def afterDeadline(t: Tref): TxB[Unit] = readDot(_.votingClosed, t)

        def addPlaces: Build[RBRPlaceId, RBRTransitionId, RBRPlaces] =
            for {
                ballots <- b.place(Ballots, RBRPlace(initialBallots, ballotSort))
                owner <- b.place(Owner, RBRPlace(ownership, ownerSort))
                votingOpen <- b.place(VotingOpen, RBRPlace(oneDot, Sort.Dot))
                votingClosed <- b.place(VotingClosed, RBRPlace(noDots, Sort.Dot))
                unresolvedTreasury <- b.place(UnresolvedTreasury, RBRPlace(oneDot, Sort.Dot))
                resolvedTreasury <- b.place(ResolvedTreasury, RBRPlace(noDots, Sort.Dot))
                regimeRef <- b.place(RegimeRef, RBRPlace(oneDot, Sort.Dot))
                disputeScriptRef <- b.place(DisputeScriptRef, RBRPlace(oneDot, Sort.Dot))
                treasuryScriptRef <- b.place(TreasuryScriptRef, RBRPlace(oneDot, Sort.Dot))
                setupLadder <- b.place(SetupLadder, RBRPlace(setupRungs, setupRungClass))
                collateral <- b.place(Collateral, RBRPlace(allPeers, peerClass))
                votableVersionsPlace <- b.place(VotableVersions, RBRPlace(votable, versionClass))
                payoutObligations <- b.place(
                  PayoutObligations,
                  RBRPlace(initialObligations, payoutSort)
                )
                resolvedVersion <- b.place(ResolvedVersion, RBRPlace(noVersions, versionClass))
                evacuationOutput <- b.place(EvacuationOutput, RBRPlace(noOutputs, payoutSort))
                withdrawalOutput <- b.place(WithdrawalOutput, RBRPlace(withdrawnBag, outputClass))
            } yield RBRPlaces(
              ballots,
              owner,
              votingOpen,
              votingClosed,
              unresolvedTreasury,
              resolvedTreasury,
              regimeRef,
              disputeScriptRef,
              treasuryScriptRef,
              setupLadder,
              collateral,
              votableVersionsPlace,
              payoutObligations,
              resolvedVersion,
              evacuationOutput,
              withdrawalOutput
            )

        // ---- Vote (mirrors VoteTx.Build.buildVoteTx) ----
        def vote: TxB[Unit] =
            for {
                t <- transition(RBRTransitionId.Vote, List(peer, key, link, version), Guard.True)
                // the voted version must be a votable candidate SEC (binds `version` from tokens)
                _ <- read(_.votableVersions, t, one(Ref(version)))
                // uncastBallotBox.votingSpend / votedOutput.send: AwaitingVote → Voted(version)
                _ <- input(_.ballots, t, one(ballot(Ref(key), Ref(link), awaiting, version0)))
                _ <- output(t, _.ballots, one(ballot(Ref(key), Ref(link), voted, Ref(version))))
                // addRequiredSigners(votingSigners): the box's peer signs — presence in Owner
                _ <- read(_.owner, t, ownerToken)
                // collateralUtxo.spend / collateralOutput.send: the peer's collateral, recreated
                _ <- collateral(t, peerToken)
                _ <- unresolvedReferences(t)
                _ <- whileVotingOpen(t)
            } yield ()

        // ---- Abstain (mirrors AbstainTx.Build.buildAbstainTx) ----
        // Leaner than Vote: no treasury/regime references, no validity window.
        def abstain: TxB[Unit] =
            for {
                t <- transition(RBRTransitionId.Abstain, List(peer, key, link), Guard.True)
                // uncastBallotBox.votingSpend(Abstain) / abstainOutput.send: AwaitingVote → Abstain
                _ <- input(_.ballots, t, one(ballot(Ref(key), Ref(link), awaiting, version0)))
                _ <- output(
                  t,
                  _.ballots,
                  one(ballot(Ref(key), Ref(link), abstainedStatus, version0))
                )
                // addRequiredSigners(votingSigners): the box's peer signs
                _ <- read(_.owner, t, ownerToken)
                // collateralUtxo.spend / collateralOutput.send
                _ <- collateral(t, peerToken)
                // config.referenceDispute
                _ <- readDot(_.disputeScriptRef, t)
            } yield ()

        // ---- RatchetVote (mirrors RatchetVoteTx.Build; spent box is Voted or Abstain) ----
        // The on-chain script skips the tx-signer check: any peer ratchets with a multisigned SEC,
        // supplying its own collateral. Monotonicity (the on-chain `VoteRatchetNotMonotonic` check)
        // is the Lt guard on the linear Version class — Abstained boxes carry version 0, so "Abstain
        // ratchets as prev = 0" is the same guard.
        def ratchetVote: TxB[Unit] =
            for {
                t <- transition(
                  RBRTransitionId.RatchetVote,
                  List(collateralPeer, key, link, status, versionOld, versionNew),
                  Guard.And(
                    Guard.Not(Guard.Eq(Ref(status), awaiting)),
                    Guard.Lt(Ref(versionOld), Ref(versionNew))
                  )
                )
                // the ratcheted-to version must be a votable candidate SEC (binds `versionNew`)
                _ <- read(_.votableVersions, t, one(Ref(versionNew)))
                // openBallotBox.spend / votedOutput.send: (status, versionOld) → Voted(versionNew)
                _ <- input(
                  _.ballots,
                  t,
                  one(ballot(Ref(key), Ref(link), Ref(status), Ref(versionOld)))
                )
                _ <- output(t, _.ballots, one(ballot(Ref(key), Ref(link), voted, Ref(versionNew))))
                // collateralUtxo.spend / collateralOutput.send: the acting peer's collateral
                _ <- collateral(t, collateralPeerToken)
                _ <- unresolvedReferences(t)
                _ <- whileVotingOpen(t)
            } yield ()

        // ---- Tally (mirrors TallyTx.Build; split by the maxVote winner) ----
        // The continuing box (1) absorbs its successor (2): adjacency is the Eq(link1, key2) guard,
        // the fold direction is the Lt(key1, key2) guard (the on-chain
        // `removed.key > continuing.key` check — so key 0, the public box, is never removed and the
        // linked list always contracts down to the (0, 0) terminal box), and the result keeps key1
        // and inherits link2. maxVote's ordering is the Status/Version linear orders; ties go to the
        // removed box (`else b` in maxVote).
        def tallyContinuingWins: TxB[Unit] =
            for {
                t <- transition(
                  RBRTransitionId.TallyContinuingWins,
                  List(
                    collateralPeer,
                    key1,
                    link1,
                    status1,
                    version1,
                    key2,
                    link2,
                    status2,
                    version2
                  ),
                  Guard.And(
                    Guard.And(
                      Guard.Lt(Ref(key1), Ref(key2)),
                      Guard.Eq(Ref(link1), Ref(key2))
                    ),
                    Guard.Or(
                      Guard.Lt(Ref(status2), Ref(status1)),
                      Guard.And(
                        Guard.Eq(Ref(status1), Ref(status2)),
                        Guard.Lt(Ref(version2), Ref(version1))
                      )
                    )
                  )
                )
                // continuingBallotBox.spend + removedBallotBox.spend (one Union inscription — W is a
                // function on F, so both boxes ride a single input arc) / tallied.send
                _ <- input(
                  _.ballots,
                  t,
                  Inscription.Union(
                    one(ballot(Ref(key1), Ref(link1), Ref(status1), Ref(version1))),
                    one(ballot(Ref(key2), Ref(link2), Ref(status2), Ref(version2)))
                  )
                )
                _ <- output(
                  t,
                  _.ballots,
                  one(ballot(Ref(key1), Ref(link2), Ref(status1), Ref(version1)))
                )
                // collateralUtxo.add only — presence, never spent
                _ <- collateral(t, collateralPeerToken)
                _ <- unresolvedReferences(t)
                _ <- afterDeadline(t)
            } yield ()

        def tallyRemovedWins: TxB[Unit] =
            for {
                t <- transition(
                  RBRTransitionId.TallyRemovedWins,
                  List(
                    collateralPeer,
                    key1,
                    link1,
                    status1,
                    version1,
                    key2,
                    link2,
                    status2,
                    version2
                  ),
                  Guard.And(
                    Guard.And(
                      Guard.Lt(Ref(key1), Ref(key2)),
                      Guard.Eq(Ref(link1), Ref(key2))
                    ),
                    Guard.Or(
                      Guard.Lt(Ref(status1), Ref(status2)),
                      Guard.And(
                        Guard.Eq(Ref(status1), Ref(status2)),
                        Guard.Or(
                          Guard.Lt(Ref(version1), Ref(version2)),
                          Guard.Eq(Ref(version1), Ref(version2))
                        )
                      )
                    )
                  )
                )
                _ <- input(
                  _.ballots,
                  t,
                  Inscription.Union(
                    one(ballot(Ref(key1), Ref(link1), Ref(status1), Ref(version1))),
                    one(ballot(Ref(key2), Ref(link2), Ref(status2), Ref(version2)))
                  )
                )
                _ <- output(
                  t,
                  _.ballots,
                  one(ballot(Ref(key1), Ref(link2), Ref(status2), Ref(version2)))
                )
                _ <- collateral(t, collateralPeerToken)
                _ <- unresolvedReferences(t)
                _ <- afterDeadline(t)
            } yield ()

        // ---- VotingDeadline (untimed ISO Clause-10 [D,D] projection) ----
        def votingDeadline: TxB[Unit] =
            for {
                t <- transition(RBRTransitionId.VotingDeadline, List(), Guard.True)
                _ <- input(_.votingOpen, t, dotToken)
                _ <- output(t, _.votingClosed, dotToken)
            } yield ()

        // ---- Resolution (mirrors ResolutionTx.Build.buildResolutionTx) ----
        // The single fully-tallied box — key = link = 0, Voted (link 0 is reachable only from a full
        // fold; on-chain this is "carries all n+1 vote tokens") — is consumed, and the treasury
        // flips Unresolved → Resolved. `version` is the box's minor version, bound from its token.
        // No validity window: the terminal box exists only post-deadline, so timing is implicit.
        def resolution: TxB[Unit] =
            for {
                t <- transition(
                  RBRTransitionId.Resolution,
                  List(version, collateralPeer),
                  Guard.True
                )
                // talliedBallotBox.spend(Resolve): consume (0, 0, Voted, version)
                _ <- input(_.ballots, t, one(ballot(key0, key0, voted, Ref(version))))
                // treasuryUtxo.spendAttached(Resolve) / newTreasury.send: Unresolved → Resolved
                _ <- input(_.unresolvedTreasury, t, dotToken)
                _ <- output(t, _.resolvedTreasury, dotToken)
                // resolved-version selector: reveals which SEC won → gates Evacuation to its obligations
                _ <- output(t, _.resolvedVersion, one(Ref(version)))
                // collateralUtxo.spend / collateralOutput.send: the acting peer's collateral
                _ <- collateral(t, collateralPeerToken)
                // config.referenceTreasury / config.referenceDispute / regimeUtxo.referenceOutput
                _ <- readDot(_.treasuryScriptRef, t)
                _ <- readDot(_.disputeScriptRef, t)
                _ <- readDot(_.regimeRef, t)
            } yield ()

        // ---- Evacuation (mirrors EvacuationTx.Build) ----
        // One firing drains a whole batch: `version` is bound by reading the ResolvedVersion
        // selector, and a collection arc gathers every `(version, *)` obligation up to
        // maxEvacuationsPerTx — so only the resolved SEC's outputs are payable, losing SECs'
        // commitments stay put, and the paid-out set follows the resolution outcome. The treasury is
        // spent-and-recreated (its value shrink, and the residual check, are unmodeled — Dot); the
        // setup-ladder rung is the reference input authenticating the batch. Evacuation touches the
        // treasury validator, not the dispute script (no ballot boxes remain post-resolution).
        val evacuatedBatch = Inscription.Collect(batch, Tuple(Ref(version), Wildcard(outputClass)))
        def evacuation: TxB[Unit] =
            for {
                t <- transition(
                  RBRTransitionId.Evacuation,
                  List(version, collateralPeer, rung),
                  Guard.True
                )
                // ValidityStartSlot / resolved-version read: binds `version` to the resolved SEC
                _ <- read(_.resolvedVersion, t, one(Ref(version)))
                // treasuryUtxo.spendAttached(Evacuate) / newTreasury.send: treasury stays Resolved
                _ <- readDot(_.resolvedTreasury, t)
                // the resolved version's committed batch → evacuatedOutputs.send
                _ <- input(_.payoutObligations, t, evacuatedBatch)
                _ <- output(t, _.evacuationOutput, evacuatedBatch)
                // collateralUtxo.spend / collateralOutput.send
                _ <- collateral(t, collateralPeerToken)
                // config.referenceTreasury / regimeUtxo.referenceOutput / ReferenceOutput(setupRung)
                _ <- readDot(_.treasuryScriptRef, t)
                _ <- readDot(_.regimeRef, t)
                // ReferenceOutput(setupRung): the rung authenticating this batch
                _ <- read(_.setupLadder, t, one(Ref(rung)))
            } yield ()

        // ---- Deinit (mirrors DeinitTx.Build) ----
        // The teardown: spend the (now empty) resolved treasury and the regime utxo, closing the
        // head. "Empty" — no resolved-version obligation left to evacuate — is the inhibitor on
        // PayoutObligations: Deinit is disabled while any `(version, *)` obligation remains, so it
        // can only fire once Evacuation has drained them. Treasury and regime are spent, not
        // recreated (head-token burns are unmodeled — no places).
        def deinit: TxB[Unit] =
            for {
                t <- transition(RBRTransitionId.Deinit, List(version, collateralPeer), Guard.True)
                // resolved-version read: binds `version` for the emptiness inhibitor
                _ <- read(_.resolvedVersion, t, one(Ref(version)))
                // empty treasury: no obligation of the resolved version remains
                _ <- input(
                  _.payoutObligations,
                  t,
                  Inscription.Inhibit(Tuple(Ref(version), Wildcard(outputClass)))
                )
                // treasuryUtxo.spendAttached(Deinit): the resolved treasury is spent, not recreated
                _ <- input(_.resolvedTreasury, t, dotToken)
                // regimeUtxo.spend: the regime is spent (earlier transitions only read it)
                _ <- input(_.regimeRef, t, dotToken)
                // collateralUtxo.spend / collateralOutput.send
                _ <- collateral(t, collateralPeerToken)
                // config.referenceTreasury
                _ <- readDot(_.treasuryScriptRef, t)
            } yield ()

        val transitions: TxB[Unit] = for {
            _ <- vote
            _ <- abstain
            _ <- ratchetVote
            _ <- tallyContinuingWins
            _ <- tallyRemovedWins
            _ <- votingDeadline
            _ <- resolution
            _ <- evacuation
            _ <- deinit
        } yield ()

        // `addPlaces` produces the `RBRPlaces`; the transitions read them once as the ReaderT env.
        val program = for {
            places <- addPlaces
            _ <- transitions.run(places)
        } yield ()

        b.build(program)
    }
}
