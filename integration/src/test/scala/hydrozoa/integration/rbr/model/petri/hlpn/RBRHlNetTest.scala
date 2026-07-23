package hydrozoa.integration.rbr.model.petri.hlpn

import hydrozoa.integration.rbr.model.petri.hlpn.RBRHlNet.{Ballot, BallotStatus, RBRPlaceId, RBRTransitionId, committedOutputs}
import hydrozoa.integration.rbr.model.petri.hlpn.RBRHlNet.BallotStatus.{Abstained, Awaiting, Voted}
import hydrozoa.lib.collection.Multiset
import hydrozoa.lib.petri.hlpn.*
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import org.scalatest.funsuite.AnyFunSuite
import spire.algebra.Order
import spire.math.SafeLong

/** Drives the RBR HLPN with explicit modes (the tally transitions bind 9 variables — the
  * enumerating selector's candidate space is ~10⁵, so tests construct bindings directly).
  */
class RBRHlNetTest extends AnyFunSuite:

    private val (peer0, peer1, peer2) =
        (HeadPeerNumber(0), HeadPeerNumber(1), HeadPeerNumber(2))

    // n = 3 peers → boxes (0,(1,(Voted,0))) public, (1,(2,·)), (2,(3,·)), (3,(0,·)) for peers 0..2
    private def net = RBRHlNet(nHeadPeers = 3, maxVersionMinor = 2).toOption.get

    private type Net = HlNet[RBRPlaceId, RBRTransitionId, Any]

    private def box(k: Int, l: Int, s: BallotStatus, v: Int): Ballot =
        Ballot(BigInt(k), BigInt(l), s, BigInt(v))

    private def ballotCount(n: Net, b: Ballot): SafeLong =
        n.placesMap(RBRPlaceId.Ballots).marking.get(b)

    private def bindAll(n: Net, tid: RBRTransitionId)(values: Any*): Binding =
        n.transitionsMap(tid)
            .variables
            .zip(values)
            .foldLeft(Binding.empty) { case (acc, (variable, value)) =>
                Binding.bind(acc, variable, value)
            }

    // Transition variable orders: Vote(p,k,l,v) · Abstain(p,k,l) · RatchetVote(q,k,l,s,vOld,vNew)
    // · Tally*(q,k1,l1,s1,v1,k2,l2,s2,v2) · VotingDeadline()
    private def voteMode(n: Net, p: HeadPeerNumber, k: Int, l: Int, v: Int): Binding =
        bindAll(n, RBRTransitionId.Vote)(p, BigInt(k), BigInt(l), BigInt(v))
    private def abstainMode(n: Net, p: HeadPeerNumber, k: Int, l: Int): Binding =
        bindAll(n, RBRTransitionId.Abstain)(p, BigInt(k), BigInt(l))
    private def ratchetMode(
        n: Net,
        q: HeadPeerNumber,
        k: Int,
        l: Int,
        s: BallotStatus,
        vOld: Int,
        vNew: Int
    ): Binding =
        bindAll(n, RBRTransitionId.RatchetVote)(
          q,
          BigInt(k),
          BigInt(l),
          s,
          BigInt(vOld),
          BigInt(vNew)
        )
    private def tallyMode(
        n: Net,
        tid: RBRTransitionId,
        q: HeadPeerNumber,
        continuing: Ballot,
        removed: Ballot
    ): Binding =
        bindAll(n, tid)(
          q,
          continuing.key,
          continuing.link,
          continuing.status,
          continuing.versionMinor,
          removed.key,
          removed.link,
          removed.status,
          removed.versionMinor
        )

    private def resolutionMode(n: Net, version: Int, q: HeadPeerNumber): Binding =
        bindAll(n, RBRTransitionId.Resolution)(BigInt(version), q)

    // Deinit binds only scalars (version, collateralPeer); its emptiness inhibitor is a net-level
    // enabling check, so an explicit mode is fine.
    private def deinitMode(n: Net, version: Int, q: HeadPeerNumber): Binding =
        bindAll(n, RBRTransitionId.Deinit)(BigInt(version), q)

    private def fired(n: Net, tid: RBRTransitionId, mode: Binding): Net =
        n.fire(tid, mode).toOption.get

    // Evacuation binds a collection variable (the batch), so it is driven through the unifying
    // selector rather than an explicit mode — the selector gathers the batch from present tokens.
    private def firedVia(n: Net, tid: RBRTransitionId): Net =
        HlSimulator(n, ModeSelector.unifying).fire(tid).toOption.get._1.net

    // The core offers no enabled-set query (it would be selector-relative); compose it from the
    // unifying selector's candidates and the net's exact isModeEnabled.
    private def enabledVia(n: Net, tid: RBRTransitionId): Boolean =
        ModeSelector.unifying.candidates(n, tid).exists(n.isModeEnabled(tid, _))

    /** Drive the dispute to the single fully-tallied box `(0, 0, Voted, 2)`: peer0 votes v=1, peer1
      * votes v=2, peer2 abstains, the deadline passes, and the four boxes fold in.
      */
    private def fullyTallied: Net =
        var n = fired(net, RBRTransitionId.Vote, voteMode(net, peer0, 1, 2, 1))
        n = fired(n, RBRTransitionId.Vote, voteMode(n, peer1, 2, 3, 2))
        n = fired(n, RBRTransitionId.Abstain, abstainMode(n, peer2, 3, 0))
        n = fired(n, RBRTransitionId.VotingDeadline, Binding.empty)
        n = fired(
          n,
          RBRTransitionId.TallyRemovedWins,
          tallyMode(n, RBRTransitionId.TallyRemovedWins, peer0, box(0, 1, Voted, 0), box(1, 2, Voted, 1))
        )
        n = fired(
          n,
          RBRTransitionId.TallyRemovedWins,
          tallyMode(n, RBRTransitionId.TallyRemovedWins, peer0, box(0, 2, Voted, 1), box(2, 3, Voted, 2))
        )
        fired(
          n,
          RBRTransitionId.TallyContinuingWins,
          tallyMode(n, RBRTransitionId.TallyContinuingWins, peer0, box(0, 3, Voted, 2), box(3, 0, Abstained, 0))
        )

    /** `fullyTallied` then resolved at the terminal version (2), where the payouts are revealed. */
    private def resolved: Net =
        fired(fullyTallied, RBRTransitionId.Resolution, resolutionMode(fullyTallied, 2, peer0))

    test("BallotStatus order matches maxVote precedence: Abstained < Awaiting < Voted") {
        val order = summon[Order[BallotStatus]]
        val _ = assert(order.lt(Abstained, Awaiting))
        val _ = assert(order.lt(Awaiting, Voted))
        assert(order.lt(Abstained, Voted))
    }

    test("the net assembles and is well-sorted") {
        assert(SortCheck.errors(net).isEmpty)
    }

    test("Ballot domain: the listing excludes self-links and non-Voted versions") {
        val valid = RBRHlNet.validBallots(nHeadPeers = 3, maxVersionMinor = 2)
        val _ = assert(valid.contains(box(0, 0, Voted, 2))) // fully-tallied terminal box
        val _ = assert(valid.contains(box(1, 2, Awaiting, 0))) // awaiting box carries version 0
        val _ = assert(!valid.contains(box(1, 1, Voted, 1))) // non-terminal self-link
        assert(!valid.contains(box(1, 2, Awaiting, 1))) // non-Voted box with a version
    }

    test("Owner domain: the listing is exactly the ownership diagonal") {
        val valid = RBRHlNet.validOwners(nHeadPeers = 3)
        val _ = assert(valid == Set(peer0 -> BigInt(1), peer1 -> BigInt(2), peer2 -> BigInt(3)))
        assert(!valid.contains(peer1 -> BigInt(1))) // peer1 does not own box key 1
    }

    test("Ballots place rejects a malformed box: a self-link fails markingError") {
        val place = net.placesMap(RBRPlaceId.Ballots)
        given Order[Any] = place.colorDomain.order
        val bad = Multiset(place.marking.multiplicityMap.updated(box(1, 1, Voted, 1), SafeLong(1)))
        assert(place.mark(bad).markingError.isDefined)
    }

    test("Owner place rejects an off-diagonal pair via markingError") {
        val place = net.placesMap(RBRPlaceId.Owner)
        given Order[Any] = place.colorDomain.order
        val bad = Multiset(place.marking.multiplicityMap.updated((peer1, BigInt(1)), SafeLong(1)))
        assert(place.mark(bad).markingError.isDefined)
    }

    test("Vote: the pending box flips to Voted(v); owner, collateral, references restored") {
        val n = fired(net, RBRTransitionId.Vote, voteMode(net, peer0, 1, 2, 1))
        val _ = assert(ballotCount(n, box(1, 2, Awaiting, 0)) == SafeLong(0))
        val _ = assert(ballotCount(n, box(1, 2, Voted, 1)) == SafeLong(1))
        val _ = assert(n.placesMap(RBRPlaceId.Owner).marking.get((peer0, BigInt(1))) == SafeLong(1))
        val _ = assert(n.placesMap(RBRPlaceId.Collateral).marking.get(peer0) == SafeLong(1))
        assert(n.placesMap(RBRPlaceId.UnresolvedTreasury).marking.get(()) == SafeLong(1))
    }

    test("Vote: only the box's owner can vote it") {
        // peer1 does not own box key=1 (peer0 does) — the Owner read fails
        assert(!net.isModeEnabled(RBRTransitionId.Vote, voteMode(net, peer1, 1, 2, 1)))
    }

    test("Vote: a peer cannot vote twice") {
        val n = fired(net, RBRTransitionId.Vote, voteMode(net, peer0, 1, 2, 1))
        assert(!n.isModeEnabled(RBRTransitionId.Vote, voteMode(n, peer0, 1, 2, 2)))
    }

    test("VotingDeadline: closes voting for Vote and RatchetVote, opens tallying") {
        val n0 = fired(net, RBRTransitionId.Vote, voteMode(net, peer0, 1, 2, 1))
        val n = fired(n0, RBRTransitionId.VotingDeadline, Binding.empty)
        val _ = assert(!n.isModeEnabled(RBRTransitionId.Vote, voteMode(n, peer1, 2, 3, 1)))
        val _ = assert(
          !n.isModeEnabled(
            RBRTransitionId.RatchetVote,
            ratchetMode(n, peer2, 1, 2, Voted, 1, 2)
          )
        )
        // tallying the public box with box 1 is now possible (removed box has the higher vote)
        assert(
          n.isModeEnabled(
            RBRTransitionId.TallyRemovedWins,
            tallyMode(n, RBRTransitionId.TallyRemovedWins, peer0, box(0, 1, Voted, 0), box(1, 2, Voted, 1))
          )
        )
    }

    test("Abstain: has no validity window — enabled even after the deadline") {
        val n = fired(net, RBRTransitionId.VotingDeadline, Binding.empty)
        val n2 = fired(n, RBRTransitionId.Abstain, abstainMode(n, peer1, 2, 3))
        val _ = assert(ballotCount(n2, box(2, 3, Abstained, 0)) == SafeLong(1))
        assert(ballotCount(n2, box(2, 3, Awaiting, 0)) == SafeLong(0))
    }

    test("RatchetVote: Voted boxes ratchet strictly monotonically, by any acting peer") {
        val n0 = fired(net, RBRTransitionId.Vote, voteMode(net, peer0, 1, 2, 1))
        // vOld=1 → vNew=2, acted by peer2 (not the owner)
        val n = fired(n0, RBRTransitionId.RatchetVote, ratchetMode(n0, peer2, 1, 2, Voted, 1, 2))
        val _ = assert(ballotCount(n, box(1, 2, Voted, 2)) == SafeLong(1))
        val _ = assert(ballotCount(n, box(1, 2, Voted, 1)) == SafeLong(0))
        // non-monotonic: vNew == vOld and vNew < vOld both rejected
        val _ = assert(
          !n0.isModeEnabled(RBRTransitionId.RatchetVote, ratchetMode(n0, peer2, 1, 2, Voted, 1, 1))
        )
        assert(
          !n.isModeEnabled(RBRTransitionId.RatchetVote, ratchetMode(n, peer2, 1, 2, Voted, 2, 1))
        )
    }

    test("RatchetVote: an Abstained box ratchets from version 0; an Awaiting box cannot ratchet") {
        val n0 = fired(net, RBRTransitionId.Abstain, abstainMode(net, peer0, 1, 2))
        // Abstained box carries version 0 — the prev=0 rule is the same Lt guard
        val n = fired(
          n0,
          RBRTransitionId.RatchetVote,
          ratchetMode(n0, peer1, 1, 2, Abstained, 0, 1)
        )
        val _ = assert(ballotCount(n, box(1, 2, Voted, 1)) == SafeLong(1))
        // Awaiting boxes are not Open-phase — the status guard rejects them
        assert(
          !net.isModeEnabled(
            RBRTransitionId.RatchetVote,
            ratchetMode(net, peer1, 2, 3, Awaiting, 0, 1)
          )
        )
    }

    test("Tally: adjacency is required; tallying is disabled before the deadline") {
        val n = fired(net, RBRTransitionId.VotingDeadline, Binding.empty)
        // non-adjacent: continuing (0, link 1) with removed key 2
        val _ = assert(
          !n.isModeEnabled(
            RBRTransitionId.TallyRemovedWins,
            tallyMode(n, RBRTransitionId.TallyRemovedWins, peer0, box(0, 1, Voted, 0), box(2, 3, Awaiting, 0))
          )
        )
        // before the deadline nothing tallies
        assert(
          !net.isModeEnabled(
            RBRTransitionId.TallyContinuingWins,
            tallyMode(net, RBRTransitionId.TallyContinuingWins, peer0, box(0, 1, Voted, 0), box(1, 2, Awaiting, 0))
          )
        )
    }

    test("Tally: maxVote winner selection — status precedence, version order, tie to removed") {
        // Voted beats Awaiting: continuing (0,(1,(Voted,0))) absorbs awaiting box 1
        val closed = fired(net, RBRTransitionId.VotingDeadline, Binding.empty)
        val a = fired(
          closed,
          RBRTransitionId.TallyContinuingWins,
          tallyMode(closed, RBRTransitionId.TallyContinuingWins, peer0, box(0, 1, Voted, 0), box(1, 2, Awaiting, 0))
        )
        val _ = assert(ballotCount(a, box(0, 2, Voted, 0)) == SafeLong(1))

        // Higher version wins (removed): vote box 1 at v=2 first
        val n0 = fired(net, RBRTransitionId.Vote, voteMode(net, peer0, 1, 2, 2))
        val n1 = fired(n0, RBRTransitionId.VotingDeadline, Binding.empty)
        val b1 = fired(
          n1,
          RBRTransitionId.TallyRemovedWins,
          tallyMode(n1, RBRTransitionId.TallyRemovedWins, peer1, box(0, 1, Voted, 0), box(1, 2, Voted, 2))
        )
        val _ = assert(ballotCount(b1, box(0, 2, Voted, 2)) == SafeLong(1))

        // Voted tie goes to the removed box: continuing-wins is not enabled on a tie
        assert(
          !n1.isModeEnabled(
            RBRTransitionId.TallyContinuingWins,
            tallyMode(n1, RBRTransitionId.TallyContinuingWins, peer1, box(0, 1, Voted, 0), box(1, 2, Voted, 0))
          )
        )
    }

    test("Tally: full fold converges to the single box (0,(0,(Voted,vmax)))") {
        val n = fullyTallied
        val ballots = n.placesMap(RBRPlaceId.Ballots).marking.multiplicityMap
        val _ = assert(ballots.size == 1)
        assert(ballotCount(n, box(0, 0, Voted, 2)) == SafeLong(1))
    }

    test("Resolution: not enabled until the fully-tallied box exists") {
        // the terminal (0, 0, Voted, ·) box does not exist before the fold completes
        assert(!net.isModeEnabled(RBRTransitionId.Resolution, resolutionMode(net, 0, peer0)))
    }

    test("Resolution: consumes the tallied box and flips the treasury to Resolved") {
        val n = fired(fullyTallied, RBRTransitionId.Resolution, resolutionMode(fullyTallied, 2, peer0))
        // the tallied box is spent — ballots empty
        val _ = assert(n.placesMap(RBRPlaceId.Ballots).marking.multiplicityMap.isEmpty)
        // treasury flips Unresolved → Resolved
        val _ = assert(n.placesMap(RBRPlaceId.UnresolvedTreasury).marking.get(()) == SafeLong(0))
        val _ = assert(n.placesMap(RBRPlaceId.ResolvedTreasury).marking.get(()) == SafeLong(1))
        // collateral spent and recreated; script/regime references read back
        val _ = assert(n.placesMap(RBRPlaceId.Collateral).marking.get(peer0) == SafeLong(1))
        assert(n.placesMap(RBRPlaceId.TreasuryScriptRef).marking.get(()) == SafeLong(1))
    }

    test("Resolution: reveals the resolved version via the ResolvedVersion selector") {
        val n = fired(fullyTallied, RBRTransitionId.Resolution, resolutionMode(fullyTallied, 2, peer0))
        assert(n.placesMap(RBRPlaceId.ResolvedVersion).marking.get(BigInt(2)) == SafeLong(1))
    }

    test("Evacuation: not enabled until Resolution reveals the version") {
        // no ResolvedVersion token yet — the selector cannot bind `version`, so no batch is proposed
        assert(!enabledVia(fullyTallied, RBRTransitionId.Evacuation))
    }

    test("Evacuation: drains the resolved version's whole batch, leaving other SECs' committed") {
        // version 2 commits two obligations; one firing drains both
        val version2 = committedOutputs(2)
        val _ = assert(version2.sizeIs == 2)
        val n = firedVia(resolved, RBRTransitionId.Evacuation)
        val obligations = n.placesMap(RBRPlaceId.PayoutObligations).marking
        val evacuated = n.placesMap(RBRPlaceId.EvacuationOutput).marking
        version2.foreach { output =>
            // each version-2 obligation is paid out (tagged with its version) and gone from the pool
            val _ = assert(obligations.get((BigInt(2), output)) == SafeLong(0))
            val _ = assert(evacuated.get((BigInt(2), output)) == SafeLong(1))
        }
        // the losing SEC's commitment (version 1) stays put
        val _ = assert(obligations.get((BigInt(1), committedOutputs(1).head)) == SafeLong(1))
        // treasury stays Resolved; the selector persists; collateral recreated
        val _ = assert(n.placesMap(RBRPlaceId.ResolvedTreasury).marking.get(()) == SafeLong(1))
        val _ = assert(n.placesMap(RBRPlaceId.ResolvedVersion).marking.get(BigInt(2)) == SafeLong(1))
        assert(n.placesMap(RBRPlaceId.Collateral).marking.get(peer0) == SafeLong(1))
    }

    test("Evacuation: drains the whole resolved batch in one firing, then is spent") {
        // the entire version-2 batch leaves in a single firing, so a second is not enabled
        val once = firedVia(resolved, RBRTransitionId.Evacuation)
        assert(!enabledVia(once, RBRTransitionId.Evacuation))
    }

    test("Deinit: not enabled while a resolved-version obligation remains") {
        // resolved but not yet evacuated — the emptiness inhibitor blocks Deinit
        assert(!resolved.isModeEnabled(RBRTransitionId.Deinit, deinitMode(resolved, 2, peer0)))
    }

    test("Deinit: after evacuation, spends the treasury and regime, tearing down the head") {
        val evacuated = firedVia(resolved, RBRTransitionId.Evacuation)
        // the obligations are drained, so Deinit is now enabled
        val _ = assert(evacuated.isModeEnabled(RBRTransitionId.Deinit, deinitMode(evacuated, 2, peer0)))
        val n = fired(evacuated, RBRTransitionId.Deinit, deinitMode(evacuated, 2, peer0))
        // treasury and regime are spent (not recreated); collateral returns
        val _ = assert(n.placesMap(RBRPlaceId.ResolvedTreasury).marking.get(()) == SafeLong(0))
        val _ = assert(n.placesMap(RBRPlaceId.RegimeRef).marking.get(()) == SafeLong(0))
        assert(n.placesMap(RBRPlaceId.Collateral).marking.get(peer0) == SafeLong(1))
    }
