package hydrozoa.integration.rbr.model.petri.hlpn

import hydrozoa.integration.rbr.model.petri.hlpn.RBRHlNet.{RBRPlaceId, RBRTransitionId}
import hydrozoa.lib.petri.hlpn.*
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import org.scalatest.funsuite.AnyFunSuite
import spire.math.SafeLong

class RBRHlNetTest extends AnyFunSuite:

    private val (peer0, peer1) = (HeadPeerNumber(0), HeadPeerNumber(1))

    private def net = RBRHlNet(nHeadPeers = 3, maxVersionMinor = 2).toOption.get

    test("the net assembles and is well-sorted") {
        assert(SortCheck.errors(net).isEmpty)
    }

    test("Vote: a peer's pending ballot becomes a (peer, version) ballot; references unchanged") {
        val n = net
        val vote = n.transitionsMap(RBRTransitionId.Vote)
        val (p, v) = (vote.variables(0), vote.variables(1))
        val mode = Binding.bind(Binding.bind(Binding.empty, p, peer0), v, BigInt(1))

        val fired = n.fire(RBRTransitionId.Vote, mode).toOption.get
        val _ = assert(fired.placesMap(RBRPlaceId.AwaitingVote).marking.get(peer0) == SafeLong(0))
        val _ = assert(fired.placesMap(RBRPlaceId.AwaitingVote).marking.get(peer1) == SafeLong(1))
        val _ =
            assert(fired.placesMap(RBRPlaceId.Voted).marking.get((peer0, BigInt(1))) == SafeLong(1))
        // collateral is spent and recreated; references are read back
        val _ = assert(fired.placesMap(RBRPlaceId.Collateral).marking.get(peer0) == SafeLong(1))
        assert(fired.placesMap(RBRPlaceId.UnresolvedTreasury).marking.get(()) == SafeLong(1))
    }

    test("Vote: a peer cannot vote twice") {
        val n = net
        val vote = n.transitionsMap(RBRTransitionId.Vote)
        val (p, v) = (vote.variables(0), vote.variables(1))
        def mode(peer: HeadPeerNumber, version: Int) =
            Binding.bind(Binding.bind(Binding.empty, p, peer), v, BigInt(version))

        val once = n.fire(RBRTransitionId.Vote, mode(peer0, 1)).toOption.get
        val _ = assert(!once.isModeEnabled(RBRTransitionId.Vote, mode(peer0, 2)))
        assert(once.isModeEnabled(RBRTransitionId.Vote, mode(peer1, 2)))
    }

    test("Vote: the enumerating selector finds peers × versions modes") {
        val sim = HlSimulator(net, ModeSelector.enumerating)
        // 3 pending peers × 2 versions
        assert(sim.enabledModes(RBRTransitionId.Vote).size == 6)
    }

    test("RatchetVoted: strictly monotonic; any peer q supplies the collateral") {
        val n = net
        val vote = n.transitionsMap(RBRTransitionId.Vote)
        val voted = n
            .fire(
              RBRTransitionId.Vote,
              Binding.bind(
                Binding.bind(Binding.empty, vote.variables(0), peer0),
                vote.variables(1),
                BigInt(1)
              )
            )
            .toOption
            .get

        val ratchet = n.transitionsMap(RBRTransitionId.RatchetVoted)
        val (p, vOld, vNew, q) =
            (ratchet.variables(0), ratchet.variables(1), ratchet.variables(2), ratchet.variables(3))
        def mode(oldV: Int, newV: Int, actor: HeadPeerNumber) =
            Binding.bind(
              Binding.bind(
                Binding.bind(Binding.bind(Binding.empty, p, peer0), vOld, BigInt(oldV)),
                vNew,
                BigInt(newV)
              ),
              q,
              actor
            )

        // 1 → 2, ratcheted by a different peer than the box owner
        val fired = voted.fire(RBRTransitionId.RatchetVoted, mode(1, 2, peer1)).toOption.get
        val _ = assert(fired.placesMap(RBRPlaceId.Voted).marking.get((peer0, BigInt(2))) == SafeLong(1))
        val _ = assert(fired.placesMap(RBRPlaceId.Voted).marking.get((peer0, BigInt(1))) == SafeLong(0))
        val _ = assert(fired.placesMap(RBRPlaceId.Collateral).marking.get(peer1) == SafeLong(1))
        // non-monotonic ratchets are rejected by the Lt guard
        val _ = assert(!voted.isModeEnabled(RBRTransitionId.RatchetVoted, mode(1, 1, peer1)))
        assert(!fired.isModeEnabled(RBRTransitionId.RatchetVoted, mode(2, 1, peer1)))
    }

    test("RatchetAbstained: an abstained box ratchets to Voted at any version") {
        val n = net
        val abstain = n.transitionsMap(RBRTransitionId.Abstain)
        val abstainedNet = n
            .fire(RBRTransitionId.Abstain, Binding.bind(Binding.empty, abstain.variables(0), peer0))
            .toOption
            .get

        val ratchet = n.transitionsMap(RBRTransitionId.RatchetAbstained)
        val (p, vNew, q) = (ratchet.variables(0), ratchet.variables(1), ratchet.variables(2))
        val mode = Binding.bind(
          Binding.bind(Binding.bind(Binding.empty, p, peer0), vNew, BigInt(1)),
          q,
          peer1
        )

        val fired = abstainedNet.fire(RBRTransitionId.RatchetAbstained, mode).toOption.get
        val _ = assert(fired.placesMap(RBRPlaceId.Abstained).marking.get(peer0) == SafeLong(0))
        assert(fired.placesMap(RBRPlaceId.Voted).marking.get((peer0, BigInt(1))) == SafeLong(1))
    }

    test("Abstain: a peer's pending ballot moves to abstained; voting it afterwards is disabled") {
        val n = net
        val abstain = n.transitionsMap(RBRTransitionId.Abstain)
        val p = abstain.variables(0)
        val mode = Binding.bind(Binding.empty, p, peer0)

        val fired = n.fire(RBRTransitionId.Abstain, mode).toOption.get
        val _ = assert(fired.placesMap(RBRPlaceId.AwaitingVote).marking.get(peer0) == SafeLong(0))
        val _ = assert(fired.placesMap(RBRPlaceId.Abstained).marking.get(peer0) == SafeLong(1))
        val _ = assert(fired.placesMap(RBRPlaceId.Collateral).marking.get(peer0) == SafeLong(1))

        // The abstained peer can no longer vote (its pending ballot is gone)
        val vote = n.transitionsMap(RBRTransitionId.Vote)
        val voteMode = Binding.bind(
          Binding.bind(Binding.empty, vote.variables(0), peer0),
          vote.variables(1),
          BigInt(1)
        )
        assert(!fired.isModeEnabled(RBRTransitionId.Vote, voteMode))
    }
