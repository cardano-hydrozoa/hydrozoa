package hydrozoa.rulebased.ledger.l1.script.plutus

import hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionValidator.maxVote
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus
import org.scalatest.funsuite.AnyFunSuite
import scala.annotation.nowarn
import scalus.cardano.onchain.plutus.v3.PubKeyHash
import scalus.uplc.builtin.ByteString

@nowarn("msg=unused value")
class DisputeResolutionScriptTest extends AnyFunSuite {

    // ---- maxVote: precedence is Voted > AwaitingVote > Abstain; ties prefer b. ----

    private val peerA =
        VoteStatus.AwaitingVote(PubKeyHash(ByteString.fromArray(Array.fill[Byte](28)(0xa.toByte))))
    private val peerB =
        VoteStatus.AwaitingVote(PubKeyHash(ByteString.fromArray(Array.fill[Byte](28)(0xb.toByte))))
    private val voted1 = VoteStatus.Voted(ByteString.empty, versionMinor = BigInt(1))
    private val voted2 = VoteStatus.Voted(ByteString.empty, versionMinor = BigInt(2))

    test("maxVote: higher versionMinor wins between two Voted") {
        assert(maxVote(voted2, voted1) == voted2)
        assert(maxVote(voted1, voted2) == voted2)
    }

    test("maxVote: equal-versionMinor Voted ties to b") {
        assert(maxVote(voted1, voted1) == voted1)
    }

    test("maxVote: Voted beats AwaitingVote") {
        assert(maxVote(voted1, peerA) == voted1)
        assert(maxVote(peerA, voted1) == voted1)
    }

    test("maxVote: Voted beats Abstain") {
        assert(maxVote(voted1, VoteStatus.Abstain) == voted1)
        assert(maxVote(VoteStatus.Abstain, voted1) == voted1)
    }

    test("maxVote: AwaitingVote beats Abstain") {
        assert(maxVote(peerA, VoteStatus.Abstain) == peerA)
        assert(maxVote(VoteStatus.Abstain, peerA) == peerA)
    }

    test("maxVote: AwaitingVote ties to b") {
        assert(maxVote(peerA, peerB) == peerB)
    }

    test("maxVote: Abstain ties to b") {
        assert(maxVote(VoteStatus.Abstain, VoteStatus.Abstain) == VoteStatus.Abstain)
    }
}
