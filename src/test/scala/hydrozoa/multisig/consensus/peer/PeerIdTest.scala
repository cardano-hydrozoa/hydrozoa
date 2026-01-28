package hydrozoa.multisig.consensus.peer

import hydrozoa.multisig.ledger.block.BlockNumber
import org.scalacheck.*
import org.scalacheck.Test.Parameters

object PeerIdTest extends Properties("Peer/RoundRobin") {
    import Prop.forAll

    override def overrideParameters(p: Parameters): Parameters = {
        p.withMinSuccessfulTests(1000)
    }

    private val genSmallInt = Gen.choose(0, 200)

    val _ = property("Peer ID correctly calculates round-robin leadership schedule") =
        forAll(genSmallInt, genSmallInt, genSmallInt) { (x, y, z) =>
            val peerNum = x.abs
            val nPeers = x.abs + y.abs + 1
            val peerId = PeerId(peerNum, nPeers)

            val roundNum = z.abs
            val blockNum = BlockNumber(roundNum * nPeers + peerNum)

            val peerIsLeader = peerId.isLeader(blockNum)

            val otherPeersNotLeaders = Range(0, nPeers - 1)
                .filter(_ != peerNum)
                .forall((i: Int) => !PeerId(i, nPeers).isLeader(blockNum))

            val nextLeaderBlock = peerId.nextLeaderBlock(blockNum)
            val peerIsLeaderNextTime = peerId.isLeader(nextLeaderBlock)

            val expectedNextLeaderBlock = BlockNumber(nPeers + blockNum)
            val nextLeaderBlockIsAsExpected = nextLeaderBlock == expectedNextLeaderBlock

            peerIsLeader && peerIsLeaderNextTime && otherPeersNotLeaders && nextLeaderBlockIsAsExpected
        }
}
