package hydrozoa.multisig.consensus.peer

import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.ledger.block.BlockNumber
import scala.annotation.targetName

final case class HeadPeerId(peerNum: HeadPeerNumber, nHeadPeers: PositiveInt) {
    require(peerNum.convert < nHeadPeers, "Peer ID must be less than the number of peers.")

    /** Is the peer the consensus leader of the given block number? */
    def isLeader(blockNum: BlockNumber): Boolean =
        blockNum.convert % nHeadPeers == peerNum.convert

    /** After the given block number, for which block number will the peer next be leader? */
    def nextLeaderBlock(blockNum: BlockNumber): BlockNumber = {
        val roundNumber = blockNum.convert / nHeadPeers
        val leaderBlockThisRound = roundNumber * nHeadPeers + peerNum.convert

        val result =
            if blockNum.convert < leaderBlockThisRound then leaderBlockThisRound
            else leaderBlockThisRound + nHeadPeers
        BlockNumber(result)
    }
}

object HeadPeerId {
    @targetName("applyIntPeerMane")
    def apply(headPeerNumber: Int, nHeadPeers: Int): HeadPeerId =
        new HeadPeerId(HeadPeerNumber(headPeerNumber), PositiveInt.unsafeApply(nHeadPeers))

    given Ordering[HeadPeerId] with {
        override def compare(self: HeadPeerId, other: HeadPeerId): Int = {
            require(self.nHeadPeers == other.nHeadPeers)
            self.peerNum.convert.compare(other.peerNum.convert)
        }
    }

}
