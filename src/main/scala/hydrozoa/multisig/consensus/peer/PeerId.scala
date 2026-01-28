package hydrozoa.multisig.consensus.peer

import hydrozoa.multisig.ledger.block.BlockNumber
import scala.annotation.targetName

final case class PeerId(peerNum: PeerNumber, nPeers: Int) {
    require(peerNum.convert < nPeers, "Peer ID must be less than the number of peers.")

    /** Is the peer the consensus leader of the given block number? */
    def isLeader(blockNum: BlockNumber): Boolean =
        blockNum.convert % nPeers == peerNum.convert

    /** After the given block number, for which block number will the peer next be leader? */
    def nextLeaderBlock(blockNum: BlockNumber): BlockNumber = {
        val roundNumber = blockNum.convert / nPeers
        val leaderBlockThisRound = roundNumber * nPeers + peerNum.convert

        val result =
            if blockNum.convert < leaderBlockThisRound then leaderBlockThisRound
            else leaderBlockThisRound + nPeers
        BlockNumber(result)
    }
}

object PeerId {
    @targetName("applyIntPeerMane")
    def apply(peerNumber: Int, nPeers: Int): PeerId = new PeerId(PeerNumber(peerNumber), nPeers)

    given Ordering[PeerId] with {
        override def compare(self: PeerId, other: PeerId): Int = {
            require(self.nPeers == other.nPeers)
            self.peerNum.convert.compare(other.peerNum.convert)
        }
    }

}
