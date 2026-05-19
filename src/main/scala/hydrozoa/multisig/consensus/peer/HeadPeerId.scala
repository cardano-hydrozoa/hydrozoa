package hydrozoa.multisig.consensus.peer

import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import io.circe.*
import io.circe.generic.semiauto.*
import scala.annotation.targetName

final case class HeadPeerId(peerNum: HeadPeerNumber, nHeadPeers: PositiveInt) {
    require(peerNum.convert < nHeadPeers, "Peer ID must be less than the number of peers.")

    /** Is the peer the fast-cycle consensus leader of the given block number? */
    def isLeader(blockNum: BlockNumber): Boolean =
        blockNum.convert % nHeadPeers == peerNum.convert

    /** Is the peer the slow-cycle leader of the given stack number? Round-robin schedule
      * independent of the fast cycle, keyed on `stackNum` instead of `blockNum`.
      */
    def isSlowLeader(stackNum: StackNumber): Boolean =
        stackNum.convert % nHeadPeers == peerNum.convert

    /** After the given block number, for which block number will the peer next be leader? */
    def nextLeaderBlock(blockNum: BlockNumber): BlockNumber = {
        val roundNumber = blockNum.convert / nHeadPeers
        val leaderBlockThisRound = roundNumber * nHeadPeers + peerNum.convert

        val result =
            if blockNum.convert < leaderBlockThisRound then leaderBlockThisRound
            else leaderBlockThisRound + nHeadPeers
        BlockNumber(result)
    }

    /** After the given stack number, for which stack number will the peer next be slow-leader?
      * Mirrors [[nextLeaderBlock]] for the slow cycle: strictly-after semantics, so
      * `nextSlowLeaderStack(StackNumber.zero)` deliberately steps past stack 0 (the bootstrap
      * stack, never broadcast as a brief).
      */
    def nextSlowLeaderStack(stackNum: StackNumber): StackNumber = {
        val roundNumber = stackNum.convert / nHeadPeers
        val leaderStackThisRound = roundNumber * nHeadPeers + peerNum.convert

        val result =
            if stackNum.convert < leaderStackThisRound then leaderStackThisRound
            else leaderStackThisRound + nHeadPeers
        StackNumber(result)
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

    given headPeerIdEncoder: Encoder[HeadPeerId] = deriveEncoder[HeadPeerId]
    given headPeerIdDecoder: Decoder[HeadPeerId] = deriveDecoder[HeadPeerId]

}
