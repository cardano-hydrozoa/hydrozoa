package hydrozoa.multisig.consensus.peer

import hydrozoa.multisig.ledger.block.BlockHeader

type PeerNumber = PeerNumber.PeerNumber

object PeerNumber {
    opaque type PeerNumber = Int

    def apply(i: Int): PeerNumber = i

    /** The given block will be confirmed when AckBlocks with this AckBlock.Number are received from
      * all peers. It is equal to the block number plus the major version number because:
      *   - Minor blocks each need only one ack and don't increment the major version.
      *   - Major and final blocks each need two acks and do increment the major version.
      */
    def neededToConfirm(header: BlockHeader): PeerNumber =
        header.blockNum + header.blockVersion.major

    given Conversion[PeerNumber, Int] = identity

    given Ordering[PeerNumber] with {
        override def compare(x: PeerNumber, y: PeerNumber): Int =
            x.compare(y)
    }

    extension (self: PeerNumber)
        def increment: PeerNumber = PeerNumber(self + 1)
        def decrement: PeerNumber = PeerNumber(self - 1)
}
