package hydrozoa.multisig.consensus.peer

import cats.Order
import hydrozoa.multisig.ledger.block.BlockHeader

type HeadPeerNumber = HeadPeerNumber.HeadPeerNumber

object HeadPeerNumber {
    opaque type HeadPeerNumber = Int

    def apply(i: Int): HeadPeerNumber = {
        require(i >= 0)
        i
    }

    val zero: HeadPeerNumber = 0

    /** The given block will be confirmed when AckBlocks with this AckBlock.Number are received from
      * all peers. It is equal to the block number plus the major version number because:
      *   - Minor blocks each need only one ack and don't increment the major version.
      *   - Major and final blocks each need two acks and do increment the major version.
      */
    def neededToConfirm(header: BlockHeader): HeadPeerNumber =
        header.blockNum + header.blockVersion.major

    given Conversion[HeadPeerNumber, Int] = identity

    given Ordering[HeadPeerNumber] with {
        override def compare(x: HeadPeerNumber, y: HeadPeerNumber): Int =
            x.convert.compare(y.convert)
    }

    given Order[HeadPeerNumber] with {
        override def compare(x: HeadPeerNumber, y: HeadPeerNumber): Int =
            x.convert.compare(y.convert)
    }

    extension (self: HeadPeerNumber)
        def increment: HeadPeerNumber = HeadPeerNumber(self + 1)
        def decrement: HeadPeerNumber = HeadPeerNumber(self - 1)
}
