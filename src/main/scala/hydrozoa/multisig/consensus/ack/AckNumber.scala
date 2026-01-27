package hydrozoa.multisig.consensus.ack

import hydrozoa.multisig.ledger.block.BlockHeader

type AckNumber = AckNumber.AckNumber

object AckNumber {
    opaque type AckNumber = Int

    def apply(i: Int): AckNumber = i

    val zero: AckNumber = 0

    /** The given block will be confirmed when AckBlocks with this AckBlock.Number are received from
      * all peers. It is equal to the block number plus the major version number because:
      *   - Minor blocks each need only one ack and don't increment the major version.
      *   - Major and final blocks each need two acks and do increment the major version.
      */
    def neededToConfirm(header: BlockHeader.Section): AckNumber =
        header.blockNum + header.blockVersion.major

    given Conversion[AckNumber, Int] = identity

    given Ordering[AckNumber] with {
        override def compare(x: AckNumber, y: AckNumber): Int =
            x.compare(y)
    }

    extension (self: AckNumber)
        def increment: AckNumber = AckNumber(self + 1)
        def decrement: AckNumber = AckNumber(self - 1)
}
