package hydrozoa.multisig.ledger.block

type BlockNumber = BlockNumber.BlockNumber

object BlockNumber {
    opaque type BlockNumber = Int

    def apply(i: Int): BlockNumber = {
        require(i >= 0)
        i
    }

    val zero: BlockNumber = 0

    /** Number of the first (non-initial) block, i.e. 1. */
    val first: BlockNumber = zero.increment

    given Conversion[BlockNumber, Int] = identity

    given Ordering[BlockNumber] with {
        override def compare(x: BlockNumber, y: BlockNumber): Int =
            x.compare(y)
    }

    extension (self: BlockNumber)
        def increment: BlockNumber = BlockNumber(self + 1)
        def decrement: BlockNumber = BlockNumber(self - 1)
}
