package hydrozoa.multisig.ledger.block

type BlockNumber = BlockNumber.Number

object BlockNumber {
    opaque type Number = Int

    def apply(i: Int): Number = i

    val zero: Number = apply(0)

    /** Number of the first (non-initial) block, i.e. 1. */
    val first: Number = zero.increment

    given Conversion[Number, Int] = identity

    given Ordering[Number] with {
        override def compare(x: Number, y: Number): Int =
            x.compare(y)
    }

    extension (self: Number)
        def increment: Number = BlockNumber(self + 1)
        def decrement: Number = BlockNumber(self - 1)
}
