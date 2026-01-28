package hydrozoa.multisig.ledger.event

type LedgerEventNumber = LedgerEventNumber.LedgerEventNumber

object LedgerEventNumber {
    opaque type LedgerEventNumber = Int

    def apply(i: Int): LedgerEventNumber = i

    given Conversion[LedgerEventNumber, Int] = identity

    given Ordering[LedgerEventNumber] with {
        override def compare(x: LedgerEventNumber, y: LedgerEventNumber): Int =
            x.convert.compare(y.convert)
    }

    extension (self: LedgerEventNumber)
        def increment: LedgerEventNumber = LedgerEventNumber(self + 1)
}
