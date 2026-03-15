package hydrozoa.multisig.ledger.event

type RequestNumber = RequestNumber.RequestNumber

object RequestNumber {
    opaque type RequestNumber = Int

    def apply(i: Int): RequestNumber = {
        require(i >= 0 && i < (1 << 40))
        i
    }

    given Conversion[RequestNumber, Int] = identity

    given Ordering[RequestNumber] with {
        override def compare(x: RequestNumber, y: RequestNumber): Int =
            x.convert.compare(y.convert)
    }

    extension (self: RequestNumber) def increment: RequestNumber = RequestNumber(self + 1)
}
