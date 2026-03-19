package hydrozoa.multisig.ledger.event

type RequestNumber = RequestNumber.RequestNumber

object RequestNumber {
    opaque type RequestNumber = Long

    def apply(n: Long): RequestNumber = {
        require(
          n >= 0 && n < (1L << 40),
          s"The request number should be in [0; ${1L << 40}), got: $n"
        )
        n
    }

    given Conversion[RequestNumber, Long] = identity

    given Ordering[RequestNumber] with {
        override def compare(x: RequestNumber, y: RequestNumber): Int =
            x.convert.compare(y.convert)
    }

    extension (self: RequestNumber) def increment: RequestNumber = RequestNumber(self + 1)
}
