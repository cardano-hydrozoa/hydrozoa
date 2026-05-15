package hydrozoa.multisig.ledger.stack

import io.circe.*
import scala.util.Try

type StackNumber = StackNumber.StackNumber

object StackNumber {
    given Encoder[StackNumber] = Encoder.encodeInt.contramap(identity)
    given Decoder[StackNumber] =
        Decoder.decodeInt.emap(i => Try(StackNumber(i)).toEither.left.map(e => e.getMessage))

    opaque type StackNumber = Int

    def apply(i: Int): StackNumber = {
        require(i >= 0)
        i
    }

    val zero: StackNumber = 0

    /** Number of the first non-initial stack, i.e. 1. */
    val first: StackNumber = zero.increment

    given Conversion[StackNumber, Int] = identity

    given Ordering[StackNumber] with {
        override def compare(x: StackNumber, y: StackNumber): Int =
            x.compare(y)
    }

    extension (self: StackNumber)
        def increment: StackNumber = StackNumber(self + 1)
        def decrement: StackNumber = {
            if self == zero
            then throw RuntimeException("Attempt of stack number decrement on 0")
            StackNumber(self - 1)
        }
}
