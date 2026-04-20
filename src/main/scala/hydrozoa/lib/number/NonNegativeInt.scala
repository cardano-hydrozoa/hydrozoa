package hydrozoa.lib.number

import io.circe.*

type NonNegativeInt = NonNegativeInt.NonNegativeInt

object NonNegativeInt {
    opaque type NonNegativeInt = scala.Int

    def apply(i: scala.Int): Option[NonNegativeInt] = if i >= 0 then Some(i) else None

    def unsafeApply(i: scala.Int): NonNegativeInt = {
        require(i >= 0)
        i
    }

    given Conversion[NonNegativeInt, scala.Int] = identity

    given nonNegativeIntEncoder: Encoder[NonNegativeInt] = Encoder.encodeInt

    given nonNegativeIntDecoder: Decoder[NonNegativeInt] = Decoder.decodeInt.emap { i =>
        NonNegativeInt.apply(i) match {
            case None      => Left(s"Expected a non-negative integer, got $i")
            case Some(nni) => Right(nni)
        }
    }
}
