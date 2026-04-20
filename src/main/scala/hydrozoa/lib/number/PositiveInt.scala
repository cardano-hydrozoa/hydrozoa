package hydrozoa.lib.number

import io.circe.{Decoder, Encoder}

type PositiveInt = PositiveInt.PositiveInt

object PositiveInt {
    opaque type PositiveInt = scala.Int

    def apply(i: scala.Int): Option[PositiveInt] = if i > 0 then Some(i) else None

    def unsafeApply(i: scala.Int): PositiveInt = {
        require(i > 0)
        i
    }

    given Conversion[PositiveInt, scala.Int] = identity

    given positiveIntEncoder: Encoder[PositiveInt] = Encoder.encodeInt.contramap(_.toInt)

    given positiveIntDecoder: Decoder[PositiveInt] = Decoder.decodeInt.emap { i =>
        PositiveInt.apply(i) match {
            case None     => Left(s"Expected a positive integer, got $i")
            case Some(pi) => Right(pi)
        }
    }
}
