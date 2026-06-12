package hydrozoa.multisig.consensus.peer

import cats.Order
import hydrozoa.lib.number.NonNegativeInt
import io.circe.*

type CoilPeerNumber = CoilPeerNumber.CoilPeerNumber

object CoilPeerNumber {

    given coilPeerNumberEncoder: Encoder[CoilPeerNumber] = Encoder.encodeInt

    given coilPeerNumberDecoder: Decoder[CoilPeerNumber] = Decoder.decodeInt.emap(i =>
        Either.cond(
          i >= 0 && i < (1 << 8),
          right = i,
          left = s"Expected a number `i` such  that `i >= 0 && i < (1 << 8)`, but got $i"
        )
    )

    given coilPeerNumberKeyEncoder: KeyEncoder[CoilPeerNumber] =
        KeyEncoder.encodeKeyInt

    given coilPeerNumberKeyDecoder: KeyDecoder[CoilPeerNumber] with {
        override def apply(s: String): Option[CoilPeerNumber] = {

            for {
                i <- KeyDecoder.decodeKeyInt(s)
                pi <- NonNegativeInt(i)
            } yield i

        }
    }

    opaque type CoilPeerNumber = Int

    def apply(i: Int): CoilPeerNumber = {
        require(i >= 0 && i < (1 << 8))
        i
    }

    val zero: CoilPeerNumber = 0

    given Conversion[CoilPeerNumber, Int] = identity

    given Ordering[CoilPeerNumber] with {
        override def compare(x: CoilPeerNumber, y: CoilPeerNumber): Int =
            x.convert.compare(y.convert)
    }

    given Order[CoilPeerNumber] with {
        override def compare(x: CoilPeerNumber, y: CoilPeerNumber): Int =
            x.convert.compare(y.convert)
    }

    extension (self: CoilPeerNumber)
        def increment: CoilPeerNumber = CoilPeerNumber(self + 1)
        def decrement: CoilPeerNumber = CoilPeerNumber(self - 1)
}
