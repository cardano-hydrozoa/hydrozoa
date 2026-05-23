package hydrozoa.multisig.consensus.ack

import io.circe.*

/** Monotonic per-peer cursor for hard-ack delivery in the slow-consensus stream.
  *
  * Increments once per `(stackNum, round)` pair the peer emits — round 1 / round 2 / sole-round.
  * Independent of [[AckNumber]] (which paces fast-cycle soft-acks).
  *
  * Wire ordering invariant: for any given `stackNum`, the same peer's round-1 hard-ack is sent with
  * a strictly smaller `HardAckNumber` than its round-2 hard-ack.
  */
type HardAckNumber = HardAckNumber.HardAckNumber

object HardAckNumber {
    opaque type HardAckNumber = Int

    given Codec[HardAckNumber] = Codec.from(
      encodeA = Encoder.encodeInt,
      decodeA = Decoder.decodeInt
    )

    def apply(i: Int): HardAckNumber = {
        require(i >= 0)
        i
    }

    val zero: HardAckNumber = 0

    given Conversion[HardAckNumber, Int] = identity

    given Ordering[HardAckNumber] with {
        override def compare(x: HardAckNumber, y: HardAckNumber): Int = x.compare(y)
    }

    extension (self: HardAckNumber) def increment: HardAckNumber = HardAckNumber(self + 1)
}
