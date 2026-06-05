package hydrozoa.multisig.consensus.ack

import io.circe.*

/** Monotonic hub-local sequence number for the [[HardAckWithId]] relay lane.
  *
  * A hub head assigns these to the coil hard-acks it relays (one per newly-received coil ack,
  * across all of its coils, in arrival order), so the per-link relay lane is contiguous and slots
  * into the same next-expected cursor machinery as the other lanes. Independent of the coil's own
  * [[HardAckNumber]] (which the embedded [[HardAck]] still carries for end-to-end verification).
  */
type HubHardAckNumber = HubHardAckNumber.HubHardAckNumber

object HubHardAckNumber {
    opaque type HubHardAckNumber = Int

    given Codec[HubHardAckNumber] = Codec.from(
      encodeA = Encoder.encodeInt,
      decodeA = Decoder.decodeInt
    )

    def apply(i: Int): HubHardAckNumber = {
        require(i >= 0)
        i
    }

    val zero: HubHardAckNumber = 0

    given Conversion[HubHardAckNumber, Int] = identity

    given Ordering[HubHardAckNumber] with {
        override def compare(x: HubHardAckNumber, y: HubHardAckNumber): Int = x.compare(y)
    }

    extension (self: HubHardAckNumber) def increment: HubHardAckNumber = HubHardAckNumber(self + 1)
}
