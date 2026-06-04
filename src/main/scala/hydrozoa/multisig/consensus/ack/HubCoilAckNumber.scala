package hydrozoa.multisig.consensus.ack

import io.circe.*

/** Monotonic hub-local sequence number for the [[HubCoilAck]] relay lane.
  *
  * A hub head assigns these to the coil hard-acks it relays (one per newly-received coil ack,
  * across all of its coils, in arrival order), so the per-link relay lane is contiguous and slots
  * into the same next-expected cursor machinery as the other lanes. Independent of the coil's own
  * [[HardAckNumber]] (which the embedded [[HardAck]] still carries for end-to-end verification).
  */
type HubCoilAckNumber = HubCoilAckNumber.HubCoilAckNumber

object HubCoilAckNumber {
    opaque type HubCoilAckNumber = Int

    given Codec[HubCoilAckNumber] = Codec.from(
      encodeA = Encoder.encodeInt,
      decodeA = Decoder.decodeInt
    )

    def apply(i: Int): HubCoilAckNumber = {
        require(i >= 0)
        i
    }

    val zero: HubCoilAckNumber = 0

    given Conversion[HubCoilAckNumber, Int] = identity

    given Ordering[HubCoilAckNumber] with {
        override def compare(x: HubCoilAckNumber, y: HubCoilAckNumber): Int = x.compare(y)
    }

    extension (self: HubCoilAckNumber) def increment: HubCoilAckNumber = HubCoilAckNumber(self + 1)
}
