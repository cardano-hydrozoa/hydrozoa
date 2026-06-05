package hydrozoa.multisig.consensus.ack

import io.circe.*

/** Monotonic hub-local sequence number for the `relayedAck` lane on a hub→coil link.
  *
  * A hub assigns these to every ack it relays to a coil — head soft-acks, head hard-acks, and coil
  * hard-acks alike — in arrival order, so the per-link relay lane is contiguous and reuses the
  * next-expected cursor machinery. It is pure transport ordering: the embedded [[SoftAck]] /
  * [[HardAck]] still carries its own author + number for end-to-end verification and per-author
  * aggregation at the coil. Distinct from [[HubHardAckNumber]] (the head-mesh coil-ack lane).
  */
type RelayedAckNumber = RelayedAckNumber.RelayedAckNumber

object RelayedAckNumber {
    opaque type RelayedAckNumber = Int

    given Codec[RelayedAckNumber] = Codec.from(
      encodeA = Encoder.encodeInt,
      decodeA = Decoder.decodeInt
    )

    def apply(i: Int): RelayedAckNumber = {
        require(i >= 0)
        i
    }

    val zero: RelayedAckNumber = 0

    given Conversion[RelayedAckNumber, Int] = identity

    given Ordering[RelayedAckNumber] with {
        override def compare(x: RelayedAckNumber, y: RelayedAckNumber): Int = x.compare(y)
    }

    extension (self: RelayedAckNumber) def increment: RelayedAckNumber = RelayedAckNumber(self + 1)
}
