package hydrozoa.multisig.consensus.ack

import io.circe.*

/** Monotonic hub-local sequence number for the `relayedMsg` lane on a hub→coil link.
  *
  * A hub assigns these to every ack it relays to a coil peer — head peer soft-acks, head peer
  * hard-acks, and coil peer hard-acks alike — in arrival order, so the per-link relay lane is
  * contiguous and reuses the next-expected cursor machinery. It is pure transport ordering: the
  * embedded [[SoftAck]] / [[HardAck]] still carries its own author + number for end-to-end
  * verification and per-author aggregation at the coil peer. Distinct from [[HubHardAckNumber]]
  * (the coil-peer-ack lane on the head-peer mesh).
  */
type RelayedMsgNumber = RelayedMsgNumber.RelayedMsgNumber

object RelayedMsgNumber {
    opaque type RelayedMsgNumber = Int

    given Codec[RelayedMsgNumber] = Codec.from(
      encodeA = Encoder.encodeInt,
      decodeA = Decoder.decodeInt
    )

    def apply(i: Int): RelayedMsgNumber = {
        require(i >= 0)
        i
    }

    val zero: RelayedMsgNumber = 0

    given Conversion[RelayedMsgNumber, Int] = identity

    given Ordering[RelayedMsgNumber] with {
        override def compare(x: RelayedMsgNumber, y: RelayedMsgNumber): Int = x.compare(y)
    }

    extension (self: RelayedMsgNumber) def increment: RelayedMsgNumber = RelayedMsgNumber(self + 1)
}
