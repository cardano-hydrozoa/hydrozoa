package hydrozoa.multisig.consensus.ack

import hydrozoa.multisig.consensus.ack.HardAckNumber.given
import hydrozoa.multisig.consensus.peer.PeerId
import io.circe.*

/** Per-peer identifier for a single hard-ack. Mirrors [[SoftAckId]] but uses [[HardAckNumber]] for
  * the per-peer slow-cycle cursor, and a [[PeerId]] author (head or coil) — both peer kinds sign
  * hard-acks.
  */
type HardAckId = HardAckId.HardAckId

object HardAckId {
    opaque type HardAckId = (PeerId, HardAckNumber)

    given Codec[HardAckId] = Codec.from(
      encodeA = Encoder.encodeTuple2,
      decodeA = Decoder.decodeTuple2
    )

    def apply(peerId: PeerId, hardAckNum: HardAckNumber): HardAckId =
        (peerId, hardAckNum)

    // Compare components explicitly: inside this scope `HardAckId` dealiases to the bare tuple,
    // so a tuple-derived `Ordering.by` would summon the very given being defined.
    given Ordering[HardAckId] with {
        override def compare(x: HardAckId, y: HardAckId): Int = {
            val byPeer = Ordering[PeerId].compare(x._1, y._1)
            if byPeer != 0 then byPeer else Ordering[HardAckNumber].compare(x._2, y._2)
        }
    }

    extension (self: HardAckId)
        def peerId: PeerId = self._1
        def hardAckNum: HardAckNumber = self._2
}
