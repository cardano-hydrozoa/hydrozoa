package hydrozoa.multisig.consensus.ack

import cats.implicits.catsSyntaxOrder
import hydrozoa.multisig.consensus.ack.HardAckNumber.given
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import io.circe.*
import scala.annotation.targetName

/** Per-peer identifier for a single hard-ack. Mirrors [[SoftAckId]] but uses [[HardAckNumber]] for
  * the per-peer slow-cycle cursor.
  */
type HardAckId = HardAckId.HardAckId

object HardAckId {
    opaque type HardAckId = (HeadPeerNumber, HardAckNumber)

    given Codec[HardAckId] = Codec.from(
      encodeA = Encoder.encodeTuple2,
      decodeA = Decoder.decodeTuple2
    )

    def apply(peerNum: HeadPeerNumber, hardAckNum: HardAckNumber): HardAckId =
        (peerNum, hardAckNum)

    @targetName("apply_int")
    def apply(peerNum: Int, hardAckNum: Int): HardAckId =
        (HeadPeerNumber(peerNum), HardAckNumber(hardAckNum))

    given Conversion[HardAckId, (Int, Int)] = id => id._1.convert -> id._2.convert

    given Ordering[HardAckId] with {
        override def compare(x: HardAckId, y: HardAckId): Int =
            x.convert.compare(y.convert)
    }

    extension (self: HardAckId)
        def peerNum: HeadPeerNumber = HeadPeerNumber(self._1)
        def hardAckNum: HardAckNumber = HardAckNumber(self._2)
}
