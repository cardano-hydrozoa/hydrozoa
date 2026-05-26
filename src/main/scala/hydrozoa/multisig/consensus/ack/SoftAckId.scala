package hydrozoa.multisig.consensus.ack

import cats.implicits.catsSyntaxOrder
import hydrozoa.multisig.consensus.ack.SoftAckNumber.given
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import io.circe.*
import scala.annotation.targetName

type SoftAckId = SoftAckId.SoftAckId

object SoftAckId {
    opaque type SoftAckId = (HeadPeerNumber, SoftAckNumber)

    given Codec[SoftAckId] = Codec.from(
      encodeA = Encoder.encodeTuple2,
      decodeA = Decoder.decodeTuple2
    )

    def apply(peerNum: HeadPeerNumber, ackNum: SoftAckNumber): SoftAckId = (peerNum, ackNum)

    @targetName("apply_int")
    def apply(peerNum: Int, ackNum: Int): SoftAckId =
        (HeadPeerNumber(peerNum), SoftAckNumber(ackNum))

    def unapply(self: SoftAckId): (HeadPeerNumber, SoftAckNumber) =
        (HeadPeerNumber(self._1), SoftAckNumber(self._2))

    given Conversion[SoftAckId, (Int, Int)] = id => id._1.convert -> id._2.convert

    given Ordering[SoftAckId] with {
        override def compare(x: SoftAckId, y: SoftAckId): Int =
            x.convert.compare(y.convert)
    }

    extension (self: SoftAckId)
        def increment: SoftAckId = SoftAckId(self._1, self._2 + 1)
        def peerNum: HeadPeerNumber = HeadPeerNumber(self._1)
        def ackNum: SoftAckNumber = SoftAckNumber(self._2)
}
