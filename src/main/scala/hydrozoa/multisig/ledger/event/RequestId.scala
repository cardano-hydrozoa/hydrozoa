package hydrozoa.multisig.ledger.event

import cats.implicits.catsSyntaxOrder
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag.{Invalid, Valid}
import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.syntax.*
import scala.annotation.targetName

type RequestId = RequestId.Id

object RequestId {
    def fromI64(requestIdI64: Long): RequestId = {
        val peerNum = (requestIdI64 >> 40).intValue
        val requestNum = requestIdI64 & ((1L << 40) - 1)
        apply(HeadPeerNumber(peerNum), RequestNumber(requestNum))
    }

    /** Used in the transient fields to indicate whether an event is valid or invalid. (This type is
      * solely here to avoid boolean blindness)
      */
    enum ValidityFlag:
        case Valid
        case Invalid

    given Codec[ValidityFlag] = Codec.from(
      decodeA = Decoder.instance(c =>
          c.as[String].flatMap {
              case "Valid"   => Right(Valid)
              case "Invalid" => Right(Invalid)
          }
      ),
      encodeA = Encoder.instance {
          case Valid   => "Valid".asJson
          case Invalid => "Invalid".asJson
      }
    )

    given Encoder[Id] = Encoder.instance(id =>
        Json.obj(
          "headPeerNumber" -> id._1.asJson,
          "requestNumber" -> id._2.asJson
        )
    )
    given Decoder[Id] = Decoder.instance(c =>
        for {
            hpn <- c.downField("headPeerNumber").as[HeadPeerNumber]
            rn <- c.downField("requstNumber").as[RequestNumber]
        } yield (hpn, rn)
    )

    opaque type Id = (HeadPeerNumber, RequestNumber)

    def apply(peerNum: HeadPeerNumber, requestNum: RequestNumber): Id = (peerNum, requestNum)

    @targetName("apply_Int")
    def apply(peerNum: Int, requestNum: Long): Id =
        (HeadPeerNumber(peerNum), RequestNumber(requestNum))

    def unapply(self: Id): (HeadPeerNumber, RequestNumber) =
        (HeadPeerNumber(self._1), RequestNumber(self._2))

    given Conversion[Id, (Int, Long)] = identity

    given Ordering[Id] with {
        override def compare(x: Id, y: Id): Int =
            x.convert.compare(y.convert)
    }

    extension (self: Id)
        def asI64: Long = (self._1.toLong << 40) + self.requestNum

        def increment: Id = RequestId(self._1, self._2 + 1)
        def peerNum: HeadPeerNumber = HeadPeerNumber(self._1)
        def requestNum: RequestNumber = RequestNumber(self._2)

        def precedes(other: Id): Boolean =
            self.peerNum == other.peerNum &&
                self.requestNum.increment == other.requestNum
}
