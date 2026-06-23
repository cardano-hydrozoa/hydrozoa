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

    // Canonical JSON shape, used everywhere by default (GUM-131): a `{ headPeerNumber, requestNumber }`
    // object, in implicit scope automatically as the companion codec.
    given Encoder[Id] = Encoder.instance(id =>
        Json.obj(
          "headPeerNumber" -> id._1.asJson,
          "requestNumber" -> id._2.asJson
        )
    )
    given Decoder[Id] = Decoder.instance(c =>
        for {
            hpn <- c.downField("headPeerNumber").as[HeadPeerNumber]
            rn <- c.downField("requestNumber").as[RequestNumber]
        } yield (hpn, rn)
    )

    /** The **L2-ledger / SugarRush** wire form: the RequestId packed into a single i64 (`asI64` /
      * `fromI64`), which the remote L2 ledger expects as a `u64`. Opt in *locally* at the L2 codec
      * sites with `import RequestId.i64.given` — it shadows the companion object codec there. The
      * default everywhere else (consensus wire, persistence journals, HTTP API) stays the object
      * shape above; this is deliberately **not** the global default (GUM-131).
      */
    object i64 {
        given Encoder[Id] = Encoder.instance(id => Json.fromLong(id.asI64))
        given Decoder[Id] = Decoder.decodeLong.map(fromI64)
    }

    opaque type Id = (HeadPeerNumber, RequestNumber)

    def apply(peerNum: HeadPeerNumber, requestNum: RequestNumber): Id = (peerNum, requestNum)

    @targetName("apply_Int")
    def apply(peerNum: Int, requestNum: Long): Id =
        (HeadPeerNumber(peerNum), RequestNumber(requestNum))

    def unapply(self: Id): (HeadPeerNumber, RequestNumber) =
        (HeadPeerNumber(self._1), RequestNumber(self._2))

    // See docs/style-guide.md — opaque tuple conversions must call .convert explicitly on each element.
    given Conversion[Id, (Int, Long)] = id => id._1.convert -> id._2.convert

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
