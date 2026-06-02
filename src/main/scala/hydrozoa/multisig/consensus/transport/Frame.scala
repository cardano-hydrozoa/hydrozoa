package hydrozoa.multisig.consensus.transport

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.consensus.PeerLiaison
import hydrozoa.multisig.consensus.PeerLiaison.Request.{GetMsgBatch, NewMsgBatch}
import hydrozoa.multisig.consensus.transport.Codecs.given
import io.circe.*
import io.circe.parser.decode
import io.circe.syntax.*

/** Wire envelope for the peer-to-peer WebSocket transport.
  *
  *   - [[Hello]] is sent as the first frame on a fresh connection so the recipient knows which peer
  *     is on the other end.
  *   - [[Msg]] carries a wire-eligible [[PeerLiaison.Request]] (currently [[GetMsgBatch]] or
  *     [[NewMsgBatch]]).
  */
sealed trait Frame
object Frame {
    final case class Hello(peerNum: Int) extends Frame
    final case class Msg(payload: PeerLiaison.Request) extends Frame

    /** The wire-eligible subset of [[PeerLiaison.Request]]. The proxy actor only forwards these
      * over the transport; everything else is local-only and gets dropped with a log line.
      */
    type Wire = GetMsgBatch | NewMsgBatch

    def fromWire(req: PeerLiaison.Request): Option[Wire] =
        req match {
            case x: GetMsgBatch => Some(x)
            case x: NewMsgBatch => Some(x)
            case _              => None
        }

    given (using CardanoNetwork.Section): Encoder[Frame] = Encoder.instance {
        case Hello(peerNum) =>
            Json.obj("t" -> "hello".asJson, "peerNum" -> peerNum.asJson)
        case Msg(payload) =>
            payload match {
                case x: GetMsgBatch =>
                    Json.obj("t" -> "msg".asJson, "kind" -> "GetMsgBatch".asJson, "v" -> x.asJson)
                case x: NewMsgBatch =>
                    Json.obj("t" -> "msg".asJson, "kind" -> "NewMsgBatch".asJson, "v" -> x.asJson)
                case _ =>
                    // Should be filtered out before reaching this codec; defensive fallback.
                    Json.obj(
                      "t" -> "msg".asJson,
                      "kind" -> "Unknown".asJson,
                      "v" -> Json.Null
                    )
            }
    }

    given (using CardanoNetwork.Section): Decoder[Frame] = Decoder.instance(c =>
        c.downField("t").as[String].flatMap {
            case "hello" =>
                c.downField("peerNum").as[Int].map(Hello(_))
            case "msg" =>
                c.downField("kind").as[String].flatMap {
                    case "GetMsgBatch" =>
                        c.downField("v").as[GetMsgBatch].map(Msg(_))
                    case "NewMsgBatch" =>
                        c.downField("v").as[NewMsgBatch].map(Msg(_))
                    case other =>
                        Left(DecodingFailure(s"Unknown msg kind: $other", c.history))
                }
            case other =>
                Left(DecodingFailure(s"Unknown frame type: $other", c.history))
        }
    )

    def encode(frame: Frame)(using CardanoNetwork.Section): String = frame.asJson.noSpaces

    def parse(text: String)(using CardanoNetwork.Section): Either[Error, Frame] =
        decode[Frame](text)
}
