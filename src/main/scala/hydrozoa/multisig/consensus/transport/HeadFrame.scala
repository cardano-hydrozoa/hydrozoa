package hydrozoa.multisig.consensus.transport

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.consensus.liaison.BatchMessages.Mesh
import hydrozoa.multisig.consensus.liaison.LiaisonProtocol
import hydrozoa.multisig.consensus.transport.Codecs.given
import io.circe.*
import io.circe.parser.decode
import io.circe.syntax.*

/** Wire envelope for the head-peer-mesh WebSocket transport.
  *
  *   - [[Hello]] is sent as the first frame on a fresh connection so the recipient knows which peer
  *     is on the other end.
  *   - [[Msg]] carries a wire-eligible head↔head batch message ([[Mesh.Get]] or [[Mesh.New]]).
  *
  * This is the `/head` (head-mesh) envelope only. The hub↔coil link has its own envelope
  * ([[CoilFrame]], on the `/hub` route), so `Population` / `OwnHardAck` batches never reach here.
  */
sealed trait HeadFrame
object HeadFrame {
    final case class Hello(peerNum: Int) extends HeadFrame
    final case class Msg(payload: LiaisonProtocol.HeadToHeadRequest) extends HeadFrame

    /** The wire-eligible subset of a head↔head liaison's `Request`. The proxy actor only forwards
      * these over the transport; everything else is local-only and gets dropped with a log line.
      */
    type Wire = Mesh.Get | Mesh.New

    def fromWire(req: LiaisonProtocol.HeadToHeadRequest): Option[Wire] =
        req match {
            case x: Mesh.Get => Some(x)
            case x: Mesh.New => Some(x)
            case _           => None
        }

    given (using CardanoNetwork.Section): Encoder[HeadFrame] = Encoder.instance {
        case Hello(peerNum) =>
            Json.obj("t" -> "hello".asJson, "peerNum" -> peerNum.asJson)
        case Msg(payload) =>
            payload match {
                case x: Mesh.Get =>
                    Json.obj("t" -> "msg".asJson, "kind" -> "MeshGet".asJson, "v" -> x.asJson)
                case x: Mesh.New =>
                    Json.obj("t" -> "msg".asJson, "kind" -> "MeshNew".asJson, "v" -> x.asJson)
                case _ =>
                    // Should be filtered out before reaching this codec; defensive fallback.
                    Json.obj(
                      "t" -> "msg".asJson,
                      "kind" -> "Unknown".asJson,
                      "v" -> Json.Null
                    )
            }
    }

    given (using CardanoNetwork.Section): Decoder[HeadFrame] = Decoder.instance(c =>
        c.downField("t").as[String].flatMap {
            case "hello" =>
                c.downField("peerNum").as[Int].map(Hello(_))
            case "msg" =>
                c.downField("kind").as[String].flatMap {
                    case "MeshGet" =>
                        c.downField("v").as[Mesh.Get].map(Msg(_))
                    case "MeshNew" =>
                        c.downField("v").as[Mesh.New].map(Msg(_))
                    case other =>
                        Left(DecodingFailure(s"Unknown msg kind: $other", c.history))
                }
            case other =>
                Left(DecodingFailure(s"Unknown frame type: $other", c.history))
        }
    )

    def encode(frame: HeadFrame)(using CardanoNetwork.Section): String = frame.asJson.noSpaces

    def parse(text: String)(using CardanoNetwork.Section): Either[Error, HeadFrame] =
        decode[HeadFrame](text)
}
