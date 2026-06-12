package hydrozoa.multisig.consensus.transport

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.consensus.liaison.BatchMessages.{OwnHardAck, Population}
import hydrozoa.multisig.consensus.liaison.LiaisonProtocol
import hydrozoa.multisig.consensus.transport.Codecs.given
import io.circe.*
import io.circe.parser.decode
import io.circe.syntax.*

/** Wire envelope for the hub→coil WebSocket link.
  *
  *   - [[Hello]] is sent as the coil's first frame so the hub learns which coil peer is on the
  *     other end (the link is a star — each coil dials its single hub).
  *   - [[Msg]] carries a wire-eligible hub↔coil batch message ([[Population.Get]] /
  *     [[Population.New]] pulling/serving the population, [[OwnHardAck.Get]] / [[OwnHardAck.New]]
  *     pulling/serving the coil's own hard-ack).
  *
  * Both link directions ride one duplex, so the wire vocabulary is all four batch shapes; each
  * transport's `send` filters to the subset it actually emits.
  */
sealed trait CoilFrame
object CoilFrame {
    final case class Hello(coilNum: Int) extends CoilFrame
    final case class Msg(payload: Wire) extends CoilFrame

    /** The wire-eligible hub↔coil batch messages. */
    type Wire = Population.Get | Population.New | OwnHardAck.Get | OwnHardAck.New

    /** Project the wire-eligible subset out of either link direction's request (the appended
      * artifacts and control ticks are local-only and never cross the wire).
      */
    def fromWire(
        req: LiaisonProtocol.HubToCoilRequest | LiaisonProtocol.CoilToHubRequest
    ): Option[Wire] =
        req match {
            case x: Population.Get => Some(x)
            case x: Population.New => Some(x)
            case x: OwnHardAck.Get => Some(x)
            case x: OwnHardAck.New => Some(x)
            case _                 => None
        }

    given (using CardanoNetwork.Section): Encoder[CoilFrame] = Encoder.instance {
        case Hello(coilNum) =>
            Json.obj("t" -> "hello".asJson, "coilNum" -> coilNum.asJson)
        case Msg(payload) =>
            payload match {
                case x: Population.Get =>
                    Json.obj("t" -> "msg".asJson, "kind" -> "PopGet".asJson, "v" -> x.asJson)
                case x: Population.New =>
                    Json.obj("t" -> "msg".asJson, "kind" -> "PopNew".asJson, "v" -> x.asJson)
                case x: OwnHardAck.Get =>
                    Json.obj("t" -> "msg".asJson, "kind" -> "OwnGet".asJson, "v" -> x.asJson)
                case x: OwnHardAck.New =>
                    Json.obj("t" -> "msg".asJson, "kind" -> "OwnNew".asJson, "v" -> x.asJson)
            }
    }

    given (using CardanoNetwork.Section): Decoder[CoilFrame] = Decoder.instance(c =>
        c.downField("t").as[String].flatMap {
            case "hello" =>
                c.downField("coilNum").as[Int].map(Hello(_))
            case "msg" =>
                c.downField("kind").as[String].flatMap {
                    case "PopGet" => c.downField("v").as[Population.Get].map(Msg(_))
                    case "PopNew" => c.downField("v").as[Population.New].map(Msg(_))
                    case "OwnGet" => c.downField("v").as[OwnHardAck.Get].map(Msg(_))
                    case "OwnNew" => c.downField("v").as[OwnHardAck.New].map(Msg(_))
                    case other =>
                        Left(DecodingFailure(s"Unknown coil msg kind: $other", c.history))
                }
            case other =>
                Left(DecodingFailure(s"Unknown coil frame type: $other", c.history))
        }
    )

    def encode(frame: CoilFrame)(using CardanoNetwork.Section): String = frame.asJson.noSpaces

    def parse(text: String)(using CardanoNetwork.Section): Either[Error, CoilFrame] =
        decode[CoilFrame](text)
}
