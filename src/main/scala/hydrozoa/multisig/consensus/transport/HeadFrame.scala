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
  *   - [[Handshake]] is sent as the first frame on a fresh connection so the recipient knows which
  *     peer is on the other end, which protocol version it speaks, and what it offers as proof of
  *     identity.
  *   - [[Msg]] carries a wire-eligible head↔head batch message ([[Mesh.Get]] or [[Mesh.New]]).
  *
  * This is the `/head` (head-mesh) envelope only. The hub↔coil link has its own envelope
  * ([[CoilFrame]], on the `/hub` route), so `Population` / `OwnHardAck` batches never reach here.
  *
  * **The head-mesh handshake carries no start point**, unlike the coil→hub one: a head peer holds
  * the full roster from config and always catches up from its own store, so there is nothing to
  * negotiate beyond who it is and whether the two ends speak the same protocol.
  */
sealed trait HeadFrame
object HeadFrame {

    /** The dialing peer's opening frame. `protocolVersion` is `None` from a counterpart too old to
      * announce one, which is refused the same way a mismatch is ([[ProtocolVersion.check]]).
      */
    final case class Handshake(
        peerNum: Int,
        protocolVersion: Option[Int],
        auth: HandshakeAuth
    ) extends HeadFrame

    object Handshake {

        /** This node's own handshake: its peer number, the version it speaks, and — until GUM-322 —
          * no proof of either.
          */
        def own(peerNum: Int): Handshake =
            Handshake(peerNum, Some(ProtocolVersion.current), HandshakeAuth.Unauthenticated)
    }

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
        case Handshake(peerNum, protocolVersion, auth) =>
            Json.obj(
              "t" -> "handshake".asJson,
              "peerNum" -> peerNum.asJson,
              "protocolVersion" -> protocolVersion.asJson,
              "auth" -> auth.asJson
            )
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
            case "handshake" =>
                for {
                    peerNum <- c.downField("peerNum").as[Int]
                    // Absent rather than required: a counterpart that announces no version is
                    // refused by the version check with a legible reason, not by a decode failure
                    // that reads as a malformed frame.
                    protocolVersion <- c.downField("protocolVersion").as[Option[Int]]
                    auth <- c.downField("auth").as[Option[HandshakeAuth]]
                } yield Handshake(
                  peerNum,
                  protocolVersion,
                  auth.getOrElse(HandshakeAuth.Unauthenticated)
                )
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
