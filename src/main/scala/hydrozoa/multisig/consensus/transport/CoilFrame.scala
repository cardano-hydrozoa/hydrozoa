package hydrozoa.multisig.consensus.transport

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.consensus.liaison.BatchMessages.{Join, OwnHardAck, Population}
import hydrozoa.multisig.consensus.liaison.LiaisonProtocol
import hydrozoa.multisig.consensus.transport.Codecs.given
import io.circe.*
import io.circe.parser.decode
import io.circe.syntax.*

/** Wire envelope for the hub→coil WebSocket link.
  *
  *   - [[Handshake]] is sent as the coil's first frame so the hub learns which coil peer is on the
  *     other end (the link is a star — each coil dials its single hub), which protocol version it
  *     speaks, what it offers as proof of identity, and where it stands.
  *   - [[Msg]] carries a wire-eligible hub↔coil message: [[Join.Offer]] seating a joining coil at a
  *     start point, then the pull traffic ([[Population.Get]] / [[Population.New]] pulling/serving
  *     the population, [[OwnHardAck.Get]] / [[OwnHardAck.New]] pulling/serving the coil's own
  *     hard-ack).
  *
  * Both link directions ride one duplex, so the wire vocabulary is every shape either end emits;
  * each transport's `send` filters to the subset it actually emits.
  */
sealed trait CoilFrame
object CoilFrame {

    /** The coil's opening frame. `protocolVersion` is `None` from a counterpart too old to announce
      * one, which the hub refuses the same way it refuses a mismatch ([[ProtocolVersion.check]]).
      *
      * `marks` is a **claim, not a credential.** A coil that overstates where it stands is told to
      * catch up instead of being seated at a start point, and then stalls on acks it cannot produce
      * — its own link, its own problem. What it can never do is make the hub hand back a cursor
      * below one the hub already holds, which is the direction that would actually hurt (see
      * `CoilStartPoint`).
      */
    final case class Handshake(
        coilNum: Int,
        protocolVersion: Option[Int],
        auth: HandshakeAuth,
        marks: Join.Connected,
        head: Option[HeadIdentity]
    ) extends CoilFrame

    object Handshake {

        /** This node's own handshake: its coil number, the version it speaks, where it stands, and
          * — until GUM-322 — no proof of any of it.
          */
        def own(coilNum: Int, marks: Join.Connected, head: HeadIdentity): Handshake =
            Handshake(
              coilNum,
              Some(ProtocolVersion.current),
              HandshakeAuth.Unauthenticated,
              marks,
              Some(head)
            )
    }

    final case class Msg(payload: Wire) extends CoilFrame

    /** The wire-eligible hub↔coil messages. */
    type Wire =
        Join.Offer | Join.NoOffer | Population.Get | Population.New | OwnHardAck.Get |
            OwnHardAck.New

    /** Project the wire-eligible subset out of either link direction's request (the appended
      * artifacts, control ticks, and the transport-local [[Join.Connected]] never cross the wire).
      */
    def fromWire(
        req: LiaisonProtocol.HubToCoilRequest | LiaisonProtocol.CoilToHubRequest
    ): Option[Wire] =
        req match {
            case x: Join.Offer     => Some(x)
            case x: Join.NoOffer   => Some(x)
            case x: Population.Get => Some(x)
            case x: Population.New => Some(x)
            case x: OwnHardAck.Get => Some(x)
            case x: OwnHardAck.New => Some(x)
            case _                 => None
        }

    given (using CardanoNetwork.Section): Encoder[CoilFrame] = Encoder.instance {
        case Handshake(coilNum, protocolVersion, auth, marks, head) =>
            Json.obj(
              "t" -> "handshake".asJson,
              "coilNum" -> coilNum.asJson,
              "protocolVersion" -> protocolVersion.asJson,
              "auth" -> auth.asJson,
              "marks" -> marks.asJson,
              "head" -> head.asJson
            )
        case Msg(payload) =>
            payload match {
                case x: Join.Offer =>
                    Json.obj("t" -> "msg".asJson, "kind" -> "JoinOffer".asJson, "v" -> x.asJson)
                case x: Join.NoOffer =>
                    Json.obj("t" -> "msg".asJson, "kind" -> "JoinNoOffer".asJson, "v" -> x.asJson)
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
            case "handshake" =>
                for {
                    coilNum <- c.downField("coilNum").as[Int]
                    // Absent rather than required: a counterpart that announces no version is
                    // refused by the version check with a legible reason, not by a decode failure
                    // that reads as a malformed frame.
                    protocolVersion <- c.downField("protocolVersion").as[Option[Int]]
                    auth <- c.downField("auth").as[Option[HandshakeAuth]]
                    // Optional for the same reason as `protocolVersion`: a counterpart that
                    // sends no marks is refused by a check that says so, not by a decode failure
                    // that reads as a malformed frame. Absent means "I claim nothing", which is
                    // the safe reading — a coil claiming nothing gets seeded, never catch-up.
                    marks <- c.downField("marks").as[Option[Join.Connected]]
                    // Optional for the same reason as the two above: a counterpart that announces
                    // no head is refused by a check that says so, not by a decode failure.
                    head <- c.downField("head").as[Option[HeadIdentity]]
                } yield Handshake(
                  coilNum,
                  protocolVersion,
                  auth.getOrElse(HandshakeAuth.Unauthenticated),
                  marks.getOrElse(Join.Connected(None, None)),
                  head
                )
            case "msg" =>
                c.downField("kind").as[String].flatMap {
                    case "JoinOffer"   => c.downField("v").as[Join.Offer].map(Msg(_))
                    case "JoinNoOffer" => c.downField("v").as[Join.NoOffer].map(Msg(_))
                    case "PopGet"      => c.downField("v").as[Population.Get].map(Msg(_))
                    case "PopNew"      => c.downField("v").as[Population.New].map(Msg(_))
                    case "OwnGet"      => c.downField("v").as[OwnHardAck.Get].map(Msg(_))
                    case "OwnNew"      => c.downField("v").as[OwnHardAck.New].map(Msg(_))
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
