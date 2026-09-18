package hydrozoa.multisig.consensus.transport

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.consensus.liaison.BatchMessages.{OwnHardAck, Population}
import hydrozoa.multisig.consensus.liaison.LiaisonProtocol
import hydrozoa.multisig.consensus.peer.PeerWallet
import hydrozoa.multisig.consensus.transport.Codecs.given
import io.circe.*
import io.circe.parser.decode
import io.circe.syntax.*
import scalus.cardano.ledger.Hash32

/** Wire envelope for the hub→coil WebSocket link.
  *
  * The link opens with a three-frame exchange, in this order:
  *
  *   1. [[Challenge]] — the hub's first frame on a freshly accepted socket, carrying the nonce that
  *      binds the coil's proof to this socket.
  *   2. [[Handshake]] — the coil's answer, naming which coil peer is on the other end (the link is
  *      a star: each coil dials its single hub), which protocol version it speaks, and its proof of
  *      the claim.
  *   3. [[Refused]] — sent instead of accepting, naming why, immediately before the hub closes the
  *      socket.
  *
  * Then [[Msg]] carries a wire-eligible hub↔coil batch message ([[Population.Get]] /
  * [[Population.New]] pulling/serving the population, [[OwnHardAck.Get]] / [[OwnHardAck.New]]
  * pulling/serving the coil's own hard-ack).
  *
  * Both link directions ride one duplex, so the wire vocabulary is all four batch shapes; each
  * transport's `send` filters to the subset it actually emits.
  */
sealed trait CoilFrame
object CoilFrame {

    /** The hub's opening frame: a nonce drawn fresh for this socket, which the coil's [[Handshake]]
      * signs over. See [[HandshakeNonce]] for why the server issues it rather than the coil.
      *
      * It announces `protocolVersion` too, so the coil reaches its own verdict instead of waiting
      * to be told. A coil that checks here refuses before signing a proof for a hub it cannot talk
      * to, and — the reason that matters — it can name both versions even when the hub says
      * nothing, which is exactly what a hub too old to send [[Refused]] does. `None` from a
      * counterpart too old to announce one, refused the same way a mismatch is
      * ([[ProtocolVersion.check]]).
      */
    final case class Challenge(nonce: HandshakeNonce, protocolVersion: Option[Int])
        extends CoilFrame

    object Challenge {

        /** This hub's own challenge: a fresh nonce and the version it speaks. */
        def own(nonce: HandshakeNonce): Challenge =
            Challenge(nonce, Some(ProtocolVersion.current))
    }

    /** The coil's answer to a [[Challenge]]. `protocolVersion` is `None` from a counterpart too old
      * to announce one, which the hub refuses the same way it refuses a mismatch
      * ([[ProtocolVersion.check]]).
      */
    final case class Handshake(
        coilNum: Int,
        protocolVersion: Option[Int],
        auth: HandshakeAuth
    ) extends CoilFrame

    object Handshake {

        /** This node's own handshake: its coil number, the version it speaks, and a proof of both
          * against the hub's `nonce`, signed with this coil peer's own key.
          */
        def own(
            coilNum: Int,
            wallet: PeerWallet,
            headParamsHash: Hash32,
            nonce: HandshakeNonce
        ): Handshake =
            Handshake(
              coilNum,
              Some(ProtocolVersion.current),
              HandshakeProof.sign(
                wallet,
                HandshakeProof.Link.CoilToHub,
                coilNum,
                ProtocolVersion.current,
                headParamsHash,
                nonce
              )
            )
    }

    /** The hub's refusal, sent immediately before it closes the socket. A refused coil is told why
      * rather than watching a connection that opens and then drops on the server's idle timeout.
      */
    final case class Refused(refusal: HandshakeRefusal) extends CoilFrame

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
        case Challenge(nonce, protocolVersion) =>
            Json.obj(
              "t" -> "challenge".asJson,
              "nonce" -> nonce.asJson,
              "protocolVersion" -> protocolVersion.asJson
            )
        case Handshake(coilNum, protocolVersion, auth) =>
            Json.obj(
              "t" -> "handshake".asJson,
              "coilNum" -> coilNum.asJson,
              "protocolVersion" -> protocolVersion.asJson,
              "auth" -> auth.asJson
            )
        case Refused(refusal) =>
            Json.obj("t" -> "refused".asJson, "refusal" -> refusal.asJson)
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
            case "challenge" =>
                for {
                    nonce <- c.downField("nonce").as[HandshakeNonce]
                    // Optional for the same reason the handshake's is: a counterpart announcing
                    // no version gets a legible refusal, not a decode failure.
                    protocolVersion <- c.downField("protocolVersion").as[Option[Int]]
                } yield Challenge(nonce, protocolVersion)
            case "refused" =>
                c.downField("refusal").as[HandshakeRefusal].map(Refused(_))
            case "handshake" =>
                for {
                    coilNum <- c.downField("coilNum").as[Int]
                    // Absent rather than required: a counterpart that announces no version is
                    // refused by the version check with a legible reason, not by a decode failure
                    // that reads as a malformed frame.
                    protocolVersion <- c.downField("protocolVersion").as[Option[Int]]
                    auth <- c.downField("auth").as[HandshakeAuth]
                } yield Handshake(coilNum, protocolVersion, auth)
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
