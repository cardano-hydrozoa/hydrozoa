package hydrozoa.multisig.consensus.transport

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.consensus.liaison.BatchMessages.Mesh
import hydrozoa.multisig.consensus.liaison.LiaisonProtocol
import hydrozoa.multisig.consensus.peer.PeerWallet
import hydrozoa.multisig.consensus.transport.Codecs.given
import io.circe.*
import io.circe.parser.decode
import io.circe.syntax.*
import scalus.cardano.ledger.Hash32

/** Wire envelope for the head-peer-mesh WebSocket transport.
  *
  * The link opens with a three-frame exchange, in this order:
  *
  *   1. [[Challenge]] — the accepting peer's first frame, carrying the nonce that binds the
  *      dialer's proof to this socket.
  *   2. [[Handshake]] — the dialer's answer, naming which peer is on the other end, which protocol
  *      version it speaks, and its proof of the claim.
  *   3. [[Refused]] — sent instead of accepting, naming why, immediately before the socket closes.
  *
  * Then [[Msg]] carries a wire-eligible head↔head batch message ([[Mesh.Get]] or [[Mesh.New]]).
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

    /** The accepting peer's opening frame: a nonce drawn fresh for this socket, which the dialer's
      * [[Handshake]] signs over. See [[HandshakeNonce]] for why the server issues it.
      *
      * It announces `protocolVersion` too, so the dialer reaches its own verdict instead of waiting
      * to be told. A dialer that checks here refuses before signing a proof for a peer it cannot
      * talk to, and — the reason that matters — it can name both versions even when the server says
      * nothing, which is exactly what a server too old to send [[Refused]] does. `None` from a
      * counterpart too old to announce one, refused the same way a mismatch is
      * ([[ProtocolVersion.check]]).
      */
    final case class Challenge(nonce: HandshakeNonce, protocolVersion: Option[Int])
        extends HeadFrame

    object Challenge {

        /** This node's own challenge: a fresh nonce and the version it speaks. */
        def own(nonce: HandshakeNonce): Challenge =
            Challenge(nonce, Some(ProtocolVersion.current))
    }

    /** The dialing peer's answer to a [[Challenge]]. `protocolVersion` is `None` from a counterpart
      * too old to announce one, which is refused the same way a mismatch is
      * ([[ProtocolVersion.check]]).
      */
    final case class Handshake(
        peerNum: Int,
        protocolVersion: Option[Int],
        auth: HandshakeAuth,
        head: Option[HeadIdentity]
    ) extends HeadFrame

    object Handshake {

        /** This node's own handshake: its peer number, the version it speaks, and a proof of both
          * against the remote's `nonce`, signed with this head peer's own key.
          */
        def own(
            peerNum: Int,
            wallet: PeerWallet,
            headParamsHash: Hash32,
            nonce: HandshakeNonce,
            head: HeadIdentity
        ): Handshake =
            Handshake(
              peerNum,
              Some(ProtocolVersion.current),
              HandshakeProof.sign(
                wallet,
                HandshakeProof.Link.HeadToHead,
                peerNum,
                ProtocolVersion.current,
                headParamsHash,
                nonce
              ),
              Some(head)
            )
    }

    /** The accepting peer's refusal, sent immediately before it closes the socket, so a refused
      * dialer is told why rather than watching a connection that opens and then drops on the
      * server's idle timeout.
      */
    final case class Refused(refusal: HandshakeRefusal) extends HeadFrame

    final case class Msg(payload: LiaisonProtocol.MeshLiaisonMessage) extends HeadFrame

    /** The wire-eligible subset of a head↔head liaison's `Request`. The proxy actor only forwards
      * these over the transport; everything else is local-only and gets dropped with a log line.
      */
    type Wire = Mesh.Get | Mesh.New

    def fromWire(req: LiaisonProtocol.MeshLiaisonMessage): Option[Wire] =
        req match {
            case x: Mesh.Get => Some(x)
            case x: Mesh.New => Some(x)
            case _           => None
        }

    given (using CardanoNetwork.Section): Encoder[HeadFrame] = Encoder.instance {
        case Challenge(nonce, protocolVersion) =>
            Json.obj(
              "t" -> "challenge".asJson,
              "nonce" -> nonce.asJson,
              "protocolVersion" -> protocolVersion.asJson
            )
        case Refused(refusal) =>
            Json.obj("t" -> "refused".asJson, "refusal" -> refusal.asJson)
        case Handshake(peerNum, protocolVersion, auth, head) =>
            Json.obj(
              "t" -> "handshake".asJson,
              "peerNum" -> peerNum.asJson,
              "protocolVersion" -> protocolVersion.asJson,
              "auth" -> auth.asJson,
              "head" -> head.asJson
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
                    peerNum <- c.downField("peerNum").as[Int]
                    // Absent rather than required: a counterpart that announces no version is
                    // refused by the version check with a legible reason, not by a decode failure
                    // that reads as a malformed frame.
                    protocolVersion <- c.downField("protocolVersion").as[Option[Int]]
                    auth <- c.downField("auth").as[HandshakeAuth]
                    // Optional for the same reason as `protocolVersion`: a counterpart that
                    // announces no head is refused by a check that says so, not by a decode
                    // failure that reads as a malformed frame.
                    head <- c.downField("head").as[Option[HeadIdentity]]
                } yield Handshake(peerNum, protocolVersion, auth, head)
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
