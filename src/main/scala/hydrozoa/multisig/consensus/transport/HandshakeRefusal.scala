package hydrozoa.multisig.consensus.transport

import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
import io.circe.syntax.*
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import scalus.cardano.ledger.Hash32

/** Why a liaison server refused a [[CoilFrame.Handshake]] / [[HeadFrame.Handshake]].
  *
  * The server sends this back — as `CoilFrame.Refused` / `HeadFrame.Refused` — and then closes the
  * socket. Each case is a **different operator action**, which is the whole reason they are
  * separate: a roster that does not list this peer is a config error on one side,
  * [[HeadParamsMismatch]] is a config disagreement between the two, and [[BadSignature]] is either
  * a key mix-up or somebody impersonating a peer. Collapsing them into one "rejected" makes all
  * three read as the same fault.
  *
  * The dialer keeps redialing across a refusal. A refusal is a statement about this attempt, not a
  * verdict on the peer: a config is fixed, a clock-adjacent race clears, and nothing else would
  * bring the link back if the dialer treated it as terminal.
  */
enum HandshakeRefusal {

    /** The counterpart speaks another protocol version. `found` is `None` for one too old to
      * announce a version at all — see [[ProtocolVersion.check]].
      */
    case ProtocolVersionMismatch(found: Option[Int], expected: Int)

    /** The hub does not hub the coil peer this handshake claims to be. */
    case NotHubbed(coilNum: Int)

    /** The head roster has no peer with the number this handshake claims. */
    case NotInRoster(peerNum: Int)

    /** The head mesh's dial topology: a server accepts inbound only from lower-numbered peers. */
    case WrongDialDirection(claimedPeerNum: Int, ownPeerNum: Int)

    /** The counterpart is on another head, or disagrees with this node about this one. */
    case HeadParamsMismatch(found: Hash32, expected: Hash32)

    /** The counterpart announced a different head identity, or announced none at all. `detail`
      * names which half disagreed and both values, so the log points at the config to fix.
      */
    case WrongHead(detail: String)

    /** The proof did not verify under the roster key for the claimed number. */
    case BadSignature
}

object HandshakeRefusal {

    /** A one-line rendering for a log line and for the WebSocket close frame's reason. */
    def describe(refusal: HandshakeRefusal): String = refusal match {
        case ProtocolVersionMismatch(found, expected) =>
            s"protocol version ${ProtocolVersion.describe(found)}, this node speaks $expected"
        case NotHubbed(coilNum) =>
            s"this node does not hub coil peer $coilNum"
        case NotInRoster(peerNum) =>
            s"this head has no peer $peerNum"
        case WrongDialDirection(claimedPeerNum, ownPeerNum) =>
            s"peer $claimedPeerNum must not dial peer $ownPeerNum (lower dials higher)"
        case HeadParamsMismatch(found, expected) =>
            s"head params ${found.toHex}, this node's are ${expected.toHex}"
        case WrongHead(detail) =>
            detail
        case BadSignature =>
            "the handshake signature did not verify under the roster key"
    }

    given Encoder[HandshakeRefusal] = Encoder.instance {
        case ProtocolVersionMismatch(found, expected) =>
            Json.obj(
              "r" -> "protocolVersion".asJson,
              "found" -> found.asJson,
              "expected" -> expected.asJson
            )
        case NotHubbed(coilNum) =>
            Json.obj("r" -> "notHubbed".asJson, "coilNum" -> coilNum.asJson)
        case NotInRoster(peerNum) =>
            Json.obj("r" -> "notInRoster".asJson, "peerNum" -> peerNum.asJson)
        case WrongDialDirection(claimedPeerNum, ownPeerNum) =>
            Json.obj(
              "r" -> "wrongDialDirection".asJson,
              "peerNum" -> claimedPeerNum.asJson,
              "ownPeerNum" -> ownPeerNum.asJson
            )
        case HeadParamsMismatch(found, expected) =>
            Json.obj(
              "r" -> "headParamsMismatch".asJson,
              "found" -> found.asJson,
              "expected" -> expected.asJson
            )
        case WrongHead(detail) =>
            Json.obj("r" -> "wrongHead".asJson, "detail" -> detail.asJson)
        case BadSignature =>
            Json.obj("r" -> "badSignature".asJson)
    }

    given Decoder[HandshakeRefusal] = Decoder.instance(c =>
        c.downField("r").as[String].flatMap {
            case "protocolVersion" =>
                for {
                    found <- c.downField("found").as[Option[Int]]
                    expected <- c.downField("expected").as[Int]
                } yield ProtocolVersionMismatch(found, expected)
            case "notHubbed" =>
                c.downField("coilNum").as[Int].map(NotHubbed(_))
            case "notInRoster" =>
                c.downField("peerNum").as[Int].map(NotInRoster(_))
            case "wrongDialDirection" =>
                for {
                    peerNum <- c.downField("peerNum").as[Int]
                    ownPeerNum <- c.downField("ownPeerNum").as[Int]
                } yield WrongDialDirection(peerNum, ownPeerNum)
            case "headParamsMismatch" =>
                for {
                    found <- c.downField("found").as[Hash32]
                    expected <- c.downField("expected").as[Hash32]
                } yield HeadParamsMismatch(found, expected)
            case "wrongHead" =>
                c.downField("detail").as[String].map(WrongHead(_))
            case "badSignature" => Right(BadSignature)
            case other =>
                Left(DecodingFailure(s"Unknown handshake refusal: $other", c.history))
        }
    )
}
