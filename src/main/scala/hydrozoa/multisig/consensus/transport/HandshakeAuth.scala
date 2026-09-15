package hydrozoa.multisig.consensus.transport

import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
import hydrozoa.multisig.consensus.transport.HandshakeSignature.given
import io.circe.syntax.*
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import scalus.cardano.ledger.Hash32

/** How the peer on the other end of a liaison link proved its identity.
  *
  * A liaison link is the transport's answer to "who is this", and the liaison above it trusts that
  * answer — so the proof belongs in the frame that establishes the session, checked before the
  * socket is bound to a peer number, rather than arriving later on an accepted connection.
  *
  * [[Unauthenticated]] stays in the vocabulary so a counterpart that offers nothing is refused with
  * a legible reason ([[HandshakeRefusal.Unauthenticated]]) instead of by a circe `DecodingFailure`
  * that reads as a malformed frame. It is refused on every link; no deployment accepts it.
  */
enum HandshakeAuth:

    /** The counterpart asserted its identity and offered no proof of it. */
    case Unauthenticated

    /** A proof over [[HandshakeProof.preimage]], verified against the roster key the claimed peer
      * number resolves to — `CoilPeers` on the `/hub` link, `HeadPeers` on the mesh.
      *
      * `headParamsHash` rides here rather than beside it in the frame because it is part of what
      * the signature covers: it is one digest over the whole head config, so a single field answers
      * both "are you on my head" and "do you agree with me about it".
      */
    case Signed(headParamsHash: Hash32, signature: HandshakeSignature)

object HandshakeAuth:

    /** Wire tag for each case. Absent on the wire reads as [[Unauthenticated]], so a counterpart
      * that omits the field is treated as what it is — unproven — rather than rejected.
      */
    given Encoder[HandshakeAuth] = Encoder.instance {
        case Unauthenticated =>
            Json.obj("t" -> "unauthenticated".asJson)
        case Signed(headParamsHash, signature) =>
            Json.obj(
              "t" -> "signed".asJson,
              "headParamsHash" -> headParamsHash.asJson,
              "signature" -> signature.asJson
            )
    }

    given Decoder[HandshakeAuth] = Decoder.instance(c =>
        c.downField("t").as[String].flatMap {
            case "unauthenticated" => Right(Unauthenticated)
            case "signed" =>
                for {
                    headParamsHash <- c.downField("headParamsHash").as[Hash32]
                    signature <- c.downField("signature").as[HandshakeSignature]
                } yield Signed(headParamsHash, signature)
            case other => Left(DecodingFailure(s"Unknown handshake auth: $other", c.history))
        }
    )
