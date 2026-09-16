package hydrozoa.multisig.consensus.transport

import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
import hydrozoa.multisig.consensus.transport.HandshakeSignature.given
import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json}
import scalus.cardano.ledger.Hash32

/** How the peer on the other end of a liaison link proved its identity: a signature over
  * [[HandshakeProof.preimage]], verified against the roster key the claimed peer number resolves to
  * — `CoilPeers` on the `/hub` link, `HeadPeers` on the mesh.
  *
  * A liaison link is the transport's answer to "who is this", and the liaison above it trusts that
  * answer — so the proof belongs in the frame that establishes the session, checked before the
  * socket is bound to a peer number, rather than arriving later on an accepted connection. It is
  * required, and there is no unproven form: every link on every deployment carries one.
  *
  * `headParamsHash` rides here rather than beside it in the frame because it is part of what the
  * signature covers: it is one digest over the whole head config, so a single field answers both
  * "are you on my head" and "do you agree with me about it".
  */
final case class HandshakeAuth(headParamsHash: Hash32, signature: HandshakeSignature)

object HandshakeAuth:

    given Encoder[HandshakeAuth] = Encoder.instance(auth =>
        Json.obj(
          "headParamsHash" -> auth.headParamsHash.asJson,
          "signature" -> auth.signature.asJson
        )
    )

    given Decoder[HandshakeAuth] = Decoder.instance(c =>
        for {
            headParamsHash <- c.downField("headParamsHash").as[Hash32]
            signature <- c.downField("signature").as[HandshakeSignature]
        } yield HandshakeAuth(headParamsHash, signature)
    )
