package hydrozoa.multisig.consensus.transport

import hydrozoa.config.head.HeadParamsHash
import hydrozoa.config.head.initialization.InitializationParameters
import hydrozoa.config.head.initialization.InitializationParameters.HeadId
import hydrozoa.config.head.initialization.InitializationParameters.HeadId.given
import io.circe.syntax.*
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import scalus.cardano.ledger.Hash32
import scalus.uplc.builtin.ByteString

/** Which head a peer believes it is in, announced in its handshake and checked by whoever receives
  * it.
  *
  * **Two peers can speak the same protocol version and still not belong together.** The version
  * says the two ends agree on the vocabulary; this says they agree on the subject. A peer pointed
  * at the wrong head — a stale config, a copied deployment, the second of two heads on one box —
  * otherwise links up, starts pulling, and diverges on content that looks structurally fine.
  *
  * Both fields are needed and neither implies the other. `headId` names the head instance; two
  * deployments of the same head with different parameters share it. `headParamsHash` pins the whole
  * configuration (`docs/spec/head-params-hash.md`); a peer whose parameters differ derives
  * different effects from the same blocks.
  *
  * The store's own copy of both is already checked at open
  * ([[hydrozoa.multisig.persistence.StoreIdentity]]), so this closes the remaining side: store
  * against config is enforced locally, and config against counterpart is enforced here.
  */
final case class HeadIdentity(headId: HeadId, headParamsHash: Hash32)

object HeadIdentity {

    type Config = InitializationParameters.Section & HeadParamsHash.Section

    /** What this node announces. */
    def own(using config: Config): HeadIdentity =
        HeadIdentity(config.headId, config.headParamsHash)

    enum Check:
        case Compatible

        /** `field` names which half disagreed, so the log line points at the config to fix. */
        case Mismatch(field: String, expected: String, found: String)

        /** A counterpart that announced no head identity at all. Refused like a mismatch: it is
          * either too old to say, or not saying, and neither is something to link up with.
          */
        case Absent

    /** Check a counterpart's announced identity against this node's own.
      *
      * Run **after** the protocol-version check: a peer speaking another protocol may not mean the
      * same thing by these fields either, so the version is what makes them comparable at all.
      */
    def check(found: Option[HeadIdentity], own: HeadIdentity): Check =
        found match {
            case None => Check.Absent
            case Some(remote) =>
                if remote.headId != own.headId then
                    Check.Mismatch("headId", own.headId.toHex, remote.headId.toHex)
                else if remote.headParamsHash != own.headParamsHash then
                    Check.Mismatch(
                      "headParamsHash",
                      own.headParamsHash.toHex,
                      remote.headParamsHash.toHex
                    )
                else Check.Compatible
        }

    /** One line naming what to fix, for the refusal log. */
    def describe(check: Check): String = check match {
        case Check.Compatible => "head identity matches"
        case Check.Absent =>
            "counterpart announced no head identity; refusing to link"
        case Check.Mismatch(field, expected, found) =>
            s"$field mismatch: this node is in $expected, the counterpart is in $found"
    }

    // `HeadId` has one canonical JSON shape on its own companion; reuse it rather than coining a
    // second. The params hash rides as hex, the form it is rendered in everywhere else.
    given Encoder[HeadIdentity] = Encoder.instance(h =>
        Json.obj(
          "headId" -> h.headId.asJson,
          "headParamsHash" -> h.headParamsHash.toHex.asJson
        )
    )

    given Decoder[HeadIdentity] = Decoder.instance(c =>
        for {
            headId <- c.downField("headId").as[HeadId]
            hex <- c.downField("headParamsHash").as[String]
            hash <- scala.util
                .Try(Hash32.fromByteString(ByteString.fromHex(hex)))
                .toEither
                .left
                .map(_ => DecodingFailure(s"not a hex-encoded head params hash: $hex", c.history))
        } yield HeadIdentity(headId, hash)
    )
}
