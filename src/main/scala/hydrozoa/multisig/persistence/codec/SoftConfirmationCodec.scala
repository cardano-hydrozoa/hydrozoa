package hydrozoa.multisig.persistence.codec

import cats.syntax.functor.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.ledger.block.Block
import hydrozoa.multisig.persistence.codec.SecCodec.{headerSignatureDecoder, headerSignatureEncoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.*
import io.circe.{Decoder, DecodingFailure, Encoder, Json}

/** Persistence-layer JSON codec for [[Block.SoftConfirmed.Next]] — the value stored at
  * `StoreKey.SoftConfirmation`, the FastConsensusActor aggregate (block brief + every head peer's
  * header signature) written at soft-confirmation (§6). Tag-discriminated
  * `{"kind": "Minor"|"Major"|"Final"}` like the other variant codecs.
  *
  * Reuses the per-variant `BlockBrief` codecs (their companion, implicit scope) and the
  * header-signature codec ([[SecCodec]]).
  */
object SoftConfirmationCodec:

    private given minorEncoder(using CardanoNetwork.Section): Encoder[Block.SoftConfirmed.Minor] =
        deriveEncoder[Block.SoftConfirmed.Minor]
    private given minorDecoder(using CardanoNetwork.Section): Decoder[Block.SoftConfirmed.Minor] =
        deriveDecoder[Block.SoftConfirmed.Minor]

    private given majorEncoder(using CardanoNetwork.Section): Encoder[Block.SoftConfirmed.Major] =
        deriveEncoder[Block.SoftConfirmed.Major]
    private given majorDecoder(using CardanoNetwork.Section): Decoder[Block.SoftConfirmed.Major] =
        deriveDecoder[Block.SoftConfirmed.Major]

    private given finalEncoder(using CardanoNetwork.Section): Encoder[Block.SoftConfirmed.Final] =
        deriveEncoder[Block.SoftConfirmed.Final]
    private given finalDecoder(using CardanoNetwork.Section): Decoder[Block.SoftConfirmed.Final] =
        deriveDecoder[Block.SoftConfirmed.Final]

    given softConfirmedNextEncoder(using
        CardanoNetwork.Section
    ): Encoder[Block.SoftConfirmed.Next] = Encoder.instance {
        case x: Block.SoftConfirmed.Minor => Json.obj("kind" -> "Minor".asJson, "v" -> x.asJson)
        case x: Block.SoftConfirmed.Major => Json.obj("kind" -> "Major".asJson, "v" -> x.asJson)
        case x: Block.SoftConfirmed.Final => Json.obj("kind" -> "Final".asJson, "v" -> x.asJson)
    }

    given softConfirmedNextDecoder(using
        CardanoNetwork.Section
    ): Decoder[Block.SoftConfirmed.Next] = Decoder.instance(c =>
        c.downField("kind").as[String].flatMap {
            case "Minor" => c.downField("v").as[Block.SoftConfirmed.Minor].widen
            case "Major" => c.downField("v").as[Block.SoftConfirmed.Major].widen
            case "Final" => c.downField("v").as[Block.SoftConfirmed.Final].widen
            case other =>
                Left(DecodingFailure(s"Unknown SoftConfirmed.Next kind: $other", c.history))
        }
    )
