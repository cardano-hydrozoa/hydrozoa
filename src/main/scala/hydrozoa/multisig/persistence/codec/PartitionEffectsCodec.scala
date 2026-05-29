package hydrozoa.multisig.persistence.codec

import cats.syntax.functor.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.ledger.l1.tx.{FallbackTx, FinalizationTx, RefundTx, RolloutTx, SettlementTx}
import hydrozoa.multisig.ledger.stack.{PartitionEffects, StandaloneEvacuationCommitment}
import hydrozoa.multisig.persistence.codec.FallbackTxCodec.{fallbackTxDecoder, fallbackTxEncoder}
import hydrozoa.multisig.persistence.codec.FinalizationTxCodec.{finalizationTxDecoder, finalizationTxEncoder}
import hydrozoa.multisig.persistence.codec.RefundTxCodec.{refundTxDecoder, refundTxEncoder}
import hydrozoa.multisig.persistence.codec.RolloutTxCodec.{rolloutTxDecoder, rolloutTxEncoder}
import hydrozoa.multisig.persistence.codec.SecCodec.{multiSignedDecoder, multiSignedEncoder}
import hydrozoa.multisig.persistence.codec.SettlementTxCodec.{settlementTxDecoder, settlementTxEncoder}
import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json}

/** Persistence-layer JSON codec for [[PartitionEffects]] — sealed hierarchy with `Major`, `Final`,
  * `Minor` cases. Tag-discriminated per the agreed convention.
  *
  * Specialised at `StandaloneEvacuationCommitment.MultiSigned` for the hard-confirmed shape
  * (`PartitionEffects[StandaloneEvacuationCommitment.MultiSigned]`) — the only one persisted in the
  * `HardConfirmation` CF. The unsigned generic variant isn't persisted.
  */
object PartitionEffectsCodec:

    private type SecMS = StandaloneEvacuationCommitment.MultiSigned
    private type Effects = PartitionEffects[SecMS]

    given majorEncoder(using CardanoNetwork.Section): Encoder[PartitionEffects.Major[SecMS]] =
        Encoder.instance { m =>
            Json.obj(
              "settlement" -> m.settlement.asJson,
              "fallback" -> m.fallback.asJson,
              "rollouts" -> m.rollouts.asJson,
              "refunds" -> m.refunds.asJson,
              "sec" -> m.sec.asJson
            )
        }

    given majorDecoder(using CardanoNetwork.Section): Decoder[PartitionEffects.Major[SecMS]] =
        Decoder.instance { c =>
            for
                settlement <- c.downField("settlement").as[SettlementTx]
                fallback <- c.downField("fallback").as[FallbackTx]
                rollouts <- c.downField("rollouts").as[List[RolloutTx]]
                refunds <- c.downField("refunds").as[List[RefundTx]]
                sec <- c.downField("sec").as[Option[SecMS]]
            yield PartitionEffects.Major[SecMS](settlement, fallback, rollouts, refunds, sec)
        }

    given finalEncoder(using CardanoNetwork.Section): Encoder[PartitionEffects.Final] =
        Encoder.instance { f =>
            Json.obj(
              "finalization" -> f.finalization.asJson,
              "rollouts" -> f.rollouts.asJson
            )
        }

    given finalDecoder(using CardanoNetwork.Section): Decoder[PartitionEffects.Final] =
        Decoder.instance { c =>
            for
                finalization <- c.downField("finalization").as[FinalizationTx]
                rollouts <- c.downField("rollouts").as[List[RolloutTx]]
            yield PartitionEffects.Final(finalization, rollouts)
        }

    given minorEncoder(using CardanoNetwork.Section): Encoder[PartitionEffects.Minor[SecMS]] =
        Encoder.instance { m =>
            Json.obj(
              "sec" -> m.sec.asJson,
              "refunds" -> m.refunds.asJson
            )
        }

    given minorDecoder(using CardanoNetwork.Section): Decoder[PartitionEffects.Minor[SecMS]] =
        Decoder.instance { c =>
            for
                sec <- c.downField("sec").as[SecMS]
                refunds <- c.downField("refunds").as[List[RefundTx]]
            yield PartitionEffects.Minor[SecMS](sec, refunds)
        }

    given partitionEffectsEncoder(using CardanoNetwork.Section): Encoder[Effects] =
        Encoder.instance {
            case m: PartitionEffects.Major[SecMS] =>
                addKind(majorEncoder.apply(m), "Major")
            case f: PartitionEffects.Final =>
                addKind(finalEncoder.apply(f), "Final")
            case mi: PartitionEffects.Minor[SecMS] =>
                addKind(minorEncoder.apply(mi), "Minor")
        }

    given partitionEffectsDecoder(using CardanoNetwork.Section): Decoder[Effects] =
        Decoder.instance { c =>
            c.downField("kind").as[String].flatMap {
                case "Major" => c.as[PartitionEffects.Major[SecMS]].widen
                case "Final" => c.as[PartitionEffects.Final].widen
                case "Minor" => c.as[PartitionEffects.Minor[SecMS]].widen
                case other =>
                    Left(
                      io.circe.DecodingFailure(s"unknown PartitionEffects kind: $other", c.history)
                    )
            }
        }

    private def addKind(json: Json, kind: String): Json =
        json.deepMerge(Json.obj("kind" -> Json.fromString(kind)))
