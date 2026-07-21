package hydrozoa.multisig.persistence.codec

import cats.syntax.functor.*
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.FinalizationTxEndTime
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.codecs.json.Codecs.{transactionDecoder, transactionEncoder}
import hydrozoa.multisig.ledger.block.BlockVersion
import hydrozoa.multisig.ledger.l1.tx.FinalizationTx
import hydrozoa.multisig.ledger.l1.utxo.{MultisigRegimeUtxo, MultisigTreasuryUtxo, RolloutUtxo}
import hydrozoa.multisig.persistence.codec.FoundationCodecs.{resolvedUtxosDecoder, resolvedUtxosEncoder}
import hydrozoa.multisig.persistence.codec.TreasuryCodec.{multisigTreasuryUtxoDecoder, multisigTreasuryUtxoEncoder}
import hydrozoa.multisig.persistence.codec.UtxoWrapperCodecs.{blockVersionMajorDecoder, blockVersionMajorEncoder, multisigRegimeUtxoDecoder, multisigRegimeUtxoEncoder, rolloutUtxoDecoder, rolloutUtxoEncoder}
import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json}
import scalus.cardano.ledger.Transaction
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos

/** Persistence-layer JSON codec for [[FinalizationTx]] — sealed trait with `NoPayouts`,
  * `WithOnlyDirectPayouts`, `WithRollouts` variants. Tag-discriminated. Lens field skipped per the
  * agreed convention.
  */
object FinalizationTxCodec:

    given noPayoutsEncoder(using CardanoNetwork.Section): Encoder[FinalizationTx.NoPayouts] =
        Encoder.instance { t =>
            Json.obj(
              "finalizationTxEndTime" -> t.finalizationTxEndTime.asJson,
              "tx" -> t.tx.asJson,
              "majorVersionProduced" -> t.majorVersionProduced.asJson,
              "treasurySpent" -> t.treasurySpent.asJson,
              "multisigRegimeUtxoSpent" -> t.multisigRegimeUtxoSpent.asJson,
              "resolvedUtxos" -> t.resolvedUtxos.asJson
            )
        }

    given noPayoutsDecoder(using CardanoNetwork.Section): Decoder[FinalizationTx.NoPayouts] =
        Decoder.instance { c =>
            for
                fet <- c.downField("finalizationTxEndTime").as[FinalizationTxEndTime]
                tx <- c.downField("tx").as[Transaction]
                mv <- c.downField("majorVersionProduced").as[BlockVersion.Major]
                ts <- c.downField("treasurySpent").as[MultisigTreasuryUtxo]
                mrus <- c.downField("multisigRegimeUtxoSpent").as[MultisigRegimeUtxo]
                ru <- c.downField("resolvedUtxos").as[ResolvedUtxos]
            yield FinalizationTx.NoPayouts(
              finalizationTxEndTime = fet,
              tx = tx,
              majorVersionProduced = mv,
              treasurySpent = ts,
              multisigRegimeUtxoSpent = mrus,
              resolvedUtxos = ru
            )
        }

    given withOnlyDirectPayoutsEncoder(using
        CardanoNetwork.Section
    ): Encoder[FinalizationTx.WithOnlyDirectPayouts] = Encoder.instance { t =>
        Json.obj(
          "finalizationTxEndTime" -> t.finalizationTxEndTime.asJson,
          "tx" -> t.tx.asJson,
          "majorVersionProduced" -> t.majorVersionProduced.asJson,
          "treasurySpent" -> t.treasurySpent.asJson,
          "multisigRegimeUtxoSpent" -> t.multisigRegimeUtxoSpent.asJson,
          "payoutCount" -> t.payoutCount.asJson,
          "payoutOffset" -> t.payoutOffset.asJson,
          "resolvedUtxos" -> t.resolvedUtxos.asJson
        )
    }

    given withOnlyDirectPayoutsDecoder(using
        CardanoNetwork.Section
    ): Decoder[FinalizationTx.WithOnlyDirectPayouts] = Decoder.instance { c =>
        for
            fet <- c.downField("finalizationTxEndTime").as[FinalizationTxEndTime]
            tx <- c.downField("tx").as[Transaction]
            mv <- c.downField("majorVersionProduced").as[BlockVersion.Major]
            ts <- c.downField("treasurySpent").as[MultisigTreasuryUtxo]
            mrus <- c.downField("multisigRegimeUtxoSpent").as[MultisigRegimeUtxo]
            pc <- c.downField("payoutCount").as[Int]
            po <- c.downField("payoutOffset").as[Int]
            ru <- c.downField("resolvedUtxos").as[ResolvedUtxos]
        yield FinalizationTx.WithOnlyDirectPayouts(
          finalizationTxEndTime = fet,
          tx = tx,
          majorVersionProduced = mv,
          treasurySpent = ts,
          multisigRegimeUtxoSpent = mrus,
          payoutCount = pc,
          payoutOffset = po,
          resolvedUtxos = ru
        )
    }

    given withRolloutsEncoder(using
        CardanoNetwork.Section
    ): Encoder[FinalizationTx.WithRollouts] = Encoder.instance { t =>
        Json.obj(
          "finalizationTxEndTime" -> t.finalizationTxEndTime.asJson,
          "tx" -> t.tx.asJson,
          "majorVersionProduced" -> t.majorVersionProduced.asJson,
          "treasurySpent" -> t.treasurySpent.asJson,
          "multisigRegimeUtxoSpent" -> t.multisigRegimeUtxoSpent.asJson,
          "rolloutProduced" -> t.rolloutProduced.asJson,
          "payoutCount" -> t.payoutCount.asJson,
          "payoutOffset" -> t.payoutOffset.asJson,
          "resolvedUtxos" -> t.resolvedUtxos.asJson
        )
    }

    given withRolloutsDecoder(using
        CardanoNetwork.Section
    ): Decoder[FinalizationTx.WithRollouts] = Decoder.instance { c =>
        for
            fet <- c.downField("finalizationTxEndTime").as[FinalizationTxEndTime]
            tx <- c.downField("tx").as[Transaction]
            mv <- c.downField("majorVersionProduced").as[BlockVersion.Major]
            ts <- c.downField("treasurySpent").as[MultisigTreasuryUtxo]
            mrus <- c.downField("multisigRegimeUtxoSpent").as[MultisigRegimeUtxo]
            rp <- c.downField("rolloutProduced").as[RolloutUtxo]
            pc <- c.downField("payoutCount").as[Int]
            po <- c.downField("payoutOffset").as[Int]
            ru <- c.downField("resolvedUtxos").as[ResolvedUtxos]
        yield FinalizationTx.WithRollouts(
          finalizationTxEndTime = fet,
          tx = tx,
          majorVersionProduced = mv,
          treasurySpent = ts,
          multisigRegimeUtxoSpent = mrus,
          rolloutProduced = rp,
          payoutCount = pc,
          payoutOffset = po,
          resolvedUtxos = ru
        )
    }

    given finalizationTxEncoder(using CardanoNetwork.Section): Encoder[FinalizationTx] =
        Encoder.instance {
            case t: FinalizationTx.NoPayouts =>
                addKind(noPayoutsEncoder.apply(t), "NoPayouts")
            case t: FinalizationTx.WithOnlyDirectPayouts =>
                addKind(withOnlyDirectPayoutsEncoder.apply(t), "WithOnlyDirectPayouts")
            case t: FinalizationTx.WithRollouts =>
                addKind(withRolloutsEncoder.apply(t), "WithRollouts")
        }

    given finalizationTxDecoder(using CardanoNetwork.Section): Decoder[FinalizationTx] =
        Decoder.instance { c =>
            c.downField("kind").as[String].flatMap {
                case "NoPayouts"             => c.as[FinalizationTx.NoPayouts].widen
                case "WithOnlyDirectPayouts" => c.as[FinalizationTx.WithOnlyDirectPayouts].widen
                case "WithRollouts"          => c.as[FinalizationTx.WithRollouts].widen
                case other =>
                    Left(
                      io.circe.DecodingFailure(s"unknown FinalizationTx kind: $other", c.history)
                    )
            }
        }

    private def addKind(json: Json, kind: String): Json =
        json.deepMerge(Json.obj("kind" -> Json.fromString(kind)))
