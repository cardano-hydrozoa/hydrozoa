package hydrozoa.multisig.persistence.codec

import cats.syntax.functor.*
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.SettlementTxEndTime
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.{byteStringDecoder, byteStringEncoder}
import hydrozoa.lib.cardano.scalus.codecs.json.Codecs.{transactionDecoder, transactionEncoder}
import hydrozoa.multisig.ledger.block.BlockVersion
import hydrozoa.multisig.ledger.l1.tx.SettlementTx
import hydrozoa.multisig.ledger.l1.utxo.{DepositUtxo, MultisigTreasuryUtxo, RolloutUtxo}
import hydrozoa.multisig.persistence.codec.DepositUtxoCodec.{
    depositUtxoDecoder,
    depositUtxoEncoder
}
import hydrozoa.multisig.persistence.codec.FoundationCodecs.{
    resolvedUtxosDecoder,
    resolvedUtxosEncoder
}
import hydrozoa.multisig.persistence.codec.TreasuryCodec.{
    multisigTreasuryUtxoDecoder,
    multisigTreasuryUtxoEncoder
}
import hydrozoa.multisig.persistence.codec.UtxoWrapperCodecs.{
    blockVersionMajorDecoder,
    blockVersionMajorEncoder,
    rolloutUtxoDecoder,
    rolloutUtxoEncoder
}
import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json}
import scalus.cardano.ledger.Transaction
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.uplc.builtin.ByteString

/** Persistence-layer JSON codec for [[SettlementTx]] — sealed trait with `NoPayouts`,
  * `WithOnlyDirectPayouts`, `WithRollouts` variants. Tag-discriminated.
  */
object SettlementTxCodec:

    given noPayoutsEncoder(using CardanoNetwork.Section): Encoder[SettlementTx.NoPayouts] =
        Encoder.instance { t =>
            Json.obj(
              "settlementTxEndTime" -> t.settlementTxEndTime.asJson,
              "tx" -> t.tx.asJson,
              "majorVersionProduced" -> t.majorVersionProduced.asJson,
              "kzgCommitment" -> (t.kzgCommitment: ByteString).asJson,
              "treasurySpent" -> t.treasurySpent.asJson,
              "treasuryProduced" -> t.treasuryProduced.asJson,
              "depositsSpent" -> t.depositsSpent.asJson,
              "resolvedUtxos" -> t.resolvedUtxos.asJson
            )
        }

    given noPayoutsDecoder(using CardanoNetwork.Section): Decoder[SettlementTx.NoPayouts] =
        Decoder.instance { c =>
            for
                set <- c.downField("settlementTxEndTime").as[SettlementTxEndTime]
                tx <- c.downField("tx").as[Transaction]
                mv <- c.downField("majorVersionProduced").as[BlockVersion.Major]
                kzg <- c.downField("kzgCommitment").as[ByteString]
                ts <- c.downField("treasurySpent").as[MultisigTreasuryUtxo]
                tp <- c.downField("treasuryProduced").as[MultisigTreasuryUtxo]
                ds <- c.downField("depositsSpent").as[List[DepositUtxo]]
                ru <- c.downField("resolvedUtxos").as[ResolvedUtxos]
            yield SettlementTx.NoPayouts(
              settlementTxEndTime = set,
              tx = tx,
              majorVersionProduced = mv,
              kzgCommitment = kzg,
              treasurySpent = ts,
              treasuryProduced = tp,
              depositsSpent = ds,
              resolvedUtxos = ru
            )
        }

    given withOnlyDirectPayoutsEncoder(using
        CardanoNetwork.Section
    ): Encoder[SettlementTx.WithOnlyDirectPayouts] = Encoder.instance { t =>
        Json.obj(
          "settlementTxEndTime" -> t.settlementTxEndTime.asJson,
          "tx" -> t.tx.asJson,
          "majorVersionProduced" -> t.majorVersionProduced.asJson,
          "kzgCommitment" -> (t.kzgCommitment: ByteString).asJson,
          "treasurySpent" -> t.treasurySpent.asJson,
          "treasuryProduced" -> t.treasuryProduced.asJson,
          "depositsSpent" -> t.depositsSpent.asJson,
          "payoutCount" -> t.payoutCount.asJson,
          "resolvedUtxos" -> t.resolvedUtxos.asJson
        )
    }

    given withOnlyDirectPayoutsDecoder(using
        CardanoNetwork.Section
    ): Decoder[SettlementTx.WithOnlyDirectPayouts] = Decoder.instance { c =>
        for
            set <- c.downField("settlementTxEndTime").as[SettlementTxEndTime]
            tx <- c.downField("tx").as[Transaction]
            mv <- c.downField("majorVersionProduced").as[BlockVersion.Major]
            kzg <- c.downField("kzgCommitment").as[ByteString]
            ts <- c.downField("treasurySpent").as[MultisigTreasuryUtxo]
            tp <- c.downField("treasuryProduced").as[MultisigTreasuryUtxo]
            ds <- c.downField("depositsSpent").as[List[DepositUtxo]]
            pc <- c.downField("payoutCount").as[Int]
            ru <- c.downField("resolvedUtxos").as[ResolvedUtxos]
        yield SettlementTx.WithOnlyDirectPayouts(
          settlementTxEndTime = set,
          tx = tx,
          majorVersionProduced = mv,
          kzgCommitment = kzg,
          treasurySpent = ts,
          treasuryProduced = tp,
          depositsSpent = ds,
          payoutCount = pc,
          resolvedUtxos = ru
        )
    }

    given withRolloutsEncoder(using CardanoNetwork.Section): Encoder[SettlementTx.WithRollouts] =
        Encoder.instance { t =>
            Json.obj(
              "settlementTxEndTime" -> t.settlementTxEndTime.asJson,
              "tx" -> t.tx.asJson,
              "majorVersionProduced" -> t.majorVersionProduced.asJson,
              "kzgCommitment" -> (t.kzgCommitment: ByteString).asJson,
              "treasurySpent" -> t.treasurySpent.asJson,
              "treasuryProduced" -> t.treasuryProduced.asJson,
              "depositsSpent" -> t.depositsSpent.asJson,
              "rolloutProduced" -> t.rolloutProduced.asJson,
              "payoutCount" -> t.payoutCount.asJson,
              "resolvedUtxos" -> t.resolvedUtxos.asJson
            )
        }

    given withRolloutsDecoder(using CardanoNetwork.Section): Decoder[SettlementTx.WithRollouts] =
        Decoder.instance { c =>
            for
                set <- c.downField("settlementTxEndTime").as[SettlementTxEndTime]
                tx <- c.downField("tx").as[Transaction]
                mv <- c.downField("majorVersionProduced").as[BlockVersion.Major]
                kzg <- c.downField("kzgCommitment").as[ByteString]
                ts <- c.downField("treasurySpent").as[MultisigTreasuryUtxo]
                tp <- c.downField("treasuryProduced").as[MultisigTreasuryUtxo]
                ds <- c.downField("depositsSpent").as[List[DepositUtxo]]
                rp <- c.downField("rolloutProduced").as[RolloutUtxo]
                pc <- c.downField("payoutCount").as[Int]
                ru <- c.downField("resolvedUtxos").as[ResolvedUtxos]
            yield SettlementTx.WithRollouts(
              settlementTxEndTime = set,
              tx = tx,
              majorVersionProduced = mv,
              kzgCommitment = kzg,
              treasurySpent = ts,
              treasuryProduced = tp,
              depositsSpent = ds,
              rolloutProduced = rp,
              payoutCount = pc,
              resolvedUtxos = ru
            )
        }

    given settlementTxEncoder(using CardanoNetwork.Section): Encoder[SettlementTx] =
        Encoder.instance {
            case t: SettlementTx.NoPayouts =>
                addKind(noPayoutsEncoder.apply(t), "NoPayouts")
            case t: SettlementTx.WithOnlyDirectPayouts =>
                addKind(withOnlyDirectPayoutsEncoder.apply(t), "WithOnlyDirectPayouts")
            case t: SettlementTx.WithRollouts =>
                addKind(withRolloutsEncoder.apply(t), "WithRollouts")
        }

    given settlementTxDecoder(using CardanoNetwork.Section): Decoder[SettlementTx] =
        Decoder.instance { c =>
            c.downField("kind").as[String].flatMap {
                case "NoPayouts" => c.as[SettlementTx.NoPayouts].widen
                case "WithOnlyDirectPayouts" =>
                    c.as[SettlementTx.WithOnlyDirectPayouts].widen
                case "WithRollouts" => c.as[SettlementTx.WithRollouts].widen
                case other =>
                    Left(
                      io.circe.DecodingFailure(s"unknown SettlementTx kind: $other", c.history)
                    )
            }
        }

    private def addKind(json: Json, kind: String): Json =
        json.deepMerge(Json.obj("kind" -> Json.fromString(kind)))
