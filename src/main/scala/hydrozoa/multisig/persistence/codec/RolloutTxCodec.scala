package hydrozoa.multisig.persistence.codec

import cats.syntax.functor.*
import hydrozoa.lib.cardano.scalus.codecs.json.Codecs.{transactionDecoder, transactionEncoder}
import hydrozoa.multisig.ledger.l1.tx.RolloutTx
import hydrozoa.multisig.ledger.l1.utxo.RolloutUtxo
import hydrozoa.multisig.persistence.codec.FoundationCodecs.{resolvedUtxosDecoder, resolvedUtxosEncoder}
import hydrozoa.multisig.persistence.codec.UtxoWrapperCodecs.{rolloutUtxoDecoder, rolloutUtxoEncoder}
import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json}
import scalus.cardano.ledger.Transaction
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos

/** Persistence-layer JSON codec for [[RolloutTx]] — sealed trait with `Last` and `NotLast`
  * variants. Tag-discriminated `{"kind": "Last"|"NotLast", ...}` per the agreed convention.
  *
  * Hand-rolled (not `deriveEncoder`/`deriveDecoder`) because every variant carries a `monocle.Lens`
  * field with a sensible default — we don't persist it; on decode the case-class default
  * constructor expression rebuilds it.
  */
object RolloutTxCodec:

    given lastEncoder: Encoder[RolloutTx.Last] = Encoder.instance { t =>
        Json.obj(
          "tx" -> t.tx.asJson,
          "rolloutSpent" -> t.rolloutSpent.asJson,
          "payoutCount" -> t.payoutCount.asJson,
          "payoutOffset" -> t.payoutOffset.asJson,
          "resolvedUtxos" -> t.resolvedUtxos.asJson
        )
    }

    given lastDecoder: Decoder[RolloutTx.Last] = Decoder.instance { c =>
        for
            tx <- c.downField("tx").as[Transaction]
            rolloutSpent <- c.downField("rolloutSpent").as[RolloutUtxo]
            payoutCount <- c.downField("payoutCount").as[Int]
            payoutOffset <- c.downField("payoutOffset").as[Int]
            resolvedUtxos <- c.downField("resolvedUtxos").as[ResolvedUtxos]
        yield RolloutTx.Last(
          tx = tx,
          rolloutSpent = rolloutSpent,
          payoutCount = payoutCount,
          payoutOffset = payoutOffset,
          resolvedUtxos = resolvedUtxos
        )
    }

    given notLastEncoder: Encoder[RolloutTx.NotLast] = Encoder.instance { t =>
        Json.obj(
          "tx" -> t.tx.asJson,
          "rolloutSpent" -> t.rolloutSpent.asJson,
          "rolloutProduced" -> t.rolloutProduced.asJson,
          "payoutCount" -> t.payoutCount.asJson,
          "payoutOffset" -> t.payoutOffset.asJson,
          "resolvedUtxos" -> t.resolvedUtxos.asJson
        )
    }

    given notLastDecoder: Decoder[RolloutTx.NotLast] = Decoder.instance { c =>
        for
            tx <- c.downField("tx").as[Transaction]
            rolloutSpent <- c
                .downField("rolloutSpent")
                .as[RolloutUtxo]
            rolloutProduced <- c
                .downField("rolloutProduced")
                .as[RolloutUtxo]
            payoutCount <- c.downField("payoutCount").as[Int]
            payoutOffset <- c.downField("payoutOffset").as[Int]
            resolvedUtxos <- c.downField("resolvedUtxos").as[ResolvedUtxos]
        yield RolloutTx.NotLast(
          tx = tx,
          rolloutSpent = rolloutSpent,
          rolloutProduced = rolloutProduced,
          payoutCount = payoutCount,
          payoutOffset = payoutOffset,
          resolvedUtxos = resolvedUtxos
        )
    }

    given rolloutTxEncoder: Encoder[RolloutTx] = Encoder.instance {
        case t: RolloutTx.Last    => addKind(lastEncoder(t), "Last")
        case t: RolloutTx.NotLast => addKind(notLastEncoder(t), "NotLast")
    }

    given rolloutTxDecoder: Decoder[RolloutTx] = Decoder.instance { c =>
        c.downField("kind").as[String].flatMap {
            case "Last"    => c.as[RolloutTx.Last].widen
            case "NotLast" => c.as[RolloutTx.NotLast].widen
            case other =>
                Left(io.circe.DecodingFailure(s"unknown RolloutTx kind: $other", c.history))
        }
    }

    private def addKind(json: Json, kind: String): Json =
        json.deepMerge(Json.obj("kind" -> Json.fromString(kind)))
