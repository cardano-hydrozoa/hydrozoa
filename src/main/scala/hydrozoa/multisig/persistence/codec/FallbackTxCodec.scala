package hydrozoa.multisig.persistence.codec

import cats.data.NonEmptyList
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.FallbackTxStartTime
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.codecs.json.Codecs.{transactionDecoder, transactionEncoder, utxoDecoder, utxoEncoder}
import hydrozoa.multisig.ledger.l1.tx.FallbackTx
import hydrozoa.multisig.ledger.l1.utxo.{MultisigRegimeUtxo, MultisigTreasuryUtxo}
import hydrozoa.multisig.persistence.codec.FoundationCodecs.{resolvedUtxosDecoder, resolvedUtxosEncoder}
import hydrozoa.multisig.persistence.codec.RuleBasedCodecs.{ruleBasedTreasuryUtxoDecoder, ruleBasedTreasuryUtxoEncoder}
import hydrozoa.multisig.persistence.codec.TreasuryCodec.{multisigTreasuryUtxoDecoder, multisigTreasuryUtxoEncoder}
import hydrozoa.multisig.persistence.codec.UtxoWrapperCodecs.{multisigRegimeUtxoDecoder, multisigRegimeUtxoEncoder}
import hydrozoa.rulebased.ledger.l1.utxo.RuleBasedTreasuryUtxo
import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json}
import scalus.cardano.ledger.{Transaction, Utxo}
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos

/** Persistence-layer JSON codec for [[FallbackTx]] — single case class but spans both multisig and
  * rule-based domains. `treasuryProduced` is a [[RuleBasedTreasuryUtxo]] (rule-based side); it's
  * persisted in the multisig store as part of the multisig regime's fallback effect data, per the
  * design note ("those post-dated effects, not immediate, they are needed to actually fall back
  * into the rule-based regime").
  *
  * `peerVoteUtxosProduced` is `cats.data.NonEmptyList[Utxo]`; round-trips as a JSON array using
  * cats's `NonEmptyList` Circe codec (which Circe provides automatically once `Utxo` has one).
  */
object FallbackTxCodec:

    private given nonEmptyListUtxoEncoder: Encoder[NonEmptyList[Utxo]] =
        Encoder.encodeList[Utxo].contramap(_.toList)

    private given nonEmptyListUtxoDecoder: Decoder[NonEmptyList[Utxo]] =
        Decoder
            .decodeList[Utxo]
            .emap(list =>
                NonEmptyList
                    .fromList(list)
                    .toRight("expected non-empty list of Utxo for FallbackTx")
            )

    given fallbackTxEncoder(using CardanoNetwork.Section): Encoder[FallbackTx] =
        Encoder.instance { t =>
            Json.obj(
              "fallbackTxStartTime" -> t.fallbackTxStartTime.asJson,
              "treasurySpent" -> t.treasurySpent.asJson,
              "treasuryProduced" -> t.treasuryProduced.asJson,
              "multisigRegimeUtxoSpent" -> t.multisigRegimeUtxoSpent.asJson,
              "tx" -> t.tx.asJson,
              "resolvedUtxos" -> t.resolvedUtxos.asJson,
              "peerBallotBoxesProduced" -> t.peerBallotBoxesProduced.asJson
            )
        }

    given fallbackTxDecoder(using CardanoNetwork.Section): Decoder[FallbackTx] =
        Decoder.instance { c =>
            for
                fst <- c.downField("fallbackTxStartTime").as[FallbackTxStartTime]
                ts <- c.downField("treasurySpent").as[MultisigTreasuryUtxo]
                tp <- c.downField("treasuryProduced").as[RuleBasedTreasuryUtxo]
                mrus <- c.downField("multisigRegimeUtxoSpent").as[MultisigRegimeUtxo]
                tx <- c.downField("tx").as[Transaction]
                ru <- c.downField("resolvedUtxos").as[ResolvedUtxos]
                pbbs <- c.downField("peerBallotBoxesProduced").as[NonEmptyList[Utxo]]
            yield FallbackTx(
              fallbackTxStartTime = fst,
              treasurySpent = ts,
              treasuryProduced = tp,
              multisigRegimeUtxoSpent = mrus,
              tx = tx,
              resolvedUtxos = ru,
              peerBallotBoxesProduced = pbbs
            )
        }
