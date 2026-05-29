package hydrozoa.multisig.persistence.codec

import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.InitializationTxEndTime
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.codecs.json.Codecs.{
    transactionDecoder,
    transactionEncoder,
    transactionOutputDecoder,
    transactionOutputEncoder,
    utxoDecoder,
    utxoEncoder
}
import hydrozoa.multisig.ledger.l1.token.CIP67.HeadTokenNames
import hydrozoa.multisig.ledger.l1.tx.InitializationTx
import hydrozoa.multisig.ledger.l1.utxo.{MultisigRegimeUtxo, MultisigTreasuryUtxo}
import hydrozoa.multisig.persistence.codec.FoundationCodecs.{
    resolvedUtxosDecoder,
    resolvedUtxosEncoder
}
import hydrozoa.multisig.persistence.codec.TreasuryCodec.{
    multisigTreasuryUtxoDecoder,
    multisigTreasuryUtxoEncoder
}
import hydrozoa.multisig.persistence.codec.UtxoWrapperCodecs.{
    multisigRegimeUtxoDecoder,
    multisigRegimeUtxoEncoder
}
import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json}
import scalus.cardano.ledger.{Transaction, TransactionOutput, Utxo}
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos

/** Persistence-layer JSON codec for [[InitializationTx]] — the constructor was made public
  * (was `private`) so the codec can reconstruct directly.
  *
  * `headTokenNames` is **derived** from `seedUtxo.input` so it isn't persisted; on decode we
  * reconstruct it via `HeadTokenNames(seedUtxo.input)`. `txLens` is similarly a default. The
  * persisted shape carries: `initializationTxEndTime`, `treasuryProduced`, `multisigRegimeProduced`,
  * `resolvedUtxos`, `tx`, `seedUtxo`, `additionalFundingUtxos`, `changeUtxos`.
  *
  * `additionalFundingUtxos: Utxos` round-trips as `List[Utxo]` (same pattern as `ResolvedUtxos`).
  * `changeUtxos: List[Output]` round-trips through the existing `TransactionOutput` CBOR-hex
  * codec.
  */
object InitializationTxCodec:

    given initializationTxEncoder(using CardanoNetwork.Section): Encoder[InitializationTx] =
        Encoder.instance { t =>
            val additionalAsList =
                t.additionalFundingUtxos.iterator.map { case (input, output) =>
                    Utxo(input, output)
                }.toList
            Json.obj(
              "initializationTxEndTime" -> t.initializationTxEndTime.asJson,
              "treasuryProduced" -> t.treasuryProduced.asJson,
              "multisigRegimeProduced" -> t.multisigRegimeProduced.asJson,
              "resolvedUtxos" -> t.resolvedUtxos.asJson,
              "tx" -> t.tx.asJson,
              "seedUtxo" -> t.seedUtxo.asJson,
              "additionalFundingUtxos" -> additionalAsList.asJson,
              "changeUtxos" -> t.changeUtxos.asJson
            )
        }

    given initializationTxDecoder(using CardanoNetwork.Section): Decoder[InitializationTx] =
        Decoder.instance { c =>
            for
                iet <- c.downField("initializationTxEndTime").as[InitializationTxEndTime]
                tp <- c.downField("treasuryProduced").as[MultisigTreasuryUtxo]
                mrp <- c.downField("multisigRegimeProduced").as[MultisigRegimeUtxo]
                ru <- c.downField("resolvedUtxos").as[ResolvedUtxos]
                tx <- c.downField("tx").as[Transaction]
                seed <- c.downField("seedUtxo").as[Utxo]
                additionalList <- c.downField("additionalFundingUtxos").as[List[Utxo]]
                changes <- c.downField("changeUtxos").as[List[TransactionOutput]]
            yield InitializationTx(
              initializationTxEndTime = iet,
              treasuryProduced = tp,
              multisigRegimeProduced = mrp,
              headTokenNames = HeadTokenNames(seed.input),
              resolvedUtxos = ru,
              tx = tx,
              seedUtxo = seed,
              additionalFundingUtxos = additionalList.iterator.map(u => u.input -> u.output).toMap,
              changeUtxos = changes
            )
        }
