package hydrozoa.multisig.persistence.codec

import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.{transactionInputDecoder, transactionInputEncoder}
import hydrozoa.lib.cardano.scalus.codecs.json.Codecs.{utxoDecoder, utxoEncoder}
import hydrozoa.multisig.ledger.block.BlockVersion
import hydrozoa.multisig.ledger.l1.utxo.{MultisigRegimeUtxo, RolloutUtxo}
import hydrozoa.multisig.persistence.codec.HydrozoaLocalCodecs.{regimeDatumDecoder, regimeDatumEncoder}
import hydrozoa.rulebased.ledger.l1.utxo.RuleBasedRegimeUtxo
import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json}
import scalus.cardano.ledger.TransactionInput

/** Small persistence-layer codecs for the Utxo-wrapper types that several EnrichedTx-wrapper codecs
  * share: [[RolloutUtxo]] (wraps `Utxo`), [[MultisigRegimeUtxo]] (a `TransactionInput` plus the
  * datum pinning `headParamsHash`), [[RuleBasedRegimeUtxo]] (wraps `TransactionInput`), and the
  * opaque-`Int` `BlockVersion.Major`. Each is small enough to live here rather than in one tiny
  * file per type.
  */
object UtxoWrapperCodecs:

    given rolloutUtxoEncoder: Encoder[RolloutUtxo] = utxoEncoder.contramap(_.utxo)
    given rolloutUtxoDecoder: Decoder[RolloutUtxo] = utxoDecoder.map(RolloutUtxo.apply)

    given multisigRegimeUtxoEncoder: Encoder[MultisigRegimeUtxo] = Encoder.instance { u =>
        Json.obj(
          "input" -> u.input.asJson,
          "datum" -> u.datum.asJson
        )
    }
    given multisigRegimeUtxoDecoder: Decoder[MultisigRegimeUtxo] = Decoder.instance { c =>
        for
            input <- c.downField("input").as[TransactionInput]
            datum <- c.downField("datum").as[MultisigRegimeUtxo.Datum]
        yield MultisigRegimeUtxo(input, datum)
    }

    given ruleBasedRegimeUtxoEncoder: Encoder[RuleBasedRegimeUtxo] =
        transactionInputEncoder.contramap(_.input)
    given ruleBasedRegimeUtxoDecoder: Decoder[RuleBasedRegimeUtxo] =
        transactionInputDecoder.map(RuleBasedRegimeUtxo.apply)

    given blockVersionMajorEncoder: Encoder[BlockVersion.Major] =
        Encoder.encodeInt.contramap(m => m: Int)
    given blockVersionMajorDecoder: Decoder[BlockVersion.Major] =
        Decoder.decodeInt.emap(i =>
            if i >= 0 then Right(BlockVersion.Major(i))
            else Left(s"BlockVersion.Major must be non-negative; got $i")
        )
