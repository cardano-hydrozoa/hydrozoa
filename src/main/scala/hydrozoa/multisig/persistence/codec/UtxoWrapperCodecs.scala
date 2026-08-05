package hydrozoa.multisig.persistence.codec

import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.{transactionInputDecoder, transactionInputEncoder}
import hydrozoa.lib.cardano.scalus.codecs.json.Codecs.{utxoDecoder, utxoEncoder}
import hydrozoa.multisig.ledger.block.BlockVersion
import hydrozoa.multisig.ledger.l1.utxo.{MultisigRegimeUtxo, RolloutUtxo}
import hydrozoa.rulebased.ledger.l1.utxo.RuleBasedRegimeUtxo
import io.circe.{Decoder, Encoder}

/** Small persistence-layer codecs for the Utxo-wrapper types that several EnrichedTx-wrapper codecs
  * share: [[RolloutUtxo]] (wraps `Utxo`), [[MultisigRegimeUtxo]] and [[RuleBasedRegimeUtxo]] (wrap
  * `TransactionInput`), and the opaque-`Int` `BlockVersion.Major`. Each is a one-liner so they live
  * together rather than one tiny file per type.
  */
object UtxoWrapperCodecs:

    given rolloutUtxoEncoder: Encoder[RolloutUtxo] = utxoEncoder.contramap(_.utxo)
    given rolloutUtxoDecoder: Decoder[RolloutUtxo] = utxoDecoder.map(RolloutUtxo.apply)

    given multisigRegimeUtxoEncoder: Encoder[MultisigRegimeUtxo] =
        transactionInputEncoder.contramap(_.input)
    given multisigRegimeUtxoDecoder: Decoder[MultisigRegimeUtxo] =
        transactionInputDecoder.map(MultisigRegimeUtxo.apply)

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
