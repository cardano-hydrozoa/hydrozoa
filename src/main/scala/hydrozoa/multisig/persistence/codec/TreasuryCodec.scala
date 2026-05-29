package hydrozoa.multisig.persistence.codec

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.{
    assetNameValueDecoder,
    assetNameValueEncoder,
    shelleyAddressDecoder,
    shelleyAddressEncoder,
    transactionInputDecoder,
    transactionInputEncoder,
    valueDecoder,
    valueEncoder
}
import hydrozoa.multisig.ledger.l1.utxo.MultisigTreasuryUtxo
import hydrozoa.multisig.persistence.codec.HydrozoaLocalCodecs.{
    datumDecoder,
    datumEncoder,
    equityDecoder,
    equityEncoder
}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/** Persistence-layer JSON codec for [[MultisigTreasuryUtxo]] — the slow-side treasury snapshot
  * stored in the `Treasury` CF at every own hard-ack stack-close.
  *
  * Every field is covered by an existing codec set: `AssetName` / `TransactionInput` /
  * `ShelleyAddress` / `Value` from CIP-116; `Datum` / `Equity` from [[HydrozoaLocalCodecs]]. The
  * constructor is public, so Circe's semiauto derivation suffices.
  */
object TreasuryCodec:

    given multisigTreasuryUtxoEncoder(using
        CardanoNetwork.Section
    ): Encoder[MultisigTreasuryUtxo] = deriveEncoder[MultisigTreasuryUtxo]

    given multisigTreasuryUtxoDecoder(using
        CardanoNetwork.Section
    ): Decoder[MultisigTreasuryUtxo] = deriveDecoder[MultisigTreasuryUtxo]
