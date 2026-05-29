package hydrozoa.multisig.persistence.codec

import hydrozoa.lib.cardano.scalus.codecs.json.Codecs.{utxoDecoder, utxoEncoder}
import io.circe.{Decoder, Encoder}
import scalus.cardano.ledger.Utxo
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos

/** Persistence-layer JSON codecs for shared *Scalus* / framework types that several Tx-wrapper
  * codecs reference but that aren't part of CIP-116. Lives in the persistence/codec package because
  * each instance is shaped for **on-disk storage round-trip**; if other consumers (wire, server,
  * L2) need a different shape, they keep their own instance.
  *
  * The codecs here:
  *   - keep persistence isolated from the wire-codec set's quirks (e.g. wire codecs may carry extra
  *     metadata that persistence doesn't need to round-trip);
  *   - rely on the Scalus CBOR-hex codecs in `lib/cardano/scalus/codecs/json/Codecs` for the leaf
  *     types (Transaction, TransactionOutput, Utxo, …);
  *   - reuse CIP-116 leaves via `HydrozoaLocalCodecs` and `lib/cardano/cip116/JsonCodecs`.
  */
object FoundationCodecs:

    /** [[ResolvedUtxos]] (Scalus `txbuilder` wrapper around
      * `Map[TransactionInput, TransactionOutput]`) persisted as a JSON array of [[Utxo]]s —
      * order-independent on decode (`ResolvedUtxos.apply` takes the underlying map). Going through
      * `List[Utxo]` avoids needing a `KeyEncoder` for `TransactionInput` at the persistence layer
      * (the existing `txhash#idx` KeyEncoder in `lib/cardano/scalus/codecs/json` is documented as
      * **not** CIP-116-compliant, so we keep persistence on the value-encoder side).
      */
    given resolvedUtxosEncoder: Encoder[ResolvedUtxos] =
        Encoder
            .encodeList[Utxo]
            .contramap(rs =>
                rs.utxos.iterator.map { case (input, output) => Utxo(input, output) }.toList
            )

    given resolvedUtxosDecoder: Decoder[ResolvedUtxos] =
        Decoder
            .decodeList[Utxo]
            .map(list => ResolvedUtxos(list.iterator.map(u => u.input -> u.output).toMap))
