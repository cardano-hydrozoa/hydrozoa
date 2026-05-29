package hydrozoa.multisig.persistence.codec

import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.{
    byteStringDecoder,
    byteStringEncoder,
    coinDecoder,
    coinEncoder
}
import hydrozoa.multisig.ledger.l1.utxo.{Equity, MultisigTreasuryUtxo}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/** Persistence-layer JSON codecs for Hydrozoa-local wrapper types — the small case classes that
  * compose into the larger P2 records ([[MultisigTreasuryUtxo]], `StackEffects.HardConfirmed`).
  *
  * These instances are **separate from** the user-facing API codecs (`multisig/server/JsonCodecs`),
  * the inter-peer wire codecs (`multisig/consensus/transport/Codecs`), and the L2 RPC codecs
  * (`multisig/ledger/remote/RemoteL2LedgerCodecs`) — they're a dedicated persistence set per the
  * design decision (2026-05-28). CIP-116 leaves and the Scalus CBOR-hex codecs are reused as-is.
  *
  * `KzgCommitment` is a `type` alias for `scalus.uplc.builtin.ByteString` and is therefore
  * covered transparently by CIP-116's `byteStringEncoder` / `byteStringDecoder` — no separate
  * instance is needed here.
  */
object HydrozoaLocalCodecs:

    /** Codec for [[MultisigTreasuryUtxo.Datum]] = `(commit: KzgCommitment, versionMajor: BigInt)`.
      * `commit` rides through the CIP-116 ByteString codec (hex string); `versionMajor` rides
      * through Circe's built-in BigInt support (JSON number string).
      */
    given datumEncoder: Encoder[MultisigTreasuryUtxo.Datum] =
        deriveEncoder[MultisigTreasuryUtxo.Datum]

    given datumDecoder: Decoder[MultisigTreasuryUtxo.Datum] =
        deriveDecoder[MultisigTreasuryUtxo.Datum]

    /** Codec for [[Equity]]. The wrapper has a private constructor + validating smart constructor
      * `apply(amount: Coin): Option[Equity]` (positivity check); on decode we go `Coin → Equity`
      * via `apply` and surface a typed `DecodingFailure` on negative input.
      */
    given equityEncoder: Encoder[Equity] =
        coinEncoder.contramap(_.coin)

    given equityDecoder: Decoder[Equity] =
        coinDecoder.emap(coin =>
            Equity(coin).toRight(s"Equity must be non-negative; got coin=${coin.value}")
        )
