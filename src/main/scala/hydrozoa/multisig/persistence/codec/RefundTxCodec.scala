package hydrozoa.multisig.persistence.codec

import cats.syntax.functor.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.QuantizedTime.{quantizedInstantDecoder, quantizedInstantEncoder}
import hydrozoa.lib.cardano.scalus.codecs.json.Codecs.{transactionDecoder, transactionEncoder}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.l1.tx.RefundTx
import hydrozoa.multisig.persistence.codec.DestinationCodec.given
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/** Persistence-layer JSON codec for [[RefundTx.PostDated]] — the simplest Tx wrapper (4 fields, no
  * `MultisigTreasuryUtxo` chain).
  *
  * Reuses:
  *   - `Transaction` CBOR-hex codec from `lib/cardano/scalus/codecs/json/Codecs` (hoisted in #13);
  *   - `QuantizedInstant` codec from `lib/cardano/scalus/QuantizedTime` (the decoder needs
  *     `CardanoNetwork.Section`, threaded through from this codec);
  *   - [[DestinationCodec]] — fresh CBOR-hex round-trip (the codec on the type itself is
  *     asymmetric);
  *   - the `RequestId.i64` codec (packed i64 form) — shadows the companion object's default
  *     object shape so the on-disk `requestId` field is a plain number, matching the
  *     SugarRush `u64` expectation.
  *
  * `txLens` and `resolvedUtxos = ResolvedUtxos.empty` are derived from the case-class body, not
  * stored in JSON; on decode the case-class default values restore them.
  */
object RefundTxCodec:

    given postDatedEncoder(using CardanoNetwork.Section): Encoder[RefundTx.PostDated] =
        import RequestId.i64.given
        deriveEncoder[RefundTx.PostDated]

    given postDatedDecoder(using CardanoNetwork.Section): Decoder[RefundTx.PostDated] =
        import RequestId.i64.given
        deriveDecoder[RefundTx.PostDated]

    given refundTxEncoder(using CardanoNetwork.Section): Encoder[RefundTx] = Encoder.instance {
        case pd: RefundTx.PostDated => postDatedEncoder(using summon).apply(pd)
    }

    given refundTxDecoder(using CardanoNetwork.Section): Decoder[RefundTx] =
        postDatedDecoder.widen
