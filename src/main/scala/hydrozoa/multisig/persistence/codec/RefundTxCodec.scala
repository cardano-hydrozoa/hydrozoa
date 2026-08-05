package hydrozoa.multisig.persistence.codec

import cats.syntax.functor.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedInstant, quantizedInstantDecoder, quantizedInstantEncoder}
import hydrozoa.lib.cardano.scalus.codecs.json.Codecs.{transactionDecoder, transactionEncoder}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.l1.tx.RefundTx
import hydrozoa.multisig.ledger.l2.Destination
import hydrozoa.multisig.persistence.codec.DestinationCodec.given
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}
import scala.annotation.unused
import scalus.cardano.ledger.Transaction
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos

/** Persistence-layer JSON codec for [[RefundTx.PostDated]] — the simplest EnrichedTx wrapper (4
  * fields, no `MultisigTreasuryUtxo` chain).
  *
  * Reuses:
  *   - `Transaction` CBOR-hex codec from `lib/cardano/scalus/codecs/json/Codecs` (hoisted in #13);
  *   - `QuantizedInstant` codec from `lib/cardano/scalus/QuantizedTime` (the decoder needs
  *     `CardanoNetwork.Section`, threaded through from this codec);
  *   - [[DestinationCodec]] — fresh CBOR-hex round-trip (the codec on the type itself is
  *     asymmetric);
  *   - the `RequestId.i64` codec (packed i64 form) — shadows the companion object's default object
  *     shape so the on-disk `requestId` field is a plain number, matching the SugarRush `u64`
  *     expectation.
  *
  * `txLens` is derived from the case-class body, not stored in JSON. `resolvedUtxos` isn't
  * persisted either — on decode we restore an empty `ResolvedUtxos`, since the ledger context isn't
  * available at read time.
  */
object RefundTxCodec:

    given postDatedEncoder(using @unused ev: CardanoNetwork.Section): Encoder[RefundTx.PostDated] =
        import RequestId.i64.given
        Encoder.instance { pd =>
            Json.obj(
              "tx" -> pd.tx.asJson,
              "refundStart" -> pd.refundStart.asJson,
              "refundDestination" -> pd.refundDestination.asJson,
              "requestId" -> pd.requestId.asJson
            )
        }

    given postDatedDecoder(using CardanoNetwork.Section): Decoder[RefundTx.PostDated] =
        import RequestId.i64.given
        Decoder
            .forProduct4[RefundTx.PostDated, Transaction, QuantizedInstant, Destination, RequestId](
              "tx",
              "refundStart",
              "refundDestination",
              "requestId"
            )((tx, refundStart, refundDestination, requestId) =>
                RefundTx.PostDated(
                  tx = tx,
                  refundStart = refundStart,
                  refundDestination = refundDestination,
                  requestId = requestId,
                  resolvedUtxos = ResolvedUtxos.empty
                )
            )

    given refundTxEncoder(using CardanoNetwork.Section): Encoder[RefundTx] = Encoder.instance {
        case pd: RefundTx.PostDated => postDatedEncoder(using summon).apply(pd)
    }

    given refundTxDecoder(using CardanoNetwork.Section): Decoder[RefundTx] =
        postDatedDecoder.widen
