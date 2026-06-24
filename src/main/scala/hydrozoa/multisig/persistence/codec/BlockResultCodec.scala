package hydrozoa.multisig.persistence.codec

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.ledger.block.BlockResult
import hydrozoa.multisig.ledger.remote.RemoteL2LedgerCodecs.{payoutObligationDecoder, payoutObligationEncoder}
import hydrozoa.multisig.persistence.codec.DepositUtxoCodec.given
import hydrozoa.multisig.persistence.codec.RefundTxCodec.{postDatedDecoder, postDatedEncoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/** Persistence-layer JSON codec for [[BlockResult.Persisted]] — the value stored at
  * `StoreKey.BlockResult`: JointLedger's per-block deltas. The `brief` is **not** stored here (it
  * already lives in the `Block` journal and is rehydrated from there at recovery — see
  * [[BlockResult.persisted]]). The slow side rebuilds its `pending` map from these on restart (§6).
  *
  * Derived structurally from the field leaf codecs:
  *   - `EvacuationDiff` — the diff codec on `EvacuationMap`'s companion;
  *   - `Payout.Obligation` — `RemoteL2LedgerCodecs`;
  *   - `RefundTx.PostDated` — [[RefundTxCodec]];
  *   - `DepositUtxo` — [[DepositUtxoCodec]];
  *   - `FallbackTxStartTime` — the `QuantizedInstant` codec on its companion (implicit scope).
  */
object BlockResultCodec:

    given blockResultEncoder(using CardanoNetwork.Section): Encoder[BlockResult.Persisted] =
        deriveEncoder[BlockResult.Persisted]

    given blockResultDecoder(using CardanoNetwork.Section): Decoder[BlockResult.Persisted] =
        deriveDecoder[BlockResult.Persisted]
