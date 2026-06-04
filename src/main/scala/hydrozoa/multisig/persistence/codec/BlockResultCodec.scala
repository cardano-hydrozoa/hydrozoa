package hydrozoa.multisig.persistence.codec

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.consensus.transport.Codecs.given
import hydrozoa.multisig.ledger.block.BlockResult
import hydrozoa.multisig.ledger.remote.RemoteL2LedgerCodecs.{payoutObligationDecoder, payoutObligationEncoder}
import hydrozoa.multisig.persistence.codec.DepositUtxoCodec.given
import hydrozoa.multisig.persistence.codec.RefundTxCodec.{postDatedDecoder, postDatedEncoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/** Persistence-layer JSON codec for [[BlockResult]] — the value stored at `StoreKey.BlockResult`,
  * JointLedger's per-block output that the slow side rebuilds its `pending` map from on restart
  * (§6).
  *
  * Derived structurally, composed from the leaf codecs the fields already have:
  *   - `BlockBrief.Next` — the wire codec (`consensus.transport.Codecs`);
  *   - `EvacuationDiff` — the diff codec on `EvacuationMap`'s companion;
  *   - `Payout.Obligation` — `RemoteL2LedgerCodecs`;
  *   - `RefundTx.PostDated` — [[RefundTxCodec]];
  *   - `DepositUtxo` — [[DepositUtxoCodec]];
  *   - `FallbackTxStartTime` — the `QuantizedInstant` codec on its companion (implicit scope).
  */
object BlockResultCodec:

    given blockResultEncoder(using CardanoNetwork.Section): Encoder[BlockResult] =
        deriveEncoder[BlockResult]

    given blockResultDecoder(using CardanoNetwork.Section): Decoder[BlockResult] =
        deriveDecoder[BlockResult]
