package hydrozoa.node.server

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import hydrozoa.*
import hydrozoa.infra.serializeTxHex
import hydrozoa.l1.multisig.tx.PostDatedRefundTx
import scalus.cardano.ledger.Value
import sttp.tapir.Schema

// Types for NODE API

// Head initialization
type InitializationError = String

// Simple depositing
type DepositId = UtxoIdL1

case class DepositRequest(
    txId: TxId,
    txIx: TxIx,
    depositAmount: BigInt,
    deadline: Option[BigInt],
    address: AddressBechL2,
    datum: Option[Datum],
    refundAddress: AddressBechL1,
    refundDatum: Option[Datum]
)

case class DepositResponse(postDatedRefundTx: PostDatedRefundTx, depositId: DepositId) {
    override def toString: String =
        s"refundTx: ${serializeTxHex(postDatedRefundTx)}, deposit utxo: $depositId"
}

given depositResponseCodec: JsonValueCodec[DepositResponse] =
    JsonCodecMaker.make

given depositResponseSchema: Schema[DepositResponse] =
    Schema.derived[DepositResponse]

given utxoIdL1Schema: Schema[UtxoIdL1] =
    Schema.derived[UtxoIdL1]

given txIdSchema: Schema[TxId] =
    Schema.derived[TxId]

given txL1Schema: Schema[TxL1] =
    Schema.derived[TxL1]

given txIxSchema: Schema[TxIx] =
    Schema.derived[TxIx]

type DepositError = String
