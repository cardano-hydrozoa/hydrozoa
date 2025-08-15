package hydrozoa.node.server

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import hydrozoa.*
import hydrozoa.infra.serializeTxHex
import hydrozoa.l1.multisig.tx.PostDatedRefundTx
import scalus.builtin.Data
import scalus.cardano.ledger.TransactionHash
import sttp.tapir.Schema

// Types for NODE API

// Head initialization
type InitializationError = String

// Simple depositing
type DepositId = UtxoId[L1]

case class DepositRequest(
    txId: TransactionHash,
    txIx: TxIx,
    depositAmount: BigInt,
    /** A non-negative Posix time that indicates the time by which the user wants to have their
      * deposit absorbed on the L2 or have it refunded back on L1. See the comment for the `deposit`
      * function in `Node.scala`.
      */
    deadline: BigInt,
    address: AddressL2,
    /** Represents an optional inline datum */
    datum: Option[Data],
    refundAddress: AddressL1,
    /** Represents an optional inline datum */
    refundDatum: Option[Data]
)

case class DepositResponse(postDatedRefundTx: PostDatedRefundTx, depositId: DepositId) {
    override def toString: String =
        s"refundTx: ${serializeTxHex(postDatedRefundTx)}, deposit utxo: $depositId"
}

given depositResponseCodec: JsonValueCodec[DepositResponse] =
    ??? // FIXME: JsonCodecMaker.make

given depositResponseSchema: Schema[DepositResponse] =
    Schema.derived[DepositResponse]

given utxoIdL1Schema: Schema[UtxoIdL1] =
    Schema.binary[UtxoIdL1]

given txIdSchema: Schema[TransactionHash] =
    Schema.binary[TransactionHash]

given txL1Schema: Schema[TxL1] =
    Schema.binary[TxL1]

type DepositError = String
