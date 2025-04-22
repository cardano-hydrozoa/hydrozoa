package hydrozoa.node.server

import hydrozoa.*
import hydrozoa.infra.serializeTxHex
import hydrozoa.l1.multisig.tx.PostDatedRefundTx

// Types for NODE API

// Head initialization
type InitializeError = String

// Simple depositing
type DepositId = UtxoIdL1

case class DepositRequest(
    txId: TxId,
    txIx: TxIx,
    deadline: Option[BigInt],
    address: AddressBechL2,
    datum: Option[Datum],
    refundAddress: AddressBechL1,
    refundDatum: Option[Datum]
)


case class DepositResponse(postDatedRefundTx: PostDatedRefundTx, depositId: DepositId) {
    // TODO: use JSON
    override def toString: String =
        s"refundTx: ${serializeTxHex(postDatedRefundTx)}, deposit utxo: $depositId"
}

type DepositError = String
