package hydrozoa.node.server

import hydrozoa.*
import hydrozoa.infra.serializeTx

// Types for NODE API

// Head initialization
type InitializeError = String

// Simple depositing
type DepositId = (TxId, TxIx)

case class DepositRequest(
    txId: TxId,
    txIx: TxIx,
    address: AddressBechL2,
    datum: Datum,
    refundAddress: AddressBechL1,
    refundDatum: Datum
)

case class DepositResponse(postDatedRefundTx: L1Tx, depositId: DepositId) {
    // FIXME: use JSON
    override def toString: String =
        s"refundTx: ${serializeTx(postDatedRefundTx)}, deposit utxo: ${depositId}"
}

type DepositError = String
