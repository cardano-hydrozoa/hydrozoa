package hydrozoa.node.server

import hydrozoa.*
import hydrozoa.infra.serializeTxHex

// Types for NODE API

// Head initialization
type InitializeError = String

// Simple depositing
type DepositId = (TxId, TxIx)

case class DepositRequest(
    txId: TxId,
    txIx: TxIx,
    deadline: BigInt,
    address: AddressBechL2,
    datum: Option[Datum],
    refundAddress: AddressBechL1,
    refundDatum: Option[Datum]
)

case class DepositResponse(postDatedRefundTx: L1Tx, depositId: DepositId) {
    // TODO: use JSON
    override def toString: String =
        s"refundTx: ${serializeTxHex(postDatedRefundTx)}, deposit utxo: ${depositId}"
}

type DepositError = String
