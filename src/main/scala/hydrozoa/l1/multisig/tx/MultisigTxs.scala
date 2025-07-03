package hydrozoa.l1.multisig.tx

import hydrozoa.TxL1

// TODO: revise, add new multisig txs
sealed trait MultisigTxTag
sealed trait InitializationTxTag extends MultisigTxTag
sealed trait PostDatedRefundTxTag extends MultisigTxTag
sealed trait SettlementTxTag extends MultisigTxTag
sealed trait FinalizationTxTag extends MultisigTxTag

type MultisigTx[+T <: MultisigTxTag] = TxL1

type InitTx = MultisigTx[InitializationTxTag]
type PostDatedRefundTx = MultisigTx[PostDatedRefundTxTag]
type SettlementTx = MultisigTx[SettlementTxTag]
type FinalizationTx = MultisigTx[FinalizationTxTag]

object MultisigTx:
    inline def apply[T <: MultisigTxTag](tx: TxL1): MultisigTx[T] = tx

extension [T <: MultisigTxTag](tx: MultisigTx[T]) {
    def toL1Tx: TxL1 = tx
}
