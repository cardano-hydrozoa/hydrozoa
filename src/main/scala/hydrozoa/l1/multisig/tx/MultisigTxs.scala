package hydrozoa.l1.multisig.tx

import hydrozoa.TxL1

sealed trait MultisigTxTag
sealed trait InitializationTxTag extends MultisigTxTag
sealed trait DepositTxTag extends MultisigTxTag
sealed trait PostDatedRefundTxTag extends MultisigTxTag
sealed trait SettlementTxTag extends MultisigTxTag
sealed trait FinalizationTxTag extends MultisigTxTag

opaque type MultisigTx[+T <: MultisigTxTag] = TxL1

type InitializationTx = MultisigTx[InitializationTxTag]
type DepositTx = MultisigTx[DepositTxTag] // it's not a multisig tx
type PostDatedRefundTx = MultisigTx[PostDatedRefundTxTag]
type SettlementTx = MultisigTx[SettlementTxTag]
type FinalizationTx = MultisigTx[FinalizationTxTag]

object MultisigTx:
    def apply[T <: MultisigTxTag](tx: TxL1): MultisigTx[T] = tx

extension [T <: MultisigTxTag](tx: MultisigTx[T]) {
    def toL1Tx: TxL1 = tx
}
