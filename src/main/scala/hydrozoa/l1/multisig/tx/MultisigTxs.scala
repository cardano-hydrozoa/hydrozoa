package hydrozoa.l1.multisig.tx

import hydrozoa.TxL1

sealed trait MultisigTxTag
sealed trait InitializationTxTag extends MultisigTxTag
sealed trait DepositTxTag extends MultisigTxTag
sealed trait PostDatedRefundTxTag extends MultisigTxTag
sealed trait SettlementTxTag extends MultisigTxTag
sealed trait FinalizationTxTag extends MultisigTxTag

type MultisigTx[T <: MultisigTxTag] = TxL1

type InitializationTx = MultisigTx[InitializationTxTag]
type DepositTx = MultisigTx[DepositTxTag]
type PostDatedRefundTx = MultisigTx[PostDatedRefundTxTag]
type SettlementTx = MultisigTx[SettlementTxTag]
type FinalizationTx = MultisigTx[FinalizationTxTag]
