package hydrozoa.l1.multisig.tx

import hydrozoa.{L1, Tx, TxL1}
import scalus.cardano.ledger.Transaction

// TODO: revise, add new multisig txs
sealed trait MultisigTxTag
sealed trait InitializationTxTag extends MultisigTxTag
sealed trait PostDatedRefundTxTag extends MultisigTxTag
sealed trait SettlementTxTag extends MultisigTxTag
sealed trait FinalizationTxTag extends MultisigTxTag
sealed trait DeinitTxTag extends MultisigTxTag

/* FIXME: This is refactored (Peter, 2025-08-16) from using type aliases throughout.
The problem with using type aliases here is that the tags are only phantom types, and thus get erased at runtime,
and thus we cannot use them as pattern matches.

If we do it this way, we run into the issue that we (A) have t
 */
object MultisigTx:
    opaque type MultisigTx[+T <: MultisigTxTag] = TxL1
    def apply[T <: MultisigTxTag](tx: Transaction): MultisigTx[T] = Tx[L1](tx)
    given [T <: MultisigTxTag]: Conversion[MultisigTx[T], TxL1] = identity
    extension [T <: MultisigTxTag](tx: MultisigTx[T]) def untagged: TxL1 = identity(tx)

type MultisigTx[+T <: MultisigTxTag] = MultisigTx.MultisigTx[T]
type InitTx = MultisigTx[InitializationTxTag]
type PostDatedRefundTx = (MultisigTx[PostDatedRefundTxTag])
type SettlementTx = (MultisigTx[SettlementTxTag])
type FinalizationTx = MultisigTx[FinalizationTxTag]
type DeinitTx = MultisigTx[DeinitTxTag]
