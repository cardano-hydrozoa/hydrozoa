package hydrozoa.l1.multisig.tx.refund

import hydrozoa.TxIx
import hydrozoa.l1.multisig.tx.{DepositTxTag, MultisigTx, PostDatedRefundTx}

trait RefundTxBuilder {
    def mkPostDatedRefundTxDraft(
        recipe: PostDatedRefundRecipe
    ): Either[String, PostDatedRefundTx]
}

case class PostDatedRefundRecipe(
    depositTx: MultisigTx[DepositTxTag], // =:= DepositTx, breaks extensions somehow
    txIx: TxIx
)
