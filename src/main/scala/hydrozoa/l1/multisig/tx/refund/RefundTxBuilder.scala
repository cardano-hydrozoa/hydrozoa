package hydrozoa.l1.multisig.tx.refund

import hydrozoa.{TxIx, TxL1}
import hydrozoa.l1.multisig.tx.PostDatedRefundTx

trait RefundTxBuilder {
    def mkPostDatedRefundTxDraft(
        recipe: PostDatedRefundRecipe
    ): Either[String, PostDatedRefundTx]
}

case class PostDatedRefundRecipe(
    depositTx: TxL1,
    txIx: TxIx
)
