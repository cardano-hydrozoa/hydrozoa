package hydrozoa.l1.multisig.tx.refund

import hydrozoa.l1.multisig.tx.MultisigTxs.{DepositTx, PostDatedRefundTx}
import hydrozoa.{ParticipantSecretKey, TxIx}

trait RefundTxBuilder {
    def mkPostDatedRefundTxDraft(
        recipe: PostDatedRefundRecipe
    ): Either[String, PostDatedRefundTx]
}

case class PostDatedRefundRecipe(
    depositTx: DepositTx,
    txIx: TxIx
)
