package hydrozoa.l1.multisig.tx.refund

import hydrozoa.l1.multisig.tx.MultisigTxs.{DepositTx, PostDatedRefundTx}
import hydrozoa.{ParticipantSecretKey, TxIx}

trait RefundTxBuilder {
    def mkPostDatedRefund(
        recipe: PostDatedRefundRecipe,
        ownKey: ParticipantSecretKey // this is temporal - used to fill slots for witnesses
    ): Either[String, PostDatedRefundTx]
}

case class PostDatedRefundRecipe(
    depositTx: DepositTx,
    txIx: TxIx
)
