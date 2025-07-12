package hydrozoa.l1.multisig.tx.refund

import hydrozoa.{TxIx, TxL1}
import hydrozoa.l1.multisig.tx.PostDatedRefundTx
import scalus.cardano.address.Network
import scalus.cardano.ledger.Transaction

trait RefundTxBuilder {
    def mkPostDatedRefundTxDraft(
        recipe: PostDatedRefundRecipe
    ): Either[String, PostDatedRefundTx]
}

case class PostDatedRefundRecipe(
    // TODO: newtype this transaction?
    depositTx: Transaction,
    txIx: TxIx,
    network: Network
)
