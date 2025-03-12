package hydrozoa.l1.multisig.tx.settlement

import hydrozoa.l1.multisig.tx.MultisigTxs.{DepositTx, PostDatedRefundTx, SettlementTx}
import hydrozoa.{L1, OutputRef, ParticipantSecretKey, TxIx}

trait SettlementTxBuilder {
    def mkSettlementTxDraft(
        recipe: SettlementRecipe
    ): Either[String, SettlementTx]
}

case class SettlementRecipe(
    deposits: Seq[OutputRef[L1]],
    majorVersion: Int
)
