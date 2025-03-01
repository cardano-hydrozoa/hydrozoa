package hydrozoa.l1.multisig.tx.settlement

import hydrozoa.l1.multisig.tx.MultisigTxs.{DepositTx, PostDatedRefundTx, SettlementTx}
import hydrozoa.node.server.AwaitingDeposit
import hydrozoa.{ParticipantSecretKey, TxIx}

trait SettlementTxBuilder {
    def mkSettlement(
        recipe: SettlementRecipe
    ): Either[String, SettlementTx]
}

case class SettlementRecipe(
    deposits: Set[AwaitingDeposit],
    majorVersion: Int
)
