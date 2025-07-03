package hydrozoa.l1.rulebased.tx.withdraw

import hydrozoa.{TxIx, TxL1}

trait WithdrawTxBuilder {

    /** @param recipe
      * @return
      */
    def buildWithdrawTxDraft(recipe: WithdrawTxRecipe): Either[String, (TxL1, TxIx)]
}

case class WithdrawTxRecipe(
)
    