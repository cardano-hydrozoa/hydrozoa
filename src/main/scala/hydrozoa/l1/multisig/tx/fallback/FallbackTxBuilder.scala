package hydrozoa.l1.multisig.tx.fallback

import hydrozoa.{TxIx, TxL1}

trait DepositTxBuilder {

    /** @param recipe
      * @return
      */
    def buildFallbackTxDraft(recipe: FallbackTxRecipe): Either[String, (TxL1, TxIx)]
}

case class FallbackTxRecipe(
)
