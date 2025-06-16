package hydrozoa.l1.rulebased.tx.dispute.resolve

import hydrozoa.{TxIx, TxL1}

trait ResolveTxBuilder {

    /** @param recipe
      * @return
      */
    def buildResolveTxDraft(recipe: ResolveTxRecipe): Either[String, (TxL1, TxIx)]
}

case class ResolveTxRecipe(
)
