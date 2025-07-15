package hydrozoa.l1.rulebased.tx.deinit

import hydrozoa.*

trait DeinitTxBuilder {

    /** @param recipe
      * @return
      */
    def buildDeinitTxDraft(recipe: DeinitTxRecipe): Either[String, TxL1]
}

case class DeinitTxRecipe(
)
