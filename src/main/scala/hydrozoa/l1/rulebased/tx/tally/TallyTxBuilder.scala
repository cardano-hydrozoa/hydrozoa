package hydrozoa.l1.rulebased.tx.tally

import hydrozoa.{TxIx, TxL1}

trait TallyTxBuilder {

    /** @param recipe
      * @return
      */
    def buildTallyTxDraft(recipe: TallyTxRecipe): Either[String, (TxL1, TxIx)]
}

case class TallyTxRecipe(
)
