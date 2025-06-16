package hydrozoa.l1.rulebased.tx.dispute.vote

import hydrozoa.{TxIx, TxL1}

trait VoteTxBuilder {

    /** @param recipe
      * @return
      */
    def buildVoteTxDraft(recipe: VoteTxRecipe): Either[String, (TxL1, TxIx)]
}

case class VoteTxRecipe(
)
