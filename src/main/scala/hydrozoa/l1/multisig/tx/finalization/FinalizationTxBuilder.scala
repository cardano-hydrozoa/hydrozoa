package hydrozoa.l1.multisig.tx.finalization

import hydrozoa.l1.multisig.tx.FinalizationTx

trait FinalizationTxBuilder {
    def buildFinalizationTxDraft(
        recipe: FinalizationRecipe
    ): Either[String, FinalizationTx]
}

case class FinalizationRecipe(
    majorVersion: Int
)
