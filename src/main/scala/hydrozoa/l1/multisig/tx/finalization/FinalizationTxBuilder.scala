package hydrozoa.l1.multisig.tx.finalization

import hydrozoa.l1.multisig.tx.FinalizationTx
import hydrozoa.l2.ledger.UtxosDiff

trait FinalizationTxBuilder {
    def buildFinalizationTxDraft(
        recipe: FinalizationRecipe
    ): Either[String, FinalizationTx]
}

case class FinalizationRecipe(
    majorVersion: Int,
    utxosWithdrawn: UtxosDiff
)
