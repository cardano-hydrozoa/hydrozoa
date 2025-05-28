package hydrozoa.l1.multisig.tx.finalization

import hydrozoa.l1.multisig.tx.FinalizationTx
import hydrozoa.l2.ledger.simple.UtxosSet

trait FinalizationTxBuilder {
    def buildFinalizationTxDraft(
        recipe: FinalizationRecipe
    ): Either[String, FinalizationTx]
}

case class FinalizationRecipe(
    majorVersion: Int,
    utxosWithdrawn: UtxosSet // FIXME: add phantom type parameter
)
