package hydrozoa.l1.multisig.tx.settlement

import hydrozoa.UtxoIdL1
import hydrozoa.l1.multisig.tx.SettlementTx
import hydrozoa.l2.ledger.UtxosDiff

trait SettlementTxBuilder {
    def mkSettlementTxDraft(
        recipe: SettlementRecipe
    ): Either[String, SettlementTx]
}

case class SettlementRecipe(
    majorVersion: Int,
    deposits: Seq[UtxoIdL1],
    utxosWithdrawn: UtxosDiff
)
