package hydrozoa.l1.multisig.tx.settlement

import hydrozoa.{UtxoIdL1, UtxoSetL2}
import hydrozoa.l1.multisig.tx.SettlementTx

trait SettlementTxBuilder {
    def mkSettlementTxDraft(
        recipe: SettlementRecipe
    ): Either[String, SettlementTx]
}

case class SettlementRecipe(
    majorVersion: Int,
    deposits: Seq[UtxoIdL1],
    utxosWithdrawn: UtxoSetL2
)
