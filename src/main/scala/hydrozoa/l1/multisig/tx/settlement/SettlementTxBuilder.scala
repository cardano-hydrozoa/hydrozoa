package hydrozoa.l1.multisig.tx.settlement

import hydrozoa.l1.multisig.tx.SettlementTx
import hydrozoa.{UtxoIdL1, UtxoSetL2}

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
