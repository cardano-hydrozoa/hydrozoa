package hydrozoa.l1.multisig.tx.settlement

import hydrozoa.l1.multisig.tx.SettlementTx
import hydrozoa.l2.ledger.state.UtxosDiff
import hydrozoa.{L1, OutputRef}

trait SettlementTxBuilder {
    def mkSettlementTxDraft(
        recipe: SettlementRecipe
    ): Either[String, SettlementTx]
}

case class SettlementRecipe(
    majorVersion: Int,
    deposits: Seq[OutputRef[L1]],
    utxosWithdrawn: UtxosDiff
)
