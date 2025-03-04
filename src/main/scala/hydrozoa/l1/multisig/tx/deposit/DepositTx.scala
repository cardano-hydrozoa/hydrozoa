package hydrozoa.l1.multisig.tx.deposit

import hydrozoa.l1.multisig.state.DepositDatum
import hydrozoa.{L1Tx, TxId, TxIx}

trait DepositTxBuilder {
    def buildDepositTxDraft(recipe: DepositTxRecipe): Either[String, (L1Tx, TxIx)]
}

case class DepositTxRecipe(
    utxo: (TxId, TxIx), // the only UTXO for funds in user's wallet
    datum: DepositDatum // datum for deposit utxo
)
