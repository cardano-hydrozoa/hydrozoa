package hydrozoa.l1.multisig.tx.deposit

import hydrozoa.l1.multisig.state.DepositDatum
import hydrozoa.l1.multisig.tx.DepositTx
import hydrozoa.{TxIx, UtxoIdL1}

trait DepositTxBuilder {

    /** @param recipe
      * @return
      *   deposit tx draft + output index that points to the deposit utxo
      */
    def buildDepositTxDraft(recipe: DepositTxRecipe): Either[String, (DepositTx, TxIx)]
}

case class DepositTxRecipe(
    deposit: UtxoIdL1, // the only UTXO with deposit funds in user's wallet
    datum: DepositDatum // datum for deposit utxo
)
