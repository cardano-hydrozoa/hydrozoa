package hydrozoa.l1.multisig.tx.deposit

import hydrozoa.l1.multisig.state.DepositDatum
import hydrozoa.{TxIx, TxL1, UtxoIdL1}

trait DepositTxBuilder {

    /** @param recipe
      * @return
      *   deposit tx draft + output index that points to the deposit utxo
      */
    def buildDepositTxDraft(recipe: DepositTxRecipe): Either[String, (TxL1, TxIx)]
}

case class DepositTxRecipe(
    deposit: UtxoIdL1, // the only UTXO with deposit funds in user's wallet
    depositAmount: BigInt,
    datum: DepositDatum // datum for deposit utxo
)
