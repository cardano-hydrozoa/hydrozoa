package hydrozoa.l1.multisig.tx.deposit

import hydrozoa.l1.multisig.state.DepositDatum
import hydrozoa.l1.multisig.tx.MultisigTxs.DepositTx
import hydrozoa.{TxAny, TxId, TxIx}

trait DepositTxBuilder {

    /** @param recipe
      * @return
      *   deposit tx draft + output index that points to deposit utxo
      */
    def buildDepositTxDraft(recipe: DepositTxRecipe): Either[String, (DepositTx, TxIx)]
}

case class DepositTxRecipe(
    utxo: (TxId, TxIx), // the only UTXO with deposit funds in user's wallet
    datum: DepositDatum // datum for deposit utxo
)
