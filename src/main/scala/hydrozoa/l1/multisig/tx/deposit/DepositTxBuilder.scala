package hydrozoa.l1.multisig.tx.deposit

import hydrozoa.l1.multisig.state.DepositDatum
import hydrozoa.{TxIx, TxL1, UtxoIdL1}
import scalus.cardano.ledger.Slot

trait DepositTxBuilder {

    /** @param recipe
      * @return
      *   deposit tx draft + output index that points to the deposit utxo
      */
    def buildDepositTxDraft(recipe: DepositTxRecipe): Either[String, (TxL1, TxIx)]
}

/** Margin for the upper bound of the deposit txs validity interval, slots.
  */
val ttlMargin = 30L;

case class DepositTxRecipe(
    deposit: UtxoIdL1, // the only UTXO with deposit funds in the user's wallet
    depositAmount: BigInt,
    datum: DepositDatum, // datum for deposit utxo
    ttl: Slot // upper bound for the validity interval
)
