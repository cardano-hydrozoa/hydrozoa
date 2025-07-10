package hydrozoa.l1.rulebased.tx.withdraw

import com.bloxbean.cardano.client.account.Account
import hydrozoa.l2.commitment.UtxoSetCommitment
import hydrozoa.{TxL1, UtxoIdL1, UtxoSetL2}

trait WithdrawTxBuilder {

    /** @param recipe
      * @return
      */
    def buildWithdrawTx(recipe: WithdrawTxRecipe): Either[String, TxL1]
}

case class WithdrawTxRecipe(
    withdrawals: UtxoSetL2,
    resolvedTreasuryUtxoId: UtxoIdL1,
    proof: UtxoSetCommitment,   
    // TODO: Account is used to build and submit in the builder,
    //   though likely we want to separate these two phases.
    nodeAccount: Account
)
