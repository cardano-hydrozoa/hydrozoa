package hydrozoa.l1.rulebased.tx.resolution

import com.bloxbean.cardano.client.account.Account
import hydrozoa.{TxIx, TxL1, UtxoIdL1}

trait ResolutionTxBuilder {

    /** @param recipe
      * @return
      */
    def buildResolutionTx(recipe: ResolutionTxRecipe): Either[String, TxL1]
}

case class ResolutionTxRecipe(
    talliedVote: UtxoIdL1,
    treasuryUtxoId: UtxoIdL1,
    // TODO: Account is used to build and submit in the builder,
    //   though likely we want to separate these two phases.
    nodeAccount: Account
)
