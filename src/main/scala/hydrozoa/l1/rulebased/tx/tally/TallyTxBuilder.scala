package hydrozoa.l1.rulebased.tx.tally

import com.bloxbean.cardano.client.account.Account
import hydrozoa.{TxL1, UtxoIdL1}

trait TallyTxBuilder {

    /** @param recipe
      * @return
      */
    def buildTallyTxDraft(recipe: TallyTxRecipe): Either[String, TxL1]
}

case class TallyTxRecipe(
    continuingVote: UtxoIdL1,
    removedVote: UtxoIdL1,
    treasuryUtxoId: UtxoIdL1,
    // TODO: Account is used to build and submit in the builder,
    //   though likely we want to separate these two phases.
    nodeAccount: Account
)
