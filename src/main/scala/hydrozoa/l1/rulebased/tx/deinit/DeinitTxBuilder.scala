package hydrozoa.l1.rulebased.tx.deinit

import com.bloxbean.cardano.client.account.Account
import hydrozoa.*
import scalus.cardano.ledger.PolicyId
import scalus.cardano.ledger.Script.Native

trait DeinitTxBuilder {

    /** @param recipe
      * @return
      */
    def buildDeinitTxDraft(recipe: DeinitTxRecipe): Either[String, TxL1]
}

// This is a trivial deinit recipe that builds a tx that sends all the treasury back to the initializer
// and burns all tokens
case class DeinitTxRecipe(
    // The treasury we want to dissolve
    resolvedTreasuryUtxoId: UtxoIdL1,
    // The receiver of the funds
    initializerAddress: AddressL1,
    // This is needed to calculate the head minting policy
    headNativeScript: Native,
    headMintingPolicy: PolicyId,
    // TODO: Account is used to build and submit in the builder,
    //   though likely we want to separate these two phases.
    nodeAccount: Account
)
