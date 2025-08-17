package hydrozoa.l1.rulebased.tx.vote

import com.bloxbean.cardano.client.account.Account
import hydrozoa.l1.rulebased.onchain.DisputeResolutionValidator.BlockHeader
import hydrozoa.node.state.L1BlockEffect.MinorBlockL1Effect
import hydrozoa.{AddressL1, Ed25519Signature, TxL1, UtxoIdL1}

trait VoteTxBuilder {

    /** @param recipe
      * @return
      */
    def buildVoteTxDraft(recipe: VoteTxRecipe): Either[String, TxL1]
}

case class VoteTxRecipe(
    voteUtxoId: UtxoIdL1,
    treasuryUtxoId: UtxoIdL1,
    blockHeader: BlockHeader,
    proof: MinorBlockL1Effect,
    // address for collateral (and fees for now)
    nodeAddress: AddressL1,
    // TODO:Account is used to build and submit in the builder,
    //   though likely we want to separate these two phases.
    nodeAccount: Account
)
