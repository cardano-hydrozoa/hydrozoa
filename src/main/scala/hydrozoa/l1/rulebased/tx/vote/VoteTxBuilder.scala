package hydrozoa.l1.rulebased.tx.vote

import com.bloxbean.cardano.client.account.Account
import hydrozoa.l1.rulebased.onchain.DisputeResolutionValidator.BlockHeader
import hydrozoa.{AddressBechL1, Ed25519Signature, TxL1, UtxoIdL1}

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
    proof: Seq[Ed25519Signature],
    // address for collateral (and fees for now)
    nodeAddress: AddressBechL1,
    // TODO:Account is used to build and submit in the builder,
    //   though likely we want to separate these two phases.
    nodeAccount: Account
)
