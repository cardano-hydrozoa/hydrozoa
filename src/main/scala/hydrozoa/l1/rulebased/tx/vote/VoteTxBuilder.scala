package hydrozoa.l1.rulebased.tx.vote

import hydrozoa.l1.rulebased.onchain.DisputeResolutionValidator.BlockHeader
import hydrozoa.{Ed25519Signature, TxIx, TxL1, UtxoIdL1}

trait VoteTxBuilder {

    /** @param recipe
      * @return
      */
    def buildVoteTxDraft(recipe: VoteTxRecipe): Either[String, (TxL1, TxIx)]
}

case class VoteTxRecipe(
    voteUtxo: UtxoIdL1,
    blockHeader: BlockHeader,
    proof: Seq[Ed25519Signature]
)
