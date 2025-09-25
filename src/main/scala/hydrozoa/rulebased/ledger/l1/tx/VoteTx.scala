package hydrozoa.rulebased.ledger.l1.tx

import cats.implicits.*
// import hydrozoa.lib.tx.CredentialWitness.PlutusScriptCredential // TODO: Will be needed for actual script witness
import hydrozoa.lib.tx.ScriptWitness.ScriptValue
import hydrozoa.lib.tx.TransactionBuilderStep.{Pay, SpendOutput}
import hydrozoa.lib.tx.{OutputWitness, TransactionBuilder, TransactionUnspentOutput, TxBuildError}
import hydrozoa.rulebased.ledger.l1.state.{VoteDatum, VoteDetails, VoteStatus}
// import hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionScript // TODO: Will be needed for actual script
import hydrozoa.multisig.ledger.dapp.utxo.{TreasuryUtxo, VoteUtxo}
import hydrozoa.{AddressL1, L1, UtxoId, Utxo, Output}
import scalus.builtin.Data.{fromData, toData}
import scalus.builtin.{ByteString, Data}
import scala.util.{Failure, Success, Try}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.txbuilder.*
// import hydrozoa.datumOption // TODO: Will be needed if we add datum hash support

// Simplified types for now - these would need to be defined based on the actual L2 block structure
case class BlockHeader(hash: ByteString, version: BigInt) derives scalus.builtin.ToData
case class MinorBlockL1Effect(header: BlockHeader, multisigProof: ByteString) derives scalus.builtin.ToData
case class Account(address: AddressL1, signingKey: ByteString) // Simplified - actual implementation would need proper key management

final case class VoteTx(
    voteUtxoSpent: VoteUtxo,
    voteUtxoProduced: VoteUtxo,
    tx: Transaction
) // TODO: extends appropriate Tx trait

object VoteTx {
    
    case class Recipe(
        voteUtxo: VoteUtxo, // The vote UTXO to spend
        treasuryUtxo: TreasuryUtxo, // Treasury UTXO for reference
        blockHeader: BlockHeader,
        proof: MinorBlockL1Effect,
        nodeAddress: AddressL1,
        nodeAccount: Account,
        newVoteDetails: VoteDetails, // The new vote to cast
        context: BuilderContext
    )

    enum BuildError:
        case SomeBuilderError(e: TxBuildError)
        case OtherScalusBalancingError(e: TxBalancingError)
        case OtherScalusTransactionException(e: TransactionException)
        case InvalidVoteDatum(msg: String)
        case MissingDisputeResolutionScript

    def build(recipe: Recipe): Either[BuildError, VoteTx] = {
        import BuildError.*

        // Extract current vote datum from the UTXO
        // Get the TransactionOutput from VoteUtxo
        val voteOutput = recipe.voteUtxo.utxo.output.untagged
        
        voteOutput.datumOption match {
            case Some(DatumOption.Inline(datumData)) =>
                Try(fromData[VoteDatum](datumData)) match {
                    case Success(voteDatum) => 
                        // Create updated vote datum with new vote
                        val updatedVoteDatum = VoteDatum(
                            key = voteDatum.key,
                            link = voteDatum.link, 
                            peer = voteDatum.peer,
                            voteStatus = VoteStatus.Vote(recipe.newVoteDetails)
                        )
                        
                        buildVoteTx(recipe, updatedVoteDatum)
                        
                    case Failure(e) => 
                        Left(InvalidVoteDatum(s"Failed to parse VoteDatum from inline datum: ${e.getMessage}"))
                }
            case _ => 
                Left(InvalidVoteDatum("Vote UTXO must have inline datum"))
        }
    }
    
    private def buildVoteTx(recipe: Recipe, updatedVoteDatum: VoteDatum): Either[BuildError, VoteTx] = {
        import BuildError.*
        
        // Get the TransactionInput and TransactionOutput from VoteUtxo
        val (voteInput, voteOutput) = (recipe.voteUtxo.utxo.input.untagged, recipe.voteUtxo.utxo.output.untagged)

        // Create redeemer for dispute resolution script
        val redeemer = recipe.proof.toData

        // Get dispute resolution script (placeholder - would need actual implementation)
        val disputeResolutionScript = Script.PlutusV3(ByteString.fromHex("deadbeef")) // TODO: actual script

        // Build the transaction
        val buildResult = for {
                unbalancedTx <- TransactionBuilder
                    .buildTransaction(
                        List(
                            // Spend the vote UTXO with dispute resolution script witness
                            SpendOutput(
                                TransactionUnspentOutput(voteInput, voteOutput),
                                Some(OutputWitness.PlutusScriptOutput(
                                    ScriptValue(disputeResolutionScript),
                                    redeemer,
                                    None // No datum witness needed for spending
                                ))
                            ),
                            // TODO: Add treasury UTXO as reference input if needed
                            // For now, we'll skip the treasury reference to simplify the implementation
                            // Pay back to the vote contract address with updated datum
                            Pay(Babbage(
                                address = voteOutput.address,
                                value = voteOutput.value,
                                datumOption = Some(Inline(updatedVoteDatum.toData)),
                                scriptRef = None
                            ))
                        )
                    )
                    .left
                    .map(SomeBuilderError(_))

                // Balance the transaction
                balanced <- LowLevelTxBuilder
                    .balanceFeeAndChange(
                        initial = unbalancedTx,
                        changeOutputIdx = 0, // Send change to the vote output
                        protocolParams = recipe.context.protocolParams,
                        resolvedUtxo = recipe.context.utxo,
                        evaluator = recipe.context.evaluator
                    )
                    .left
                    .map(OtherScalusBalancingError(_))

                // Validate the transaction
                validated <- recipe.context
                    .validate(balanced)
                    .left
                    .map(OtherScalusTransactionException(_))

        } yield validated

        buildResult.map { validatedTx =>
            VoteTx(
                voteUtxoSpent = recipe.voteUtxo,
                voteUtxoProduced = VoteUtxo(
                    Utxo[L1](
                        UtxoId[L1](validatedTx.id, 0), // Vote output is at index 0
                        Output[L1](validatedTx.body.value.outputs(0).value.asInstanceOf[Babbage]) // The updated vote output
                    )
                ),
                tx = validatedTx
            )
        }
    }
}
