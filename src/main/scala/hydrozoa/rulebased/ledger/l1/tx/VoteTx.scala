package hydrozoa.rulebased.ledger.l1.tx

import cats.implicits.*
import hydrozoa.*
import hydrozoa.lib.tx.ScriptWitness.ScriptValue
import hydrozoa.lib.tx.TransactionBuilderStep.{Pay, SpendOutput}
import hydrozoa.lib.tx.{OutputWitness, TransactionBuilder, TransactionUnspentOutput, TxBuildError}
import hydrozoa.multisig.ledger.dapp.utxo.VoteUtxo
import hydrozoa.rulebased.ledger.l1.dapp.utxo.RuleBasedTreasuryUtxo
import hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionScript
import hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionValidator.{
    OnchainBlockHeader,
    VoteRedeemer
}
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus.NoVote
import hydrozoa.rulebased.ledger.l1.state.VoteState.{VoteDatum, VoteDetails, VoteStatus}
import monocle.syntax.all.*
import scalus.builtin.Data.{fromData, toData}
import scalus.builtin.{ByteString, Data}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.txbuilder.*
import scalus.prelude.List as SList

import scala.util.{Failure, Success, Try}
// import hydrozoa.datumOption // TODO: Will be needed if we add datum hash support

final case class VoteTx(
    // TODO: what we want to keep here if anything?
    voteUtxoSpent: VoteUtxo,
    voteUtxoProduced: VoteUtxo,
    tx: Transaction
) // TODO: rule-based trait analogous to Tx?

object VoteTx {

    case class Recipe(
        voteUtxo: VoteUtxo, // The vote UTXO to spend
        // TODO: use rule-based treasury utxo
        treasuryUtxo: RuleBasedTreasuryUtxo, // Treasury UTXO for reference
        blockHeader: OnchainBlockHeader,
        signatures: List[Ed25519Signature],
        newVoteDetails: VoteDetails, // The new vote to cast
        context: BuilderContext
    )

    enum BuildError:
        case SomeBuilderError(e: TxBuildError)
        case SomeBalancingError(e: TxBalancingError)
        case SomeTransactionException(e: TransactionException)
        case InvalidVoteDatum(msg: String)
        case VoteAlreadyCast

    def build(recipe: Recipe): Either[BuildError, VoteTx] = {
        import BuildError.*

        // Extract current vote datum from the UTXO
        val voteOutput = recipe.voteUtxo.utxo.output.untagged

        voteOutput.datumOption match {
            case Some(DatumOption.Inline(datumData)) =>
                Try(fromData[VoteDatum](datumData)) match {
                    case Success(voteDatum) =>
                        if voteDatum.voteStatus == NoVote then
                            val updatedVoteDatum = voteDatum.copy(
                              voteStatus = VoteStatus.Vote(recipe.newVoteDetails)
                            )
                            buildVoteTx(recipe, updatedVoteDatum)
                        else Left(VoteAlreadyCast)

                    case Failure(e) =>
                        Left(
                          InvalidVoteDatum(
                            s"Failed to parse VoteDatum from inline datum: ${e.getMessage}"
                          )
                        )
                }
            case _ =>
                Left(InvalidVoteDatum("Vote utxo must have inline datum"))
        }
    }

    private def buildVoteTx(
        recipe: Recipe,
        datumWithVote: VoteDatum
    ): Either[BuildError, VoteTx] = {
        import BuildError.*

        // Get the TransactionInput and TransactionOutput from VoteUtxo
        val (voteInput, voteOutput) =
            (recipe.voteUtxo.utxo.input.untagged, recipe.voteUtxo.utxo.output.untagged)

        // Create redeemer for dispute resolution script
        val redeemer = VoteRedeemer(
          recipe.blockHeader,
          SList.from(
            recipe.signatures.map(sig => ByteString.fromArray(IArray.genericWrapArray(sig).toArray))
          )
        )

        // Get dispute resolution script
        // TODO: use ref script/config?
        val disputeResolutionScript =
            Script.PlutusV3(ByteString.fromHex(DisputeResolutionScript.getScriptHex))

        // Build the transaction
        val buildResult = for {
            unbalancedTx <- TransactionBuilder
                .buildTransaction(
                  List(
                    // Spend the vote utxo with dispute resolution script witness
                    // So far we use in-place script
                    SpendOutput(
                      TransactionUnspentOutput(voteInput, voteOutput),
                      Some(
                        OutputWitness.PlutusScriptOutput(
                          ScriptValue(disputeResolutionScript),
                          redeemer.toData,
                          None // No datum witness needed for an inline datum?
                        )
                      )
                    ),
                    // Pay back to the vote contract address with updated datum
                    Pay(
                      Babbage(
                        address = voteOutput.address,
                        value = voteOutput.value,
                        datumOption = Some(Inline(datumWithVote.toData)),
                        scriptRef = None
                      )
                    )
                  )
                )
                .left
                .map(SomeBuilderError(_))

            // add the treasury as a reference utxo
            _ = unbalancedTx
                .focus(_.body.value.referenceInputs)
                .replace(TaggedOrderedSet.from(List(recipe.treasuryUtxo.txId)))

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
                .map(SomeBalancingError(_))

            // Validate the transaction
            validated <- recipe.context
                .validate(balanced)
                .left
                .map(SomeTransactionException(_))

        } yield validated

        buildResult.map { validatedTx =>
            VoteTx(
              voteUtxoSpent = recipe.voteUtxo,
              voteUtxoProduced = VoteUtxo(
                Utxo[L1](
                  UtxoId[L1](validatedTx.id, 0), // Vote output is at index 0
                  Output[L1](
                    validatedTx.body.value.outputs(0).value.asInstanceOf[Babbage]
                  ) // The updated vote output
                )
              ),
              tx = validatedTx
            )
        }
    }
}
