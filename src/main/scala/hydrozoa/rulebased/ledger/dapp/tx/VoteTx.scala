package hydrozoa.rulebased.ledger.dapp.tx

import cats.implicits.*
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.*
import hydrozoa.lib.tx.Datum.DatumInlined
import hydrozoa.lib.tx.ScriptSource.PlutusScriptValue
import hydrozoa.lib.tx.TransactionBuilderStep.{
    AddCollateral,
    Fee,
    ReferenceOutput,
    Send,
    Spend,
    ValidityEndSlot
}
import hydrozoa.lib.tx.{
    ExpectedSigner,
    ScriptSource,
    ThreeArgumentPlutusScriptWitness,
    TransactionBuilder,
    TransactionUnspentOutput,
    TxBuildError
}
import hydrozoa.rulebased.ledger.dapp.script.plutus.DisputeResolutionScript
import hydrozoa.rulebased.ledger.dapp.script.plutus.DisputeResolutionValidator.{
    DisputeRedeemer,
    OnchainBlockHeader,
    VoteRedeemer
}
import hydrozoa.rulebased.ledger.dapp.state.VoteState.VoteStatus.NoVote
import hydrozoa.rulebased.ledger.dapp.state.VoteState.{VoteDatum, VoteDetails, VoteStatus}
import hydrozoa.rulebased.ledger.dapp.utxo.{OwnVoteUtxo, RuleBasedTreasuryUtxo, VoteUtxoCast}
import scala.util.{Failure, Success, Try}
import scalus.builtin.Data.{fromData, toData}
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.txbuilder.*
import scalus.cardano.ledger.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler
import scalus.prelude.List as SList
// import hydrozoa.datumOption // TODO: Will be needed if we add datum hash support

final case class VoteTx(
    // TODO: what we want to keep here if anything?
    voteUtxoSpent: OwnVoteUtxo,
    voteUtxoProduced: VoteUtxoCast,
    tx: Transaction
) // TODO: rule-based trait analogous to Tx?

object VoteTx {

    case class Recipe(
        voteUtxo: OwnVoteUtxo,
        treasuryUtxo: RuleBasedTreasuryUtxo,
        collateralUtxo: Utxo[L1],
        blockHeader: OnchainBlockHeader,
        signatures: List[Ed25519Signature],
        validityEndSlot: Long,
        context: BuilderContext
    )

    enum BuildError:
        case SomeBuilderError(e: TxBuildError)
        case SomeBalancingError(e: TxBalancingError | PlutusScriptEvaluationException)
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
                              voteStatus = VoteStatus.Vote(
                                VoteDetails(
                                  recipe.blockHeader.commitment,
                                  recipe.blockHeader.versionMinor
                                )
                              )
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
        val redeemer = DisputeRedeemer.Vote(
          VoteRedeemer(
            recipe.blockHeader,
            SList.from(
              recipe.signatures.map(sig =>
                  ByteString.fromArray(IArray.genericWrapArray(sig).toArray)
              )
            )
          )
        )

        // Build the transaction
        for {
            context <- TransactionBuilder
                .build(
                  Mainnet,
                  List(
                    // Spend the vote utxo with dispute resolution script witness
                    // So far we use in-place script
                    Spend(
                      TransactionUnspentOutput(voteInput, voteOutput),
                      ThreeArgumentPlutusScriptWitness(
                        // TODO: use a reference utxo
                        //  Rule-based regime scripts will be deployed as reference script with well-known coordinates.
                        //  So in practice we don't need to resolve them, we can just reconstruct them manually.
                        PlutusScriptValue(DisputeResolutionScript.compiledPlutusV3Script),
                        redeemer.toData,
                        DatumInlined,
                        Set(ExpectedSigner(recipe.voteUtxo.voter))
                      )
                    ),
                    // Send back to the vote contract address with updated datum
                    Send(
                      Babbage(
                        address = voteOutput.address,
                        value = voteOutput.value,
                        datumOption = Some(Inline(datumWithVote.toData)),
                        scriptRef = None
                      )
                    ),
                    ReferenceOutput(TransactionUnspentOutput(recipe.treasuryUtxo.toUtxo)),
                    AddCollateral(
                      TransactionUnspentOutput(
                        recipe.collateralUtxo.input,
                        recipe.collateralUtxo.output
                      )
                    ),
                    Fee(Coin(1_000_000)),
                    ValidityEndSlot(recipe.validityEndSlot)
                  )
                )
                .left
                .map(SomeBuilderError(_))

            _ = println(HexUtil.encodeHexString(context.transaction.toCbor))

            finalized <- context
                .finalizeContext(
                  protocolParams = recipe.context.protocolParams,
                  diffHandler = new ChangeOutputDiffHandler(
                    recipe.context.protocolParams,
                    0
                  ).changeOutputDiffHandler,
                  evaluator = recipe.context.evaluator,
                  validators = recipe.context.validators
                )
                .left
                .map({
                    case balanceError: TxBalancingError => SomeBalancingError(balanceError)
                    case validationError: TransactionException =>
                        SomeTransactionException(validationError)
                })

        } yield VoteTx(
          voteUtxoSpent = recipe.voteUtxo,
          voteUtxoProduced = VoteUtxoCast(
            Utxo[L1](
              UtxoId[L1](finalized.transaction.id, 0), // Vote output is at index 0
              Output[L1](
                finalized.transaction.body.value.outputs(0).value.asInstanceOf[Babbage]
              ) // The updated vote output
            )
          ),
          tx = finalized.transaction
        )
    }
}
