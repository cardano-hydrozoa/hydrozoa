package hydrozoa.rulebased.ledger.l1.tx

import cats.implicits.*
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.*
import hydrozoa.lib.tx.ScriptWitness.ScriptValue
import hydrozoa.lib.tx.TransactionBuilderStep.{Pay, SpendOutput}
import hydrozoa.lib.tx.{OutputWitness, TransactionBuilder, TransactionUnspentOutput, TxBuildError}
import hydrozoa.multisig.ledger.dapp.utxo.{OwnVoteUtxo, VoteUtxoCast}
import hydrozoa.rulebased.ledger.l1.dapp.utxo.RuleBasedTreasuryUtxo
import hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionScript
import hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionValidator.{
    DisputeRedeemer,
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
import scalus.serialization.cbor.Cbor as ScalusCbor

import scala.util.{Failure, Success, Try}
// import hydrozoa.datumOption // TODO: Will be needed if we add datum hash support

final case class VoteTx(
    // TODO: what we want to keep here if anything?
    voteUtxoSpent: OwnVoteUtxo,
    voteUtxoProduced: VoteUtxoCast,
    tx: Transaction
) // TODO: rule-based trait analogous to Tx?

object VoteTx {

    case class Recipe(
        voteUtxo: OwnVoteUtxo, // The vote UTXO to spend
        // TODO: use rule-based treasury utxo
        treasuryUtxo: RuleBasedTreasuryUtxo, // Treasury UTXO for reference
        collateralUtxo: Utxo[L1],
        blockHeader: OnchainBlockHeader,
        signatures: List[Ed25519Signature],
        ttl: Long,
        context: BuilderContext
        //// TODO: should be accessible to the builder, since the vote tx requires voter signature
        // wallet: Wallet
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
                          // TODO: use a reference utxo? Rule-based regime scripts will be deployed as reference script,
                          //  though nodes don't necessarily need to resolve those utxos, they may reconstruct them based manually.
                          ScriptValue(DisputeResolutionScript.compiledPlutusV3Script),
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

            unbalancedTx1 = unbalancedTx
                // add the treasury as a reference utxo
                .focus(_.body.value.referenceInputs)
                .replace(TaggedOrderedSet.from(List(recipe.treasuryUtxo.txId)))
                // set the voter as the only required signer
                .focus(_.body.value.requiredSigners)
                .replace(TaggedOrderedSet.from(List(recipe.voteUtxo.voter)))
                // set TTL
                .focus(_.body.value.ttl)
                .replace(Some(recipe.ttl))
                // set collateral
                .focus(_.body.value.collateralInputs)
                .replace(TaggedOrderedSet.from(List(recipe.collateralUtxo.input)))
                // TODO: remove - fake coins
                .focus(_.body.value.fee)
                .replace(Coin(100000))

            // ping keep raw
            unbalancedTx2 = unbalancedTx1
                .focus(_.body.raw)
                .replace(ScalusCbor.encode(unbalancedTx1.body.value))

            // _ = println(unbalancedTx)
            // _ = println(HexUtil.encodeHexString(unbalancedTx2.toCbor))

            // Balance the transaction
            balanced <- LowLevelTxBuilder
                .balanceFeeAndChange(
                  initial = unbalancedTx1,
                  changeOutputIdx = 0, // Send change to the vote output
                  protocolParams = recipe.context.protocolParams,
                  resolvedUtxo = recipe.context.utxo,
                  evaluator = recipe.context.evaluator
                )
                .left
                .map(SomeBalancingError(_))

            // _ = println(HexUtil.encodeHexString(balanced.toCbor))
            // _ = println("validating the balanced tx")

            // Validate the transaction
            validated <- recipe.context
                .validate(balanced)
                .left
                .map(SomeTransactionException(_))

        } yield validated

        buildResult.map { validatedTx =>
            VoteTx(
              voteUtxoSpent = recipe.voteUtxo,
              voteUtxoProduced = VoteUtxoCast(
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
