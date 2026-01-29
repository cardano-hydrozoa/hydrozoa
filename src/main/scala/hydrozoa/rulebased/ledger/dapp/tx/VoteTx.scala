package hydrozoa.rulebased.ledger.dapp.tx

import cats.implicits.*
import hydrozoa.*
import hydrozoa.multisig.protocol.types.AckBlock.HeaderSignature
import hydrozoa.rulebased.ledger.dapp.script.plutus.DisputeResolutionScript
import hydrozoa.rulebased.ledger.dapp.script.plutus.DisputeResolutionValidator.{DisputeRedeemer, OnchainBlockHeader, VoteRedeemer}
import hydrozoa.rulebased.ledger.dapp.state.VoteState.VoteStatus.*
import hydrozoa.rulebased.ledger.dapp.state.VoteState.{VoteDatum, VoteStatus}
import hydrozoa.rulebased.ledger.dapp.utxo.{OwnVoteUtxo, RuleBasedTreasuryUtxo, VoteUtxoCast}
import scala.util.{Failure, Success, Try}
import scalus.builtin.Data.{fromData, toData}
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.Network
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.{Utxo as SUtxo, *}
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.Datum.DatumInlined
import scalus.cardano.txbuilder.ScriptSource.PlutusScriptValue
import scalus.cardano.txbuilder.TransactionBuilderStep.*
import scalus.prelude.List as SList

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
        blockHeader: BlockHeader.Minor.Onchain,
        signatures: List[BlockHeader.Minor.HeaderSignature],
        network: Network,
        protocolParams: ProtocolParams,
        evaluator: PlutusScriptEvaluator,
        validators: Seq[Validator]
    )

    enum VoteTxError:
        case InvalidVoteDatum(msg: String)
        case VoteAlreadyCast

    def build(recipe: Recipe): Either[SomeBuildError | VoteTxError, VoteTx] = {
        import VoteTxError.*

        // Extract current vote datum from the UTXO
        val voteOutput = recipe.voteUtxo.utxo.output.untagged

        voteOutput.datumOption match {
            case Some(DatumOption.Inline(datumData)) =>
                Try(fromData[VoteDatum](datumData)) match {
                    case Success(voteDatum) =>
                        voteDatum.voteStatus match {
                            case AwaitingVote(_) =>
                                val updatedVoteDatum = voteDatum.copy(
                                  voteStatus = VoteStatus.Voted(
                                    recipe.blockHeader.commitment,
                                    recipe.blockHeader.versionMinor
                                  )
                                )
                                buildVoteTx(recipe, updatedVoteDatum)
                            case _ => Left(VoteAlreadyCast)
                        }

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
    ): Either[SomeBuildError, VoteTx] = {

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
                  recipe.network,
                  List(
                    // Use collateral to pay fees
                    Spend(recipe.collateralUtxo.toScalus, PubKeyWitness),
                    Send(recipe.collateralUtxo._2),
                    // Spend the vote utxo with dispute resolution script witness
                    // So far we use in-place script
                    Spend(
                      SUtxo(voteInput, voteOutput),
                      ThreeArgumentPlutusScriptWitness(
                        // TODO: use a reference utxo
                        //  Rule-based regime scripts will be deployed as reference script with well-known coordinates.
                        //  So in practice we don't need to resolve them, we can just reconstruct them manually.
                        PlutusScriptValue(DisputeResolutionScript.compiledPlutusV3Script),
                        redeemer.toData,
                        DatumInlined,
                        // Set.empty
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
                    ReferenceOutput(SUtxo(recipe.treasuryUtxo.asTuple)),
                    AddCollateral(recipe.collateralUtxo.toScalus)
                  )
                )

            // _ = println(HexUtil.encodeHexString(context.transaction.toCbor))

            finalized <- context
                .finalizeContext(
                  protocolParams = recipe.protocolParams,
                  diffHandler = Change.changeOutputDiffHandler(_, _, recipe.protocolParams, 0),
                  evaluator = recipe.evaluator,
                  validators = recipe.validators
                )

        } yield VoteTx(
          voteUtxoSpent = recipe.voteUtxo,
          voteUtxoProduced = VoteUtxoCast(
            Utxo[L1](
              UtxoId[L1](finalized.transaction.id, 0), // Vote output is at index 0
              Output[L1](
                finalized.transaction.body.value.outputs(0).value
              ) // The updated vote output
            )
          ),
          tx = finalized.transaction
        )
    }
}
