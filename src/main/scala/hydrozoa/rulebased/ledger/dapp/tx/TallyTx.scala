package hydrozoa.rulebased.ledger.dapp.tx

import cats.implicits.*
import hydrozoa.*
import hydrozoa.lib.tx.BuildError.*
import hydrozoa.lib.tx.Datum.DatumInlined
import hydrozoa.lib.tx.ScriptSource.PlutusScriptValue
import hydrozoa.lib.tx.TransactionBuilderStep.{AddCollateral, ReferenceOutput, Send, Spend, ValidityEndSlot}
import hydrozoa.lib.tx.{BuildError, ThreeArgumentPlutusScriptWitness, TransactionBuilder, TransactionUnspentOutput}
import hydrozoa.rulebased.ledger.dapp.script.plutus.DisputeResolutionScript
import hydrozoa.rulebased.ledger.dapp.script.plutus.DisputeResolutionValidator.{DisputeRedeemer, TallyRedeemer, maxVote}
import hydrozoa.rulebased.ledger.dapp.state.VoteState.*
import hydrozoa.rulebased.ledger.dapp.utxo.{RuleBasedTreasuryUtxo, TallyVoteUtxo}
import scala.util.{Failure, Success, Try}
import scalus.builtin.Data.{fromData, toData}
import scalus.cardano.address.Network
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.txbuilder.*
import scalus.cardano.ledger.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler
import scalus.prelude.Option.None as SNone

final case class TallyTx(
    continuingVoteUtxo: TallyVoteUtxo,
    removedVoteUtxo: TallyVoteUtxo,
    treasuryUtxo: RuleBasedTreasuryUtxo,
    tx: Transaction
)

object TallyTx {

    case class Recipe(
        continuingVoteUtxo: TallyVoteUtxo,
        removedVoteUtxo: TallyVoteUtxo,
        treasuryUtxo: RuleBasedTreasuryUtxo,
        collateralUtxo: Utxo[L1],
        validityEndSlot: Long,
        network: Network,
        protocolParams: ProtocolParams,
        evaluator: PlutusScriptEvaluator,
        validators: Seq[Validator]
    )

    enum TallyTxError:
        case AbsentVoteDatum(utxo: UtxoIdL1)
        case MalformedVoteDatum(utxo: UtxoIdL1, msg: String)
        case IncompatibleVotes(continuing: (Key, Link), removed: (Key, Link))

    def build(recipe: Recipe): Either[BuildError | TallyTxError, TallyTx] = {
        for {
            continuingVoteDatum <- extractVoteDatum(recipe.continuingVoteUtxo)
            removedVoteDatum <- extractVoteDatum(recipe.removedVoteUtxo)
            outputDatum <- tallyVotes(continuingVoteDatum, removedVoteDatum)
            result <- buildTallyTx(recipe, outputDatum)
        } yield result
    }

    private def extractVoteDatum(
        voteUtxo: TallyVoteUtxo
    ): Either[TallyTxError, VoteDatum] = {
        import TallyTxError.*

        val voteOutput = voteUtxo.utxo.output.untagged
        voteOutput.datumOption match {
            case Some(DatumOption.Inline(datumData)) =>
                Try(fromData[VoteDatum](datumData)) match {
                    case Success(voteDatum) => Right(voteDatum)
                    case Failure(e) =>
                        Left(
                          MalformedVoteDatum(
                            voteUtxo.utxo.input,
                            s"Malformed VoteDatum data: ${datumData.toString}"
                          )
                        )
                }
            case _ => Left(AbsentVoteDatum(voteUtxo.utxo.input))
        }
    }

    private def tallyVotes(
        continuing: VoteDatum,
        removed: VoteDatum
    ): Either[TallyTxError, VoteDatum] = {
        import TallyTxError.*

        if continuing.link != removed.key
        then Left(IncompatibleVotes((continuing.key, continuing.link), (removed.key, removed.link)))
        else {
            val higherVote = maxVote(
              continuing.voteStatus,
              removed.voteStatus
            )
            Right(VoteDatum(continuing.key, removed.link, SNone, higherVote))
        }
    }

    private def buildTallyTx(
        recipe: Recipe,
        outputDatum: VoteDatum
    ): Either[BuildError, TallyTx] = {
        import recipe.* 
        
        val outputValue = continuingVoteUtxo.utxo.output.value +
            removedVoteUtxo.utxo.output.value

        val continuingRedeemer = DisputeRedeemer.Tally(TallyRedeemer.Continuing)
        val removedRedeemer = DisputeRedeemer.Tally(TallyRedeemer.Removed)

        for {
            context <- TransactionBuilder
                .build(
                  network,
                  List(
                    // Spend the continuing vote utxo with tally redeemer
                    Spend(
                      TransactionUnspentOutput.fromUtxo(continuingVoteUtxo.utxo),
                      ThreeArgumentPlutusScriptWitness(
                        PlutusScriptValue(DisputeResolutionScript.compiledPlutusV3Script),
                        continuingRedeemer.toData,
                        DatumInlined,
                        Set.empty
                      )
                    ),
                    // Spend the removed vote utxo with tally redeemer
                    Spend(
                      TransactionUnspentOutput.fromUtxo(removedVoteUtxo.utxo),
                      ThreeArgumentPlutusScriptWitness(
                        PlutusScriptValue(DisputeResolutionScript.compiledPlutusV3Script),
                        removedRedeemer.toData,
                        DatumInlined,
                        Set.empty
                      )
                    ),
                    // Send back the continuing vote utxo (the removed one is consumed)
                    Send(
                      Babbage(
                        address = continuingVoteUtxo.utxo.output.address,
                        value = outputValue,
                        datumOption = Some(Inline(outputDatum.toData)),
                        scriptRef = None
                      )
                    ),
                    ReferenceOutput(TransactionUnspentOutput(treasuryUtxo.toUtxo)),
                    AddCollateral(TransactionUnspentOutput.fromUtxo(collateralUtxo)),
                    ValidityEndSlot(validityEndSlot)
                  )
                )
                .left
                .map(StepError(_))

            finalized <- context
                .finalizeContext(
                  protocolParams = protocolParams,
                  diffHandler = new ChangeOutputDiffHandler(
                    protocolParams,
                    0
                  ).changeOutputDiffHandler,
                  evaluator = evaluator,
                  validators = validators
                )

        } yield TallyTx(
          continuingVoteUtxo = continuingVoteUtxo,
          removedVoteUtxo = removedVoteUtxo,
          treasuryUtxo = treasuryUtxo,
          tx = finalized.transaction
        )
    }
}
