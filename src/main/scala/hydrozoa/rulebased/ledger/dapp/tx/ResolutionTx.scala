package hydrozoa.rulebased.ledger.dapp.tx

import cats.implicits.*
import hydrozoa.*
import hydrozoa.rulebased.ledger.dapp.script.plutus.DisputeResolutionValidator.DisputeRedeemer
import hydrozoa.rulebased.ledger.dapp.script.plutus.RuleBasedTreasuryValidator.TreasuryRedeemer
import hydrozoa.rulebased.ledger.dapp.script.plutus.{
    DisputeResolutionScript,
    RuleBasedTreasuryScript
}
import hydrozoa.rulebased.ledger.dapp.state.TreasuryState.{
    ResolvedDatum,
    RuleBasedTreasuryDatum,
    UnresolvedDatum
}
import hydrozoa.rulebased.ledger.dapp.state.VoteState.{KzgCommitment, VoteDatum, VoteStatus}
import hydrozoa.rulebased.ledger.dapp.utxo.{RuleBasedTreasuryUtxo, TallyVoteUtxo}
import scala.util.{Failure, Success, Try}
import scalus.builtin.Data.{fromData, toData}
import scalus.cardano.address.Network
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.{Utxo as _, *}
import scalus.cardano.txbuilder.Datum.DatumInlined
import scalus.cardano.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler
import scalus.cardano.txbuilder.ScriptSource.PlutusScriptValue
import scalus.cardano.txbuilder.TransactionBuilderStep.{AddCollateral, Send, Spend, ValidityEndSlot}
import scalus.cardano.txbuilder.{
    SomeBuildError,
    ThreeArgumentPlutusScriptWitness,
    TransactionBuilder,
    TransactionUnspentOutput
}

final case class ResolutionTx(
    talliedVoteUtxo: TallyVoteUtxo,
    treasuryUnresolvedUtxoSpent: RuleBasedTreasuryUtxo,
    treasuryResolvedUtxoProduced: RuleBasedTreasuryUtxo,
    tx: Transaction
)

object ResolutionTx {

    case class Recipe(
        talliedVoteUtxo: TallyVoteUtxo,
        treasuryUtxo: RuleBasedTreasuryUtxo,
        collateralUtxo: Utxo[L1],
        validityEndSlot: Long,
        network: Network,
        protocolParams: ProtocolParams,
        evaluator: PlutusScriptEvaluator,
        validators: Seq[Validator]
    )

    enum ResolutionTxError:
        case AbsentVoteDatum(utxo: UtxoIdL1)
        case InvalidVoteDatum(utxo: UtxoIdL1, msg: String)
        case InvalidTreasuryDatum(msg: String)
        case TalliedNoVote
        case TreasuryAlreadyResolved

    def build(recipe: Recipe): Either[SomeBuildError | ResolutionTxError, ResolutionTx] = {

        for {
            voteDetails <- extractVoteDetails(recipe.talliedVoteUtxo)
            treasuryDatum <- extractTreasuryDatum(recipe.treasuryUtxo)
            resolvedTreasuryDatum = mkResolvedTreasuryDatum(treasuryDatum, voteDetails)
            result <- buildResolutionTx(recipe, resolvedTreasuryDatum)
        } yield result
    }

    private def extractVoteDetails(
        talliedUtxo: TallyVoteUtxo
    ): Either[ResolutionTxError, (KzgCommitment, BigInt)] = {
        import ResolutionTxError.*

        val voteOutput = talliedUtxo.utxo.output.untagged
        voteOutput.datumOption match {
            case Some(DatumOption.Inline(datumData)) =>
                Try(fromData[VoteDatum](datumData)) match {
                    case Success(voteDatum) =>
                        voteDatum.voteStatus match {
                            case VoteStatus.AwaitingVote(_) => Left(TalliedNoVote)
                            case VoteStatus.Voted(commitment, versionMinor) =>
                                Right((commitment, versionMinor))
                        }
                    case Failure(e) =>
                        Left(
                          InvalidVoteDatum(
                            talliedUtxo.utxo.input,
                            s"Malformed tallied VoteDatum data: ${datumData.toString}"
                          )
                        )
                }
            case _ =>
                Left(AbsentVoteDatum(talliedUtxo.utxo.input))
        }
    }

    private def extractTreasuryDatum(
        treasuryUtxo: RuleBasedTreasuryUtxo
    ): Either[ResolutionTxError, UnresolvedDatum] = {
        import ResolutionTxError.*

        treasuryUtxo.datum match {
            case RuleBasedTreasuryDatum.Unresolved(unresolved) => Right(unresolved)
            case RuleBasedTreasuryDatum.Resolved(_)            => Left(TreasuryAlreadyResolved)
        }
    }

    private def mkResolvedTreasuryDatum(
        unresolved: UnresolvedDatum,
        voteDetails: (KzgCommitment, BigInt)
    ): RuleBasedTreasuryDatum = {

        val resolvedDatum = ResolvedDatum(
          headMp = unresolved.headMp,
          utxosActive = voteDetails._1,
          version = (unresolved.versionMajor, voteDetails._2),
          params = unresolved.params,
          setup = unresolved.setup
        )

        RuleBasedTreasuryDatum.Resolved(resolvedDatum)
    }

    private def buildResolutionTx(
        recipe: Recipe,
        resolvedTreasuryDatum: RuleBasedTreasuryDatum
    ): Either[SomeBuildError, ResolutionTx] = {
        import recipe.*

        val voteRedeemer = DisputeRedeemer.Resolve
        val treasuryRedeemer = TreasuryRedeemer.Resolve

        val newTreasuryValue = treasuryUtxo.value + talliedVoteUtxo.utxo.output.value

        for {
            context <- TransactionBuilder
                .build(
                  recipe.network,
                  List(
                    // Spend the tallied vote utxo
                    Spend(
                      TransactionUnspentOutput(talliedVoteUtxo.utxo.toScalus),
                      ThreeArgumentPlutusScriptWitness(
                        PlutusScriptValue(DisputeResolutionScript.compiledPlutusV3Script),
                        voteRedeemer.toData,
                        DatumInlined,
                        Set.empty
                      )
                    ),
                    // Spend the treasury utxo and update its datum to resolved state
                    Spend(
                      TransactionUnspentOutput(treasuryUtxo.toUtxo),
                      ThreeArgumentPlutusScriptWitness(
                        PlutusScriptValue(RuleBasedTreasuryScript.compiledPlutusV3Script),
                        treasuryRedeemer.toData,
                        DatumInlined,
                        Set.empty
                      )
                    ),
                    // Send resolved treasury back with resolved datum and total value
                    Send(
                      Babbage(
                        address = treasuryUtxo.addr,
                        value = newTreasuryValue,
                        datumOption = Some(Inline(resolvedTreasuryDatum.toData)),
                        scriptRef = None
                      )
                    ),
                    AddCollateral(TransactionUnspentOutput(collateralUtxo.toScalus)),
                    ValidityEndSlot(recipe.validityEndSlot)
                  )
                )

            finalized <- context
                .finalizeContext(
                  protocolParams = recipe.protocolParams,
                  diffHandler = new ChangeOutputDiffHandler(
                    recipe.protocolParams,
                    0
                  ).changeOutputDiffHandler,
                  evaluator = recipe.evaluator,
                  validators = recipe.validators
                )

            newTreasuryUtxo = RuleBasedTreasuryUtxo(
              beaconTokenName = recipe.treasuryUtxo.beaconTokenName,
              txId = TransactionInput(finalized.transaction.id, 0), // Treasury output at index 0
              addr = recipe.treasuryUtxo.addr,
              datum = resolvedTreasuryDatum,
              value = recipe.treasuryUtxo.value
            )

        } yield ResolutionTx(
          talliedVoteUtxo = recipe.talliedVoteUtxo,
          treasuryUnresolvedUtxoSpent = recipe.treasuryUtxo,
          treasuryResolvedUtxoProduced = newTreasuryUtxo,
          tx = finalized.transaction
        )
    }
}
