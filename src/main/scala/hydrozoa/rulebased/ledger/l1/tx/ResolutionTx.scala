package hydrozoa.rulebased.ledger.l1.tx

import cats.implicits.*
import hydrozoa.*
import hydrozoa.config.ScriptReferenceUtxos
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.ledger.l1.tx.Tx
import hydrozoa.multisig.ledger.l1.tx.Tx.Validators.nonSigningValidators
import hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionScript
import hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionValidator.DisputeRedeemer
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.TreasuryRedeemer
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.{ResolvedDatum, RuleBasedTreasuryDatum, UnresolvedDatum}
import hydrozoa.rulebased.ledger.l1.state.VoteState
import hydrozoa.rulebased.ledger.l1.state.VoteState.{KzgCommitment, VoteDatum, VoteStatus}
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedTreasuryUtxo, TallyVoteUtxo}
import monocle.*
import scala.util.{Failure, Success, Try}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.EvaluatorMode.EvaluateAndComputeCost
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.txbuilder.Datum.DatumInlined
import scalus.cardano.txbuilder.ScriptSource.{PlutusScriptAttached, PlutusScriptValue}
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.{AddCollateral, Send, Spend}
import scalus.cardano.txbuilder.{Change, PubKeyWitness, SomeBuildError, ThreeArgumentPlutusScriptWitness, TransactionBuilder}
import scalus.uplc.builtin.Data.{fromData, toData}

final case class ResolutionTx(
    talliedVoteUtxo: TallyVoteUtxo,
    treasuryUnresolvedUtxoSpent: RuleBasedTreasuryUtxo,
    treasuryResolvedUtxoProduced: RuleBasedTreasuryUtxo,
    override val tx: Transaction,
    override val txLens: Lens[ResolutionTx, Transaction] = Focus[ResolutionTx](_.tx),
    override val resolvedUtxos: ResolvedUtxos = ResolvedUtxos.empty
) extends Tx[ResolutionTx]

object ResolutionTx {
    export ResolutionTxOps.{Build, Config}
}

private object ResolutionTxOps {
    type Config = CardanoNetwork.Section & ScriptReferenceUtxos.Section

    object Build {
        enum Error extends Throwable:
            case AbsentVoteDatum(utxo: TransactionInput)
            case InvalidVoteDatum(utxo: TransactionInput, msg: String)
            case InvalidTreasuryDatum(msg: String)
            case TalliedNoVote
            case TreasuryAlreadyResolved
            case BuildError(wrapped: SomeBuildError)

            override def getMessage: String = this match {
                case AbsentVoteDatum(utxo: TransactionInput) =>
                    s"Vote datum missing from transaction input ${utxo}"
                case InvalidVoteDatum(utxo: TransactionInput, msg: String) =>
                    s"Vote datum is malformed for transaction input $utxo. $msg"
                case InvalidTreasuryDatum(msg: String) =>
                    s"Treasury datum is invalid. $msg"
                case TalliedNoVote => "Expected to find a tailled vote, but it was absent"
                case TreasuryAlreadyResolved =>
                    "Expected to find an unresolved treasury, but it was resolved."
                case BuildError(wrapped: SomeBuildError) =>
                    s"Build error occurred in resolution tx. ${wrapped.toString}"
            }
    }

    final case class Build(config: Config)(
        _talliedVoteUtxo: TallyVoteUtxo,
        _treasuryUtxo: RuleBasedTreasuryUtxo,
        _collateralUtxo: Utxo,
    ) {
        val talliedVoteUtxo: TallyVoteUtxo = _talliedVoteUtxo
        val treasuryUtxo: RuleBasedTreasuryUtxo = _treasuryUtxo
        val collateralUtxo: Utxo = _collateralUtxo
        def result: Either[Build.Error, ResolutionTx] =
            for {
                voteDetails <- extractVoteDetails(talliedVoteUtxo)
                treasuryDatum <- extractTreasuryDatum(treasuryUtxo)
                resolvedTreasuryDatum = mkResolvedTreasuryDatum(treasuryDatum, voteDetails)
                result <- buildResolutionTx(resolvedTreasuryDatum).left.map(
                  Build.Error.BuildError(_)
                )
            } yield result

        private def extractVoteDetails(
            talliedUtxo: TallyVoteUtxo
        ): Either[Build.Error, (KzgCommitment, BigInt)] = {
            import Build.Error.*

            val voteOutput = talliedUtxo.utxo.output
            voteOutput.datumOption match {
                case Some(DatumOption.Inline(datumData)) =>
                    Try(
                      fromData[VoteDatum](datumData)(using VoteState.given_FromData_VoteDatum)
                    ) match {
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
        ): Either[Build.Error, UnresolvedDatum] = {
            import Build.Error.*

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
              evacuationActive = voteDetails._1,
              version = (unresolved.versionMajor, voteDetails._2),
              setup = unresolved.setup
            )

            RuleBasedTreasuryDatum.Resolved(resolvedDatum)
        }

        private def buildResolutionTx(
            resolvedTreasuryDatum: RuleBasedTreasuryDatum
        ): Either[SomeBuildError, ResolutionTx] = {

            val voteRedeemer = DisputeRedeemer.Resolve
            val treasuryRedeemer = TreasuryRedeemer.Resolve

            val newTreasuryValue = treasuryUtxo.value + talliedVoteUtxo.utxo.output.value

            for {
                context <- TransactionBuilder
                    .build(
                      config.network,
                      List(
                        config.referenceTreasury,
                        // Spend the tallied vote utxo
                        Spend(
                          talliedVoteUtxo.utxo,
                          ThreeArgumentPlutusScriptWitness(
                            PlutusScriptValue(DisputeResolutionScript.compiledPlutusV3Script),
                            voteRedeemer.toData,
                            DatumInlined,
                            Set.empty
                          )
                        ),
                        // Spend the treasury utxo and update its datum to resolved state
                        Spend(
                          treasuryUtxo.asUtxo,
                          ThreeArgumentPlutusScriptWitness(
                            PlutusScriptAttached,
                            treasuryRedeemer.toData,
                            DatumInlined,
                            Set.empty
                          )
                        ),
                        // Send resolved treasury back with resolved datum and total value
                        Send(
                          Babbage(
                            address = treasuryUtxo.address,
                            value = newTreasuryValue,
                            datumOption = Some(Inline(resolvedTreasuryDatum.toData)),
                            scriptRef = None
                          )
                        ),
                        // Change Utxo
                        Send(
                          Babbage(
                            collateralUtxo.output.address,
                            value = Value.zero
                          )
                        ),
                        Spend(collateralUtxo, PubKeyWitness),
                        AddCollateral(collateralUtxo),
                      )
                    )

                finalized <- context
                    .finalizeContext(
                      protocolParams = config.cardanoProtocolParams,
                      diffHandler =
                          Change.changeOutputDiffHandler(_, _, config.cardanoProtocolParams, 1),
                      evaluator = PlutusScriptEvaluator(config.cardanoInfo, EvaluateAndComputeCost),
                      validators = nonSigningValidators
                    )

                newTreasuryUtxo = RuleBasedTreasuryUtxo(
                  utxoId =
                      TransactionInput(finalized.transaction.id, 0), // Treasury output at index 0
                  address = treasuryUtxo.address,
                  datum = resolvedTreasuryDatum,
                  value = treasuryUtxo.value
                )

            } yield ResolutionTx(
              talliedVoteUtxo = talliedVoteUtxo,
              treasuryUnresolvedUtxoSpent = treasuryUtxo,
              treasuryResolvedUtxoProduced = newTreasuryUtxo,
              tx = finalized.transaction
            )
        }
    }
}
