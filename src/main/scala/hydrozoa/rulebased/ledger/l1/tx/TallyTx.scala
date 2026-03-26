package hydrozoa.rulebased.ledger.l1.tx

import cats.implicits.*
import hydrozoa.*
import hydrozoa.config.ScriptReferenceUtxos
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.ledger.CollateralUtxo
import hydrozoa.multisig.ledger.l1.tx.Tx
import hydrozoa.multisig.ledger.l1.tx.Tx.Validators.nonSigningValidators
import hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionValidator.{DisputeRedeemer, TallyRedeemer, maxVote}
import hydrozoa.rulebased.ledger.l1.state.VoteState
import hydrozoa.rulebased.ledger.l1.state.VoteState.*
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedTreasuryUtxo, TallyVoteUtxo}
import monocle.*
import scala.util.{Failure, Success, Try}
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.EvaluatorMode.EvaluateAndComputeCost
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{Utxo as SUtxo, *}
import scalus.cardano.txbuilder.Datum.DatumInlined
import scalus.cardano.txbuilder.ScriptSource.PlutusScriptAttached
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.*
import scalus.cardano.txbuilder.{Change, ExpectedSigner, SomeBuildError, ThreeArgumentPlutusScriptWitness, TransactionBuilder}
import scalus.uplc.builtin.Data.{fromData, toData}

final case class TallyTx(
    continuingVoteUtxo: TallyVoteUtxo,
    removedVoteUtxo: TallyVoteUtxo,
    treasuryUtxo: RuleBasedTreasuryUtxo,
    override val tx: Transaction,
    override val txLens: Lens[TallyTx, Transaction] = Focus[TallyTx](_.tx),
    override val resolvedUtxos: ResolvedUtxos = ResolvedUtxos.empty
) extends Tx[TallyTx]

object TallyTx {
    export TallyTxOps.{Build, Config}
}

object TallyTxOps {
    type Config = CardanoNetwork.Section & ScriptReferenceUtxos.Section

    object Build {
        enum Error extends Throwable:
            case AbsentVoteDatum(utxo: TransactionInput)
            case MalformedVoteDatum(utxo: TransactionInput, msg: String)
            case IncompatibleVotes(continuing: (Key, Link), removed: (Key, Link))
            case BuildError(wrapped: SomeBuildError)

            override def getMessage: String = this match {
                case AbsentVoteDatum(utxo: TransactionInput) =>
                    s"Expected to find a vote datum, but tx input $utxo lacks a datum"
                case MalformedVoteDatum(utxo: TransactionInput, msg: String) =>
                    s"Expected to find a vote datum, but parsing the tx input $utxo yielded the following message: $msg"
                case IncompatibleVotes(continuing: (Key, Link), removed: (Key, Link)) =>
                    s"Tried to tally incompatible votes. The continuing vote `(Key, Link)` is $continuing, but the " +
                        s"removed on is $removed"
                case BuildError(wrapped: SomeBuildError) =>
                    s"Encountered a build error in the tallying tx: ${wrapped.toString}"
            }

    }

    final case class Build(config: Config)(
        _continuingVoteUtxo: TallyVoteUtxo,
        _removedVoteUtxo: TallyVoteUtxo,
        _treasuryUtxo: RuleBasedTreasuryUtxo,
        _collateralUtxo: CollateralUtxo,
    ) {
        val continuingVoteUtxo: TallyVoteUtxo = _continuingVoteUtxo
        val removedVoteUtxo: TallyVoteUtxo = _removedVoteUtxo
        val treasuryUtxo: RuleBasedTreasuryUtxo = _treasuryUtxo
        val collateralUtxo: CollateralUtxo = _collateralUtxo

        def result: Either[Build.Error, TallyTx] =
            for {
                continuingVoteDatum <- extractVoteDatum(continuingVoteUtxo)
                removedVoteDatum <- extractVoteDatum(removedVoteUtxo)
                outputDatum <- tallyVotes(continuingVoteDatum, removedVoteDatum)
                result <- buildTallyTx(outputDatum).left.map(Build.Error.BuildError(_))
            } yield result

        private def extractVoteDatum(
            voteUtxo: TallyVoteUtxo
        ): Either[Build.Error, VoteDatum] = {
            import Build.Error.*

            val voteOutput = voteUtxo.utxo.output
            voteOutput.datumOption match {
                case Some(DatumOption.Inline(datumData)) =>
                    Try(
                      fromData[VoteDatum](datumData)(using VoteState.given_FromData_VoteDatum)
                    ) match {
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
        ): Either[Build.Error, VoteDatum] = {
            import Build.Error.*

            if continuing.link != removed.key
            then
                Left(
                  IncompatibleVotes((continuing.key, continuing.link), (removed.key, removed.link))
                )
            else {
                val higherVote = maxVote(
                  continuing.voteStatus,
                  removed.voteStatus
                )
                Right(VoteDatum(continuing.key, removed.link, higherVote))
            }
        }

        private def buildTallyTx(
            outputDatum: VoteDatum
        ): Either[SomeBuildError, TallyTx] = {
            val outputValue = continuingVoteUtxo.utxo.output.value +
                removedVoteUtxo.utxo.output.value

            val continuingRedeemer = DisputeRedeemer.Tally(TallyRedeemer.Continuing)
            val removedRedeemer = DisputeRedeemer.Tally(TallyRedeemer.Removed)

            // TODO: The AddCollateral step should really add the signer to expected signers automatically,
            //  but we don't have that yet. So I'll add it on the vote utxo instead.
            val collateralSigner = ExpectedSigner(collateralUtxo.addrKeyHash)

            for {
                context <- TransactionBuilder
                    .build(
                      config.network,
                      List(
                        config.referenceDispute,
                        // Spend the continuing vote utxo with tally redeemer
                        Spend(
                          continuingVoteUtxo.utxo,
                          ThreeArgumentPlutusScriptWitness(
                            PlutusScriptAttached,
                            continuingRedeemer.toData,
                            DatumInlined,
                            Set(collateralSigner)
                          )
                        ),
                        // Spend the removed vote utxo with tally redeemer
                        Spend(
                          removedVoteUtxo.utxo,
                          ThreeArgumentPlutusScriptWitness(
                            PlutusScriptAttached,
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
                        ReferenceOutput(SUtxo(treasuryUtxo.asTuple._1, treasuryUtxo.asTuple._2)),
                        collateralUtxo.add,
                      )
                    )

                finalized <- context
                    .finalizeContext(
                      protocolParams = config.cardanoProtocolParams,
                      diffHandler =
                          Change.changeOutputDiffHandler(_, _, config.cardanoProtocolParams, 0),
                      evaluator = PlutusScriptEvaluator(
                        config.cardanoInfo,
                        EvaluateAndComputeCost
                      ),
                      validators = nonSigningValidators
                    )

            } yield TallyTx(
              continuingVoteUtxo = continuingVoteUtxo,
              removedVoteUtxo = removedVoteUtxo,
              treasuryUtxo = treasuryUtxo,
              tx = finalized.transaction
            )
        }
    }
}
