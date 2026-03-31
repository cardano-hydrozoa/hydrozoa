package hydrozoa.rulebased.ledger.l1.tx

import cats.implicits.*
import hydrozoa.*
import hydrozoa.config.ScriptReferenceUtxos
import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.cardano.scalus.implicitscalus.TransactionBuilder.{build, finalizeContext}
import hydrozoa.lib.cardano.scalus.ledger.CollateralUtxo
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.multisig.ledger.l1.tx.Tx
import hydrozoa.multisig.ledger.l1.tx.Tx.Validators.nonSigningValidators
import hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionScript
import hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionValidator.DisputeRedeemer
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.TreasuryRedeemer
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.{ResolvedDatum, RuleBasedTreasuryDatum, UnresolvedDatum}
import hydrozoa.rulebased.ledger.l1.state.VoteState
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus.Voted
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedTreasuryUtxo, VoteUtxo}
import monocle.*
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.txbuilder.Datum.DatumInlined
import scalus.cardano.txbuilder.ScriptSource.{PlutusScriptAttached, PlutusScriptValue}
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.{Send, Spend}
import scalus.cardano.txbuilder.{Change, SomeBuildError, ThreeArgumentPlutusScriptWitness}
import scalus.uplc.builtin.Data.toData

final case class ResolutionTx(
    talliedVoteUtxo: VoteUtxo[Voted],
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
    type Config = CardanoNetwork.Section & ScriptReferenceUtxos.Section & HeadPeers.Section &
        FallbackContingency.Section & HasTokenNames

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

    final case class Build(
        talliedVoteUtxo: VoteUtxo[Voted],
        treasuryUtxo: RuleBasedTreasuryUtxo,
        collateralUtxo: CollateralUtxo,
    )(using config: Config) {
        def result: Either[Build.Error, ResolutionTx] =
            for {
                treasuryDatum <- extractTreasuryDatum(treasuryUtxo)
                resolvedTreasuryDatum = mkResolvedTreasuryDatum(
                  treasuryDatum,
                  talliedVoteUtxo.voteOutput.status
                )
                result <- buildResolutionTx(resolvedTreasuryDatum).left.map(
                  Build.Error.BuildError(_)
                )
            } yield result

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
            voteDetails: VoteStatus.Voted
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
        )(using config: Config): Either[SomeBuildError, ResolutionTx] = {

            val voteRedeemer = DisputeRedeemer.Resolve
            val treasuryRedeemer = TreasuryRedeemer.Resolve

            val newTreasuryValue = treasuryUtxo.value + talliedVoteUtxo.voteOutput.toOutput.value

            for {
                context <-
                    build(
                      List(
                        config.referenceTreasury,
                        // Spend the tallied vote utxo
                        Spend(
                          talliedVoteUtxo.toUtxo,
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
                        collateralUtxo.add,
                        collateralUtxo.spend,
                        collateralUtxo.collateralOutput.sendContinuing,
                      )
                    )

                finalized <- context
                    .finalizeContext(
                      diffHandler =
                          Change.changeOutputDiffHandler(_, _, config.cardanoProtocolParams, 1),
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
