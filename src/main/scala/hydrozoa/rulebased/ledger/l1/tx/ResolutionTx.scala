package hydrozoa.rulebased.ledger.l1.tx

import cats.implicits.*
import hydrozoa.*
import hydrozoa.config.ScriptReferenceUtxos
import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.owninfo.OwnHeadPeerPrivate
import hydrozoa.lib.cardano.scalus.contextualscalus.Change
import hydrozoa.lib.cardano.scalus.contextualscalus.TransactionBuilder.{build, finalizeContext}
import hydrozoa.lib.cardano.scalus.ledger.CollateralUtxo
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.multisig.ledger.l1.tx.Tx
import hydrozoa.multisig.ledger.l1.tx.Tx.Validators.nonSigningValidators
import hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionValidator.DisputeRedeemer
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.TreasuryRedeemer
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.{Resolved, Unresolved}
import hydrozoa.rulebased.ledger.l1.state.VoteState
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus.Voted
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedTreasuryOutput, RuleBasedTreasuryUtxo, VoteUtxo}
import monocle.*
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.SomeBuildError
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos

final case class ResolutionTx(
    talliedVoteUtxo: VoteUtxo[Voted],
    treasuryUnresolvedUtxoSpent: RuleBasedTreasuryUtxo,
    treasuryResolvedUtxoProduced: RuleBasedTreasuryUtxo,
    override val tx: Transaction,
    override val txLens: Lens[ResolutionTx, Transaction] = Focus[ResolutionTx](_.tx),
    override val resolvedUtxos: ResolvedUtxos = ResolvedUtxos.empty
) extends Tx[ResolutionTx] {
    override def transactionFamily: String = "Resolution"
}

object ResolutionTx {
    export ResolutionTxOps.{Build, Config}
}

private object ResolutionTxOps {
    type Config = CardanoNetwork.Section & ScriptReferenceUtxos.Section & HeadPeers.Section &
        FallbackContingency.Section & HasTokenNames & OwnHeadPeerPrivate.Section

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

        // TODO: Move to a method on RuleBasedTreasuryUtxo
        private def extractTreasuryDatum(
            treasuryUtxo: RuleBasedTreasuryUtxo
        ): Either[Build.Error, Unresolved] = {
            import Build.Error.*

            treasuryUtxo.treasuryOutput.datum match {
                case unresolved: RuleBasedTreasuryDatum.Unresolved => Right(unresolved)
                case _: RuleBasedTreasuryDatum.Resolved            => Left(TreasuryAlreadyResolved)
            }
        }

        // TODO: move to a method on RuleBasedTreasuryUtxo after adding a type tag for the datum
        private def mkResolvedTreasuryDatum(
            unresolved: Unresolved,
            voteDetails: VoteStatus.Voted
        ): RuleBasedTreasuryDatum = {
            Resolved(
              evacuationActive = voteDetails._1,
              version = (unresolved.versionMajor, voteDetails._2),
              setup = unresolved.setup
            )
        }

        private def buildResolutionTx(
            resolvedTreasuryDatum: RuleBasedTreasuryDatum
        ): Either[SomeBuildError, ResolutionTx] = {

            val voteRedeemer = DisputeRedeemer.Resolve
            val treasuryRedeemer = TreasuryRedeemer.Resolve

            val newTreasuryValue =
                treasuryUtxo.treasuryOutput.value + talliedVoteUtxo.voteOutput.toOutput.value

            // TODO: Partial, we can definitely find a way to make this more type safe
            val newTreasury = RuleBasedTreasuryOutput(resolvedTreasuryDatum, newTreasuryValue)

            for {
                context <-
                    build(
                      List(
                        config.referenceTreasury,
                        config.referenceDispute,
                        // Spend the tallied vote utxo
                        talliedVoteUtxo.spend(voteRedeemer),
                        // Spend the treasury utxo and update its datum to resolved state
                        treasuryUtxo.spendAttached(treasuryRedeemer),
                        // Send resolved treasury back with resolved datum and total value
                        newTreasury.send,
                        collateralUtxo.add,
                        collateralUtxo.spend,
                        collateralUtxo.collateralOutput.send,
                      )
                    )

                finalized <- context
                    .finalizeContext(
                      diffHandler = Change.changeOutputDiffHandler(1),
                      validators = nonSigningValidators
                    )

                newTreasuryUtxo = RuleBasedTreasuryUtxo(
                  utxoId =
                      TransactionInput(finalized.transaction.id, 0), // Treasury output at index 0
                  treasuryOutput = newTreasury
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
