package hydrozoa.rulebased.ledger.l1.tx

import hydrozoa.*
import hydrozoa.config.ScriptReferenceUtxos
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.node.owninfo.OwnPeerPrivate
import hydrozoa.lib.cardano.scalus.contextualscalus
import hydrozoa.lib.cardano.scalus.contextualscalus.TransactionBuilder.{addRequiredSigners, finalizeContext}
import hydrozoa.lib.cardano.scalus.ledger.CollateralUtxo
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.multisig.ledger.block.BlockHeader.Minor
import hydrozoa.multisig.ledger.block.BlockHeader.Minor.HeaderSignature
import hydrozoa.multisig.ledger.l1.tx.Tx
import hydrozoa.multisig.ledger.l1.tx.Tx.Validators.nonSigningValidators
import hydrozoa.multisig.ledger.stack.StandaloneEvacuationCommitment
import hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionValidator.{DisputeRedeemer, VoteRedeemer}
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus.*
import hydrozoa.rulebased.ledger.l1.state.VoteState.{VoteDatum, VoteStatus}
import hydrozoa.rulebased.ledger.l1.tx.VoteTxOps.Build.Error
import hydrozoa.rulebased.ledger.l1.tx.VoteTxOps.Build.Error.{InvalidVoteDatum, VoteAlreadyCast}
import hydrozoa.rulebased.ledger.l1.utxo.*
import monocle.*
import scala.util.{Failure, Success, Try}
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.{BlockHeader as _, *}
import scalus.cardano.onchain.plutus.prelude.{List as SList, Option as SOption}
import scalus.cardano.txbuilder.SomeBuildError
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.fromData

final case class VoteTx(
    ballotBoxSpent: BallotBox[VoteStatus.AwaitingVote],
    ballotBoxProduced: BallotBox[VoteStatus.Voted],
    override val tx: Transaction,
    override val txLens: Lens[VoteTx, Transaction] = Focus[VoteTx](_.tx),
    override val resolvedUtxos: ResolvedUtxos = ResolvedUtxos.empty
) extends Tx[VoteTx] {
    override def transactionFamily: String = "VoteTx"
}

object VoteTx {
    export VoteTxOps.{Build, Config}
}

private object VoteTxOps {
    type Config = HeadConfig.Bootstrap.Section & ScriptReferenceUtxos.Section &
        FallbackContingency.Section & OwnPeerPrivate.Section

    object Build {
        enum Error extends Throwable:
            case InvalidVoteDatum(msg: String)
            case VoteAlreadyCast
            case TreasuryParseError(wrapped: RuleBasedTreasuryOutput.ParseError)
            case BuildError(wrapped: SomeBuildError)

            override def toString: String = this.getMessage

            override def getMessage: String = this match {
                case i: Error.InvalidVoteDatum     => s"Invalid vote datum: $i.msg"
                case _: Error.VoteAlreadyCast.type => "Vote has already been cast"
                case b: Error.BuildError =>
                    s"Build error encountered in vote tx. ${b.wrapped.toString}"
                case t: TreasuryParseError => t.wrapped.getMessage
            }
    }

    final case class Build(
        uncastBallotBox: BallotBox[VoteStatus.AwaitingVote],
        treasuryUtxo: RuleBasedTreasuryUtxo,
        collateralUtxo: CollateralUtxo,
        sec: StandaloneEvacuationCommitment.Onchain,
        signatures: List[BlockHeader.Minor.HeaderSignature],
        coilSignatures: List[Option[BlockHeader.Minor.HeaderSignature]],
    ) {

        // TODO relocate to "BallotBoxOutput" companion object?
        def parseAndVote(
            unparsedVoteDatum: Option[DatumOption]
        ): Either[Error, BallotBoxOutput[Voted]] =
            unparsedVoteDatum match {
                case Some(DatumOption.Inline(datumData)) =>
                    Try(fromData[VoteDatum](datumData)) match {
                        case Success(voteDatum) =>
                            voteDatum.voteStatus match {
                                case AwaitingVote(_) =>
                                    Right(
                                      uncastBallotBox.ballotBoxOutput.castVote(
                                        sec.commitment,
                                        sec.versionMinor
                                      )
                                    )
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

        def result(using config: Config): Either[Build.Error, VoteTx] = {
            import Build.Error

            // Extract current vote datum from the UTXO
            val uncastBallotBoxOutput = uncastBallotBox.toUtxo.output

            for {
                newVoteDatum <- parseAndVote(uncastBallotBoxOutput.datumOption)
                votingDeadline <- treasuryUtxo.parseVotingDeadline.left.map(
                  Error.TreasuryParseError(_)
                )
                res <- buildVoteTx(newVoteDatum, votingDeadline).left.map(Error.BuildError(_))
            } yield res
        }

        private def buildVoteTx(
            votedOutput: BallotBoxOutput[Voted],
            votingDeadline: Slot
        )(using config: Config): Either[SomeBuildError, VoteTx] = {

            // Create redeemer for dispute resolution script
            val redeemer = DisputeRedeemer.Vote(
              VoteRedeemer(
                sec,
                SList.from(
                  signatures.map(sig => ByteString.fromArray(IArray.genericWrapArray(sig).toArray))
                ),
                SList.from(
                  coilSignatures.map {
                      case Some(sig) =>
                          SOption.Some(ByteString.fromArray(IArray.genericWrapArray(sig).toArray))
                      case None =>
                          SOption.None
                  }
                )
              )
            )

            // Build the transaction
            for {
                context <- contextualscalus.TransactionBuilder.build(
                  List(
                    config.referenceDispute,
                    collateralUtxo.add,
                    collateralUtxo.spend,
                    collateralUtxo.collateralOutput.send,
                    // Spend the vote utxo with dispute resolution script witness
                    // So far we use in-place script
                    uncastBallotBox.votingSpend(redeemer),
                    // Send back to the vote contract address with updated datum
                    votedOutput.send,
                    treasuryUtxo.referenceOutput,
                    ValidityEndSlot(votingDeadline.slot)
                  )
                )

                // _ = println(HexUtil.encodeHexString(context.transaction.toCbor))

                finalized <- context
                    .addRequiredSigners(uncastBallotBox.votingSigners)
                    .finalizeContext(
                      diffHandler = contextualscalus.Change.changeOutputDiffHandler(0),
                      validators = nonSigningValidators
                    )

            } yield VoteTx(
              ballotBoxSpent = uncastBallotBox,
              ballotBoxProduced = BallotBox(
                TransactionInput(finalized.transaction.id, 1),
                votedOutput
              ),
              tx = finalized.transaction
            )
        }
    }
}
