package hydrozoa.rulebased.ledger.l1.tx

import cats.implicits.*
import hydrozoa.*
import hydrozoa.config.ScriptReferenceUtxos
import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.owninfo.OwnHeadPeerPrivate
import hydrozoa.lib.cardano.scalus.contextualscalus
import hydrozoa.lib.cardano.scalus.contextualscalus.TransactionBuilder.finalizeContext
import hydrozoa.lib.cardano.scalus.ledger.CollateralUtxo
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.multisig.ledger.block.BlockHeader.Minor
import hydrozoa.multisig.ledger.block.BlockHeader.Minor.HeaderSignature
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.multisig.ledger.l1.tx.Tx
import hydrozoa.multisig.ledger.l1.tx.Tx.Validators.nonSigningValidators
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
import scalus.cardano.onchain.plutus.prelude.List as SList
import scalus.cardano.txbuilder.SomeBuildError
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.*
import scalus.uplc.builtin.Data.fromData
import scalus.uplc.builtin.{ByteString, Data}

final case class VoteTx(
    voteUtxoSpent: VoteUtxo[VoteStatus.AwaitingVote],
    voteUtxoProduced: VoteUtxo[VoteStatus.Voted],
    override val tx: Transaction,
    override val txLens: Lens[VoteTx, Transaction] = Focus[VoteTx](_.tx),
    override val resolvedUtxos: ResolvedUtxos = ResolvedUtxos.empty
) extends Tx[VoteTx]

object VoteTx {
    export VoteTxOps.{Build, Config}
}

private object VoteTxOps {
    type Config = CardanoNetwork.Section & ScriptReferenceUtxos.Section & HeadPeers.Section &
        FallbackContingency.Section & HasTokenNames & OwnHeadPeerPrivate.Section

    object Build {
        enum Error extends Throwable:
            case InvalidVoteDatum(msg: String)
            case VoteAlreadyCast
            case TreasuryParseError(wrapped: RuleBasedTreasuryOutput.ParseError)
            case BuildError(wrapped: SomeBuildError)

            override def toString: String = this.getMessage

            override def getMessage: String = this match {
                case i: Error.InvalidVoteDatum     => s"Invalid vote datum: $i.msg"
                case v: Error.VoteAlreadyCast.type => "Vote has already been cast"
                case b: Error.BuildError =>
                    s"Build error encountered in vote tx. ${b.wrapped.toString}"
                case t: TreasuryParseError => t.wrapped.getMessage
            }
    }

    final case class Build(
        uncastVoteUtxo: VoteUtxo[VoteStatus.AwaitingVote],
        treasuryUtxo: RuleBasedTreasuryUtxo,
        collateralUtxo: CollateralUtxo,
        blockHeader: BlockHeader.Minor.Onchain,
        signatures: List[BlockHeader.Minor.HeaderSignature],
    ) {

        // TODO relocate to "VoteOutput" companion object?
        def parseAndVote(unparsedVoteDatum: Option[DatumOption]): Either[Error, VoteOutput[Voted]] =
            unparsedVoteDatum match {
                case Some(DatumOption.Inline(datumData)) =>
                    Try(fromData[VoteDatum](datumData)) match {
                        case Success(voteDatum) =>
                            voteDatum.voteStatus match {
                                case AwaitingVote(_) =>
                                    Right(
                                      uncastVoteUtxo.voteOutput.castVote(
                                        blockHeader.commitment,
                                        blockHeader.versionMinor
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
            val uncastVoteOutput = uncastVoteUtxo.toUtxo.output

            for {
                newVoteDatum <- parseAndVote(uncastVoteOutput.datumOption)
                votingDeadline <- treasuryUtxo.parseVotingDeadline.left.map(
                  Error.TreasuryParseError(_)
                )
                res <- buildVoteTx(newVoteDatum, votingDeadline).left.map(Error.BuildError(_))
            } yield res
        }

        private def buildVoteTx(
            votedOutput: VoteOutput[Voted],
            votingDeadline: Slot
        )(using config: Config): Either[SomeBuildError, VoteTx] = {

            // Create redeemer for dispute resolution script
            val redeemer = DisputeRedeemer.Vote(
              VoteRedeemer(
                blockHeader,
                SList.from(
                  signatures.map(sig => ByteString.fromArray(IArray.genericWrapArray(sig).toArray))
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
                    uncastVoteUtxo.votingSpend(redeemer),
                    // Send back to the vote contract address with updated datum
                    votedOutput.send,
                    treasuryUtxo.referenceOutput,
                    ValidityEndSlot(votingDeadline.slot)
                  )
                )

                // _ = println(HexUtil.encodeHexString(context.transaction.toCbor))

                finalized <- context
                    .finalizeContext(
                      diffHandler = contextualscalus.Change.changeOutputDiffHandler(0),
                      validators = nonSigningValidators
                    )

            } yield VoteTx(
              voteUtxoSpent = uncastVoteUtxo,
              voteUtxoProduced = VoteUtxo(
                TransactionInput(finalized.transaction.id, 0),
                votedOutput
              ),
              tx = finalized.transaction
            )
        }
    }
}
