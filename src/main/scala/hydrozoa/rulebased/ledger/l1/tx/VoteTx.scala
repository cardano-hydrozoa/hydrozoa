package hydrozoa.rulebased.ledger.l1.tx

import cats.implicits.*
import hydrozoa.*
import hydrozoa.config.ScriptReferenceUtxos
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.ledger.CollateralUtxo
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.multisig.ledger.block.BlockHeader.Minor
import hydrozoa.multisig.ledger.block.BlockHeader.Minor.HeaderSignature
import hydrozoa.multisig.ledger.l1.tx.Tx
import hydrozoa.multisig.ledger.l1.tx.Tx.Validators.nonSigningValidators
import hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionValidator.{DisputeRedeemer, VoteRedeemer}
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus.*
import hydrozoa.rulebased.ledger.l1.state.VoteState.{VoteDatum, VoteStatus}
import hydrozoa.rulebased.ledger.l1.tx.VoteTxOps.Build.Error.{InvalidVoteDatum, VoteAlreadyCast}
import hydrozoa.rulebased.ledger.l1.utxo.{OwnVoteUtxo, RuleBasedTreasuryUtxo, VoteUtxoCast}
import monocle.*
import scala.util.{Failure, Success, Try}
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.EvaluatorMode.EvaluateAndComputeCost
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{BlockHeader as _, *}
import scalus.cardano.onchain.plutus.prelude.List as SList
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.Datum.DatumInlined
import scalus.cardano.txbuilder.ScriptSource.PlutusScriptAttached
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.*
import scalus.uplc.builtin.Data.{fromData, toData}
import scalus.uplc.builtin.{ByteString, Data}

final case class VoteTx(
    // TODO: what we want to keep here if anything?
    voteUtxoSpent: OwnVoteUtxo,
    voteUtxoProduced: VoteUtxoCast,
    override val tx: Transaction,
    override val txLens: Lens[VoteTx, Transaction] = Focus[VoteTx](_.tx),
    override val resolvedUtxos: ResolvedUtxos = ResolvedUtxos.empty
) extends Tx[VoteTx]

object VoteTx {
    export VoteTxOps.{Build, Config}
}

private object VoteTxOps {
    type Config = CardanoNetwork.Section & ScriptReferenceUtxos.Section

    object Build {
        enum Error extends Throwable:
            case InvalidVoteDatum(msg: String)
            case VoteAlreadyCast
            case BuildError(wrapped: SomeBuildError)

            override def toString: String = this.getMessage

            override def getMessage: String = this match {
                case i: Error.InvalidVoteDatum     => s"Invalid vote datum: $i.msg"
                case v: Error.VoteAlreadyCast.type => "Vote has already been cast"
                case b: Error.BuildError =>
                    s"Build error encountered in vote tx. ${b.wrapped.toString}"
            }
    }

    final case class Build(config: Config)(
        _voteUtxo: OwnVoteUtxo,
        _treasuryUtxo: RuleBasedTreasuryUtxo,
        _collateralUtxo: CollateralUtxo,
        _blockHeader: BlockHeader.Minor.Onchain,
        _signatures: List[BlockHeader.Minor.HeaderSignature],
    ) {
        val voteUtxo: OwnVoteUtxo = _voteUtxo
        val treasuryUtxo: RuleBasedTreasuryUtxo = _treasuryUtxo
        val collateralUtxo: CollateralUtxo = _collateralUtxo
        val blockHeader: Minor.Onchain = _blockHeader
        val signatures: List[BlockHeader.Minor.HeaderSignature] = _signatures

        def result: Either[Build.Error, VoteTx] = {
            import Build.Error

            // Extract current vote datum from the UTXO
            val voteOutput = voteUtxo.utxo.output

            voteOutput.datumOption match {
                case Some(DatumOption.Inline(datumData)) =>
                    Try(fromData[VoteDatum](datumData)) match {
                        case Success(voteDatum) =>
                            voteDatum.voteStatus match {
                                case AwaitingVote(_) =>
                                    val updatedVoteDatum = voteDatum.copy(
                                      voteStatus = VoteStatus.Voted(
                                        blockHeader.commitment,
                                        blockHeader.versionMinor
                                      )
                                    )
                                    buildVoteTx(updatedVoteDatum).left.map(Error.BuildError(_))
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
            datumWithVote: VoteDatum
        ): Either[SomeBuildError, VoteTx] = {

            // Get the TransactionInput and TransactionOutput from VoteUtxo
            val (voteInput, voteOutput) =
                (voteUtxo.utxo.input, voteUtxo.utxo.output)

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
                context <- TransactionBuilder
                    .build(
                      config.network,
                      List(
                        config.referenceDispute,
                        collateralUtxo.add,
                        collateralUtxo.spend,
                        collateralUtxo.sendContinuing,
                        // Spend the vote utxo with dispute resolution script witness
                        // So far we use in-place script
                        Spend(
                          Utxo(voteInput, voteOutput),
                          ThreeArgumentPlutusScriptWitness(
                            PlutusScriptAttached,
                            redeemer.toData,
                            DatumInlined,
                            // Set.empty
                            Set(ExpectedSigner(voteUtxo.voter))
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
                        ReferenceOutput(Utxo(treasuryUtxo.asTuple)),
                      )
                    )

                // _ = println(HexUtil.encodeHexString(context.transaction.toCbor))

                finalized <- context
                    .finalizeContext(
                      protocolParams = config.cardanoProtocolParams,
                      diffHandler =
                          Change.changeOutputDiffHandler(_, _, config.cardanoProtocolParams, 0),
                      evaluator = PlutusScriptEvaluator(config.cardanoInfo, EvaluateAndComputeCost),
                      validators = nonSigningValidators
                    )

            } yield VoteTx(
              voteUtxoSpent = voteUtxo,
              voteUtxoProduced = VoteUtxoCast(
                Utxo(
                  TransactionInput(finalized.transaction.id, 0), // Vote output is at index 0
                  finalized.transaction.body.value.outputs(0).value
                  // The updated vote output
                )
              ),
              tx = finalized.transaction
            )
        }
    }
}
