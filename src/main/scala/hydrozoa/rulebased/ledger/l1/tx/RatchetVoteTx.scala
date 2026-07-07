package hydrozoa.rulebased.ledger.l1.tx

import hydrozoa.*
import hydrozoa.config.ScriptReferenceUtxos
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.node.owninfo.OwnPeerPrivate
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.addrKeyHash
import hydrozoa.lib.cardano.scalus.contextualscalus
import hydrozoa.lib.cardano.scalus.contextualscalus.TransactionBuilder.{addRequiredSigners, finalizeContext}
import hydrozoa.lib.cardano.scalus.ledger.CollateralUtxo
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.multisig.ledger.l1.tx.EnrichedTx.Validators.nonSigningValidators
import hydrozoa.multisig.ledger.l1.tx.{EnrichedTx, TxFamily}
import hydrozoa.multisig.ledger.stack.StandaloneEvacuationCommitment
import hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionValidator.{DisputeRedeemer, VoteRedeemer}
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus.{Abstain, Voted}
import hydrozoa.rulebased.ledger.l1.tx.RatchetVoteTxOps.Build.Error
import hydrozoa.rulebased.ledger.l1.utxo.*
import monocle.*
import scalus.cardano.ledger.{BlockHeader as _, *}
import scalus.cardano.onchain.plutus.prelude.{List as SList, Option as SOption}
import scalus.cardano.txbuilder.SomeBuildError
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.*
import scalus.uplc.builtin.ByteString

/** Ratchet a public (Open-phase) ballot box forward with a multisigned SEC. Structurally the same
  * on-chain tx as [[VoteTx]] (identical [[DisputeRedeemer.Vote]] redeemer + treasury reference +
  * collateral), but the spent box is `Voted`/`Abstain` rather than `AwaitingVote` and the Plutus
  * script skips the tx-signer check — only the SEC's multisig gates the transition (see
  * [[VoteState.VoteStatus.Voted]] / [[VoteState.VoteStatus.Abstain]]).
  */
final case class RatchetVoteTx(
    ballotBoxSpent: BallotBox[Voted | Abstain.type],
    ballotBoxProduced: BallotBox[Voted],
    override val tx: Transaction,
    override val txLens: Lens[RatchetVoteTx, Transaction] = Focus[RatchetVoteTx](_.tx),
    override val resolvedUtxos: ResolvedUtxos
) extends EnrichedTx[RatchetVoteTx] {}

object RatchetVoteTx {
    given TxFamily[RatchetVoteTx] = TxFamily.of("RatchetVoteTx")
    export RatchetVoteTxOps.{Build, Config}
}

private object RatchetVoteTxOps {
    type Config = HeadConfig.Bootstrap.Section & ScriptReferenceUtxos.Section &
        FallbackContingency.Section & OwnPeerPrivate.Section

    object Build {
        enum Error extends Throwable:
            case NotMonotonic(wrapped: RatchetNotMonotonic)
            case TreasuryParseError(wrapped: RuleBasedTreasuryOutput.ParseError)
            case BuildError(wrapped: SomeBuildError)

            override def toString: String = this.getMessage

            override def getMessage: String = this match {
                case Error.NotMonotonic(w) =>
                    s"Ratchet violates versionMinor monotonicity: prev=${w.prevVersionMinor}, " +
                        s"proposed=${w.proposedVersionMinor}"
                case Error.TreasuryParseError(w) => w.getMessage
                case Error.BuildError(w) =>
                    s"Build error encountered in ratchet vote tx. ${w.toString}"
            }
    }

    final case class Build(
        openBallotBox: BallotBox[Voted | Abstain.type],
        treasuryUtxo: RuleBasedTreasuryUtxo,
        collateralUtxo: CollateralUtxo,
        sec: StandaloneEvacuationCommitment.Onchain,
        signatures: List[BlockHeader.Minor.HeaderSignature],
        coilSignatures: List[Option[BlockHeader.Minor.HeaderSignature]],
    ) {

        def result(using config: Config): Either[Error, RatchetVoteTx] =
            for {
                newVoteOutput <- openBallotBox.ballotBoxOutput
                    .ratchet(sec.commitment, sec.versionMinor)
                    .left
                    .map(Error.NotMonotonic(_))
                votingDeadline <- treasuryUtxo.parseVotingDeadline.left.map(
                  Error.TreasuryParseError(_)
                )
                res <- buildRatchetTx(newVoteOutput, votingDeadline).left.map(Error.BuildError(_))
            } yield res

        private def buildRatchetTx(
            votedOutput: BallotBoxOutput[Voted],
            votingDeadline: Slot
        )(using config: Config): Either[SomeBuildError, RatchetVoteTx] = {
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

            for {
                context <- contextualscalus.TransactionBuilder.build(
                  List(
                    config.referenceDispute,
                    collateralUtxo.add,
                    collateralUtxo.spend,
                    collateralUtxo.collateralOutput.send,
                    openBallotBox.spend(redeemer),
                    votedOutput.send,
                    treasuryUtxo.referenceOutput,
                    ValidityEndSlot(votingDeadline.slot)
                  )
                )

                // On-chain script skips the tx-signer check for Open-phase boxes, but the
                // collateral input still needs its owner's signature. Declaring the own wallet's
                // key hash as a required signer keeps fee sizing accurate.
                finalized <- context
                    .addRequiredSigners(
                      Set(config.ownWallet.exportVerificationKey.addrKeyHash)
                    )
                    .finalizeContext(
                      diffHandler = contextualscalus.Change.changeOutputDiffHandler(0),
                      validators = nonSigningValidators
                    )
            } yield RatchetVoteTx(
              ballotBoxSpent = openBallotBox,
              ballotBoxProduced = BallotBox(
                TransactionInput(finalized.transaction.id, 1),
                votedOutput
              ),
              tx = finalized.transaction,
              resolvedUtxos = finalized.resolvedUtxos
            )
        }
    }
}
