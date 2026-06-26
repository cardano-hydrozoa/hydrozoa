package hydrozoa.rulebased.ledger.l1.tx

import hydrozoa.config.ScriptReferenceUtxos
import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.owninfo.OwnPeerPrivate
import hydrozoa.lib.cardano.scalus.contextualscalus
import hydrozoa.lib.cardano.scalus.contextualscalus.TransactionBuilder.{addRequiredSigners, finalizeContext}
import hydrozoa.lib.cardano.scalus.ledger.CollateralUtxo
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.multisig.ledger.l1.tx.EnrichedTx
import hydrozoa.multisig.ledger.l1.tx.EnrichedTx.Validators.nonSigningValidators
import hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionValidator.DisputeRedeemer
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus
import hydrozoa.rulebased.ledger.l1.tx.AbstainTxOps.Build.Error
import hydrozoa.rulebased.ledger.l1.utxo.*
import monocle.*
import scalus.cardano.ledger.{Transaction, TransactionInput}
import scalus.cardano.txbuilder.SomeBuildError
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos

/** Off-chain construction of the **Abstain** action: a peer publicly declines to vote on its own
  * [[BallotBox]] by flipping its datum from `AwaitingVote(peer)` to
  * [[VoteState.VoteStatus.Abstain]]. Once landed on L1, the resulting Abstain utxo is in the Open
  * phase: it can be ratcheted up to Voted by any multisigned SEC, or absorbed by `Tally` (which
  * always waits for `deadlineVoting` to elapse regardless of input statuses).
  */
final case class AbstainTx(
    ballotBoxSpent: BallotBox[VoteStatus.AwaitingVote],
    ballotBoxProduced: BallotBox[VoteStatus.Abstain.type],
    override val tx: Transaction,
    override val txLens: Lens[AbstainTx, Transaction] = Focus[AbstainTx](_.tx),
    override val resolvedUtxos: ResolvedUtxos = ResolvedUtxos.empty
) extends EnrichedTx[AbstainTx] {
    override def transactionFamily: String = "AbstainTx"
}

object AbstainTx {
    export AbstainTxOps.{Build, Config}
}

private object AbstainTxOps {
    type Config = CardanoNetwork.Section & ScriptReferenceUtxos.Section & HeadPeers.Section &
        FallbackContingency.Section & HasTokenNames & OwnPeerPrivate.Section

    object Build {
        enum Error extends Throwable:
            case BuildError(wrapped: SomeBuildError)

            override def toString: String = this.getMessage

            override def getMessage: String = this match {
                case b: Error.BuildError =>
                    s"Build error encountered in abstain tx. ${b.wrapped.toString}"
            }
    }

    /** Builder for an [[AbstainTx]]. No SEC, no peer signatures, no voting deadline, and no
      * treasury reference input — the on-chain `Abstain` action verifies only that the spending
      * peer signed the tx and that the continuing output preserves value/address/key/link with the
      * datum flipped to `Abstain`.
      */
    final case class Build(
        uncastBallotBox: BallotBox[VoteStatus.AwaitingVote],
        collateralUtxo: CollateralUtxo
    ) {

        def result(using config: Config): Either[Build.Error, AbstainTx] =
            buildAbstainTx.left.map(Error.BuildError(_))

        private def buildAbstainTx(using config: Config): Either[SomeBuildError, AbstainTx] = {
            val abstainOutput = uncastBallotBox.ballotBoxOutput.abstain
            val redeemer = DisputeRedeemer.Abstain

            for {
                context <- contextualscalus.TransactionBuilder.build(
                  List(
                    config.referenceDispute,
                    collateralUtxo.add,
                    collateralUtxo.spend,
                    collateralUtxo.collateralOutput.send,
                    // Spend the vote utxo with the Abstain redeemer.
                    uncastBallotBox.votingSpend(redeemer),
                    // Continuing vote utxo with status flipped to Abstain.
                    abstainOutput.send
                  )
                )

                // The peer (read from the AwaitingVote(peer) datum) is the expected signer.
                finalized <- context
                    .addRequiredSigners(uncastBallotBox.votingSigners)
                    .finalizeContext(
                      diffHandler = contextualscalus.Change.changeOutputDiffHandler(0),
                      validators = nonSigningValidators
                    )

            } yield AbstainTx(
              ballotBoxSpent = uncastBallotBox,
              ballotBoxProduced = BallotBox(
                TransactionInput(finalized.transaction.id, 1),
                abstainOutput
              ),
              tx = finalized.transaction
            )
        }
    }
}
