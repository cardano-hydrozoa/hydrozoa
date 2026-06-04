package hydrozoa.rulebased.ledger.l1.tx

import cats.implicits.*
import hydrozoa.config.ScriptReferenceUtxos
import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.owninfo.OwnHeadPeerPrivate
import hydrozoa.lib.cardano.scalus.contextualscalus
import hydrozoa.lib.cardano.scalus.contextualscalus.TransactionBuilder.finalizeContext
import hydrozoa.lib.cardano.scalus.ledger.CollateralUtxo
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.multisig.ledger.l1.tx.Tx
import hydrozoa.multisig.ledger.l1.tx.Tx.Validators.nonSigningValidators
import hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionValidator.DisputeRedeemer
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus
import hydrozoa.rulebased.ledger.l1.tx.AbstainTxOps.Build.Error
import hydrozoa.rulebased.ledger.l1.utxo.*
import monocle.*
import scalus.cardano.ledger.{Transaction, TransactionInput}
import scalus.cardano.txbuilder.SomeBuildError
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos

/** Off-chain construction of the **Abstain** action: a peer publicly declines to vote on its own
  * [[VoteUtxo]] by flipping its datum from `AwaitingVote(peer)` to
  * [[VoteState.VoteStatus.Abstain]]. Once landed on L1, the resulting Abstain utxo is terminal —
  * the `Tally` action can absorb it before the voting deadline without further peer signatures (see
  * [[hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionValidator.isAwaiting]]).
  */
final case class AbstainTx(
    voteUtxoSpent: VoteUtxo[VoteStatus.AwaitingVote],
    voteUtxoProduced: VoteUtxo[VoteStatus.Abstain.type],
    override val tx: Transaction,
    override val txLens: Lens[AbstainTx, Transaction] = Focus[AbstainTx](_.tx),
    override val resolvedUtxos: ResolvedUtxos = ResolvedUtxos.empty
) extends Tx[AbstainTx] {
    override def transactionFamily: String = "AbstainTx"
}

object AbstainTx {
    export AbstainTxOps.{Build, Config}
}

private object AbstainTxOps {
    type Config = CardanoNetwork.Section & ScriptReferenceUtxos.Section & HeadPeers.Section &
        FallbackContingency.Section & HasTokenNames & OwnHeadPeerPrivate.Section

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
        uncastVoteUtxo: VoteUtxo[VoteStatus.AwaitingVote],
        collateralUtxo: CollateralUtxo
    ) {

        def result(using config: Config): Either[Build.Error, AbstainTx] =
            buildAbstainTx.left.map(Error.BuildError(_))

        private def buildAbstainTx(using config: Config): Either[SomeBuildError, AbstainTx] = {
            val abstainOutput = uncastVoteUtxo.voteOutput.abstain
            val redeemer = DisputeRedeemer.Abstain

            for {
                context <- contextualscalus.TransactionBuilder.build(
                  List(
                    config.referenceDispute,
                    collateralUtxo.add,
                    collateralUtxo.spend,
                    collateralUtxo.collateralOutput.send,
                    // Spend the vote utxo with the Abstain redeemer; votingSpend wires the peer
                    // (read from the AwaitingVote(peer) datum) into expected signers.
                    uncastVoteUtxo.votingSpend(redeemer),
                    // Continuing vote utxo with status flipped to Abstain.
                    abstainOutput.send
                  )
                )

                finalized <- context.finalizeContext(
                  diffHandler = contextualscalus.Change.changeOutputDiffHandler(0),
                  validators = nonSigningValidators
                )

            } yield AbstainTx(
              voteUtxoSpent = uncastVoteUtxo,
              voteUtxoProduced = VoteUtxo(
                TransactionInput(finalized.transaction.id, 1),
                abstainOutput
              ),
              tx = finalized.transaction
            )
        }
    }
}
