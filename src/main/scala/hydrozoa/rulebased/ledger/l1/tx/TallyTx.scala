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
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.multisig.ledger.l1.tx.Tx
import hydrozoa.multisig.ledger.l1.tx.Tx.Validators.nonSigningNonValidityChecksValidators
import hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionValidator.{DisputeRedeemer, TallyRedeemer, maxVote}
import hydrozoa.rulebased.ledger.l1.state.VoteState
import hydrozoa.rulebased.ledger.l1.state.VoteState.*
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedTreasuryOutput, RuleBasedTreasuryUtxo, VoteOutput, VoteUtxo}
import monocle.*
import scalus.cardano.ledger.{Utxo as _, *}
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.*
import scalus.cardano.txbuilder.{SomeBuildError, TransactionBuilder}

final case class TallyTx(
    continuingVoteUtxo: VoteUtxo[VoteStatus],
    removedVoteUtxo: VoteUtxo[VoteStatus],
    treasuryUtxo: RuleBasedTreasuryUtxo,
    override val tx: Transaction,
    override val txLens: Lens[TallyTx, Transaction] = Focus[TallyTx](_.tx),
    override val resolvedUtxos: ResolvedUtxos = ResolvedUtxos.empty
) extends Tx[TallyTx]

object TallyTx {
    export TallyTxOps.{Build, Config}
}

object TallyTxOps {
    type Config = CardanoNetwork.Section & ScriptReferenceUtxos.Section & HeadPeers.Section &
        FallbackContingency.Section & HasTokenNames & OwnHeadPeerPrivate.Section

    object Build {
        enum Error extends Throwable:
            case AbsentVoteDatum(utxo: TransactionInput)
            case MalformedVoteDatum(utxo: TransactionInput, msg: String)
            case IncompatibleVotes(continuing: (Key, Link), removed: (Key, Link))
            case BuildError(wrapped: SomeBuildError)
            case TreasuryParseError(wrapped: RuleBasedTreasuryOutput.ParseError)

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

    private def tallyVoteDatums(
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

    private def tallyVoteUtxos(
        continuing: VoteUtxo[VoteStatus],
        removed: VoteUtxo[VoteStatus]
    ): Either[Build.Error, VoteOutput[VoteStatus]] =
        for {
            talliedDatum <- tallyVoteDatums(
              continuing.voteOutput.datum,
              removed = removed.voteOutput.datum
            )
        } yield VoteOutput(
          key = talliedDatum.key,
          link = talliedDatum.link,
          coin = continuing.voteOutput.coin + removed.voteOutput.coin,
          voteTokens =
              PositiveInt(continuing.voteOutput.voteTokens + removed.voteOutput.voteTokens).get,
          status = talliedDatum.voteStatus
        )

    final case class Build(
        continuingVoteUtxo: VoteUtxo[VoteStatus],
        removedVoteUtxo: VoteUtxo[VoteStatus],
        treasuryUtxo: RuleBasedTreasuryUtxo,
        collateralUtxo: CollateralUtxo,
    )(using config: Config) {

        def result: Either[Build.Error, TallyTx] =
            for {
                tallied <- tallyVoteUtxos(continuingVoteUtxo, removedVoteUtxo)
                votingDeadline <- treasuryUtxo.parseVotingDeadline.left.map(
                  Build.Error.TreasuryParseError(_)
                )
                result <- buildTallyTx(tallied, votingDeadline).left.map(Build.Error.BuildError(_))
            } yield result

        private def buildTallyTx(
            tallied: VoteOutput[VoteStatus],
            votingDeadline: Slot
        ): Either[SomeBuildError, TallyTx] = {
            val continuingRedeemer = DisputeRedeemer.Tally(TallyRedeemer.Continuing)
            val removedRedeemer = DisputeRedeemer.Tally(TallyRedeemer.Removed)

//            // TODO: The AddCollateral step should really add the signer to expected signers automatically,
//            //  but we don't have that yet. So I'll add it on the vote utxo instead.
//            val collateralSigner = ExpectedSigner(
//              collateralUtxo.collateralOutput.addrKeyHash
//            )

            for {
                context <- contextualscalus.TransactionBuilder
                    .build(
                      List(
                        config.referenceDispute,
                        // Spend the continuing vote utxo with tally redeemer
                        continuingVoteUtxo.spend(continuingRedeemer),
                        // Spend the removed vote utxo with tally redeemer
                        removedVoteUtxo.spend(removedRedeemer),
                        // Send back the continuing vote utxo (the removed one is consumed)
                        tallied.send,
                        treasuryUtxo.referenceOutput,
                        collateralUtxo.add,
                        ValidityStartSlot(votingDeadline.slot)
                      )
                    )

                finalized <- context
                    .finalizeContext(
                      diffHandler = contextualscalus.Change.changeOutputDiffHandler(0),
                      validators = nonSigningNonValidityChecksValidators
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
