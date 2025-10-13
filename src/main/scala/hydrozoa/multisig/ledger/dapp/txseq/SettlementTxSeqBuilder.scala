package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.NonEmptyList
import cats.implicits.*
import hydrozoa.lib.tx.TransactionUnspentOutput
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.txseq.tx.{RolloutTx, SettlementTx}
import hydrozoa.multisig.ledger.dapp.txseq.txbak.RolloutTxSeq
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, TreasuryUtxo}
import hydrozoa.multisig.ledger.joint.utxo.Payout
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.txbuilder.Environment

import scala.collection.immutable.Queue


case class SettlementTxSeqBuilder(
                                     majorVersion: Int,
                                     deposits: Queue[DepositUtxo],
                                     payouts: Vector[Payout.Obligation.L1],
                                     treasuryUtxo: TreasuryUtxo,
                                     headNativeScript: HeadMultisigScript,
                                     // The reference script for the HNS should live inside the multisig regime witness UTxO
                                     headNativeScriptReferenceInput: TransactionUnspentOutput,
                                     env: Environment,
                                     validators: Seq[Validator]
                                 ) {

    private val rolloutSeqBuilder = RolloutTxSeq.Builder(
        hnsReferenceOutput = headNativeScriptReferenceInput,
        headNativeScript = headNativeScript,
        env = env, validators = validators)

    def build(): Either[Error,
        (SettlementTx.Builder.Result, List[RolloutTx])] =
        for {
            rolloutPartialResults <- rolloutSeqBuilder.mkPartialChain(payouts).left.map(Error.RolloutSeqError(_))

            firstRolloutPR = rolloutPartialResults._1

            settlementTx <- SettlementTx.Builder.WithPayouts(
                majorVersion = majorVersion,
                deposits = deposits,
                firstRolloutTxPartial = firstRolloutPR,
                treasuryUtxo = treasuryUtxo,
                headNativeScript = headNativeScript,
                headNativeScriptReferenceInput = headNativeScriptReferenceInput,
                env = env,
                validators = validators
            ).build().left.map(Error.SettlementError(_))

            finishedRolloutChain <- settlementTx match {
                case tx: SettlementTx.Builder.Result.NoPayouts => Right(List.empty)
                case tx: SettlementTx.Builder.Result.WithPayouts => tx.isFirstRolloutMerged match {
                    case SettlementTx.Builder.IsFirstRolloutMerged.Merged => {
                        val rolloutTail = rolloutPartialResults._2
                        for {
                            rollouts <-
                                if rolloutTail.isEmpty
                                then Right(List.empty)
                                else rolloutSeqBuilder.finishPartialChain(NonEmptyList.fromListUnsafe(rolloutTail),
                                    tx.settlementTx.rolloutProduced.utxo._1).map(_.toList).left.map(Error.RolloutSeqError(_))
                        } yield rollouts
                    }
                    case SettlementTx.Builder.IsFirstRolloutMerged.NotMerged =>
                        for {
                            rollouts <- rolloutSeqBuilder.finishPartialChain(
                                NonEmptyList(rolloutPartialResults._1, rolloutPartialResults._2),
                                tx.settlementTx.rolloutProduced.utxo._1
                            ).map(_.toList).left.map(Error.RolloutSeqError(_))
                        } yield rollouts
                }
            }
        } yield (settlementTx, finishedRolloutChain)

    enum Error:
        case SettlementError(e: SettlementTx.Builder.Error)
        case RolloutSeqError(e: RolloutTxSeq.Builder.Error)

}

