package hydrozoa.multisig.ledger.l1.txseq

import cats.data.NonEmptyVector
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, FallbackTxStartTime}
import hydrozoa.multisig.ledger.block.BlockVersion
import hydrozoa.multisig.ledger.commitment.KzgCommitment
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.l1.tx
import hydrozoa.multisig.ledger.l1.tx.*
import hydrozoa.multisig.ledger.l1.tx.Tx.Builder.SomeBuildErrorOnly
import hydrozoa.multisig.ledger.l1.utxo.{DepositUtxo, MultisigTreasuryUtxo}
import scalus.cardano.txbuilder.SomeBuildError.{BalancingError, SomeRedeemerIndexingError, SomeStepError, ValidationError}
import scalus.cardano.txbuilder.{SomeBuildError, TxBalancingError}

import KzgCommitment.KzgCommitment

enum SettlementTxSeq {
    def settlementTx: SettlementTx
    def fallbackTx: FallbackTx

    case NoRollouts(
        override val settlementTx: SettlementTx.NoRollouts,
        override val fallbackTx: FallbackTx,
    )

    case WithRollouts(
        override val settlementTx: SettlementTx.WithRollouts,
        override val fallbackTx: FallbackTx,
        rolloutTxSeq: RolloutTxSeq,
    )

    def rolloutTxs: List[RolloutTx] = this match {
        case _: NoRollouts   => List.empty
        case r: WithRollouts => r.rolloutTxSeq.rolloutTxs
    }
}

object SettlementTxSeq {
    export SettlementTxSeqOps.Build
}

private object SettlementTxSeqOps {
    type Config = HeadConfig.Section

    private val logger = org.slf4j.LoggerFactory.getLogger("SettlementTxSeq")

    private def time[A](label: String)(block: => A): A = {
        val start = System.nanoTime()
        val result = block
        val elapsed = (System.nanoTime() - start) / 1_000_000.0
        logger.trace(f"\t\t⏱️ $label: ${elapsed}%.2f ms")
        result
    }

    object Build {
        enum Error:
            case SettlementError(e: (SomeBuildError | SettlementTx.Error, String))
            case RolloutSeqError(e: (SomeBuildErrorOnly, String))
            case FallbackError(e: SomeBuildError)

            override def toString: String = this match {
                case SettlementError((err, msg)) =>
                    s"SettlementError: ${if msg.nonEmpty then s" - $msg" else ""}." + "\n" +
                        (err match {
                            case SomeStepError(e, ctx) =>
                                s"Transaction builder step error:\n\t${e.explain}"
                            case SomeRedeemerIndexingError(e, ctx) =>
                                "Transaction builder redeemer indexing error."
                            case BalancingError(e, ctx) =>
                                ???
                                "Transaction builder balancing error:\n\t" + (e match {
                                    case TxBalancingError.EvaluationFailed(c) =>
                                        s"Plutus evaluation failed. Cause: ${c.toString}"
                                    case TxBalancingError.Failed(c) =>
                                        s"Generic tx balancing error. Cause: ${c.toString}"
                                    case TxBalancingError.BalanceDidNotConverge(i) =>
                                        s"Balance did not converge after $i iterations."
                                    case TxBalancingError.InsufficientFunds(
                                          valueDiff,
                                          minRequired
                                        ) =>
                                        "Insufficient funds:" + "\n\t\t" +
                                            s"consumed - produced = ${valueDiff.toString}" + "\n\t\t" +
                                            s"more lovelace required: ${minRequired.toString}"
                                    case TxBalancingError.InsufficientCollateralForReturn(
                                          totalCollateralAda,
                                          requiredCollateral,
                                          minAdaForReturn
                                        ) =>
                                        "Insufficient collateral for return:" + "\n\t\t" +
                                            s"Total collateral (lovelace): ${totalCollateralAda.toString}" + "\n\t\t" +
                                            s"Required collateral (lovelace): ${requiredCollateral.toString}" + "\n\t\t" +
                                            s"Min collateral return (lovelace): ${minAdaForReturn.toString}"
                                })
                            case ValidationError(e, ctx) =>
                                s"Validation error: ${e.toString}"
                            case SettlementTx.Error.TreasuryIncorrectAddress =>
                                "Head treasury is at an incorrect address."
                            case SettlementTx.Error.TooManyDeposits =>
                                "Too many deposits were provided to absorb in this settlement tx, exceeding the max-per-tx parameter."
                        })
                case RolloutSeqError((err, msg)) =>
                    s"Rollout tx sequence error: ${
                            if msg.nonEmpty then s" - $msg" else ""
                        }." + "\n" +
                        (err match {
                            case SomeStepError(e, ctx) =>
                                s"Transaction builder step error:\n\t${e.explain}"
                            case SomeRedeemerIndexingError(e, ctx) =>
                                "Transaction builder redeemer indexing error."
                            case BalancingError(e, ctx) =>
                                ???
                                "Transaction builder balancing error:\n\t" + (e match {
                                    case TxBalancingError.EvaluationFailed(c) =>
                                        s"Plutus evaluation failed. Cause: ${c.toString}"
                                    case TxBalancingError.Failed(c) =>
                                        s"Generic tx balancing error. Cause: ${c.toString}"
                                    case TxBalancingError.BalanceDidNotConverge(i) =>
                                        s"Balance did not converge after $i iterations."
                                    case TxBalancingError.InsufficientFunds(
                                          valueDiff,
                                          minRequired
                                        ) =>
                                        "Insufficient funds:" + "\n\t\t" +
                                            s"consumed - produced = ${valueDiff.toString}" + "\n\t\t" +
                                            s"more lovelace required: ${minRequired.toString}"
                                    case TxBalancingError.InsufficientCollateralForReturn(
                                          totalCollateralAda,
                                          requiredCollateral,
                                          minAdaForReturn
                                        ) =>
                                        "Insufficient collateral for return:" + "\n\t\t" +
                                            s"Total collateral (lovelace): ${totalCollateralAda.toString}" + "\n\t\t" +
                                            s"Required collateral (lovelace): ${requiredCollateral.toString}" + "\n\t\t" +
                                            s"Min collateral return (lovelace): ${minAdaForReturn.toString}"
                                })
                            case ValidationError(e, ctx) =>
                                s"Validation error: ${e.toString}"
                            case _: Void =>
                                "RolloutSeqError: Void, which is impossible."
                        })
                case FallbackError(err) =>
                    "Fallback tx sequence error:" + "\n" +
                        (err match {
                            case SomeStepError(e, ctx) =>
                                s"Transaction builder step error:\n\t${e.explain}"
                            case SomeRedeemerIndexingError(e, ctx) =>
                                "Transaction builder redeemer indexing error."
                            case BalancingError(e, ctx) =>
                                ???
                                "Transaction builder balancing error:\n\t" + (e match {
                                    case TxBalancingError.EvaluationFailed(c) =>
                                        s"Plutus evaluation failed. Cause: ${c.toString}"
                                    case TxBalancingError.Failed(c) =>
                                        s"Generic tx balancing error. Cause: ${c.toString}"
                                    case TxBalancingError.BalanceDidNotConverge(i) =>
                                        s"Balance did not converge after $i iterations."
                                    case TxBalancingError.InsufficientFunds(
                                          valueDiff,
                                          minRequired
                                        ) =>
                                        "Insufficient funds:" + "\n\t\t" +
                                            s"consumed - produced = ${valueDiff.toString}" + "\n\t\t" +
                                            s"more lovelace required: ${minRequired.toString}"
                                    case TxBalancingError.InsufficientCollateralForReturn(
                                          totalCollateralAda,
                                          requiredCollateral,
                                          minAdaForReturn
                                        ) =>
                                        "Insufficient collateral for return:" + "\n\t\t" +
                                            s"Total collateral (lovelace): ${totalCollateralAda.toString}" + "\n\t\t" +
                                            s"Required collateral (lovelace): ${requiredCollateral.toString}" + "\n\t\t" +
                                            s"Min collateral return (lovelace): ${minAdaForReturn.toString}"
                                })
                            case ValidationError(e, ctx) =>
                                s"Validation error: ${e.toString}"
                        })
            }

    }

    final case class Build(config: Config)(
        override val kzgCommitment: KzgCommitment,
        override val majorVersionProduced: BlockVersion.Major,
        override val treasuryToSpend: MultisigTreasuryUtxo,
        override val depositsToSpend: List[DepositUtxo],
        override val payoutObligationsRemaining: Vector[Payout.Obligation],
        blockCreationEndTime: BlockCreationEndTime,
        competingFallbackValidityStart: FallbackTxStartTime
    ) extends BlockVersion.Major.Produced,
          MultisigTreasuryUtxo.ToSpend,
          DepositUtxo.Many.ToSpend,
          KzgCommitment.Produced,
          Payout.Obligation.Many.Remaining {
        import Build.*

        private val settlementValidityEnd =
            config.txTiming.newSettlementEndTime(competingFallbackValidityStart)

        private val newFallbackValidityEnd =
            config.txTiming.newFallbackStartTime(blockCreationEndTime)

        lazy val result: Either[Build.Error, SettlementTxSeq] = {
            NonEmptyVector.fromVector(payoutObligationsRemaining) match {
                case None =>

                    for {
                        settlementTx <- time("SettlementTx.NoPayouts.Build") {
                            SettlementTx.Build
                                .NoPayouts(config)(
                                  kzgCommitment,
                                  majorVersionProduced,
                                  treasuryToSpend,
                                  depositsToSpend,
                                  settlementValidityEnd
                                )
                                .result
                                .left
                                .map(Build.Error.SettlementError(_))
                        }

                        fallbackTx <- time("FallbackTx.Build") {
                            FallbackTx
                                .Build(config)(
                                  newFallbackValidityEnd,
                                  settlementTx.transaction.treasuryProduced,
                                  config.multisigRegimeUtxo
                                )
                                .result
                                .left
                                .map(Build.Error.FallbackError(_))
                        }
                    } yield SettlementTxSeq.NoRollouts(settlementTx.transaction, fallbackTx)

                case Some(nePayouts) =>

                    for {
                        rolloutTxSeqPartial <- time("RolloutTxSeq.Build.partialResult") {
                            RolloutTxSeq
                                .Build(config)(nePayouts)
                                .partialResult
                                .left
                                .map(Error.RolloutSeqError(_))
                        }

                        settlementTxRes <- time("SettlementTx.WithPayouts.Build") {
                            SettlementTx.Build
                                .WithPayouts(config)(
                                  kzgCommitment,
                                  majorVersionProduced,
                                  treasuryToSpend,
                                  depositsToSpend,
                                  settlementValidityEnd,
                                  rolloutTxSeqPartial
                                )
                                .result
                                .left
                                .map(Error.SettlementError(_))
                        }

                        fallbackTx <- time("FallbackTx.Build") {
                            FallbackTx
                                .Build(config)(
                                  newFallbackValidityEnd,
                                  settlementTxRes.transaction.treasuryProduced,
                                  config.multisigRegimeUtxo
                                )
                                .result
                                .left
                                .map(Build.Error.FallbackError(_))
                        }

                        settlementTxSeq <-
                            settlementTxRes match {
                                case res: SettlementTx.Result.WithOnlyDirectPayouts =>
                                    Right(SettlementTxSeq.NoRollouts(res.transaction, fallbackTx))
                                case res: SettlementTx.Result.WithRollouts =>
                                    val tx: SettlementTx.WithRollouts = res.transaction
                                    res.rolloutTxSeqPartial
                                        .finishPostProcess(tx.rolloutProduced)
                                        .left
                                        .map(Error.RolloutSeqError(_))
                                        .map(SettlementTxSeq.WithRollouts(tx, fallbackTx, _))
                            }

                    } yield settlementTxSeq
            }
        }
    }
}
