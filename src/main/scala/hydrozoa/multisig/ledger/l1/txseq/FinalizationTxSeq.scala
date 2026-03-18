package hydrozoa.multisig.ledger.l1.txseq

import cats.data.NonEmptyVector
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.FallbackTxStartTime
import hydrozoa.multisig.ledger.block.BlockVersion
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.l1
import hydrozoa.multisig.ledger.l1.tx
import hydrozoa.multisig.ledger.l1.tx.*
import hydrozoa.multisig.ledger.l1.tx.Tx.Builder.SomeBuildErrorOnly
import hydrozoa.multisig.ledger.l1.utxo.MultisigTreasuryUtxo
import scalus.cardano.txbuilder.SomeBuildError.{BalancingError, SomeRedeemerIndexingError, SomeStepError, ValidationError}
import scalus.cardano.txbuilder.{SomeBuildError, TxBalancingError}

enum FinalizationTxSeq {
    def finalizationTx: FinalizationTx

    /** Merged deinit, optional direct payouts */
    case NoRollouts(
        override val finalizationTx: FinalizationTx.NoRollouts
    )

    /** Merged deinit, optional direct payouts, and rollout */
    case WithRollouts(
        override val finalizationTx: FinalizationTx.WithRollouts,
        rolloutTxSeq: RolloutTxSeq
    )

    def rolloutTxs: List[RolloutTx] = this match {
        case x: WithRollouts => x.rolloutTxSeq.rolloutTxs
        case _               => List.empty
    }
}

object FinalizationTxSeq {
    export FinalizationTxSeqOps.Build
}

private object FinalizationTxSeqOps {
    type Config = HeadConfig.Section

    object Build {
        enum Error:
            case FinalizationError(e: (SomeBuildError | FinalizationTx.Error, String))
            case RolloutSeqError(e: (SomeBuildErrorOnly, String))

            override def toString: String = this match {
                case FinalizationError((err, msg)) =>
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
                            case FinalizationTx.Error.TreasuryIncorrectAddress =>
                                "Head treasury is at an incorrect address."
                            case FinalizationTx.Error.ResidualTreasuryContainsTokens =>
                                ???
                                "Residual treasury contains tokens."
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
            }
    }

    final case class Build(config: Config)(
        override val majorVersionProduced: BlockVersion.Major,
        override val treasuryToSpend: MultisigTreasuryUtxo,
        override val payoutObligationsRemaining: Vector[Payout.Obligation],
        competingFallbackValidityStart: FallbackTxStartTime
    ) extends BlockVersion.Major.Produced,
          MultisigTreasuryUtxo.ToSpend,
          Payout.Obligation.Many.Remaining {
        import Build.*

        private val finalizationValidityEnd =
            config.txTiming.finalizationEndTime(competingFallbackValidityStart)

        lazy val result: Either[Build.Error, FinalizationTxSeq] = {
            NonEmptyVector.fromVector(payoutObligationsRemaining) match {
                case None =>
                    for {
                        finalizationTxRes <- FinalizationTx.Build
                            .NoPayouts(config)(
                              majorVersionProduced,
                              treasuryToSpend,
                              finalizationValidityEnd
                            )
                            .result
                            .left
                            .map(Build.Error.FinalizationError(_))
                    } yield FinalizationTxSeq.NoRollouts(finalizationTxRes.transaction)

                case Some(nePayouts) =>
                    for {
                        rolloutTxSeqPartial <- RolloutTxSeq
                            .Build(config)(nePayouts)
                            .partialResult
                            .left
                            .map(Error.RolloutSeqError(_))

                        finalizationTxRes <- FinalizationTx.Build
                            .WithPayouts(config)(
                              majorVersionProduced,
                              treasuryToSpend,
                              finalizationValidityEnd,
                              rolloutTxSeqPartial
                            )
                            .result
                            .left
                            .map(Build.Error.FinalizationError(_))

                        finalizationTxSeq <- finalizationTxRes match {
                            case res: FinalizationTx.Result.WithOnlyDirectPayouts =>
                                Right(FinalizationTxSeq.NoRollouts(res.transaction))
                            case res: FinalizationTx.Result.WithRollouts =>
                                val tx: FinalizationTx.WithRollouts = res.transaction
                                res.rolloutTxSeqPartial
                                    .finishPostProcess(tx.rolloutProduced)
                                    .left
                                    .map(Error.RolloutSeqError(_))
                                    .map(FinalizationTxSeq.WithRollouts(tx, _))
                        }
                    } yield finalizationTxSeq
            }
        }
    }
}
