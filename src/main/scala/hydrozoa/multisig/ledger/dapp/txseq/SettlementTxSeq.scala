package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.NonEmptyVector
import hydrozoa.config.head.HeadConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.block.BlockVersion
import hydrozoa.multisig.ledger.dapp.tx
import hydrozoa.multisig.ledger.dapp.tx.*
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, MultisigTreasuryUtxo}
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment
import scalus.cardano.txbuilder.SomeBuildError

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
        logger.info(f"\t\t⏱️ $label: ${elapsed}%.2f ms")
        result
    }

    final case class Result(
        settlementTxSeq: SettlementTxSeq,
        override val depositsSpent: Vector[DepositUtxo],
        override val depositsToSpend: Vector[DepositUtxo]
    ) extends DepositUtxo.Many.Spent.Partition

    object Build {
        enum Error:
            case SettlementError(e: (SomeBuildError, String))
            case RolloutSeqError(e: (SomeBuildError, String))
            case FallbackError(e: SomeBuildError)
    }

    final case class Build(config: Config)(
        override val kzgCommitment: KzgCommitment,
        override val majorVersionProduced: BlockVersion.Major,
        override val treasuryToSpend: MultisigTreasuryUtxo,
        override val depositsToSpend: Vector[DepositUtxo],
        override val payoutObligationsRemaining: Vector[Payout.Obligation],
        competingFallbackValidityStart: QuantizedInstant,
        blockCreatedOn: QuantizedInstant,
    ) extends BlockVersion.Major.Produced,
          MultisigTreasuryUtxo.ToSpend,
          DepositUtxo.Many.ToSpend,
          KzgCommitment.Produced,
          Payout.Obligation.Many.Remaining {
        import Build.*

        private val settlementValidityEnd =
            config.txTiming.newSettlementEndTime(competingFallbackValidityStart)

        private val newFallbackValidityEnd =
            config.txTiming.newFallbackStartTime(blockCreatedOn)

        lazy val result: Either[Build.Error, Result] = {
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
                    } yield Result(
                      settlementTxSeq =
                          SettlementTxSeq.NoRollouts(settlementTx.transaction, fallbackTx),
                      depositsSpent = settlementTx.depositsSpent,
                      depositsToSpend = settlementTx.depositsToSpend
                    )
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

                    } yield Result(
                      settlementTxSeq = settlementTxSeq,
                      depositsSpent = settlementTxRes.depositsSpent,
                      depositsToSpend = settlementTxRes.depositsToSpend
                    )
            }
        }
    }
}
