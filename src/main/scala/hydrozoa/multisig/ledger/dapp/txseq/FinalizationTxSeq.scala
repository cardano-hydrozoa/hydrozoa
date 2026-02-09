package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.NonEmptyVector
import hydrozoa.config.head.HeadConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.block.BlockVersion
import hydrozoa.multisig.ledger.dapp
import hydrozoa.multisig.ledger.dapp.tx
import hydrozoa.multisig.ledger.dapp.tx.*
import hydrozoa.multisig.ledger.dapp.utxo.MultisigTreasuryUtxo
import hydrozoa.multisig.ledger.joint.obligation.Payout
import scalus.cardano.txbuilder.SomeBuildError

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
            case SettlementError(e: (SomeBuildError, String))
            case DeinitTxError(e: (SomeBuildError, String))
            case FinalizationError(e: (SomeBuildError, String))
            case RolloutSeqError(e: (SomeBuildError, String))
    }

    final case class Build(config: Config)(
        override val majorVersionProduced: BlockVersion.Major,
        override val treasuryToSpend: MultisigTreasuryUtxo,
        override val payoutObligationsRemaining: Vector[Payout.Obligation],
        competingFallbackValidityStart: QuantizedInstant,
        blockCreatedOn: QuantizedInstant,
    ) extends BlockVersion.Major.Produced,
          MultisigTreasuryUtxo.ToSpend,
          Payout.Obligation.Many.Remaining {
        import Build.*

        private val finalizationValidityEnd =
            config.txTiming.newSettlementEndTime(competingFallbackValidityStart)

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
