package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.NonEmptyVector
import hydrozoa.config.EquityShares
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.block.BlockVersion
import hydrozoa.multisig.ledger.dapp
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx
import hydrozoa.multisig.ledger.dapp.tx.*
import hydrozoa.multisig.ledger.dapp.tx.FinalizationTx.Builder.Args.toArgs1
import hydrozoa.multisig.ledger.dapp.tx.FinalizationTx.Builder.PartialResult
import hydrozoa.multisig.ledger.dapp.tx.SettlementTx.Builder.Args as SingleArgs
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, MultisigRegimeUtxo, MultisigTreasuryUtxo}
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment
import scalus.cardano.ledger.CardanoInfo
import scalus.cardano.txbuilder.SomeBuildError

enum FinalizationTxSeq {
    def finalizationTx: FinalizationTx

    /** Merged deinit, optional direct payouts */
    case Monolithic(
        override val finalizationTx: FinalizationTx.Monolithic
    )

    /** Separate deinit, optional direct payouts */
    case WithDeinit(
        override val finalizationTx: FinalizationTx.WithDeinit,
        deinitTx: DeinitTx
    )

    /** Merged deinit, optional direct payouts, and rollout */
    case WithRollouts(
        override val finalizationTx: FinalizationTx.WithRolloutsMerged,
        rolloutTxSeq: RolloutTxSeq
    )

    /** Separate deinit, optional direct payouts, and rollout */
    case WithDeinitAndRollouts(
        override val finalizationTx: FinalizationTx.WithRollouts,
        deinitTx: DeinitTx,
        rolloutTxSeq: RolloutTxSeq
    )
}

object FinalizationTxSeq {
    case class Config(
        txTiming: TxTiming,
        multisigRegimeUtxo: MultisigRegimeUtxo,
        cardanoInfo: CardanoInfo,
        headMultisigScript: HeadMultisigScript,
        equityShares: EquityShares
    )

    extension (finalizationTxSeq: FinalizationTxSeq)

        def mbRollouts: List[RolloutTx] = finalizationTxSeq match {
            case FinalizationTxSeq.WithRollouts(_, rolloutTxSeq) => rolloutTxSeq.mbRollouts
            case FinalizationTxSeq.WithDeinitAndRollouts(_, _, rolloutTxSeq) =>
                rolloutTxSeq.mbRollouts
            case _ => List.empty
        }

        def mbDeinit: Option[DeinitTx] = finalizationTxSeq match {
            case FinalizationTxSeq.WithDeinit(finalizationTx, deinitTx)  => Some(deinitTx)
            case FinalizationTxSeq.WithDeinitAndRollouts(_, deinitTx, _) => Some(deinitTx)
            case _                                                       => None
        }

    import Builder.*

    final case class Builder(
        config: Config
    ) {
        def build(args: Args): Either[Builder.Error, FinalizationTxSeq] = {

            val finalizationTxConfig: FinalizationTx.Config = FinalizationTx.Config(
              cardanoInfo = config.cardanoInfo,
              headMultisigScript = config.headMultisigScript,
              multisigRegimeUtxo = config.multisigRegimeUtxo
            )
            val settlementTxConfig: SettlementTx.Config = SettlementTx.Config(
              cardanoInfo = config.cardanoInfo,
              multisigRegimeUtxo = config.multisigRegimeUtxo,
              headMultisigScript = config.headMultisigScript
            )
            val rolloutTxSeqConfig: RolloutTxSeq.Config = RolloutTxSeq.Config(
              cardanoInfo = config.cardanoInfo,
              multisigRegimeUtxo = config.multisigRegimeUtxo,
              headMultisigScript = config.headMultisigScript
            )
            val deinitConfig: DeinitTx.Config = DeinitTx.Config(
              cardanoInfo = config.cardanoInfo,
              headMultisigScript = config.headMultisigScript,
              equityShares = config.equityShares
            )

            val upgrade = FinalizationTx.Builder.upgrade(finalizationTxConfig)

            NonEmptyVector.fromVector(args.payoutObligationsRemaining) match {

                case None =>
                    for {
                        // SettlementTx, but with no deposits
                        settlementTxRes <- SettlementTx.Builder
                            .NoPayouts(settlementTxConfig)
                            .build(args.toArgsNoPayouts(config.txTiming))
                            .left
                            .map(Builder.Error.SettlementError(_))

                        // Upgrade the settlement tx into the partial finalization
                        // NB: this won't work!
                        // finalizationPartial <- upgrade(fromSettlementResult(settlementTxRes)).left
                        finalizationPartial <- upgrade(settlementTxRes.toArgs1).left
                            .map(Builder.Error.FinalizationPartialError(_))

                        //// This is exhaustive!
                        // _ = finalizationPartial match {
                        //    case _: PartialResult.NoPayouts => ()
                        // }

                        // Build deinit tx
                        deinitTx <- DeinitTx.Builder
                            .apply(
                              finalizationPartial.residualTreasuryProduced,
                              deinitConfig
                            )
                            .build
                            .left
                            .map(Builder.Error.DeinitTxError(_))

                        // Complete finalization tx
                        finalizationTx <- finalizationPartial
                            .complete(deinitTx, args.majorVersionProduced, finalizationTxConfig)
                            .left
                            .map(Builder.Error.FinalizationCompleteError(_))

                    } yield finalizationTx match {
                        case tx: FinalizationTx.NoPayouts       => WithDeinit(tx, deinitTx)
                        case tx: FinalizationTx.NoPayoutsMerged => Monolithic(tx)
                    }

                case Some(nePayouts) =>
                    for {

                        rolloutTxSeqPartial <- RolloutTxSeq
                            .Builder(rolloutTxSeqConfig)
                            .buildPartial(nePayouts)
                            .left
                            .map(Error.RolloutSeqError(_))

                        // SettlementTx, but with no deposits
                        settlementTxRes <- SettlementTx.Builder
                            .WithPayouts(settlementTxConfig)
                            .build(
                              args.toArgsWithPayouts(
                                rolloutTxSeqPartial,
                                txTiming = config.txTiming
                              )
                            )
                            .left
                            .map(Error.SettlementError(_))

                        finalizationPartial <- settlementTxRes match {
                            case s: SettlementTx.Builder.Result.WithOnlyDirectPayouts =>
                                upgrade(s.toArgs1).left
                                    .map(Builder.Error.FinalizationPartialError(_))
                            case s: SettlementTx.Builder.Result.WithRollouts =>
                                upgrade(s.toArgs1).left
                                    .map(Builder.Error.FinalizationPartialError(_))
                        }

                        // Build deinit tx
                        deinitTx <- DeinitTx.Builder
                            .apply(
                              finalizationPartial.residualTreasuryProduced,
                              deinitConfig
                            )
                            .build
                            .left
                            .map(Builder.Error.DeinitTxError(_))

                        // Complete finalization tx and build the sequence
                        finalizationTxSeq <-
                            finalizationPartial match {
                                case withOnlyDirectPayout: PartialResult.WithOnlyDirectPayouts =>
                                    for {
                                        finalizationTx <- withOnlyDirectPayout
                                            .complete(
                                              deinitTx,
                                              args.majorVersionProduced,
                                              finalizationTxConfig
                                            )
                                            .left
                                            .map(Builder.Error.FinalizationCompleteError(_))
                                    } yield finalizationTx match {
                                        case tx2: FinalizationTx.WithOnlyDirectPayouts =>
                                            FinalizationTxSeq.WithDeinit(tx2, deinitTx)
                                        case tx2: FinalizationTx.WithOnlyDirectPayoutsMerged =>
                                            FinalizationTxSeq.Monolithic(tx2)
                                    }

                                case withRollouts: PartialResult.WithRollouts =>
                                    for {
                                        finalizationTx <- withRollouts
                                            .complete(
                                              deinitTx,
                                              args.majorVersionProduced,
                                              finalizationTxConfig
                                            )
                                            .left
                                            .map(Builder.Error.FinalizationCompleteError(_))
                                        rolloutTxSeq <- withRollouts.rolloutTxSeqPartial
                                            .finishPostProcess(withRollouts.rolloutProduced)
                                            .left
                                            .map(Error.RolloutSeqError(_))
                                    } yield finalizationTx match {
                                        case tx: FinalizationTx.WithRollouts =>
                                            FinalizationTxSeq
                                                .WithDeinitAndRollouts(
                                                  tx,
                                                  deinitTx,
                                                  rolloutTxSeq
                                                )
                                        case tx: FinalizationTx.WithRolloutsMerged =>
                                            FinalizationTxSeq.WithRollouts(tx, rolloutTxSeq)
                                    }
                            }
                    } yield finalizationTxSeq
            }
        }
    }

    object Builder {

        enum Error:
            case SettlementError(e: (SomeBuildError, String))
            case DeinitTxError(e: (SomeBuildError, String))
            case FinalizationPartialError(e: (SomeBuildError, String))
            case FinalizationCompleteError(e: (SomeBuildError, String))
            case RolloutSeqError(e: (SomeBuildError, String))

        final case class Args(
            majorVersionProduced: BlockVersion.Major,
            treasuryToSpend: MultisigTreasuryUtxo,
            payoutObligationsRemaining: Vector[Payout.Obligation],
            competingFallbackValidityStart: QuantizedInstant,
            blockCreatedOn: QuantizedInstant,
        ) extends Payout.Obligation.Many.Remaining {

            def toArgsNoPayouts(txTiming: TxTiming): SingleArgs.NoPayouts =
                SingleArgs.NoPayouts(
                  majorVersionProduced = majorVersionProduced,
                  treasuryToSpend = treasuryToSpend,
                  depositsToSpend = depositsToSpend,
                  validityEnd = competingFallbackValidityStart - txTiming.silenceDuration,
                  kzgCommitment = KzgCommitment.empty
                )

            def toArgsWithPayouts(
                rolloutTxSeqPartial: RolloutTxSeq.Builder.PartialResult,
                txTiming: TxTiming
            ): SingleArgs.WithPayouts = SingleArgs.WithPayouts(
              majorVersionProduced = majorVersionProduced,
              treasuryToSpend = treasuryToSpend,
              depositsToSpend = depositsToSpend,
              validityEnd = competingFallbackValidityStart - txTiming.silenceDuration,
              rolloutTxSeqPartial = rolloutTxSeqPartial,
              kzgCommitment = KzgCommitment.empty
            )

            // No deposits in finalization tx
            val depositsToSpend: Vector[DepositUtxo] = Vector.empty
        }
    }
}
