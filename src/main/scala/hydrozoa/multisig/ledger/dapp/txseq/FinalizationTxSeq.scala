package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.NonEmptyVector
import hydrozoa.config.EquityShares
import hydrozoa.multisig.ledger.dapp
import hydrozoa.multisig.ledger.dapp.tx
import hydrozoa.multisig.ledger.dapp.tx.*
import hydrozoa.multisig.ledger.dapp.tx.FinalizationTx.Builder.Args.toArgs1
import hydrozoa.multisig.ledger.dapp.tx.FinalizationTx.Builder.PartialResult
import hydrozoa.multisig.ledger.dapp.tx.SettlementTx.Builder.Args as SingleArgs
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, MultisigRegimeUtxo, MultisigTreasuryUtxo}
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.protocol.types.Block
import hydrozoa.multisig.protocol.types.Block.Version.Major
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
    import Builder.*

    final case class Builder(
        config: Tx.Builder.Config
    ) {
        def build(args: Args): Either[Builder.Error, FinalizationTxSeq] = {

            val upgrade = FinalizationTx.Builder.upgrade(config, args.multisigRegimeUtxoToSpend)

            NonEmptyVector.fromVector(args.payoutObligationsRemaining) match {

                case None =>
                    for {
                        // SettlementTx, but with no deposits
                        settlementTxRes <- SettlementTx.Builder
                            .NoPayouts(config)
                            .build(args.toArgsNoPayouts)
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
                              args.equityShares,
                              config
                            )
                            .build
                            .left
                            .map(Builder.Error.DeinitTxError(_))

                        // Complete finalization tx
                        finalizationTx <- finalizationPartial
                            .complete(deinitTx, args.majorVersionProduced, config)
                            .left
                            .map(Builder.Error.FinalizationCompleteError(_))

                    } yield finalizationTx match {
                        case tx: FinalizationTx.NoPayouts       => WithDeinit(tx, deinitTx)
                        case tx: FinalizationTx.NoPayoutsMerged => Monolithic(tx)
                    }

                case Some(nePayouts) =>
                    for {

                        rolloutTxSeqPartial <- RolloutTxSeq
                            .Builder(config)
                            .buildPartial(nePayouts)
                            .left
                            .map(Error.RolloutSeqError(_))

                        // SettlementTx, but with no deposits
                        settlementTxRes <- SettlementTx.Builder
                            .WithPayouts(config)
                            .build(args.toArgsWithPayouts(rolloutTxSeqPartial))
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
                              args.equityShares,
                              config
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
                                            .complete(deinitTx, args.majorVersionProduced, config)
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
                                            .complete(deinitTx, args.majorVersionProduced, config)
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
            override val kzgCommitment: KzgCommitment,
            override val majorVersionProduced: Block.Version.Major,
            override val treasuryToSpend: MultisigTreasuryUtxo,
            override val payoutObligationsRemaining: Vector[Payout.Obligation],
            multisigRegimeUtxoToSpend: MultisigRegimeUtxo,
            equityShares: EquityShares
        ) extends SingleArgs(kzgCommitment),
              Payout.Obligation.Many.Remaining {

            def toArgsNoPayouts: SingleArgs.NoPayouts =
                SingleArgs.NoPayouts(
                  kzgCommitment = kzgCommitment,
                  majorVersionProduced = majorVersionProduced,
                  treasuryToSpend = treasuryToSpend,
                  depositsToSpend = depositsToSpend
                )

            def toArgsWithPayouts(
                rolloutTxSeqPartial: RolloutTxSeq.Builder.PartialResult
            ): SingleArgs.WithPayouts = SingleArgs.WithPayouts(
              kzgCommitment = kzgCommitment,
              majorVersionProduced = majorVersionProduced,
              treasuryToSpend = treasuryToSpend,
              depositsToSpend = depositsToSpend,
              rolloutTxSeqPartial = rolloutTxSeqPartial
            )

            // No deposits in finalization tx
            override def depositsToSpend: Vector[DepositUtxo] = Vector.empty
        }
    }
}
