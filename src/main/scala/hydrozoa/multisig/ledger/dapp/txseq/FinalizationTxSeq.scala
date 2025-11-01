package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.NonEmptyVector
import hydrozoa.config.EquityShares
import hydrozoa.multisig.ledger.dapp
import hydrozoa.multisig.ledger.dapp.tx
import hydrozoa.multisig.ledger.dapp.tx.*
import hydrozoa.multisig.ledger.dapp.tx.FinalizationTx1.Builder.Args.{TxWithRolloutTxSeq, toArgs1}
import hydrozoa.multisig.ledger.dapp.tx.FinalizationTx2.Builder.Args.toArgs2
import hydrozoa.multisig.ledger.dapp.tx.SettlementTx.Builder.Args as SingleArgs
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, MultisigRegimeUtxo, TreasuryUtxo}
import hydrozoa.multisig.ledger.joint.utxo.Payout
import hydrozoa.multisig.protocol.types.Block
import hydrozoa.multisig.protocol.types.Block.Version.Major
import scalus.cardano.txbuilder.SomeBuildError

enum FinalizationTxSeq {
    def finalizationTx: FinalizationTx2

    /** Merged deinit, optional direct payouts */
    case Monolithic(
        override val finalizationTx: FinalizationTx2.Monolithic
    )

    /** Separate deinit, optional direct payouts */
    case WithDeinit(
        override val finalizationTx: FinalizationTx2.WithDeinit,
        deinitTx: DeinitTx
    )

    /** Merged deinit, optional direct payouts, and rollout */
    case WithRollouts(
        override val finalizationTx: FinalizationTx2.WithRolloutsMerged,
        rolloutTxSeq: RolloutTxSeq
    )

    /** Separate deinit, optional direct payouts, and rollout */
    case WithDeinitAndRollouts(
        override val finalizationTx: FinalizationTx2.WithRollouts,
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

            val upgrade = FinalizationTx1.Builder.upgrade(config, args.multisigRegimeUtxoToSpend)

            NonEmptyVector.fromVector(args.payoutObligationsRemaining) match {

                case None =>
                    for {
                        // SettlementTx, but with no deposits
                        settlementTx <- SettlementTx.Builder
                            .NoPayouts(config)
                            .build(args.toArgsNoPayouts)
                            .left
                            .map(Builder.Error.SettlementError(_))

                        // Upgrade to finalization tx stage 1
                        finalizationTx1 <- upgrade(settlementTx.toArgs1).left
                            .map(Builder.Error.FinalizationTx1Error(_))

                        deinitTx <- DeinitTx.Builder
                            .apply(
                              finalizationTx1.residualTreasuryProduced,
                              args.equityShares,
                              config
                            )
                            .build
                            .left
                            .map(Builder.Error.DeinitTxError(_))

                        finalizationTx2 <- FinalizationTx2.Builder
                            .build(deinitTx)(finalizationTx1.toArgs2)
                            .left
                            .map(Builder.Error.FinalizationTx2Error(_))

                    } yield finalizationTx2 match {
                        case tx: FinalizationTx2.NoPayouts       => WithDeinit(tx, deinitTx)
                        case tx: FinalizationTx2.NoPayoutsMerged => Monolithic(tx)
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

                        // Upgrade to finalization tx stage 1
                        finalizationTx1 <- upgrade(
                          FinalizationTx1.Builder.Args(settlementTxRes)
                        ).left.map(Builder.Error.FinalizationTx1Error(_))

                        deinitTx <- DeinitTx.Builder
                            .apply(
                              finalizationTx1.residualTreasuryProduced,
                              args.equityShares,
                              config
                            )
                            .build
                            .left
                            .map(Builder.Error.DeinitTxError(_))

                        finalizationStage2Builder = FinalizationTx2.Builder.build(deinitTx)

                        finalizationTxSeq <-
                            finalizationTx1 match {

                                case tx: FinalizationTx1.WithOnlyDirectPayouts =>
                                    val finalizationTx2 = finalizationStage2Builder(
                                      tx.toArgs2
                                    ).toOption.get

                                    finalizationTx2 match {
                                        case tx2: FinalizationTx2.WithOnlyDirectPayouts =>
                                            Right(FinalizationTxSeq.WithDeinit(tx2, deinitTx))
                                        case tx2: FinalizationTx2.WithOnlyDirectPayoutsMerged =>
                                            Right(FinalizationTxSeq.Monolithic(tx2))
                                    }

                                case res: TxWithRolloutTxSeq =>
                                    for {

                                        finalizationTx2 <- finalizationStage2Builder(
                                          res._1.toArgs2
                                        ).left.map(Builder.Error.FinalizationTx2Error(_))

                                        rolloutTxSeq <- res._2
                                            .finishPostProcess(finalizationTx2.rolloutProduced)
                                            .left
                                            .map(Error.RolloutSeqError(_))

                                    } yield finalizationTx2 match {
                                        case tx: FinalizationTx2.WithRollouts =>
                                            FinalizationTxSeq
                                                .WithDeinitAndRollouts(
                                                  tx,
                                                  deinitTx,
                                                  rolloutTxSeq
                                                )
                                        case tx: FinalizationTx2.WithRolloutsMerged =>
                                            FinalizationTxSeq.WithRollouts(tx, rolloutTxSeq)
                                    }

                                // TODO: maybe we can do better
                                case _: FinalizationTx1.NoPayouts =>
                                    throw RuntimeException("not possible")
                                case _: FinalizationTx1.WithRollouts =>
                                    throw RuntimeException("not possible")

                            }
                    } yield finalizationTxSeq
            }
        }
    }

    object Builder {

        enum Error:
            case SettlementError(e: (SomeBuildError, String))
            case FinalizationTx1Error(e: (SomeBuildError, String))
            case DeinitTxError(e: (SomeBuildError, String))
            case FinalizationTx2Error(e: (SomeBuildError, String))
            case RolloutSeqError(e: (SomeBuildError, String))

        final case class Args(
            override val majorVersionProduced: Block.Version.Major,
            override val treasuryToSpend: TreasuryUtxo,
            override val payoutObligationsRemaining: Vector[Payout.Obligation.L1],
            multisigRegimeUtxoToSpend: MultisigRegimeUtxo,
            equityShares: EquityShares
        ) extends SingleArgs,
              Payout.Obligation.L1.Many.Remaining {

            def toArgsNoPayouts: SingleArgs.NoPayouts =
                SingleArgs.NoPayouts(
                  majorVersionProduced = majorVersionProduced,
                  treasuryToSpend = treasuryToSpend,
                  depositsToSpend = depositsToSpend
                )

            def toArgsWithPayouts(
                rolloutTxSeqPartial: RolloutTxSeq.Builder.PartialResult
            ): SingleArgs.WithPayouts = SingleArgs.WithPayouts(
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
