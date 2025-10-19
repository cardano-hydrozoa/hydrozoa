package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.NonEmptyVector
import hydrozoa.lib.tx.SomeBuildError
import hydrozoa.multisig.ledger.dapp.tx
import hydrozoa.multisig.ledger.dapp.tx.{SettlementTx, Tx}
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, TreasuryUtxo}
import hydrozoa.multisig.ledger.joint.utxo.Payout
import hydrozoa.multisig.protocol.types.Block

enum SettlementTxSeq {
    def settlementTx: SettlementTx

    case NoRollouts(
        override val settlementTx: SettlementTx.NoRollouts
    )

    case WithRollouts(
        override val settlementTx: SettlementTx.WithRollouts,
        rolloutTxSeq: RolloutTxSeq
    )
}

object SettlementTxSeq {
    import Builder.*

    final case class Builder(
        config: Tx.Builder.Config
    ) {
        def build(args: Args): Either[Builder.Error, Builder.Result] = {
            NonEmptyVector.fromVector(args.payoutObligationsRemaining) match {
                case None =>
                    SettlementTx.Builder.NoPayouts(config)
                        .build(args.toArgsNoPayouts)
                        .left
                        .map(Builder.Error.SettlementError(_))
                        .map(Result.fromNoPayouts)
                case Some(nePayouts) =>
                    for {
                        rolloutTxSeqPartial <- RolloutTxSeq
                            .Builder(config)
                            .buildPartial(nePayouts)
                            .left
                            .map(Error.RolloutSeqError(_))

                        settlementTxRes <- SettlementTx.Builder.WithPayouts(config)
                            .build(args.toArgsWithPayouts(rolloutTxSeqPartial))
                            .left
                            .map(Error.SettlementError(_))

                        settlementTxSeq <-
                            import SettlementTx.Builder.Result
                            settlementTxRes match {
                                case res: Result.WithOnlyDirectPayouts =>
                                    Right(SettlementTxSeq.NoRollouts(res.transaction))
                                case res: Result.WithRollouts =>
                                    val tx: SettlementTx.WithRollouts = res.transaction
                                    res.rolloutTxSeqPartial
                                        .finishPostProcess(tx.rolloutProduced)
                                        .left
                                        .map(Error.RolloutSeqError(_))
                                        .map(SettlementTxSeq.WithRollouts(tx, _))
                            }
                    } yield Result(
                      settlementTxSeq = settlementTxSeq,
                      depositsSpent = settlementTxRes.depositsSpent,
                      depositsToSpend = settlementTxRes.depositsToSpend
                    )
            }
        }
    }

    object Builder {
        import SettlementTx.Builder.Args as SingleArgs
        
        enum Error:
            case SettlementError(e: SomeBuildError)
            case RolloutSeqError(e: SomeBuildError)

        final case class Result(
            settlementTxSeq: SettlementTxSeq,
            override val depositsSpent: Vector[DepositUtxo],
            override val depositsToSpend: Vector[DepositUtxo]
        ) extends DepositUtxo.Many.Spent.Partition

        object Result {
            def fromNoPayouts(res: tx.SettlementTx.Builder.Result.NoPayouts): Result = {
                Result(
                  SettlementTxSeq.NoRollouts(res.transaction),
                  depositsSpent = res.depositsSpent,
                  depositsToSpend = res.depositsToSpend
                )
            }
        }

        final case class Args(
            override val majorVersionProduced: Block.Version.Major,
            override val treasuryToSpend: TreasuryUtxo,
            override val depositsToSpend: Vector[DepositUtxo],
            override val payoutObligationsRemaining: Vector[Payout.Obligation.L1]
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
        }
    }
}
