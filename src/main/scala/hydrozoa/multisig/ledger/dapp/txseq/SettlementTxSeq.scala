package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.NonEmptyVector
import hydrozoa.multisig.ledger.dapp.tx
import hydrozoa.multisig.ledger.dapp.tx.{FallbackTx, SettlementTx, Tx}
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, TreasuryUtxo}
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.protocol.types.Block
import scalus.cardano.ledger.Coin
import scalus.cardano.txbuilder.SomeBuildError
import scalus.ledger.api.v3.PosixTime

enum SettlementTxSeq {
    def settlementTx: SettlementTx

    case NoRollouts(
        override val settlementTx: SettlementTx.NoRollouts,
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
                    for {
                        settlementTx <- SettlementTx.Builder
                            .NoPayouts(config)
                            .build(args.toArgsNoPayouts)
                            .left
                            .map(Builder.Error.SettlementError(_))

                        ftxRecipe = FallbackTx.Recipe(
                          config = config,
                          treasuryUtxo = settlementTx.transaction.treasuryProduced,
                          tallyFeeAllowance = args.tallyFeeAllowance,
                          votingDuration = args.votingDuration
                        )
                        fallbackTx <- FallbackTx
                            .build(ftxRecipe)
                            .left
                            .map(Builder.Error.FallbackError(_))
                    } yield Builder.Result(
                      settlementTxSeq = SettlementTxSeq.NoRollouts(settlementTx.transaction),
                      fallbackTx = fallbackTx,
                      depositsSpent = settlementTx.depositsSpent,
                      depositsToSpend = settlementTx.depositsToSpend
                    )
                case Some(nePayouts) =>
                    for {
                        rolloutTxSeqPartial <- RolloutTxSeq
                            .Builder(config)
                            .buildPartial(nePayouts)
                            .left
                            .map(Error.RolloutSeqError(_))

                        settlementTxRes <- SettlementTx.Builder
                            .WithPayouts(config)
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
                        ftxRecipe = FallbackTx.Recipe(
                          config = config,
                          treasuryUtxo = settlementTxRes.transaction.treasuryProduced,
                          tallyFeeAllowance = args.tallyFeeAllowance,
                          votingDuration = args.votingDuration
                        )
                        fallbackTx <- FallbackTx
                            .build(ftxRecipe)
                            .left
                            .map(Builder.Error.FallbackError(_))
                    } yield Result(
                      settlementTxSeq = settlementTxSeq,
                      fallbackTx = fallbackTx,
                      depositsSpent = settlementTxRes.depositsSpent,
                      depositsToSpend = settlementTxRes.depositsToSpend
                    )
            }
        }
    }

    object Builder {
        import SettlementTx.Builder.Args as SingleArgs

        enum Error:
            case SettlementError(e: (SomeBuildError, String))
            case RolloutSeqError(e: (SomeBuildError, String))
            case FallbackError(e: SomeBuildError)

        final case class Result(
            settlementTxSeq: SettlementTxSeq,
            fallbackTx: FallbackTx,
            override val depositsSpent: Vector[DepositUtxo],
            override val depositsToSpend: Vector[DepositUtxo]
        ) extends DepositUtxo.Many.Spent.Partition

        final case class Args(
            override val majorVersionProduced: Block.Version.Major,
            override val treasuryToSpend: TreasuryUtxo,
            override val depositsToSpend: Vector[DepositUtxo],
            override val payoutObligationsRemaining: Vector[Payout.Obligation],
            tallyFeeAllowance: Coin,
            votingDuration: PosixTime
        ) extends SingleArgs,
              Payout.Obligation.Many.Remaining {
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
