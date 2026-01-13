package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.NonEmptyVector
import hydrozoa.multisig.ledger.dapp.tx
import hydrozoa.multisig.ledger.dapp.tx.TxTiming.*
import hydrozoa.multisig.ledger.dapp.tx.{FallbackTx, RolloutTx, SettlementTx, Tx, TxTiming}
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, MultisigTreasuryUtxo}
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.protocol.types.Block
import scala.concurrent.duration.FiniteDuration
import scalus.cardano.ledger.Coin
import scalus.cardano.txbuilder.SomeBuildError

enum SettlementTxSeq {
    def settlementTx: SettlementTx

    case NoRollouts(
        override val settlementTx: SettlementTx.NoRollouts,
    )

    case WithRollouts(
        override val settlementTx: SettlementTx.WithRollouts,
        rolloutTxSeq: RolloutTxSeq,
    )
}

object SettlementTxSeq {

    extension (settlementTxSeq: SettlementTxSeq)

        def mbRollouts: List[RolloutTx] = settlementTxSeq match {
            case _: NoRollouts   => List.empty
            case r: WithRollouts => r.rolloutTxSeq.mbRollouts
        }

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
                          treasuryUtxoSpent = settlementTx.transaction.treasuryProduced,
                          tallyFeeAllowance = args.tallyFeeAllowance,
                          votingDuration = args.votingDuration,
                          validityStart = (args.blockCreatedOn
                              + args.txTiming.minSettlementDuration
                              + args.txTiming.inactivityMarginDuration)
                              .toSlot(config.env.slotConfig)
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
                          treasuryUtxoSpent = settlementTxRes.transaction.treasuryProduced,
                          tallyFeeAllowance = args.tallyFeeAllowance,
                          votingDuration = args.votingDuration,
                          validityStart = (args.blockCreatedOn
                              + args.txTiming.minSettlementDuration
                              + args.txTiming.inactivityMarginDuration
                              + args.txTiming.silenceDuration)
                              .toSlot(config.env.slotConfig)
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
            majorVersionProduced: Block.Version.Major,
            treasuryToSpend: MultisigTreasuryUtxo,
            depositsToSpend: Vector[DepositUtxo],
            payoutObligationsRemaining: Vector[Payout.Obligation],
            kzgCommitment: KzgCommitment,
            tallyFeeAllowance: Coin,
            votingDuration: FiniteDuration,
            competingFallbackValidityStart: java.time.Instant,
            blockCreatedOn: java.time.Instant,
            txTiming: TxTiming
        )
        // TODO: confirm: this one is not needed
        // extends SingleArgs(kzgCommitment),
            extends Payout.Obligation.Many.Remaining {
            def toArgsNoPayouts: SingleArgs.NoPayouts =
                SingleArgs.NoPayouts(
                  majorVersionProduced = majorVersionProduced,
                  kzgCommitment = kzgCommitment,
                  treasuryToSpend = treasuryToSpend,
                  depositsToSpend = depositsToSpend,
                  validityEnd = competingFallbackValidityStart - txTiming.silenceDuration
                )

            def toArgsWithPayouts(
                rolloutTxSeqPartial: RolloutTxSeq.Builder.PartialResult
            ): SingleArgs.WithPayouts = SingleArgs.WithPayouts(
              majorVersionProduced = majorVersionProduced,
              treasuryToSpend = treasuryToSpend,
              kzgCommitment = kzgCommitment,
              depositsToSpend = depositsToSpend,
              rolloutTxSeqPartial = rolloutTxSeqPartial,
              validityEnd = competingFallbackValidityStart - txTiming.silenceDuration
            )
        }
    }
}
