package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.NonEmptyVector
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.block.BlockVersion
import hydrozoa.multisig.ledger.dapp.tx
import hydrozoa.multisig.ledger.dapp.tx.*
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, MultisigTreasuryUtxo}
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment
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

    type Config = HeadConfig.Section

    extension (settlementTxSeq: SettlementTxSeq)

        def mbRollouts: List[RolloutTx] = settlementTxSeq match {
            case _: NoRollouts   => List.empty
            case r: WithRollouts => r.rolloutTxSeq.mbRollouts
        }

    import Builder.*

    final case class Builder(
        config: SettlementTxSeq.Config
    ) {
        def build(args: Args): Either[Builder.Error, Builder.Result] = {
            NonEmptyVector.fromVector(args.payoutObligationsRemaining) match {
                case None =>

                    for {
                        settlementTx <- SettlementTx.Builder
                            .NoPayouts(config)
                            .build(args.toArgsNoPayouts(config.txTiming))
                            .left
                            .map(Builder.Error.SettlementError(_))

                        ftxRecipe = ??? // FIXME
                        fallbackTx <- FallbackTx
                            .build(???, ???, ???, ???) // FIXME
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
                            .build(args.toArgsWithPayouts(rolloutTxSeqPartial, config.txTiming))
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

                        ftxRecipe = ??? // FIXME
                        fallbackTx <- FallbackTx
                            .build(???, ???, ???, ???) // FIXME
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
            majorVersionProduced: BlockVersion.Major,
            treasuryToSpend: MultisigTreasuryUtxo,
            depositsToSpend: Vector[DepositUtxo],
            payoutObligationsRemaining: Vector[Payout.Obligation],
            kzgCommitment: KzgCommitment,
            competingFallbackValidityStart: QuantizedInstant,
            blockCreatedOn: QuantizedInstant,
        )
        // TODO: confirm: this one is not needed
        // extends SingleArgs(kzgCommitment),
            extends Payout.Obligation.Many.Remaining {
            def toArgsNoPayouts(txTiming: TxTiming): SingleArgs.NoPayouts =
                SingleArgs.NoPayouts(
                  majorVersionProduced = majorVersionProduced,
                  kzgCommitment = kzgCommitment,
                  treasuryToSpend = treasuryToSpend,
                  depositsToSpend = depositsToSpend,
                  validityEnd = competingFallbackValidityStart - txTiming.silenceDuration
                )

            def toArgsWithPayouts(
                rolloutTxSeqPartial: RolloutTxSeq.Builder.PartialResult,
                txTiming: TxTiming
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
