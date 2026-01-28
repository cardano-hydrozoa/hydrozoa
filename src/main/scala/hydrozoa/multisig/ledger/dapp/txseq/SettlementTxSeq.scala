package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.NonEmptyVector
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant}
import hydrozoa.multisig.ledger.block.BlockVersion
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67.TokenNames
import hydrozoa.multisig.ledger.dapp.tx
import hydrozoa.multisig.ledger.dapp.tx.*
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, MultisigRegimeUtxo, MultisigTreasuryUtxo}
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment
import scalus.cardano.ledger.{CardanoInfo, Coin}
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

    case class Config(
        headMultisigScript: HeadMultisigScript,
        multisigRegimeUtxo: MultisigRegimeUtxo,
        tokenNames: TokenNames,
        votingDuration: QuantizedFiniteDuration,
        txTiming: TxTiming,
        tallyFeeAllowance: Coin,
        cardanoInfo: CardanoInfo
    )

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
            lazy val settlementTxConfig: SettlementTx.Config = SettlementTx.Config(
              cardanoInfo = config.cardanoInfo,
              headMultisigScript = config.headMultisigScript,
              multisigRegimeUtxo = config.multisigRegimeUtxo
            )
            lazy val ftxConfig = FallbackTx.Config(
              headMultisigScript = config.headMultisigScript,
              multisigRegimeUtxo = config.multisigRegimeUtxo,
              tallyFeeAllowance = config.tallyFeeAllowance,
              tokenNames = config.tokenNames,
              votingDuration = config.votingDuration,
              cardanoInfo = config.cardanoInfo
            )
            lazy val rolloutTxSeqConfig = RolloutTxSeq.Config(
              cardanoInfo = config.cardanoInfo,
              headMultisigScript = config.headMultisigScript,
              multisigRegimeUtxo = config.multisigRegimeUtxo
            )

            NonEmptyVector.fromVector(args.payoutObligationsRemaining) match {
                case None =>

                    for {
                        settlementTx <- SettlementTx.Builder
                            .NoPayouts(settlementTxConfig)
                            .build(args.toArgsNoPayouts(config.txTiming))
                            .left
                            .map(Builder.Error.SettlementError(_))

                        ftxRecipe = FallbackTx.Recipe(
                          treasuryUtxoSpent = settlementTx.transaction.treasuryProduced,
                          validityStart = (args.blockCreatedOn
                              + config.txTiming.minSettlementDuration
                              + config.txTiming.inactivityMarginDuration).toSlot
                        )
                        fallbackTx <- FallbackTx
                            .build(ftxConfig, ftxRecipe)
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
                            .Builder(rolloutTxSeqConfig)
                            .buildPartial(nePayouts)
                            .left
                            .map(Error.RolloutSeqError(_))

                        settlementTxRes <- SettlementTx.Builder
                            .WithPayouts(settlementTxConfig)
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

                        ftxRecipe = FallbackTx.Recipe(
                          treasuryUtxoSpent = settlementTxRes.transaction.treasuryProduced,
                          validityStart = (args.blockCreatedOn
                              + config.txTiming.minSettlementDuration
                              + config.txTiming.inactivityMarginDuration
                              + config.txTiming.silenceDuration).toSlot
                        )
                        fallbackTx <- FallbackTx
                            .build(config = ftxConfig, recipe = ftxRecipe)
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
