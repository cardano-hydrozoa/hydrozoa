package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.multisig.ledger.dapp.tx.FinalizationTx1.Builder.txOutputsL
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.BuildErrorOr
import hydrozoa.multisig.ledger.dapp.utxo
import hydrozoa.multisig.ledger.dapp.utxo.{ResidualTreasuryUtxo, RolloutUtxo, TreasuryUtxo}
import monocle.Focus.refocus
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.{TransactionBuilder, txBodyL}
import scalus.|>

sealed trait FinalizationTx2
    extends Tx,
      TreasuryUtxo.Spent,
      ResidualTreasuryUtxo.MbProduced,
      RolloutUtxo.MbProduced,
      HasResolvedUtxos

object FinalizationTx2 {

    sealed trait Monolithic extends FinalizationTx2
    sealed trait WithDeinit extends FinalizationTx2, ResidualTreasuryUtxo.Produced

    case class NoPayouts(
        override val tx: Transaction,
        override val treasurySpent: TreasuryUtxo,
        override val residualTreasuryProduced: ResidualTreasuryUtxo,
        override val resolvedUtxos: ResolvedUtxos
    ) extends WithDeinit

    case class NoPayoutsMerged(
        override val tx: Transaction,
        override val treasurySpent: TreasuryUtxo,
        override val resolvedUtxos: ResolvedUtxos
    ) extends Monolithic

    case class WithOnlyDirectPayouts(
        override val tx: Transaction,
        override val treasurySpent: TreasuryUtxo,
        override val residualTreasuryProduced: ResidualTreasuryUtxo,
        override val resolvedUtxos: ResolvedUtxos
    ) extends WithDeinit

    case class WithOnlyDirectPayoutsMerged(
        override val tx: Transaction,
        override val treasurySpent: TreasuryUtxo,
        override val resolvedUtxos: ResolvedUtxos
    ) extends Monolithic

    case class WithRollouts(
        override val tx: Transaction,
        override val treasurySpent: TreasuryUtxo,
        override val residualTreasuryProduced: ResidualTreasuryUtxo,
        override val rolloutProduced: RolloutUtxo,
        override val resolvedUtxos: ResolvedUtxos
    ) extends FinalizationTx2,
          ResidualTreasuryUtxo.Produced,
          RolloutUtxo.Produced

    case class WithRolloutsMerged(
        override val tx: Transaction,
        override val treasurySpent: TreasuryUtxo,
        override val rolloutProduced: RolloutUtxo,
        override val resolvedUtxos: ResolvedUtxos
    ) extends FinalizationTx2,
          RolloutUtxo.Produced

    object Builder {

        /* Stage 2 of finalization tx building - an attempt to merge with a deinit tx.
         */

        /** @param args
          * @return
          *   Mapping:
          *   - FinalizationTx2.NoPayouts -> FinalizationTx2.NoPayouts |
          *     FinalizationTx2.NoPayoutsMerged
          *
          *   - FinalizationTx1.WithOnlyDirectPayouts -> FinalizationTx2.WithOnlyDirectPayouts |
          *     Finalization2.WithOnlyDirectPayoutsMerged
          *
          *   - FinalizationTx1.WithRollouts(in fact: TxWithRolloutTxSeq) ->
          *     FinalizationTx2.WithRollouts | FinalizationTx2.WithRolloutsMerged
          */
        def build(deinitTx: DeinitTx)(args: Args.Some): BuildErrorOr[args.Result] =

            val deinitOutputs: List[Sized[TransactionOutput]] =
                deinitTx.tx.body.value.outputs.toList
            val mint: Option[Mint] = deinitTx.tx.body.value.mint
            val deinitFee: Coin = deinitTx.tx.body.value.fee

            val originalTx = args.tx.tx

            // This should be done with the tx builder
            val optimisticTrial = originalTx
                |> txOutputsL.modify(outputs => outputs.tail ++ deinitOutputs)
                |> txBodyL.refocus(_.mint).replace(mint)
                |> txBodyL.refocus(_.fee).modify(fee => fee + deinitFee)

            // TODO: validate optimisticTrial
            val isValid = ???

            if isValid
            then Right(args.mkMergedResult(optimisticTrial))
            else Right(args.mkSeparateResult)

        // TODO: pull up deinitTx by currying build?
        object Args:
            trait Some:
                def tx: FinalizationTx1
                final type Result = SeparateResult | MergedResult
                type SeparateResult
                type MergedResult
                def mkSeparateResult: SeparateResult
                def mkMergedResult(mergedFinalizationTx: Transaction): MergedResult

            final case class NoPayoutsArg(
                override val tx: FinalizationTx1.NoPayouts
            ) extends Some:
                type SeparateResult = NoPayouts
                type MergedResult = NoPayoutsMerged

                override def mkSeparateResult: SeparateResult =
                    NoPayouts(
                      tx.tx,
                      tx.treasurySpent,
                      tx.residualTreasuryProduced,
                      tx.resolvedUtxos
                    )

                override def mkMergedResult(mergedFinalizationTx: Transaction): MergedResult =
                    NoPayoutsMerged(mergedFinalizationTx, tx.treasurySpent, tx.resolvedUtxos)

            extension (self: FinalizationTx1.NoPayouts) def toArgs2 = NoPayoutsArg(self)

            final case class WithOnlyDirectPayoutsArg(
                override val tx: FinalizationTx1.WithOnlyDirectPayouts
            ) extends Some:
                type SeparateResult = WithOnlyDirectPayouts
                type MergedResult = WithOnlyDirectPayoutsMerged

                override def mkSeparateResult: SeparateResult =
                    WithOnlyDirectPayouts(
                      tx.tx,
                      tx.treasurySpent,
                      tx.residualTreasuryProduced,
                      tx.resolvedUtxos
                    )
                override def mkMergedResult(mergedFinalizationTx: Transaction): MergedResult =
                    WithOnlyDirectPayoutsMerged(
                      mergedFinalizationTx,
                      tx.treasurySpent,
                      tx.resolvedUtxos
                    )

            extension (self: FinalizationTx1.WithOnlyDirectPayouts)
                def toArgs2 = WithOnlyDirectPayoutsArg(self)

            final case class WithRolloutsArg(
                override val tx: FinalizationTx1.WithRollouts
            ) extends Some:
                type SeparateResult = WithRollouts
                type MergedResult = WithRolloutsMerged

                override def mkSeparateResult: SeparateResult =
                    WithRollouts(
                      tx.tx,
                      tx.treasurySpent,
                      tx.residualTreasuryProduced,
                      tx.rolloutProduced,
                      tx.resolvedUtxos
                    )

                override def mkMergedResult(mergedFinalizationTx: Transaction): MergedResult =
                    WithRolloutsMerged(
                      mergedFinalizationTx,
                      tx.treasurySpent,
                      tx.rolloutProduced,
                      tx.resolvedUtxos
                    )

            extension (self: FinalizationTx1.WithRollouts) def toArgs2 = WithRolloutsArg(self)
    }
}
