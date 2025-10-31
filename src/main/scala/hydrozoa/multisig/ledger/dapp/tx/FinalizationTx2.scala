package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.BuildErrorOr
import hydrozoa.multisig.ledger.dapp.utxo
import hydrozoa.multisig.ledger.dapp.utxo.{ResidualTreasuryUtxo, RolloutUtxo, TreasuryUtxo}
import scalus.cardano.ledger.Transaction
import scalus.cardano.txbuilder.TransactionBuilder
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos

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
          *   - FinalizationTx1.WithRollouts -> FinalizationTx2.WithRollouts |
          *     FinalizationTx2.WithRolloutsMerged
          */
        def build(deinitTx: DeinitTx)(args: Args.Some): BuildErrorOr[args.Result] =
            ???

        // TODO: pull up deinitTx by currying build?
        object Args:
            trait Some:
                type Result <: FinalizationTx2

            final case class NoPayoutsArg(
                tx: FinalizationTx1.NoPayouts
            ) extends Some:
                type Result = NoPayouts | NoPayoutsMerged

            extension (self: FinalizationTx1.NoPayouts) def toArgs2 = NoPayoutsArg(self)

            final case class WithOnlyDirectPayoutsArg(
                tx: FinalizationTx1.WithOnlyDirectPayouts
            ) extends Some:
                type Result = WithOnlyDirectPayouts | WithOnlyDirectPayoutsMerged

            extension (self: FinalizationTx1.WithOnlyDirectPayouts)
                def toArgs2 = WithOnlyDirectPayoutsArg(self)

            final case class WithRolloutsArg(
                tx: FinalizationTx1.WithRollouts
            ) extends Some:
                type Result = WithRollouts | WithRolloutsMerged

            extension (self: FinalizationTx1.WithRollouts) def toArgs2 = WithRolloutsArg(self)
    }
}
