package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.BuildErrorOr
import hydrozoa.multisig.ledger.dapp.txseq.RolloutTxSeq
import hydrozoa.multisig.ledger.dapp.utxo.{
    MultisigRegimeUtxo,
    ResidualTreasuryUtxo,
    RolloutUtxo,
    TreasuryUtxo
}
import scalus.cardano.ledger.Transaction
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos

sealed trait FinalizationTx1
    extends Tx,
      TreasuryUtxo.Spent,
      ResidualTreasuryUtxo.Produced,
      RolloutUtxo.MbProduced,
      HasResolvedUtxos

// TODO: add versions
object FinalizationTx1 {

    case class NoPayouts(
        override val tx: Transaction,
        override val treasurySpent: TreasuryUtxo,
        override val residualTreasuryProduced: ResidualTreasuryUtxo,
        override val resolvedUtxos: ResolvedUtxos
    ) extends FinalizationTx1

    case class WithOnlyDirectPayouts(
        override val tx: Transaction,
        override val treasurySpent: TreasuryUtxo,
        override val residualTreasuryProduced: ResidualTreasuryUtxo,
        override val resolvedUtxos: ResolvedUtxos
    ) extends FinalizationTx1

    case class WithRollouts(
        override val tx: Transaction,
        override val treasurySpent: TreasuryUtxo,
        override val residualTreasuryProduced: ResidualTreasuryUtxo,
        override val rolloutProduced: RolloutUtxo,
        override val resolvedUtxos: ResolvedUtxos
    ) extends FinalizationTx1,
          RolloutUtxo.Produced

    object Builder {

        /** Stage 1 of finalization tx building - upgrading the settlement tx (which takes no
          * deposits) into a finalization tx.
          *
          * @param multisigUtxoToSpend
          *
          * @param settlementTxArg
          *   wrapper around [[SettlementTx]] with dependent Result
          *
          * @return
          *   Dependent mapping:
          *   - SettlementTx.NoPayouts -> FinalizationTx1.NoPayouts
          *   - SettlementTx.WithOnlyDirectPayouts -> FinalizationTx1.WithOnlyDirectPayouts
          *   - SettlementTx.WithRollouts -> FinalizationTx1.WithRollouts
          */
        def upgrade(
            multisigUtxoToSpend: MultisigRegimeUtxo
        )(settlementTxArg: Args.Some): BuildErrorOr[settlementTxArg.Result] = {
            ???
        }

        object Args:

            sealed trait Some:
                type Result

            // no payouts
            final case class NoPayoutsArg(tx: SettlementTx.NoPayouts) extends Some:
                override type Result = FinalizationTx1.NoPayouts

            extension (self: SettlementTx.NoPayouts) def toArgs1 = NoPayoutsArg(self)

            // with only direct payouts
            final case class WithOnlyDirectPayoutsArgs(tx: SettlementTx.WithOnlyDirectPayouts)
                extends Some:
                override type Result = FinalizationTx1.WithOnlyDirectPayouts

            extension (self: SettlementTx.WithOnlyDirectPayouts)
                def toArgs1 = WithOnlyDirectPayoutsArgs(self)

            // with rollouts
            final case class WithRolloutsArgs(
                tx: SettlementTx.WithRollouts,
                rolloutTxSeqPartial: RolloutTxSeq.Builder.PartialResult
            ) extends Some:
                override type Result = WithRolloutsResult

            final case class WithRolloutsResult(
                tx: FinalizationTx1.WithRollouts,
                rolloutTxSeqPartial: RolloutTxSeq.Builder.PartialResult
            )

            def apply(r: SettlementTx.Builder.Result.WithPayouts): Some =
                r match
                    case res: SettlementTx.Builder.Result.WithOnlyDirectPayouts =>
                        res.transaction.toArgs1
                    case res: SettlementTx.Builder.Result.WithRollouts =>
                        WithRolloutsArgs(res.transaction, res.rolloutTxSeqPartial)

    }
}
