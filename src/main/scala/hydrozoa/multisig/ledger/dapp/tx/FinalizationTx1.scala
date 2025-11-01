package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.{BuildErrorOr, explain}
import hydrozoa.multisig.ledger.dapp.txseq.RolloutTxSeq
import hydrozoa.multisig.ledger.dapp.utxo.{MultisigRegimeUtxo, ResidualTreasuryUtxo, RolloutUtxo, TreasuryUtxo}
import monocle.Focus.focus
import monocle.Monocle.refocus
import scala.Function.const
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler
import scalus.cardano.txbuilder.TransactionBuilder.{ResolvedUtxos, unsafeCtxTxOutputsL}
import scalus.cardano.txbuilder.TransactionBuilderStep.Spend
import scalus.|>

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
          * deposits) into a finalization tx. This conversion must NOT increase the tx size. This is
          * guaranteed by the following invariant: `size(treasuryUtxo.datum) < size(inputRef)`. The
          * upgrade is done by making two changes and following rebalancing:
          *   - spending multisig regime utxo
          *   - turning the treasury output into the residual treasury output.
          *
          * @param multisigUtxoToSpend
          *
          * @param args
          *   wrapper around [[SettlementTx]] with dependent Result
          *
          * @return
          *   Dependent mapping:
          *   - SettlementTx.NoPayouts -> FinalizationTx1.NoPayouts
          *   - SettlementTx.WithOnlyDirectPayouts -> FinalizationTx1.WithOnlyDirectPayouts
          *   - SettlementTx.WithRollouts -> FinalizationTx1.WithRollouts
          */
        def upgrade(
            config: Tx.Builder.Config,
            multisigUtxoToSpend: MultisigRegimeUtxo
        )(args: Args.Some): BuildErrorOr[args.Result] = {

            val ctx = args.input.ctx
            val tx = ctx.transaction

            // Manually upgrade the treasury output
            val treasuryOutputIndex = args.input.transaction.treasuryProduced.txId.index
            val treasuryOutput = tx.body.value.outputs(treasuryOutputIndex).value

            val residualTreasuryOutput = TransactionOutput.apply(
              treasuryOutput.address,
              treasuryOutput.value + multisigUtxoToSpend.value
            )

            val ctxUpgraded: TransactionBuilder.Context = ctx |> unsafeCtxTxOutputsL
                .refocus(_.index(treasuryOutputIndex))
                .replace(Sized.apply(residualTreasuryOutput))

            // Additional step
            val addMultisigRegimeInputStep =
                Spend(multisigUtxoToSpend.asUtxo, config.headNativeScript.witness)

            for {

                ctx <- TransactionBuilder
                    .modify(ctxUpgraded, List(addMultisigRegimeInputStep))
                    .explain(const("Could not modify (upgrade) settlement tx"))

                diffHandler = new ChangeOutputDiffHandler(
                  config.env.protocolParams,
                  treasuryOutputIndex
                ).changeOutputDiffHandler

                rebalanced <- ctx
                    .finalizeContext(
                      config.env.protocolParams,
                      diffHandler,
                      config.env.evaluator,
                      config.validators
                    )
                    .explain(const("Could not finalize context for finalization partial result"))

            } yield {
                val residualTreasuryUtxo = ResidualTreasuryUtxo.apply(
                  treasuryTokenName = args.input.transaction.treasurySpent.headTokenName,
                  multisigRegimeTokenName = multisigUtxoToSpend.multisigRegimeTokenName,
                  utxoId = TransactionInput(rebalanced.transaction.id, treasuryOutputIndex),
                  // FIXME: Why Shelley?
                  address = residualTreasuryOutput.address.asInstanceOf[ShelleyAddress],
                  value = residualTreasuryOutput.value
                )
                args.mkResult(
                  rebalanced.transaction,
                  residualTreasuryUtxo,
                  rebalanced.resolvedUtxos
                )
            }
        }

        object Args:

            sealed trait Some:
                def input: Input

                type Input <: SettlementTx.Builder.Result[_ <: SettlementTx]
                type Result <: FinalizationTx1

                def mkResult(
                    tx: Transaction,
                    residualTreasuryProduced: ResidualTreasuryUtxo,
                    resolvedUtxos: ResolvedUtxos
                ): Result

            // no payouts
            final case class NoPayoutsArg(override val input: SettlementTx.Builder.Result.NoPayouts)
                extends Some:
                override type Input = SettlementTx.Builder.Result.NoPayouts
                override type Result = FinalizationTx1.NoPayouts
                override def mkResult(
                    tx: Transaction,
                    residualTreasuryProduced: ResidualTreasuryUtxo,
                    resolvedUtxos: ResolvedUtxos
                ): Result =
                    NoPayouts(
                      tx,
                      this.input.transaction.treasurySpent,
                      residualTreasuryProduced,
                      resolvedUtxos
                    )

            extension (self: SettlementTx.Builder.Result.NoPayouts) def toArgs1 = NoPayoutsArg(self)

            // with only direct payouts
            final case class WithOnlyDirectPayoutsArgs(
                override val input: SettlementTx.Builder.Result.WithOnlyDirectPayouts
            ) extends Some:
                override type Input = SettlementTx.Builder.Result.WithOnlyDirectPayouts
                override type Result = FinalizationTx1.WithOnlyDirectPayouts
                override def mkResult(
                    tx: Transaction,
                    residualTreasuryProduced: ResidualTreasuryUtxo,
                    resolvedUtxos: ResolvedUtxos
                ): Result = WithOnlyDirectPayouts(
                  tx,
                  this.input.transaction.treasurySpent,
                  residualTreasuryProduced,
                  resolvedUtxos
                )

            extension (self: SettlementTx.Builder.Result.WithOnlyDirectPayouts)
                def toArgs1 = WithOnlyDirectPayoutsArgs(self)

            // with rollouts
            final case class WithRolloutsArgs(
                override val input: SettlementTx.Builder.Result.WithRollouts
            ) extends Some:
                override type Input = SettlementTx.Builder.Result.WithRollouts

                override type Result = TxWithRolloutTxSeq

                override def mkResult(
                    tx: Transaction,
                    residualTreasuryProduced: ResidualTreasuryUtxo,
                    resolvedUtxos: ResolvedUtxos
                ): Result = TxWithRolloutTxSeq(
                  WithRollouts(
                    tx,
                    this.input.transaction.treasurySpent,
                    residualTreasuryProduced,
                    this.input.transaction.rolloutProduced,
                    resolvedUtxos
                  ),
                  input.rolloutTxSeqPartial
                )

            final case class TxWithRolloutTxSeq(
                finalizationTx: FinalizationTx1.WithRollouts,
                rolloutTxSeqPartial: RolloutTxSeq.Builder.PartialResult
            ) extends FinalizationTx1 {
                override def tx: Transaction = finalizationTx.tx
                override def treasurySpent: TreasuryUtxo = finalizationTx.treasurySpent
                override def residualTreasuryProduced: ResidualTreasuryUtxo =
                    finalizationTx.residualTreasuryProduced
                override def resolvedUtxos: ResolvedUtxos = finalizationTx.resolvedUtxos
            }

            def apply[T](r: SettlementTx.Builder.Result.WithPayouts): Some =
                r match
                    case res: SettlementTx.Builder.Result.WithOnlyDirectPayouts =>
                        res.toArgs1
                    case res: SettlementTx.Builder.Result.WithRollouts =>
                        WithRolloutsArgs(res)

    }
}
