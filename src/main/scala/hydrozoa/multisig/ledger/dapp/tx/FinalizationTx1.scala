package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.BuildErrorOr
import hydrozoa.multisig.ledger.dapp.txseq.RolloutTxSeq
import hydrozoa.multisig.ledger.dapp.utxo.{
    MultisigRegimeUtxo,
    ResidualTreasuryUtxo,
    RolloutUtxo,
    TreasuryUtxo
}
import monocle.Focus.focus
import monocle.Lens
import monocle.Monocle.refocus
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TxBalancingError.Failed
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
          * @param settlementTx
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
        )(settlementTx: Args.Some): BuildErrorOr[settlementTx.Result] = {
            val tx = settlementTx.tx.tx
            val i = 0
            val treasuryOutput = tx.body.value.outputs(i).value

            val residualTreasuryOutput = TransactionOutput.apply(
              treasuryOutput.address,
              treasuryOutput.value + multisigUtxoToSpend.value
            )

            val addMultisigRegimeInput = (inputs: TaggedOrderedSet[TransactionInput]) =>
                TaggedOrderedSet.from(appendDistinct(multisigUtxoToSpend.utxoId, inputs.toSeq))

            val txUpgraded: Transaction = tx
                |> txInputsL.modify(addMultisigRegimeInput)
                |> txOutputsL.refocus(_.index(i)).replace(Sized.apply(residualTreasuryOutput))

            for {

                // Rebalance
                resolvedUtxos <- settlementTx.tx.resolvedUtxos
                    .addUtxo(multisigUtxoToSpend.asUtxo)
                    // TODO: simplify, that's awful
                    .toRight(
                      (
                        SomeBuildError.BalancingError.apply(
                          Failed(new IllegalStateException("error calculating resolvedUtxos"))
                        ),
                        "error calculating resolvedUtxos"
                      )
                    )

                rebalanced <- LowLevelTxBuilder
                    .balanceFeeAndChange(
                      txUpgraded,
                      i,
                      config.env.protocolParams,
                      resolvedUtxos.utxos,
                      config.env.evaluator
                    )
                    // TODO: simplify, too wordy
                    .left
                    .map(e =>
                        (
                          SomeBuildError.BalancingError.apply(e),
                          "error rebalancing stage 1 finalization tx "
                        )
                    )

            } yield {
                val residualTreasuryUtxo = ResidualTreasuryUtxo.apply(
                  treasuryTokenName = settlementTx.tx.treasurySpent.headTokenName,
                  multisigRegimeTokenName = multisigUtxoToSpend.multisigRegimeTokenName,
                  utxoId = TransactionInput(rebalanced.id, 0),
                  // FIXME: Why Shelley?
                  address = residualTreasuryOutput.address.asInstanceOf[ShelleyAddress],
                  value = residualTreasuryOutput.value
                )
                settlementTx.mkResult(rebalanced, residualTreasuryUtxo, resolvedUtxos)
            }
        }

        // TODO: upstream
        def txOutputsL: Lens[Transaction, IndexedSeq[Sized[TransactionOutput]]] = {
            txBodyL.refocus(_.outputs)
        }

        object Args:

            sealed trait Some:
                def tx: SettlementTx

                type Result <: FinalizationTx1

                def mkResult(
                    tx: Transaction,
                    residualTreasuryProduced: ResidualTreasuryUtxo,
                    resolvedUtxos: ResolvedUtxos
                ): Result

            // no payouts
            final case class NoPayoutsArg(override val tx: SettlementTx.NoPayouts) extends Some:
                override type Result = FinalizationTx1.NoPayouts
                override def mkResult(
                    tx: Transaction,
                    residualTreasuryProduced: ResidualTreasuryUtxo,
                    resolvedUtxos: ResolvedUtxos
                ): Result =
                    NoPayouts(tx, this.tx.treasurySpent, residualTreasuryProduced, resolvedUtxos)

            extension (self: SettlementTx.NoPayouts) def toArgs1 = NoPayoutsArg(self)

            // with only direct payouts
            final case class WithOnlyDirectPayoutsArgs(
                override val tx: SettlementTx.WithOnlyDirectPayouts
            ) extends Some:
                override type Result = FinalizationTx1.WithOnlyDirectPayouts

                override def mkResult(
                    tx: Transaction,
                    residualTreasuryProduced: ResidualTreasuryUtxo,
                    resolvedUtxos: ResolvedUtxos
                ): Result = WithOnlyDirectPayouts(
                  tx,
                  this.tx.treasurySpent,
                  residualTreasuryProduced,
                  resolvedUtxos
                )

            extension (self: SettlementTx.WithOnlyDirectPayouts)
                def toArgs1 = WithOnlyDirectPayoutsArgs(self)

            // with rollouts
            final case class WithRolloutsArgs(
                override val tx: SettlementTx.WithRollouts,
                private val rolloutTxSeqPartial: RolloutTxSeq.Builder.PartialResult
            ) extends Some:
                override type Result = TxWithRolloutTxSeq

                override def mkResult(
                    tx: Transaction,
                    residualTreasuryProduced: ResidualTreasuryUtxo,
                    resolvedUtxos: ResolvedUtxos
                ): Result = TxWithRolloutTxSeq(
                  WithRollouts(
                    tx,
                    this.tx.treasurySpent,
                    residualTreasuryProduced,
                    this.tx.rolloutProduced,
                    resolvedUtxos
                  ),
                  this.rolloutTxSeqPartial
                )

            final case class TxWithRolloutTxSeq(
                finalizationTx: FinalizationTx1.WithRollouts,
                rolloutTxSeqPartial: RolloutTxSeq.Builder.PartialResult
            ) extends FinalizationTx1 {
                override def tx: Transaction = finalizationTx.tx
                override def treasurySpent: TreasuryUtxo = finalizationTx.treasurySpent
                override def residualTreasuryProduced: ResidualTreasuryUtxo = finalizationTx.residualTreasuryProduced
                override def resolvedUtxos: ResolvedUtxos = finalizationTx.resolvedUtxos
            }

            def apply(r: SettlementTx.Builder.Result.WithPayouts): Some =
                r match
                    case res: SettlementTx.Builder.Result.WithOnlyDirectPayouts =>
                        res.transaction.toArgs1
                    case res: SettlementTx.Builder.Result.WithRollouts =>
                        WithRolloutsArgs(res.transaction, res.rolloutTxSeqPartial)

    }
}
