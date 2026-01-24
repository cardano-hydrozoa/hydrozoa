package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedInstant, toQuantizedInstant}
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.FinalizationTx.{MergedDeinit, WithDeinit}
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.{BuildErrorOr, HasCtx, explain}
import hydrozoa.multisig.ledger.dapp.txseq.RolloutTxSeq
import hydrozoa.multisig.ledger.dapp.utxo.{MultisigRegimeUtxo, MultisigTreasuryUtxo, ResidualTreasuryUtxo, RolloutUtxo}
import hydrozoa.multisig.protocol.types.Block
import hydrozoa.prebalancedLovelaceDiffHandler
import monocle.Focus.focus
import monocle.Monocle.refocus
import monocle.{Focus, Lens}
import scala.Function.const
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.ledger.EvaluatorMode.EvaluateAndComputeCost
import scalus.cardano.ledger.TransactionException.InvalidTransactionSizeException
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.TransactionBuilder.{ResolvedUtxos, unsafeCtxTxOutputsL, unsafeCtxTxReferenceInputsL}
import scalus.cardano.txbuilder.TransactionBuilderStep.{Fee, Mint as MintStep, Send, Spend}
import scalus.|>

sealed trait FinalizationTx
    extends Tx[FinalizationTx],
      Block.Version.Major.Produced,
      MultisigTreasuryUtxo.Spent,
      ResidualTreasuryUtxo.MbProduced,
      RolloutUtxo.MbProduced,
      HasResolvedUtxos,
      HasValidityEnd {
    def tx: Transaction
    def txLens: Lens[FinalizationTx, Transaction]
}

object FinalizationTx {

    case class Config(
        cardanoInfo: CardanoInfo,
        headMultisigScript: HeadMultisigScript,
        multisigRegimeUtxo: MultisigRegimeUtxo
    ) {
        def evaluator: PlutusScriptEvaluator =
            PlutusScriptEvaluator(cardanoInfo, EvaluateAndComputeCost)
    }

    sealed trait Monolithic extends FinalizationTx

    sealed trait MergedDeinit extends FinalizationTx

    sealed trait WithDeinit extends FinalizationTx, ResidualTreasuryUtxo.Produced

    case class NoPayouts(
        override val majorVersionProduced: Block.Version.Major,
        override val tx: Transaction,
        override val validityEnd: QuantizedInstant,
        override val treasurySpent: MultisigTreasuryUtxo,
        override val residualTreasuryProduced: ResidualTreasuryUtxo,
        override val resolvedUtxos: ResolvedUtxos,
        override val txLens: Lens[FinalizationTx, Transaction] =
            Focus[NoPayouts](_.tx).asInstanceOf[Lens[FinalizationTx, Transaction]]
    ) extends WithDeinit,
          MergedDeinit

    case class NoPayoutsMerged(
        override val majorVersionProduced: Block.Version.Major,
        override val tx: Transaction,
        override val validityEnd: QuantizedInstant,
        override val treasurySpent: MultisigTreasuryUtxo,
        override val resolvedUtxos: ResolvedUtxos,
        override val txLens: Lens[FinalizationTx, Transaction] =
            Focus[NoPayoutsMerged](_.tx).asInstanceOf[Lens[FinalizationTx, Transaction]]
    ) extends Monolithic,
          MergedDeinit

    case class WithOnlyDirectPayouts(
        override val majorVersionProduced: Block.Version.Major,
        override val tx: Transaction,
        override val validityEnd: QuantizedInstant,
        override val treasurySpent: MultisigTreasuryUtxo,
        override val residualTreasuryProduced: ResidualTreasuryUtxo,
        override val resolvedUtxos: ResolvedUtxos,
        override val txLens: Lens[FinalizationTx, Transaction] =
            Focus[WithOnlyDirectPayouts](_.tx).asInstanceOf[Lens[FinalizationTx, Transaction]]
    ) extends WithDeinit

    case class WithOnlyDirectPayoutsMerged(
        override val majorVersionProduced: Block.Version.Major,
        override val tx: Transaction,
        override val validityEnd: QuantizedInstant,
        override val treasurySpent: MultisigTreasuryUtxo,
        override val resolvedUtxos: ResolvedUtxos,
        override val txLens: Lens[FinalizationTx, Transaction] =
            Focus[WithOnlyDirectPayoutsMerged](_.tx).asInstanceOf[Lens[FinalizationTx, Transaction]]
    ) extends Monolithic,
          MergedDeinit

    case class WithRollouts(
        override val majorVersionProduced: Block.Version.Major,
        override val tx: Transaction,
        override val validityEnd: QuantizedInstant,
        override val treasurySpent: MultisigTreasuryUtxo,
        override val residualTreasuryProduced: ResidualTreasuryUtxo,
        override val rolloutProduced: RolloutUtxo,
        override val resolvedUtxos: ResolvedUtxos,
        override val txLens: Lens[FinalizationTx, Transaction] =
            Focus[WithRollouts](_.tx).asInstanceOf[Lens[FinalizationTx, Transaction]]
    ) extends FinalizationTx,
          ResidualTreasuryUtxo.Produced,
          RolloutUtxo.Produced,
          WithDeinit

    case class WithRolloutsMerged(
        override val majorVersionProduced: Block.Version.Major,
        override val tx: Transaction,
        override val validityEnd: QuantizedInstant,
        override val treasurySpent: MultisigTreasuryUtxo,
        override val rolloutProduced: RolloutUtxo,
        override val resolvedUtxos: ResolvedUtxos,
        override val txLens: Lens[FinalizationTx, Transaction] =
            Focus[WithRolloutsMerged](_.tx).asInstanceOf[Lens[FinalizationTx, Transaction]]
    ) extends FinalizationTx,
          RolloutUtxo.Produced,
          MergedDeinit

    object Builder {

        /** Stage 1 of finalization tx building is upgrading the settlement tx (which takes no
          * deposits) into a finalization tx. This conversion must NOT increase the tx size. This is
          * guaranteed because:
          *   - exactly one ref input (the multisig regime utxo) is moved to inputs
          *   - its spending witness is already included
          *   - increase of residual treasury output value always be compensated by the removed
          *     datum
          *
          * @param config
          * @param args
          *   wrapper around [[SettlementTx]] with dependent Result
          * @return
          *   Dependent mapping:
          *   - SettlementTx.NoPayouts -> FinalizationTx1.NoPayouts
          *   - SettlementTx.WithOnlyDirectPayouts -> FinalizationTx1.WithOnlyDirectPayouts
          *   - SettlementTx.WithRollouts -> FinalizationTx1.WithRollouts
          */
        def upgrade(
            config: Config,
        )(args: Args.Some): BuildErrorOr[args.Result] = {

            val ctx = args.input.ctx
            val tx = ctx.transaction

            // Direct tx editing:
            // - upgrade the treasury output
            // - remove multisig regime utxo from referenced utxos by setting it to the empty set
            val treasuryOutputIndex = args.input.transaction.treasuryProduced.utxoId.index
            val treasuryOutput = tx.body.value.outputs(treasuryOutputIndex).value

            val residualTreasuryOutput = TransactionOutput.apply(
              treasuryOutput.address,
              treasuryOutput.value + config.multisigRegimeUtxo.value
            )

            val ctxUpgraded: TransactionBuilder.Context =
                ctx |> unsafeCtxTxOutputsL
                    .refocus(_.index(treasuryOutputIndex))
                    .replace(Sized.apply(residualTreasuryOutput))
                    |> unsafeCtxTxReferenceInputsL
                        .replace(TaggedSortedSet.empty)

            // Additional step - spend multisig regime utxo
            val spendMultisigRegimeUtxoStep =
                Spend(config.multisigRegimeUtxo.asUtxo, config.headMultisigScript.witnessAttached)

            for {
                ctx <- TransactionBuilder
                    .modify(ctxUpgraded, List(spendMultisigRegimeUtxoStep))
                    .explain(const("Could not modify (upgrade) settlement tx"))

                diffHandler = Change.changeOutputDiffHandler(
                  _,
                  _,
                  config.cardanoInfo.protocolParams,
                  treasuryOutputIndex
                )

                rebalanced <- ctx
                    .finalizeContext(
                      config.cardanoInfo.protocolParams,
                      diffHandler,
                      config.evaluator,
                      Tx.Validators.nonSigningNonValidityChecksValidators
                    )
                    .explain(const("Could not finalize context for finalization partial result"))

            } yield {
                val residualTreasuryUtxo = ResidualTreasuryUtxo.apply(
                  treasuryTokenName = args.input.transaction.treasurySpent.treasuryTokenName,
                  multisigRegimeTokenName = config.multisigRegimeUtxo.multisigRegimeTokenName,
                  utxoId = TransactionInput(rebalanced.transaction.id, treasuryOutputIndex),
                  // FIXME: Shall we be more specific about which outputs have which addresses?
                  address = residualTreasuryOutput.address.asInstanceOf[ShelleyAddress],
                  value = residualTreasuryOutput.value
                )
                args.mkResult(rebalanced, residualTreasuryUtxo)
            }
        }

        object Args:

            sealed trait Some:
                def input: Input

                type Input <: SettlementTx.Builder.Result[? <: SettlementTx]
                type Result <: PartialResult

                def mkResult(
                    ctx: TransactionBuilder.Context,
                    residualTreasuryProduced: ResidualTreasuryUtxo
                ): Result

            // no payouts
            final case class NoPayoutsArg(override val input: SettlementTx.Builder.Result.NoPayouts)
                extends Some:
                override type Input = SettlementTx.Builder.Result.NoPayouts
                override type Result = PartialResult.NoPayouts

                override def mkResult(
                    ctx: TransactionBuilder.Context,
                    residualTreasuryProduced: ResidualTreasuryUtxo
                ): Result =
                    PartialResult.NoPayouts(
                      this.input.transaction.treasurySpent,
                      residualTreasuryProduced,
                      ctx
                    )

            extension (self: SettlementTx.Builder.Result.NoPayouts) def toArgs1 = NoPayoutsArg(self)

            // with only direct payouts
            final case class WithOnlyDirectPayoutsArgs(
                override val input: SettlementTx.Builder.Result.WithOnlyDirectPayouts
            ) extends Some:
                override type Input = SettlementTx.Builder.Result.WithOnlyDirectPayouts
                override type Result = PartialResult.WithOnlyDirectPayouts

                override def mkResult(
                    ctx: TransactionBuilder.Context,
                    residualTreasuryProduced: ResidualTreasuryUtxo
                ): Result = PartialResult.WithOnlyDirectPayouts(
                  this.input.transaction.treasurySpent,
                  residualTreasuryProduced,
                  ctx
                )

            extension (self: SettlementTx.Builder.Result.WithOnlyDirectPayouts)
                def toArgs1 = WithOnlyDirectPayoutsArgs(self)

            // with rollouts
            final case class WithRolloutsArgs(
                override val input: SettlementTx.Builder.Result.WithRollouts
            ) extends Some:
                override type Input = SettlementTx.Builder.Result.WithRollouts

                override type Result = PartialResult.WithRollouts

                override def mkResult(
                    ctx: TransactionBuilder.Context,
                    residualTreasuryProduced: ResidualTreasuryUtxo
                ): Result = PartialResult.WithRollouts(
                  input.transaction.treasurySpent,
                  residualTreasuryProduced,
                  input.transaction.rolloutProduced
                      .focus(_.utxo.input.transactionId)
                      .replace(ctx.transaction.id),
                  ctx,
                  input.rolloutTxSeqPartial
                )

            extension (self: SettlementTx.Builder.Result.WithRollouts)
                def toArgs1 = WithRolloutsArgs(self)

        sealed trait PartialResult
            extends HasCtx,
              MultisigTreasuryUtxo.Spent,
              ResidualTreasuryUtxo.Produced,
              RolloutUtxo.MbProduced {

            type ResultFinal <: FinalizationTx

            final type Result = SeparateResult | MergedResult
            type SeparateResult <: WithDeinit
            type MergedResult <: MergedDeinit

            def mkSeparateResult(
                majorVersionProduced: Block.Version.Major,
                slotConfig: SlotConfig
            ): SeparateResult

            def mkMergedResult(
                mergedFinalizationTx: Transaction,
                majorVersionProduced: Block.Version.Major,
                slotConfig: SlotConfig
            ): MergedResult

            /* Stage 2 of finalization tx building - an attempt to merge with a deinit tx.
             */
            final def complete(
                deinitTx: DeinitTx,
                majorVersionProduced: Block.Version.Major,
                config: FinalizationTx.Config
            ): BuildErrorOr[Result] =

                /** TODO: update
                  *
                  * // @param args
                  * @return
                  *   Mapping:
                  *   - FinalizationTx2.NoPayouts -> FinalizationTx2.NoPayouts |
                  *     FinalizationTx2.NoPayoutsMerged
                  *   - FinalizationTx1.WithOnlyDirectPayouts ->
                  *     FinalizationTx2.WithOnlyDirectPayouts |
                  *     Finalization2.WithOnlyDirectPayoutsMerged
                  *   - FinalizationTx1.WithRollouts(in fact: TxWithRolloutTxSeq) ->
                  *     FinalizationTx2.WithRollouts | FinalizationTx2.WithRolloutsMerged
                  */

                // Preserve the original tx
                val originalTx = ctx.transaction

                val residualTreasuryIndex = this.residualTreasuryProduced.utxoId.index

                extension [A](list: IndexedSeq[A])
                    def removeAt(index: Int): IndexedSeq[A] =
                        list.take(index) ++ list.drop(index + 1)

                val ctxUpgraded = ctx |> unsafeCtxTxOutputsL
                    .modify(outputs => outputs.removeAt(residualTreasuryIndex))

                // Additional steps

                val deinitOutputSteps =
                    deinitTx.tx.body.value.outputs.toList.map(o => Send(o.value))
                val mintSteps = deinitTx.tx.body.value.mint.get.assets.toList
                    .flatMap(p =>
                        p._2.map(a =>
                            MintStep(p._1, a._1, a._2, config.headMultisigScript.witnessAttached)
                        )
                    )
                val feeStep = Fee(originalTx.body.value.fee + deinitTx.tx.body.value.fee)

                val res = for {
                    ctx <- TransactionBuilder
                        .modify(ctxUpgraded, deinitOutputSteps ++ mintSteps :+ feeStep)
                    res <- ctx
                        .finalizeContext(
                          config.cardanoInfo.protocolParams,
                          prebalancedLovelaceDiffHandler,
                          config.evaluator,
                          Tx.Validators.nonSigningValidators
                        )
                } yield res

                val separateResult =
                    mkSeparateResult(majorVersionProduced, config.cardanoInfo.slotConfig)

                Right(
                  res match {
                      case Right(ctx) =>
                          mkMergedResult(
                            ctx.transaction,
                            majorVersionProduced,
                            config.cardanoInfo.slotConfig
                          )
                      case Left(
                            SomeBuildError.ValidationError(e: InvalidTransactionSizeException, ctx)
                          ) =>
                          separateResult
                      // FIXME: ?
                      case Left(_) => separateResult
                  }
                )
        }

        // FIXME: There's a lot of duplication in here, especially with respect to
        // passing the slotConfig and majorVersionProduced
        object PartialResult {

            case class NoPayouts(
                override val treasurySpent: MultisigTreasuryUtxo,
                override val residualTreasuryProduced: ResidualTreasuryUtxo,
                override val ctx: TransactionBuilder.Context
            ) extends PartialResult {
                type SeparateResult = FinalizationTx.NoPayouts
                type MergedResult = FinalizationTx.NoPayoutsMerged

                override def mkSeparateResult(
                    majorVersionProduced: Block.Version.Major,
                    slotConfig: SlotConfig
                ): SeparateResult =
                    FinalizationTx.NoPayouts(
                      majorVersionProduced = majorVersionProduced,
                      tx = ctx.transaction,
                      validityEnd = Slot(ctx.transaction.body.value.ttl.get)
                          .toQuantizedInstant(slotConfig = slotConfig),
                      treasurySpent = treasurySpent,
                      residualTreasuryProduced = residualTreasuryProduced,
                      resolvedUtxos = ctx.resolvedUtxos
                    )

                override def mkMergedResult(
                    mergedFinalizationTx: Transaction,
                    majorVersionProduced: Block.Version.Major,
                    slotConfig: SlotConfig
                ): MergedResult =
                    FinalizationTx.NoPayoutsMerged(
                      majorVersionProduced = majorVersionProduced,
                      tx = mergedFinalizationTx,
                      validityEnd = Slot(mergedFinalizationTx.body.value.ttl.get)
                          .toQuantizedInstant(slotConfig),
                      treasurySpent = treasurySpent,
                      resolvedUtxos = ctx.resolvedUtxos
                    )
            }

            case class WithOnlyDirectPayouts(
                override val treasurySpent: MultisigTreasuryUtxo,
                override val residualTreasuryProduced: ResidualTreasuryUtxo,
                override val ctx: TransactionBuilder.Context
            ) extends PartialResult {
                type SeparateResult = FinalizationTx.WithOnlyDirectPayouts
                type MergedResult = FinalizationTx.WithOnlyDirectPayoutsMerged

                override def mkSeparateResult(
                    majorVersionProduced: Block.Version.Major,
                    slotConfig: SlotConfig
                ): SeparateResult =
                    FinalizationTx.WithOnlyDirectPayouts(
                      majorVersionProduced = majorVersionProduced,
                      tx = ctx.transaction,
                      validityEnd =
                          Slot(ctx.transaction.body.value.ttl.get).toQuantizedInstant(slotConfig),
                      treasurySpent = treasurySpent,
                      residualTreasuryProduced = residualTreasuryProduced,
                      resolvedUtxos = ctx.resolvedUtxos
                    )

                override def mkMergedResult(
                    mergedFinalizationTx: Transaction,
                    majorVersionProduced: Block.Version.Major,
                    slotConfig: SlotConfig
                ): MergedResult =
                    FinalizationTx.WithOnlyDirectPayoutsMerged(
                      majorVersionProduced = majorVersionProduced,
                      tx = mergedFinalizationTx,
                      validityEnd = Slot(mergedFinalizationTx.body.value.ttl.get)
                          .toQuantizedInstant(slotConfig),
                      treasurySpent = treasurySpent,
                      resolvedUtxos = ctx.resolvedUtxos
                    )
            }

            case class WithRollouts(
                override val treasurySpent: MultisigTreasuryUtxo,
                override val residualTreasuryProduced: ResidualTreasuryUtxo,
                override val rolloutProduced: RolloutUtxo,
                override val ctx: TransactionBuilder.Context,
                rolloutTxSeqPartial: RolloutTxSeq.Builder.PartialResult,
            ) extends PartialResult,
                  RolloutUtxo.Produced {

                type SeparateResult = FinalizationTx.WithRollouts
                type MergedResult = FinalizationTx.WithRolloutsMerged

                override def mkSeparateResult(
                    majorVersionProduced: Block.Version.Major,
                    slotConfig: SlotConfig
                ): SeparateResult =
                    FinalizationTx.WithRollouts(
                      majorVersionProduced = majorVersionProduced,
                      tx = ctx.transaction,
                      validityEnd =
                          Slot(ctx.transaction.body.value.ttl.get).toQuantizedInstant(slotConfig),
                      treasurySpent = treasurySpent,
                      residualTreasuryProduced = residualTreasuryProduced,
                      rolloutProduced = rolloutProduced,
                      resolvedUtxos = ctx.resolvedUtxos
                    )

                override def mkMergedResult(
                    mergedFinalizationTx: Transaction,
                    majorVersionProduced: Block.Version.Major,
                    slotConfig: SlotConfig
                ): MergedResult =
                    FinalizationTx.WithRolloutsMerged(
                      majorVersionProduced = majorVersionProduced,
                      tx = mergedFinalizationTx,
                      validityEnd = Slot(mergedFinalizationTx.body.value.ttl.get)
                          .toQuantizedInstant(slotConfig),
                      treasurySpent = treasurySpent,
                      rolloutProduced = rolloutProduced,
                      resolvedUtxos = ctx.resolvedUtxos
                    )
            }
        }
    }
}
