package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.tx.Metadata.Settlement
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.{BuildErrorOr, HasCtx, explainConst}
import hydrozoa.multisig.ledger.dapp.tx.TxTiming.*
import hydrozoa.multisig.ledger.dapp.txseq.RolloutTxSeq
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, MultisigTreasuryUtxo, RolloutUtxo}
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.protocol.types.Block
import hydrozoa.multisig.protocol.types.Block.Version.Major
import java.time.Instant
import scala.annotation.tailrec
import scala.collection.immutable.Vector
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.{Sized, Slot, SlotConfig, Transaction, TransactionInput, TransactionOutput as TxOutput, Utxo, Value}
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.ScriptSource.NativeScriptAttached
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.*

// TODO: why don't we have direct payouts here?
sealed trait SettlementTx
    extends Tx,
      Block.Version.Major.Produced,
      MultisigTreasuryUtxo.Spent,
      MultisigTreasuryUtxo.Produced,
      DepositUtxo.Many.Spent,
      RolloutUtxo.MbProduced,
      HasResolvedUtxos,
      HasValidityEnd

object SettlementTx {
    import Builder.*
    import BuilderOps.*

    sealed trait WithPayouts extends SettlementTx
    sealed trait NoRollouts extends SettlementTx

    case class NoPayouts(
        override val validityEnd: Instant,
        override val tx: Transaction,
        override val majorVersionProduced: Major,
        override val treasurySpent: MultisigTreasuryUtxo,
        override val treasuryProduced: MultisigTreasuryUtxo,
        override val depositsSpent: Vector[DepositUtxo],
        override val resolvedUtxos: ResolvedUtxos
    ) extends NoRollouts

    case class WithOnlyDirectPayouts(
        override val validityEnd: Instant,
        override val tx: Transaction,
        override val majorVersionProduced: Block.Version.Major,
        override val treasurySpent: MultisigTreasuryUtxo,
        override val treasuryProduced: MultisigTreasuryUtxo,
        override val depositsSpent: Vector[DepositUtxo],
        override val resolvedUtxos: ResolvedUtxos
    ) extends WithPayouts,
          NoRollouts

    case class WithRollouts(
        override val validityEnd: Instant,
        override val tx: Transaction,
        override val majorVersionProduced: Block.Version.Major,
        override val treasurySpent: MultisigTreasuryUtxo,
        override val treasuryProduced: MultisigTreasuryUtxo,
        override val depositsSpent: Vector[DepositUtxo],
        override val rolloutProduced: RolloutUtxo,
        override val resolvedUtxos: ResolvedUtxos
    ) extends WithPayouts,
          RolloutUtxo.Produced

    object Builder {
        final case class NoPayouts(override val config: Tx.Builder.Config)
            extends Builder[SettlementTx.NoPayouts] {

            override type ArgsType = Args.NoPayouts
            override type ResultType = Result.NoPayouts

            override def complete(
                args: ArgsType,
                state: State[SettlementTx.NoPayouts]
            ): BuildErrorOr[ResultType] = Right(
              Complete.completeNoPayouts(args, state, config.env.slotConfig)
            )
        }

        final case class WithPayouts(override val config: Tx.Builder.Config)
            extends Builder[SettlementTx.WithPayouts] {

            override type ArgsType = Args.WithPayouts
            override type ResultType =
                Result.WithOnlyDirectPayouts | Result.WithRollouts // WithResult.WithPayouts

            override def complete(
                args: ArgsType,
                state: State[SettlementTx.WithPayouts]
            ): BuildErrorOr[ResultType] = for {
                mergeTrial <- Merge.tryMergeFirstRollout(
                  this.finish,
                  state,
                  args.treasuryToSpend,
                  args.rolloutTxSeqPartial
                )
                (finished, mergeResult) = mergeTrial
            } yield Complete.completeWithPayouts(args, finished, mergeResult, config.env.slotConfig)

        }

        trait Result[T <: SettlementTx]
            extends Tx.AugmentedResult[T],
              DepositUtxo.Many.Spent.Partition,
              HasCtx

        object Result {
            sealed trait WithPayouts extends Result[SettlementTx.WithPayouts]
            type NoRollouts = Result[SettlementTx.NoRollouts]

            case class NoPayouts(
                // TODO: already in the context
                override val transaction: SettlementTx.NoPayouts,
                override val depositsSpent: Vector[DepositUtxo],
                override val depositsToSpend: Vector[DepositUtxo],
                override val ctx: TransactionBuilder.Context
            ) extends Result[SettlementTx.NoPayouts]

            case class WithOnlyDirectPayouts(
                // TODO: already in the context
                override val transaction: SettlementTx.WithOnlyDirectPayouts,
                override val depositsSpent: Vector[DepositUtxo],
                override val depositsToSpend: Vector[DepositUtxo],
                override val ctx: TransactionBuilder.Context
            ) extends WithPayouts

            case class WithRollouts(
                // TODO: already in the context
                override val transaction: SettlementTx.WithRollouts,
                override val depositsSpent: Vector[DepositUtxo],
                override val depositsToSpend: Vector[DepositUtxo],
                rolloutTxSeqPartial: RolloutTxSeq.Builder.PartialResult,
                override val ctx: TransactionBuilder.Context
            ) extends WithPayouts
        }

        trait Args(val kzgCommitment: KzgCommitment)
            extends Block.Version.Major.Produced,
              MultisigTreasuryUtxo.ToSpend,
              DepositUtxo.Many.ToSpend,
              HasValidityEnd {
            final def mbRolloutValue: Option[Value] =
                this match {
                    case a: Args.WithPayouts =>
                        Some(a.rolloutTxSeqPartial.firstOrOnly.inputValueNeeded)
                    case _: Args.NoPayouts => None
                }
        }

        object Args {
            final case class NoPayouts(
                override val kzgCommitment: KzgCommitment,
                override val majorVersionProduced: Block.Version.Major,
                override val treasuryToSpend: MultisigTreasuryUtxo,
                // FIXME (Peter, 2025-12-10): If there's no deposits and no payouts,
                // then this is a "roll forward" transaction, which is supposed to
                // reset the fallback timer.
                // We want a better approach for this in the future
                override val depositsToSpend: Vector[DepositUtxo],
                override val validityEnd: java.time.Instant
            ) extends Args(kzgCommitment)

            final case class WithPayouts(
                override val kzgCommitment: KzgCommitment,
                override val majorVersionProduced: Block.Version.Major,
                override val treasuryToSpend: MultisigTreasuryUtxo,
                override val depositsToSpend: Vector[DepositUtxo],
                override val validityEnd: java.time.Instant,
                rolloutTxSeqPartial: RolloutTxSeq.Builder.PartialResult
            ) extends Args(kzgCommitment)

        }

        final case class State[T <: SettlementTx](
            override val ctx: TransactionBuilder.Context,
            override val depositsSpent: Vector[DepositUtxo],
            override val depositsToSpend: Vector[DepositUtxo]
        ) extends Tx.Builder.HasCtx,
              DepositUtxo.Many.Spent.Partition
    }

    trait Builder[T <: SettlementTx] extends Tx.Builder {
        type ArgsType <: Args

        type ResultType
        def complete(args: ArgsType, state: State[T]): BuildErrorOr[ResultType]

        final def build(args: ArgsType): BuildErrorOr[ResultType] = {
            for {
                pessimistic <- basePessimistic(args)
                addedDeposits <- addDeposits(args, pessimistic)
                // Balancing and fees
                finished <- this
                    .finish(addedDeposits.ctx)
                    .explainConst("finishing settlement tx failed")
                completed <- complete(args, addedDeposits.copy(ctx = finished))
            } yield completed
        }

        private final def basePessimistic(args: ArgsType): BuildErrorOr[State[T]] = {
            val steps =
                BasePessimistic.steps(config, args)
            for {
                ctx <- TransactionBuilder
                    .build(config.env.network, steps)
                    .explainConst("base pessimistic build failed")
                addedPessimisticRollout <- BasePessimistic.mbApplySendRollout(
                  args.treasuryToSpend,
                  args.mbRolloutValue
                )(ctx)
                _ <- finish(addedPessimisticRollout).explainConst(
                  "finishing base pessimistic failed"
                )
            } yield State[T](
              ctx = ctx,
              depositsSpent = Vector.empty,
              depositsToSpend = args.depositsToSpend
            )
        }

        private final def addDeposits(
            args: Args,
            initialState: State[T]
        ): BuildErrorOr[State[T]] = {
            val addPessimisticRollout =
                BasePessimistic.mbApplySendRollout(args.treasuryToSpend, args.mbRolloutValue)

            @tailrec
            def loop(state: State[T]): BuildErrorOr[State[T]] = {
                import state.*
                depositsToSpend match {
                    case deposit +: otherDeposits =>
                        tryAddDeposit(ctx, deposit) match {
                            case Right(x) =>
                                val newState: State[T] = state.copy(
                                  ctx = x,
                                  depositsSpent = depositsSpent :+ deposit,
                                  depositsToSpend = otherDeposits
                                )
                                loop(newState)
                            case Left(err) =>
                                Tx.Builder.Incremental
                                    .replaceInvalidSizeException(err._1, state)
                                    .explainConst(err._2)
                        }
                    case _Empty => Right(state)
                }
            }

            def tryAddDeposit(
                ctx: TransactionBuilder.Context,
                deposit: DepositUtxo
            ): BuildErrorOr[TransactionBuilder.Context] =
                val depositStep = Spend(
                  deposit.toUtxo,
                  NativeScriptWitness(
                    NativeScriptAttached,
                    config.headNativeScript.requiredSigners
                  )
                )
                for {
                    newCtx <- TransactionBuilder
                        .modify(ctx, List(depositStep))
                        .explainConst(s"adding deposit utxo failed. Deposit utxo: $deposit")
                    // TODO: update the non-ADA assets in the treasury output, based on the absorbed deposits
                    //
                    // Ensure that at least the pessimistic rollout output fits into the transaction.
                    addedPessimisticRollout <- addPessimisticRollout(newCtx)
                    _ <- finish(addedPessimisticRollout).explainConst(
                      "finishing for tryAddDeposit failed."
                    )
                } yield newCtx

            loop(initialState)
        }
    }

    private object BuilderOps {
        object BasePessimistic {
            def steps(config: Tx.Builder.Config, args: Args): List[TransactionBuilderStep] =
                List(
                  stepSettlementMetadata(config),
                  referenceHNS(config),
                  consumeTreasury(config, args.treasuryToSpend),
                  sendTreasury(args),
                  validityEndSlot(args.validityEnd.toSlot(config.env.slotConfig)),
                )

            private def stepSettlementMetadata(config: Tx.Builder.Config): ModifyAuxiliaryData =
                ModifyAuxiliaryData(_ => Some(MD(Settlement(headAddress = config.headAddress))))

            private def referenceHNS(config: Tx.Builder.Config) =
                ReferenceOutput(config.multisigRegimeUtxo.asUtxo)

            private def validityEndSlot(slot: Slot): ValidityEndSlot =
                ValidityEndSlot(slot.slot)

            private def consumeTreasury(
                config: Tx.Builder.Config,
                treasuryToSpend: MultisigTreasuryUtxo
            ): Spend =
                Spend(treasuryToSpend.asUtxo, config.headNativeScript.witness)

            private def sendTreasury(args: Args): Send =
                Send(treasuryOutput(args))

            private def treasuryOutput(args: Args): TxOutput.Babbage = {
                TxOutput.Babbage(
                  address = args.treasuryToSpend.address,
                  value = treasuryOutputValue(args.treasuryToSpend, args.mbRolloutValue),
                  datumOption = Some(
                    Inline(
                      MultisigTreasuryUtxo
                          .Datum(
                            ByteString.fromArray(
                              IArray.genericWrapArray(args.kzgCommitment).toArray
                            ),
                            args.majorVersionProduced,
                            ByteString.empty
                          )
                          .toData
                    )
                  ),
                  scriptRef = None
                )
            }

            private def treasuryOutputValue(
                treasurySpent: MultisigTreasuryUtxo,
                mbRolloutValue: Option[Value]
            ): Value =
                mbRolloutValue.fold(treasurySpent.value)(treasurySpent.value - _)

            def mbApplySendRollout(
                treasuryToSpend: MultisigTreasuryUtxo,
                mbRolloutValue: Option[Value]
            ): (
                ctx: TransactionBuilder.Context
            ) => BuildErrorOr[TransactionBuilder.Context] =
                mbRolloutValue match {
                    case None        => Right(_)
                    case Some(value) => applySendRollout(treasuryToSpend, value)
                }

            def applySendRollout(treasuryToSpend: MultisigTreasuryUtxo, rolloutValue: Value)(
                ctx: TransactionBuilder.Context
            ): BuildErrorOr[TransactionBuilder.Context] = {
                val extraStep = Send(rolloutOutput(treasuryToSpend, rolloutValue))
                for {
                    newCtx <- TransactionBuilder
                        .modify(ctx, List(extraStep))
                        .explainConst("sending the rollout tx failed")
                } yield newCtx
            }

            private def rolloutOutput(
                treasuryToSpend: MultisigTreasuryUtxo,
                firstRolloutTxInputValue: Value
            ): TxOutput.Babbage =
                TxOutput.Babbage(
                  address = treasuryToSpend.address,
                  value = firstRolloutTxInputValue,
                  datumOption = None,
                  scriptRef = None
                )
        }

        object Complete {

            import Builder.*

            /** When building a settlement transaction without payouts, apply post-processing to
              * assemble the result. Assumes that:
              *
              *   - The spent treasury utxo is the first input (unchecked).
              *   - The produced treasury utxo is the first output (asserted).
              *
              * @throws AssertionError
              *   when the asserted assumptions are broken
              */
            @throws[AssertionError]
            def completeNoPayouts(
                args: Args.NoPayouts,
                state: State[SettlementTx.NoPayouts],
                slotConfig: SlotConfig
            ): Result.NoPayouts = {
                val treasuryProduced = PostProcess.getTreasuryProduced(
                  args.majorVersionProduced,
                  args.treasuryToSpend,
                  state
                )

                val settlementTx: SettlementTx.NoPayouts = SettlementTx.NoPayouts(
                  validityEnd =
                      Slot(state.ctx.transaction.body.value.ttl.get).toInstant(slotConfig),
                  tx = state.ctx.transaction,
                  majorVersionProduced = args.majorVersionProduced,
                  treasurySpent = args.treasuryToSpend,
                  treasuryProduced = treasuryProduced,
                  depositsSpent = state.depositsSpent,
                  resolvedUtxos = state.ctx.resolvedUtxos
                )
                Result.NoPayouts(
                  transaction = settlementTx,
                  depositsSpent = state.depositsSpent,
                  depositsToSpend = state.depositsToSpend,
                  ctx = state.ctx
                )
            }

            /** When building a settlement transaction with payouts, try to merge the first rollout,
              * and then apply post-processing to assemble the result. Assumes that:
              *
              *   - The spent treasury utxo is the first input (unchecked).
              *   - The produced treasury utxo is the first output (asserted).
              *   - The produced rollout utxo is the second output (asserted).
              *
              * @throws AssertionError
              *   when the asserted assumptions are broken.
              */
            @throws[AssertionError]
            def completeWithPayouts(
                args: Args.WithPayouts,
                state: State[SettlementTx.WithPayouts],
                mergeResult: Merge.Result,
                slotConfig: SlotConfig
            ): Result.WithOnlyDirectPayouts | Result.WithRollouts = {
                import Merge.Result.*

                val tx = state.ctx.transaction

                val treasuryProduced = PostProcess.getTreasuryProduced(
                  args.majorVersionProduced,
                  args.treasuryToSpend,
                  state
                )

                def withOnlyDirectPayouts: Result.WithOnlyDirectPayouts =
                    Result.WithOnlyDirectPayouts(
                      transaction = SettlementTx.WithOnlyDirectPayouts(
                        majorVersionProduced = args.majorVersionProduced,
                        treasurySpent = args.treasuryToSpend,
                        treasuryProduced = treasuryProduced,
                        depositsSpent = state.depositsSpent,
                        tx = tx,
                        // this is safe since we always set ttl
                        validityEnd = Slot(tx.body.value.ttl.get).toInstant(slotConfig),
                        resolvedUtxos = state.ctx.resolvedUtxos
                      ),
                      depositsSpent = state.depositsSpent,
                      depositsToSpend = state.depositsToSpend,
                      ctx = state.ctx
                    )

                def withRollouts(
                    rollouts: RolloutTxSeq.Builder.PartialResult,
                    slotConfig: SlotConfig
                ): Result.WithRollouts =
                    Result.WithRollouts(
                      transaction = SettlementTx.WithRollouts(
                        majorVersionProduced = args.majorVersionProduced,
                        treasurySpent = args.treasuryToSpend,
                        treasuryProduced = treasuryProduced,
                        depositsSpent = state.depositsSpent,
                        rolloutProduced = unsafeGetRolloutProduced(state.ctx),
                        tx = tx,
                        // this is safe since we always set ttl
                        validityEnd = Slot(tx.body.value.ttl.get).toInstant(slotConfig),
                        resolvedUtxos = state.ctx.resolvedUtxos
                      ),
                      depositsSpent = state.depositsSpent,
                      depositsToSpend = state.depositsToSpend,
                      rolloutTxSeqPartial = rollouts,
                      ctx = state.ctx
                    )

                mergeResult match {
                    case NotMerged => withRollouts(args.rolloutTxSeqPartial, slotConfig)
                    case Merged(mbFirstSkipped) =>
                        mbFirstSkipped match {
                            case None => withOnlyDirectPayouts
                            case Some(firstSkipped) =>
                                withRollouts(firstSkipped.partialResult, slotConfig)
                        }
                }
            }

            /** Given the transaction context of a [[Builder.WithPayouts]] that has finished
              * building, apply post-processing to get the [[RolloutUtxo]] produced by the
              * [[SettlementTx.WithRollouts]], if it was produced. Assumes that the rollout produced
              * is the second output of the transaction.
              *
              * @param ctx
              *   The transaction context of a finished builder state.
              * @throws AssertionError
              *   when the assumption is broken.
              * @return
              */
            private def unsafeGetRolloutProduced(ctx: TransactionBuilder.Context): RolloutUtxo = {
                val tx = ctx.transaction
                val outputs = tx.body.value.outputs

                assert(outputs.nonEmpty)
                val outputsTail = outputs.tail

                assert(outputsTail.nonEmpty)
                val rolloutOutput = outputsTail.head.value

                RolloutUtxo(
                  Utxo(
                    TransactionInput(transactionId = tx.id, index = 1),
                    rolloutOutput
                  )
                )
            }
        }

        object Merge {
            enum Result {
                case NotMerged
                case Merged(
                    mbRolloutTxSeqPartialSkipped: Option[
                      RolloutTxSeq.Builder.PartialResult.SkipFirst
                    ]
                )
            }

            def tryMergeFirstRollout(
                finish: TransactionBuilder.Context => Either[
                  SomeBuildError,
                  TransactionBuilder.Context
                ],
                state: State[SettlementTx.WithPayouts],
                treasuryToSpend: MultisigTreasuryUtxo,
                rolloutTxSeqPartial: RolloutTxSeq.Builder.PartialResult
            ): BuildErrorOr[(State[SettlementTx.WithPayouts], Merge.Result)] =
                import Merge.Result.*
                import state.*

                val firstRolloutTxPartial = rolloutTxSeqPartial.firstOrOnly

                val rolloutTx: Transaction = firstRolloutTxPartial.ctx.transaction

                def sendOutput(x: Sized[TxOutput]): Send = Send(x.value)

                val optimisticSteps: List[Send] =
                    rolloutTx.body.value.outputs.map(sendOutput).toList

                val optimisticTrial: BuildErrorOr[TransactionBuilder.Context] = for {
                    newCtx <- TransactionBuilder
                        .modify(ctx, optimisticSteps)
                        .explainConst("adding optimistic steps failed")
                    finished <- finish(newCtx).explainConst("finishing optimistic trial failed")
                } yield finished

                lazy val pessimisticBackup: BuildErrorOr[TransactionBuilder.Context] =
                    for {
                        newCtx <- BasePessimistic.applySendRollout(
                          treasuryToSpend,
                          firstRolloutTxPartial.inputValueNeeded
                        )(ctx)
                        finished <- finish(newCtx).explainConst(
                          "finishing pessimistic backup failed"
                        )
                    } yield finished

                // Keep the optimistic transaction (which merged the settlement tx with the first rollout tx)
                // if it worked out. Otherwise, use the pessimistic transaction.
                for {
                    newCtx <- optimisticTrial.orElse(pessimisticBackup)

                    finishedState = State[SettlementTx.WithPayouts](
                      ctx = newCtx,
                      depositsSpent = depositsSpent,
                      depositsToSpend = depositsToSpend
                    )

                    mergeResult =
                        if optimisticTrial.isLeft then NotMerged
                        else Merged(rolloutTxSeqPartial.skipFirst)

                } yield (finishedState, mergeResult)
        }

        private object PostProcess {

            /** Given the transaction context of a [[Builder]] that has finished building, apply
              * post-processing to get the [[MultisigTreasuryUtxo]] produced by the
              * [[SettlementTx]]. Assumes that the treasury output is present in the transaction and
              * is the first output.
              *
              * @param ctx
              *   The transaction context of a finished builder state.
              * @throws AssertionError
              *   when the assumption is broken.
              * @return
              */
            @throws[AssertionError]
            def getTreasuryProduced[T <: SettlementTx](
                majorVersion: Block.Version.Major,
                treasurySpent: MultisigTreasuryUtxo,
                ctx: State[T]
            ): MultisigTreasuryUtxo = {
                val tx = ctx.ctx.transaction
                val outputs = tx.body.value.outputs

                assert(outputs.nonEmpty)
                // TODO: Throw other assertion errors in `.fromUtxo` and instead of `.get`?
                MultisigTreasuryUtxo
                    .fromUtxo(Utxo(TransactionInput(tx.id, 0), outputs.head.value))
                    .get
            }
        }
    }
}
