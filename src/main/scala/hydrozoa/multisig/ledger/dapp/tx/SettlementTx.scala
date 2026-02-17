package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.config.head.initialization.InitialBlock
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedInstant, toQuantizedInstant}
import hydrozoa.multisig.ledger.block.BlockVersion
import hydrozoa.multisig.ledger.block.BlockVersion.Major.given_Conversion_Major_Int
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.tx.Metadata.Settlement
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.{BuilderResult, explainConst}
import hydrozoa.multisig.ledger.dapp.txseq.RolloutTxSeq
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, MultisigTreasuryUtxo, RolloutUtxo}
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment
import monocle.{Focus, Lens}
import scala.annotation.tailrec
import scala.collection.immutable.Vector
import scalus.uplc.builtin.Data.toData
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.{Sized, Slot, Transaction, TransactionInput, TransactionOutput as TxOutput, Utxo, Value}
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.*

sealed trait SettlementTx
    extends Tx[SettlementTx],
      BlockVersion.Major.Produced,
      DepositUtxo.Many.Spent,
      KzgCommitment.Produced,
      MultisigTreasuryUtxo.Spent,
      MultisigTreasuryUtxo.Produced,
      RolloutUtxo.MbProduced,
      HasResolvedUtxos,
      HasValidityEnd {
    def tx: Transaction
    def txLens: Lens[SettlementTx, Transaction]
}

object SettlementTx {
    export SettlementTxOps.Build
    export SettlementTxOps.Result
    export SettlementTxOps.Error

    sealed trait WithPayouts extends SettlementTx {
        def payoutCount: Int
    }

    sealed trait NoRollouts extends SettlementTx

    final case class NoPayouts(
        override val validityEnd: QuantizedInstant,
        override val tx: Transaction,
        override val majorVersionProduced: BlockVersion.Major,
        override val kzgCommitment: KzgCommitment,
        override val treasurySpent: MultisigTreasuryUtxo,
        override val treasuryProduced: MultisigTreasuryUtxo,
        override val depositsSpent: Vector[DepositUtxo],
        override val resolvedUtxos: ResolvedUtxos,
        override val txLens: Lens[SettlementTx, Transaction] =
            Focus[NoPayouts](_.tx).asInstanceOf[Lens[SettlementTx, Transaction]]
    ) extends NoRollouts

    final case class WithOnlyDirectPayouts(
        override val validityEnd: QuantizedInstant,
        override val tx: Transaction,
        override val majorVersionProduced: BlockVersion.Major,
        override val kzgCommitment: KzgCommitment,
        override val treasurySpent: MultisigTreasuryUtxo,
        override val treasuryProduced: MultisigTreasuryUtxo,
        override val depositsSpent: Vector[DepositUtxo],
        override val payoutCount: Int,
        override val resolvedUtxos: ResolvedUtxos,
        override val txLens: Lens[SettlementTx, Transaction] =
            Focus[WithOnlyDirectPayouts](_.tx).asInstanceOf[Lens[SettlementTx, Transaction]]
    ) extends WithPayouts,
          NoRollouts

    final case class WithRollouts(
        override val validityEnd: QuantizedInstant,
        override val tx: Transaction,
        override val majorVersionProduced: BlockVersion.Major,
        override val kzgCommitment: KzgCommitment,
        override val treasurySpent: MultisigTreasuryUtxo,
        override val treasuryProduced: MultisigTreasuryUtxo,
        override val depositsSpent: Vector[DepositUtxo],
        override val rolloutProduced: RolloutUtxo,
        override val payoutCount: Int,
        override val resolvedUtxos: ResolvedUtxos,
        override val txLens: Lens[SettlementTx, Transaction] =
            Focus[WithRollouts](_.tx).asInstanceOf[Lens[SettlementTx, Transaction]]
    ) extends WithPayouts,
          RolloutUtxo.Produced

}

private object SettlementTxOps {
    sealed trait Result[T <: SettlementTx]
        extends Tx.AugmentedResult[T],
          DepositUtxo.Many.Spent.Partition

    object Result {
        type NoRollouts = Result[SettlementTx.NoRollouts]

        sealed trait WithPayouts extends Result[SettlementTx.WithPayouts]

        case class NoPayouts(
            override val transaction: SettlementTx.NoPayouts,
            override val depositsSpent: Vector[DepositUtxo],
            override val depositsToSpend: Vector[DepositUtxo],
        ) extends Result[SettlementTx.NoPayouts]

        case class WithOnlyDirectPayouts(
            override val transaction: SettlementTx.WithOnlyDirectPayouts,
            override val depositsSpent: Vector[DepositUtxo],
            override val depositsToSpend: Vector[DepositUtxo],
        ) extends WithPayouts

        case class WithRollouts(
            override val transaction: SettlementTx.WithRollouts,
            override val depositsSpent: Vector[DepositUtxo],
            override val depositsToSpend: Vector[DepositUtxo],
            rolloutTxSeqPartial: RolloutTxSeq.PartialResult,
        ) extends WithPayouts
    }

    type ResultFor[T <: SettlementTx] <: Result[?] = T match {
        case SettlementTx.NoPayouts             => Result.NoPayouts
        case SettlementTx.WithPayouts           => Result.WithPayouts
        case SettlementTx.WithOnlyDirectPayouts => Result.WithOnlyDirectPayouts
        case SettlementTx.WithRollouts          => Result.WithRollouts
    }

    enum Error:
        case TreasuryIncorrectAddress

    type TxBuilderResult[Result] = BuilderResult[Result, Error]

    object Build {
        type Config = CardanoNetwork.Section & HeadPeers.Section & InitialBlock.Section

        case class NoPayouts(override val config: Config)(
            override val kzgCommitment: KzgCommitment,
            override val majorVersionProduced: BlockVersion.Major,
            override val treasuryToSpend: MultisigTreasuryUtxo,
            override val depositsToSpend: Vector[DepositUtxo],
            override val validityEnd: QuantizedInstant,
        ) extends Build[SettlementTx.NoPayouts](
              mbRolloutTxSeqPartial = None
            ) {
            override def complete(
                state: State
            ): TxBuilderResult[ResultFor[SettlementTx.NoPayouts]] =
                Right(CompleteNoPayouts(state))
        }

        case class WithPayouts(override val config: Config)(
            override val kzgCommitment: KzgCommitment,
            override val majorVersionProduced: BlockVersion.Major,
            override val treasuryToSpend: MultisigTreasuryUtxo,
            override val depositsToSpend: Vector[DepositUtxo],
            override val validityEnd: QuantizedInstant,
            rolloutTxSeqPartial: RolloutTxSeq.PartialResult
        ) extends Build[SettlementTx.WithPayouts](
              mbRolloutTxSeqPartial = Some(rolloutTxSeqPartial)
            ) {
            override def complete(
                state: State
            ): TxBuilderResult[ResultFor[SettlementTx.WithPayouts]] =
                CompleteWithPayouts(state, rolloutTxSeqPartial)
        }
    }

    sealed trait Build[T <: SettlementTx](
        mbRolloutTxSeqPartial: Option[RolloutTxSeq.PartialResult]
    ) extends BlockVersion.Major.Produced,
          MultisigTreasuryUtxo.ToSpend,
          DepositUtxo.Many.ToSpend,
          KzgCommitment.Produced,
          HasValidityEnd {
        import Build.*
        import Error.*

        def config: Config

        def complete(state: State): TxBuilderResult[ResultFor[T]]

        final def result: TxBuilderResult[ResultFor[T]] = for {
            pessimistic <- BasePessimistic()

            addedDeposits <- AddDeposits(pessimistic)

            finished <- TxBuilder
                .finalizeContext(addedDeposits.ctx)
                .explainConst("finishing settlement tx failed")

            completed <- complete(addedDeposits.copy(ctx = finished))
        } yield completed

        final case class State(
            override val ctx: TransactionBuilder.Context,
            override val depositsSpent: Vector[DepositUtxo],
            override val depositsToSpend: Vector[DepositUtxo]
        ) extends Tx.Builder.HasCtx,
              DepositUtxo.Many.Spent.Partition

        private object BasePessimistic {
            def apply(): TxBuilderResult[State] = for {
                _ <- Either
                    .cond(checkTreasuryToSpend, (), TreasuryIncorrectAddress)
                    .explainConst("treasury to spend has incorrect head address")
                ctx <- TransactionBuilder
                    .build(config.network, definiteSteps)
                    .explainConst("definite steps failed")
                addedPessimisticRollout <- BasePessimistic
                    .mbApplySendRollout(ctx)
                    .explainConst("sending the rollout tx failed in base pessimistic")
                _ <- TxBuilder
                    .finalizeContext(addedPessimisticRollout)
                    .explainConst("finishing base pessimistic failed")
            } yield State(
              ctx = ctx,
              depositsSpent = Vector.empty,
              depositsToSpend = depositsToSpend
            )

            /////////////////////////////////////////////////////////
            // Checks

            // TODO: Ensure this holds by construction
            private def checkTreasuryToSpend: Boolean =
                treasuryToSpend.address == config.headMultisigAddress

            /////////////////////////////////////////////////////////
            // Base steps
            private val modifyAuxiliaryData = ModifyAuxiliaryData(_ =>
                Some(MD(Settlement(headAddress = config.headMultisigAddress)))
            )

            private val referenceMultisigRegime =
                ReferenceOutput(config.multisigRegimeUtxo.asUtxo)

            private val validityEndSlot = ValidityEndSlot(validityEnd.toSlot.slot)

            private val baseSteps =
                // TODO: switch back to witnessAttached after resolving https://github.com/scalus3/scalus/issues/207
                // List(modifyAuxiliaryData, referenceMultisigRegime, validityEndSlot)
                List(modifyAuxiliaryData, validityEndSlot)

            /////////////////////////////////////////////////////////
            // Spend treasury
            private val spendTreasury =
                // TODO: switch back to witnessAttached after resolving https://github.com/scalus3/scalus/issues/207
                Spend(treasuryToSpend.asUtxo, config.headMultisigScript.witnessValue)

            /////////////////////////////////////////////////////////
            // Send rollout (maybe)
            private def mkRolloutOutput(value: Value): TxOutput.Babbage = TxOutput.Babbage(
              address = config.headMultisigAddress,
              value = value,
              datumOption = None,
              scriptRef = None
            )

            private val mbRolloutValue: Option[Value] =
                mbRolloutTxSeqPartial.map(_.firstOrOnly.inputValueNeeded)

            private val mbRolloutOutput: Option[TxOutput.Babbage] =
                mbRolloutValue.map(mkRolloutOutput)

            /** We apply this step if the first rollout tx doesn't get merged into the settlement
              * tx.
              */
            def mbApplySendRollout(
                ctx: TransactionBuilder.Context
            ): Either[SomeBuildError, TransactionBuilder.Context] =
                mbRolloutOutput.fold(Right(ctx))((output: TxOutput.Babbage) =>
                    TransactionBuilder.modify(ctx, List(Send(output)))
                )

            /////////////////////////////////////////////////////////
            // Send treasury
            private val treasuryOutputValue: Value =
                mbRolloutValue.fold(treasuryToSpend.value)(treasuryToSpend.value - _)

            private val treasuryOutput: TxOutput.Babbage = {
                TxOutput.Babbage(
                  address = config.headMultisigAddress,
                  value = treasuryOutputValue,
                  datumOption = Some(
                    Inline(
                      MultisigTreasuryUtxo
                          .Datum(
                            commit = kzgCommitment,
                            versionMajor = majorVersionProduced.convert,
                          )
                          .toData
                    )
                  ),
                  scriptRef = None
                )
            }

            private val sendTreasury = Send(treasuryOutput)

            /////////////////////////////////////////////////////////
            // Definite steps
            private val definiteSteps: List[TransactionBuilderStep] =
                baseSteps ++ List(spendTreasury, sendTreasury)
        }

        private object AddDeposits {
            def apply(initialState: State): TxBuilderResult[State] = loop(initialState)

            @tailrec
            private def loop(state: State): TxBuilderResult[State] = {
                import state.*
                state.depositsToSpend match {
                    case deposit +: otherDeposits =>
                        tryAddDeposit(ctx, deposit) match {
                            case Right(x) =>
                                val newState: State = state.copy(
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

            private def tryAddDeposit(
                ctx: TransactionBuilder.Context,
                deposit: DepositUtxo
            ): TxBuilderResult[TransactionBuilder.Context] = {
                for {
                    newCtx <- TransactionBuilder
                        .modify(ctx, List(mkDepositStep(deposit)))
                        .explainConst(s"adding deposit utxo failed. Deposit utxo: $deposit")
                    // TODO: update the non-ADA assets in the treasury output, based on the absorbed deposits
                    //
                    // Ensure that at least the pessimistic rollout output fits into the transaction.
                    addedPessimisticRollout <- BasePessimistic
                        .mbApplySendRollout(newCtx)
                        .explainConst("sending the rollout tx failed after trying to add a deposit")

                    _ <- TxBuilder
                        .finalizeContext(addedPessimisticRollout)
                        .explainConst("finishing for tryAddDeposit failed.")
                } yield newCtx
            }

            private def mkDepositStep(deposit: DepositUtxo): Spend =
                Spend(deposit.toUtxo, config.headMultisigScript.witnessAttached)
        }

        private[tx] object CompleteNoPayouts {
            def apply(state: State): Result.NoPayouts = {
                val treasuryProduced = PostProcess.getTreasuryProduced(
                  majorVersionProduced,
                  treasuryToSpend,
                  state
                )

                val settlementTx: SettlementTx.NoPayouts = SettlementTx.NoPayouts(
                  validityEnd = Slot(state.ctx.transaction.body.value.ttl.get)
                      .toQuantizedInstant(config.cardanoInfo.slotConfig),
                  tx = state.ctx.transaction,
                  majorVersionProduced = majorVersionProduced,
                  kzgCommitment = kzgCommitment,
                  treasurySpent = treasuryToSpend,
                  treasuryProduced = treasuryProduced,
                  depositsSpent = state.depositsSpent,
                  resolvedUtxos = state.ctx.resolvedUtxos
                )
                Result.NoPayouts(
                  transaction = settlementTx,
                  depositsSpent = state.depositsSpent,
                  depositsToSpend = state.depositsToSpend,
                )
            }
        }

        private[tx] object CompleteWithPayouts {

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
            def apply(
                state: State,
                rolloutTxSeqPartial: RolloutTxSeq.PartialResult,
            ): TxBuilderResult[Result.WithPayouts] = for {
                mergeTrial <- TryMerge(state, rolloutTxSeqPartial)
            } yield {
                import TryMerge.Result.*

                val (finished, mergeResult) = mergeTrial

                val tx = finished.ctx.transaction

                val treasuryProduced = PostProcess.getTreasuryProduced(
                  majorVersionProduced,
                  treasuryToSpend,
                  finished
                )

                def withOnlyDirectPayouts(payoutCount: Int): Result.WithOnlyDirectPayouts =
                    Result.WithOnlyDirectPayouts(
                      transaction = SettlementTx.WithOnlyDirectPayouts(
                        majorVersionProduced = majorVersionProduced,
                        kzgCommitment = kzgCommitment,
                        treasurySpent = treasuryToSpend,
                        treasuryProduced = treasuryProduced,
                        depositsSpent = finished.depositsSpent,
                        tx = tx,
                        // this is safe since we always set ttl
                        validityEnd = Slot(tx.body.value.ttl.get)
                            .toQuantizedInstant(config.cardanoInfo.slotConfig),
                        payoutCount = payoutCount,
                        resolvedUtxos = finished.ctx.resolvedUtxos
                      ),
                      depositsSpent = finished.depositsSpent,
                      depositsToSpend = finished.depositsToSpend
                    )

                def withRollouts(
                    payoutCount: Int,
                    rollouts: RolloutTxSeq.PartialResult
                ): Result.WithRollouts =
                    Result.WithRollouts(
                      transaction = SettlementTx.WithRollouts(
                        majorVersionProduced = majorVersionProduced,
                        kzgCommitment = kzgCommitment,
                        treasurySpent = treasuryToSpend,
                        treasuryProduced = treasuryProduced,
                        depositsSpent = finished.depositsSpent,
                        rolloutProduced = unsafeGetRolloutProduced(finished.ctx),
                        tx = tx,
                        // this is safe since we always set ttl
                        validityEnd = Slot(tx.body.value.ttl.get)
                            .toQuantizedInstant(config.cardanoInfo.slotConfig),
                        payoutCount = payoutCount,
                        resolvedUtxos = finished.ctx.resolvedUtxos
                      ),
                      depositsSpent = finished.depositsSpent,
                      depositsToSpend = finished.depositsToSpend,
                      rolloutTxSeqPartial = rollouts,
                    )

                mergeResult match {
                    case NotMerged => withRollouts(0, rolloutTxSeqPartial)
                    case Merged(mbFirstSkipped, payoutCount) =>
                        mbFirstSkipped match {
                            case None => withOnlyDirectPayouts(payoutCount)
                            case Some(firstSkipped) =>
                                withRollouts(payoutCount, firstSkipped.partialResult)
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
            private def unsafeGetRolloutProduced(
                ctx: TransactionBuilder.Context
            ): RolloutUtxo = {
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

            object TryMerge {
                enum Result {
                    case NotMerged
                    case Merged(
                        mbRolloutTxSeqPartialSkipped: Option[
                          RolloutTxSeq.PartialResult.SkipFirst
                        ],
                        payoutCount: Int
                    )
                }

                def apply(
                    state: State,
                    rolloutTxSeqPartial: RolloutTxSeq.PartialResult
                ): TxBuilderResult[(State, TryMerge.Result)] =
                    import TryMerge.Result.*
                    import state.*

                    val firstRolloutTxPartial = rolloutTxSeqPartial.firstOrOnly

                    val rolloutTx: Transaction = firstRolloutTxPartial.ctx.transaction

                    def sendOutput(x: Sized[TxOutput]): Send = Send(x.value)

                    val optimisticSteps: List[Send] =
                        rolloutTx.body.value.outputs.map(sendOutput).toList

                    val optimisticTrial: TxBuilderResult[TransactionBuilder.Context] = for {
                        newCtx <- TransactionBuilder
                            .modify(ctx, optimisticSteps)
                            .explainConst("adding optimistic steps failed")
                        finished <- TxBuilder
                            .finalizeContext(newCtx)
                            .explainConst("finishing optimistic trial failed")
                    } yield finished

                    lazy val pessimisticBackup: TxBuilderResult[TransactionBuilder.Context] =
                        for {
                            newCtx <- BasePessimistic
                                .mbApplySendRollout(ctx)
                                .explainConst("pessmistic backup in merging failed")
                            finished <- TxBuilder
                                .finalizeContext(newCtx)
                                .explainConst(
                                  "finishing pessimistic backup failed"
                                )
                        } yield finished

                    // Keep the optimistic transaction (which merged the settlement tx with the first rollout tx)
                    // if it worked out. Otherwise, use the pessimistic transaction.
                    for {
                        newCtx <- optimisticTrial.orElse(pessimisticBackup)

                        finishedState = State(
                          ctx = newCtx,
                          depositsSpent = depositsSpent,
                          depositsToSpend = state.depositsToSpend
                        )

                        mergeResult =
                            if optimisticTrial.isLeft then NotMerged
                            else
                                Merged(
                                  mbRolloutTxSeqPartialSkipped = rolloutTxSeqPartial.skipFirst,
                                  payoutCount = firstRolloutTxPartial.payoutCount
                                )

                    } yield (finishedState, mergeResult)
            }
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
            def getTreasuryProduced(
                majorVersion: BlockVersion.Major,
                treasurySpent: MultisigTreasuryUtxo,
                ctx: State
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

        private object TxBuilder {
            private val diffHandler = Change.changeOutputDiffHandler(
              _,
              _,
              protocolParams = config.cardanoProtocolParams,
              changeOutputIdx = 0
            )

            def finalizeContext(
                ctx: TransactionBuilder.Context
            ): Either[SomeBuildError, TransactionBuilder.Context] =
                ctx.finalizeContext(
                  config.cardanoProtocolParams,
                  diffHandler = diffHandler,
                  evaluator = config.plutusScriptEvaluatorForTxBuild,
                  validators = Tx.Validators.nonSigningValidators
                )
        }

    }

}
