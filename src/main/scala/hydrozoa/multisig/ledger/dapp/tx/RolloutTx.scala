package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyVector
import hydrozoa.config.head.initialization.InitialBlock
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.{BuildErrorOr, explain, explainAppendConst, explainConst}
import hydrozoa.multisig.ledger.dapp.utxo.RolloutUtxo
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.{WrappedCoin, prebalancedLovelaceDiffHandler}
import monocle.{Focus, Lens}
import scala.Function.const
import scala.annotation.tailrec
import scalus.builtin.ByteString
import scalus.cardano.ledger.TransactionException.InvalidTransactionSizeException
import scalus.cardano.ledger.rules.TransactionSizeValidator
import scalus.cardano.ledger.utils.TxBalance
import scalus.cardano.ledger.{Coin, ProtocolParams, Transaction, TransactionHash, TransactionInput, TransactionOutput as TxOutput, Utxo, Value}
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.{ModifyAuxiliaryData, ReferenceOutput, Send, Spend}
import scalus.cardano.txbuilder.{SomeBuildError, TransactionBuilder, TransactionBuilderStep, TxBalancingError}

sealed trait RolloutTx extends Tx[RolloutTx], RolloutUtxo.Spent, RolloutUtxo.MbProduced {
    def tx: Transaction
    def txLens: Lens[RolloutTx, Transaction]
}

object RolloutTx {
    export RolloutTxOps.Build
    export RolloutTxOps.PartialResult

    /** The last rollout tx in the sequence. It spends a rollout utxo, but it doesn't produce a
      * rollout utxo.
      */
    final case class Last(
        override val tx: Transaction,
        override val rolloutSpent: RolloutUtxo,
        override val txLens: Lens[RolloutTx, Transaction] =
            Focus[Last](_.tx).asInstanceOf[Lens[RolloutTx, Transaction]],
        override val resolvedUtxos: ResolvedUtxos
    ) extends RolloutTx

    /** A rollout tx preceding the last one in the sequence. It both spends and produces a rollout
      * utxo.
      *
      * Invariant: the produced rollout utxo MUST have the txId of the [[tx]] field and index 0.
      */
    final case class NotLast(
        override val tx: Transaction,
        override val rolloutSpent: RolloutUtxo,
        override val rolloutProduced: RolloutUtxo,
        override val txLens: Lens[RolloutTx, Transaction] =
            Focus[NotLast](_.tx).asInstanceOf[Lens[RolloutTx, Transaction]],
        override val resolvedUtxos: ResolvedUtxos
    ) extends RolloutTx,
          RolloutUtxo.Produced
}

private object RolloutTxOps {
    import Build.*

    object Build {
        type Config = CardanoNetwork.Section & HeadPeers.Section & InitialBlock.Section

        final case class Last(override val config: Config)(
            override val nePayoutObligationsRemaining: NonEmptyVector[Payout.Obligation],
        ) extends Build[RolloutTx.Last](mbRolloutOutputValue = None) {

            /** Post-process a transaction builder context into a [[RolloutTx.Last]].
              *
              * @throws AssertionError
              *   when the assumptions of [[PostProcess.unsafeGetRolloutSpent]] fail to hold.
              */
            @throws[AssertionError]
            override def postProcess(ctx: TransactionBuilder.Context): RolloutTx.Last =
                this.PostProcess.last(ctx)
        }

        final case class NotLast(override val config: Config)(
            override val nePayoutObligationsRemaining: NonEmptyVector[Payout.Obligation],
            rolloutOutputValue: Value
        ) extends Build[RolloutTx.NotLast](mbRolloutOutputValue = Some(rolloutOutputValue)) {
            override def postProcess(ctx: TransactionBuilder.Context): RolloutTx.NotLast =
                this.PostProcess.notLast(ctx)
        }

        /** The state associated with a single [[RolloutTx]] while the builder is attempting to add
          * payout outputs to it.
          *
          * @param ctx
          *   the transaction builder context we've reached so far.
          * @tparam T
          *   indicates the type of [[RolloutTx]] being built.
          */
        final case class State[T <: RolloutTx](
            override val ctx: TransactionBuilder.Context,
            override val inputValueNeeded: Value,
            override val payoutObligationsRemaining: Vector[Payout.Obligation]
        ) extends State.Section[T] {
            override transparent inline def state: State[T] = this
        }

        object State {
            trait Section[T <: RolloutTx]
                extends Tx.Builder.HasCtx,
                  Payout.Obligation.Many.Remaining {
                def state: State[T]
                def inputValueNeeded: Value
            }
        }
    }

    trait Build[T <: RolloutTx](mbRolloutOutputValue: Option[Value])
        extends Payout.Obligation.Many.Remaining.NonEmpty {
        import Build.*
        def config: Build.Config

        /** After the [[Build]] has finished building the transaction, apply post-processing to the
          * transaction builder context to get a [[RolloutTx]].
          *
          * The result is specific to [[Build.Last]] and [[Build.NotLast]].
          */
        def postProcess(ctx: TransactionBuilder.Context): T

        /** Given a non-empty vector of payout obligations, partially build a new rollout
          * transaction that discharges as many of them as possible, leaving the rest of the payout
          * obligations to other rollout transactions.
          *
          * @return
          *   a [[PartialResult]] that can become a [[RolloutTx]] when its missing [[RolloutUtxo]]
          *   is provided.
          */
        final def partialResult: BuildErrorOr[PartialResult[T]] = for {
            pessimistic <- BasePessimistic()
            addedPayouts <- AddPayouts(pessimistic)
        } yield PartialResult(Build.this, addedPayouts)

        private[tx] object BasePessimistic {
            def apply(): BuildErrorOr[TransactionBuilder.Context] = for {
                ctx <- TransactionBuilder
                    .build(config.network, definiteSteps)
                    .explainConst("adding base pessimistic failed")
            } yield ctx

            /////////////////////////////////////////////////////////
            // Base steps
            private val modifyAuxiliaryData: ModifyAuxiliaryData =
                ModifyAuxiliaryData(_ =>
                    Some(MD(MD.Rollout(headAddress = config.headMultisigAddress)))
                )

            private val referenceMultisigRegime =
                ReferenceOutput(config.multisigRegimeUtxo.asUtxo)

            private val commonSteps: List[TransactionBuilderStep] =
                List(modifyAuxiliaryData, referenceMultisigRegime)

            /////////////////////////////////////////////////////////
            // Spend rollout
            def spendRollout(resolvedUtxo: Utxo): Spend =
                Spend(resolvedUtxo, config.headMultisigScript.witnessAttached)

            /////////////////////////////////////////////////////////
            // Send rollout (maybe)
            private val mbSendRollout: Option[Send] =
                mbRolloutOutputValue.map(rolloutOutputValue =>
                    Send(
                      TxOutput.Babbage(
                        address = config.headMultisigAddress,
                        value = rolloutOutputValue,
                        datumOption = None,
                        scriptRef = None
                      )
                    )
                )

            /////////////////////////////////////////////////////////
            // Definite steps
            private val definiteSteps = commonSteps ++ mbSendRollout
        }

        private object AddPayouts {
            def apply(ctx: TransactionBuilder.Context): BuildErrorOr[State[T]] = {
                for {
                    withPayout <- tryAddPayout(
                      ctx,
                      payoutObligation = nePayoutObligationsRemaining.head
                    )

                    basePessimistic = State[T](
                      ctx = withPayout._1,
                      inputValueNeeded = withPayout._2,
                      payoutObligationsRemaining = nePayoutObligationsRemaining.tail
                    )

                    res <- addPayoutsLoop(basePessimistic)
                } yield res
            }

            @tailrec
            private def addPayoutsLoop(
                state: State[T]
            ): BuildErrorOr[State[T]] = {
                state.payoutObligationsRemaining match {
                    case obligation +: otherObligations =>
                        tryAddPayout(state.ctx, obligation) match {
                            case Right((newCtx, value)) =>
                                val newState: State[T] = state.copy(
                                  ctx = newCtx,
                                  inputValueNeeded = value,
                                  payoutObligationsRemaining = otherObligations
                                )
                                addPayoutsLoop(newState)
                            case Left(err) =>
                                Tx.Builder.Incremental
                                    .replaceInvalidSizeException(err._1, state)
                                    .explainConst(err._2)
                        }
                    case _Empty => Right(state)
                }
            }

            /** Try to add payouts one-by-one until adding another payout would exceed transaction
              * size limits.
              */

            /** Try to add a single payout to the rollout transaction, and then trial a finalization
              * with a placeholder rollout utxo as an input. If successful, returns the new
              * [[Context]] and the actual value needed for the prior rollout utxo.
              */
            private def tryAddPayout(
                ctx: TransactionBuilder.Context,
                payoutObligation: Payout.Obligation
            ): BuildErrorOr[(TransactionBuilder.Context, Value)] =
                val payoutStep = Send(payoutObligation.utxo)
                for {
                    newCtx <- TransactionBuilder
                        .modify(ctx, List(payoutStep))
                        .explainConst("could not add payout")
                    valueNeededWithFee <- TrialFinish(Build.this, newCtx)
                } yield (newCtx, valueNeededWithFee)
        }

        private[tx] object PostProcess {

            /** Post-process a transaction builder context into a [[RolloutTx.Last]].
              *
              * @throws AssertionError
              *   when the assumptions of [[PostProcess.unsafeGetRolloutSpent]] fail to hold.
              */
            @throws[AssertionError]
            def last(ctx: TransactionBuilder.Context): RolloutTx.Last = {
                RolloutTx.Last(
                  rolloutSpent = PostProcess.unsafeGetRolloutSpent(ctx),
                  tx = ctx.transaction,
                  resolvedUtxos = ctx.resolvedUtxos
                )
            }

            /** Post-process a transaction builder context into a [[RolloutTx.Last]]. Assumes that
              * the first output of the transaction is the rollout produced.
              *
              * @throws AssertionError
              *   when there are no transaction outputs.
              */
            @throws[AssertionError]
            def notLast(ctx: TransactionBuilder.Context): RolloutTx.NotLast = {
                val tx = ctx.transaction
                val outputs = tx.body.value.outputs

                assert(outputs.nonEmpty)
                val rolloutOutput = outputs.head.value

                val rolloutProduced = Utxo(
                  TransactionInput(transactionId = tx.id, index = 0),
                  rolloutOutput
                )
                RolloutTx.NotLast(
                  rolloutSpent = PostProcess.unsafeGetRolloutSpent(ctx),
                  rolloutProduced = RolloutUtxo(rolloutProduced),
                  tx = tx,
                  resolvedUtxos = ctx.resolvedUtxos
                )
            }

            /** Given a transaction builder context, get the spent rollout utxo from its
              * transaction. Assumes that the spent rollout exists as the only input in the
              * transaction, which must always hold for a fully and properly built rollout tx.
              *
              * @param ctx
              *   the transaction builder context
              * @throws AssertionError
              *   when the assumption is broken
              * @return
              *   the resolved spend rollout utxo
              */
            @throws[AssertionError]
            def unsafeGetRolloutSpent(ctx: TransactionBuilder.Context): RolloutUtxo = {
                val tx = ctx.transaction
                val inputs = tx.body.value.inputs.toSeq

                assert(inputs.nonEmpty)
                assert(inputs.tail.isEmpty)
                val firstInput = inputs.head

                val firstInputResolved =
                    Utxo(firstInput, ctx.resolvedUtxos.utxos(firstInput))

                RolloutUtxo(firstInputResolved)
            }
        }

        private object TrialFinish {

            /** Add the placeholder rollout input to ensure that the RolloutTx will fit within size
              * constraints after fee calculation + balancing.
              */
            def apply(
                builder: Build[T],
                ctx: TransactionBuilder.Context
            ): BuildErrorOr[Value] = {
                // The deficit in the inputs to the transaction prior to adding the placeholder
                val valueNeeded =
                    TrialFinish.inputValueNeeded(ctx, config.cardanoProtocolParams)
                trialFinishLoop(builder, ctx, valueNeeded)
            }

            // A UtxoID with the largest possible size in Flat encoding
            // - Transaction ID should be 32 bytes of 1111 1111 because Flat uses the least number of bytes.
            // - The index can be zero because Flat will still use a full byte
            // https://hackage.haskell.org/package/flat-0.6/docs/Flat-Class.html
            private val utxoId = TransactionInput(
              transactionId = TransactionHash.fromByteString(
                ByteString.fromArray(Array.fill(32)(Byte.MinValue))
              ),
              index = 0
            )

            @tailrec
            private def trialFinishLoop(
                builder: Build[T],
                ctx: TransactionBuilder.Context,
                trialValue: Value
            ): BuildErrorOr[Value] = {
                val placeholder = List(spendPlaceholderRollout(trialValue))
                val res = for {
                    // TODO: move out of loop
                    addedPlaceholderRolloutInput <- TransactionBuilder.modify(ctx, placeholder)
                    res <- addedPlaceholderRolloutInput.finalizeContext(
                      config.cardanoProtocolParams,
                      prebalancedLovelaceDiffHandler,
                      builder.config.plutusScriptEvaluatorForTxBuild,
                      List(TransactionSizeValidator)
                    )

                } yield res
                res match {
                    case Left(
                          SomeBuildError.ValidationError(
                            e: InvalidTransactionSizeException,
                            errorCtx
                          )
                        ) =>
                        Left(SomeBuildError.ValidationError(e, errorCtx))
                            .explainConst("trial to add payout failed")
                    case Left(
                          SomeBuildError.BalancingError(
                            TxBalancingError.Failed(WrappedCoin(Coin(diff))),
                            _errorCtx
                          )
                        ) =>
                        trialFinishLoop(builder, ctx, trialValue - Value(Coin(diff)))
                    case Right(_) => Right(trialValue)
                    case e =>
                        throw new RuntimeException(
                          "should be impossible; " +
                              s"loop only has two possible Lefts, but got $e"
                        )
                }
            }

            private def spendPlaceholderRollout(value: Value): Spend =
                BasePessimistic.spendRollout(placeholderRolloutResolvedUtxo(value))

            private def placeholderRolloutResolvedUtxo(
                value: Value
            ): Utxo =
                Utxo(
                  TrialFinish.utxoId,
                  TxOutput.Babbage(
                    address = config.headMultisigAddress,
                    value = value
                  )
                )

            private def inputValueNeeded(
                ctx: TransactionBuilder.Context,
                params: ProtocolParams
            ): Value =
                TxBalance.produced(ctx.transaction, params)
        }
    }

    /** A [[RolloutTx]] built-up to the point where all it's missing is its [[RolloutUtxo]] input.
      *
      * @tparam T
      *   indicates the type of [[RolloutTx]] being built.
      */
    trait PartialResult[T <: RolloutTx](_payoutObligationsRemaining: Vector[Payout.Obligation])
        extends Build.State.Section[T] {
        def builder: Build[T]

        override transparent inline def state: State[T] =
            State(ctx, inputValueNeeded, payoutObligationsRemaining)
        override transparent inline def payoutObligationsRemaining: Vector[Payout.Obligation] =
            _payoutObligationsRemaining

        /** Add the missing [[RolloutUtxo]] input to the transaction and post-process it into a
          * [[RolloutTx]].
          *
          * @param rolloutSpent
          *   the [[RolloutUtxo]] input to be added.
          * @return
          */
        final def complete(rolloutSpent: RolloutUtxo): BuildErrorOr[T] = for {
            addedRolloutSpend <- addRolloutSpent(rolloutSpent)
                .explainAppendConst("could not complete partial result")
        } yield builder.postProcess(addedRolloutSpend)

        /** Just add the missing [[RolloutUtxo]] input to the transaction being built and return the
          * transaction builder context, without post-processing into a [[RolloutTx]]. This method
          * is meant to be used for debugging only.
          *
          * @param rolloutSpent
          *   the [[RolloutUtxo]] input to be added.
          * @return
          */
        private final def addRolloutSpent(
            rolloutSpent: RolloutUtxo
        ): BuildErrorOr[TransactionBuilder.Context] = {
            val steps = List(builder.BasePessimistic.spendRollout(rolloutSpent.utxo))
            for {
                addedRolloutInput <- TransactionBuilder
                    .modify(ctx, steps)
                    .explain(const("Could not add rollout to context"))
                finished <- addedRolloutInput
                    .finalizeContext(
                      protocolParams = builder.config.cardanoInfo.protocolParams,
                      diffHandler = prebalancedLovelaceDiffHandler,
                      evaluator = builder.config.plutusScriptEvaluatorForTxBuild,
                      validators = Tx.Validators.nonSigningValidators
                    )
                    .explain(const("Could not finalize context after spending rollout input"))
            } yield finished
        }
    }

    object PartialResult {

        /** Transform a [[State]] into a [[PartialResult]]. If there are no more remaining
          * obligations in the state, then the rollout tx being built must be the first in the
          * sequence because the sequence is built backwards.
          */
        def apply[T <: RolloutTx](
            builder: Build[T],
            state: State[T]
        ): PartialResult[T] = {
            import state.*
            NonEmptyVector.fromVector(payoutObligationsRemaining) match
                case None      => PartialResult.First(builder, ctx, inputValueNeeded)
                case Some(nev) => PartialResult.NotFirst(builder, ctx, inputValueNeeded, nev)
        }

        /** The first [[RolloutTx]] in the sequence. As such, it doesn't have any [[RolloutTx]]
          * predecessors in the sequence, and no payout obligations remain for these (non-existent)
          * predecessors to discharge.
          *
          * @param builder
          *   the transaction builder constructing this rollout tx.
          * @param ctx
          *   the transaction builder context we've reached so far.
          * @param inputValueNeeded
          *   the [[Value]] that this transaction needs its missing input to provide.
          * @tparam TT
          *   indicates the type of [[RolloutTx]] being built.
          */
        final case class First[TT <: RolloutTx] private[PartialResult] (
            override val builder: Build[TT],
            override val ctx: TransactionBuilder.Context,
            override val inputValueNeeded: Value
        ) extends PartialResult[TT](Vector.empty)

        /** A non-first [[RolloutTx]] in the sequence. As such, it has at least one [[RolloutTx]]
          * predecessor in the sequence, and at least one payout obligation must be discharged by
          * these predecessors.
          *
          * @param builder
          *   the transaction builder constructing this rollout tx.
          * @param ctx
          *   the transaction builder context we've reached so far.
          * @param inputValueNeeded
          *   the [[Value]] that this transaction needs its missing input to provide.
          * @param nePayoutObligationsRemaining
          *   the payout obligations (non-empty) to be fulfilled by this transaction's predecessors.
          * @tparam TT
          *   indicates the type of [[RolloutTx]] being built.
          */
        final case class NotFirst[TT <: RolloutTx] private[PartialResult] (
            override val builder: Build[TT],
            override val ctx: TransactionBuilder.Context,
            override val inputValueNeeded: Value,
            override val nePayoutObligationsRemaining: NonEmptyVector[Payout.Obligation]
        ) extends PartialResult[TT](nePayoutObligationsRemaining.toVector),
              Payout.Obligation.Many.Remaining.NonEmpty {
            def asFirst: PartialResult.First[TT] = First(
              builder = builder,
              ctx = ctx,
              inputValueNeeded = inputValueNeeded
            )
        }
    }
}
