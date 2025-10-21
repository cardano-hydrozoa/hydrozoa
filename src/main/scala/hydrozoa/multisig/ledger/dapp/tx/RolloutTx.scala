package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyVector
import hydrozoa.lib.tx.TransactionBuilderStep.{ModifyAuxiliaryData, ReferenceOutput, Send, Spend}
import hydrozoa.lib.tx.{
    TransactionBuilder,
    TransactionBuilderStep,
    TransactionUnspentOutput
}
import hydrozoa.multisig.ledger.dapp.utxo.RolloutUtxo
import hydrozoa.multisig.ledger.joint.utxo.Payout
import scalus.cardano.ledger.{
    Coin,
    Transaction,
    TransactionHash,
    TransactionInput,
    Value,
    TransactionOutput as TxOutput
}
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.BuildErrorOr
import scalus.builtin.ByteString
import scalus.cardano.ledger.utils.TxBalance

import scala.annotation.tailrec

enum RolloutTx extends Tx, RolloutUtxo.Spent, RolloutUtxo.MbProduced {

    /** The last rollout tx in the sequence. It spends a rollout utxo, but it doesn't produce a
      * rollout utxo.
      */
    case Last(
        override val tx: Transaction,
        override val rolloutSpent: RolloutUtxo
    ) extends RolloutTx

    /** A rollout tx preceding the last one in the sequence. It both spends and produces a rollout
      * utxo.
      *
      * Invariant: the produced rollout utxo MUST have the txId of the [[tx]] field and index 0.
      */
    case NotLast(
        override val tx: Transaction,
        override val rolloutSpent: RolloutUtxo,
        override val rolloutProduced: RolloutUtxo
    ) extends RolloutTx, RolloutUtxo.Produced
}

object RolloutTx {
    import Builder.*
    import BuilderOps.*

    object Builder {
        final case class Last(override val config: Tx.Builder.Config)
            extends Builder[RolloutTx.Last] {

            override type ArgsType = Args.Last

            /** Post-process a transaction builder context into a [[RolloutTx.Last]].
              *
              * @throws AssertionError
              *   when the assumptions of [[PostProcess.unsafeGetRolloutSpent]] fail to hold.
              */
            @throws[AssertionError]
            override def postProcess(ctx: TransactionBuilder.Context): RolloutTx.Last =
                PostProcess.last(ctx)
        }

        final case class NotLast(override val config: Tx.Builder.Config)
            extends Builder[RolloutTx.NotLast] {

            override type ArgsType = Args.NotLast

            override def postProcess(ctx: TransactionBuilder.Context): RolloutTx.NotLast =
                PostProcess.notLast(ctx)
        }

        /** A [[RolloutTx]] built-up to the point where all it's missing is its [[RolloutUtxo]]
          * input.
          *
          * @tparam T
          *   indicates the type of [[RolloutTx]] being built.
          */
        trait PartialResult[T <: RolloutTx]
            extends Tx.Builder.HasCtx,
              State.Fields.HasInputRequired {
            def builder: Builder[T]

            /** Add the missing [[RolloutUtxo]] input to the transaction and post-process it into a
              * [[RolloutTx]].
              *
              * @param rolloutSpent
              *   the [[RolloutUtxo]] input to be added.
              * @return
              */
            final def complete(rolloutSpent: RolloutUtxo): BuildErrorOr[T] = for {
                addedRolloutSpend <- addRolloutSpent(rolloutSpent)
            } yield builder.postProcess(addedRolloutSpend)

            /** Just add the missing [[RolloutUtxo]] input to the transaction being built and return
              * the transaction builder context, without post-processing into a [[RolloutTx]]. This
              * method is meant to be used for debugging only.
              *
              * @param rolloutSpent
              *   the [[RolloutUtxo]] input to be added.
              * @return
              */
            private final def addRolloutSpent(
                rolloutSpent: RolloutUtxo
            ): BuildErrorOr[TransactionBuilder.Context] = {
                val steps = List(SpendRollout.spendRollout(builder.config, rolloutSpent.utxo))
                for {
                    addedRolloutInput <- TransactionBuilder.modify(ctx, steps)
                    finished <- builder.finish(addedRolloutInput)
                } yield finished
            }
        }

        object PartialResult {

            /** The first [[RolloutTx]] in the sequence. As such, it doesn't have any [[RolloutTx]]
              * predecessors in the sequence, and no payout obligations remain for these
              * (non-existent) predecessors to discharge.
              *
              * @param builder
              *   the transaction builder constructing this rollout tx.
              * @param ctx
              *   the transaction builder context we've reached so far.
              * @param inputValueNeeded
              *   the [[Value]] that this transaction needs its missing input to provide.
              * @tparam T
              *   indicates the type of [[RolloutTx]] being built.
              */
            final case class First[T <: RolloutTx](
                override val builder: Builder[T],
                override val ctx: TransactionBuilder.Context,
                override val inputValueNeeded: Value
            ) extends PartialResult[T]

            /** A non-first [[RolloutTx]] in the sequence. As such, it has at least one
              * [[RolloutTx]] predecessor in the sequence, and at least one payout obligation must
              * be discharged by these predecessors.
              *
              * @param builder
              *   the transaction builder constructing this rollout tx.
              * @param ctx
              *   the transaction builder context we've reached so far.
              * @param inputValueNeeded
              *   the [[Value]] that this transaction needs its missing input to provide.
              * @param payoutObligationsRemaining
              *   the payout obligations to be fulfilled by this transaction's predecessors.
              * @tparam T
              *   indicates the type of [[RolloutTx]] being built.
              */
            final case class NotFirst[T <: RolloutTx](
                override val builder: Builder[T],
                override val ctx: TransactionBuilder.Context,
                override val inputValueNeeded: Value,
                override val payoutObligationsRemaining: NonEmptyVector[Payout.Obligation.L1]
            ) extends PartialResult[T],
                  Payout.Obligation.L1.Many.Remaining.NonEmpty {
                def asFirst: First[T] = First(
                  builder = builder,
                  ctx = ctx,
                  inputValueNeeded = inputValueNeeded
                )
            }

            /** Transform a [[State]] into a [[PartialResult]]. If there are no more remaining
              * obligations in the state, then the rollout tx being built must be the first in the
              * sequence because the sequence is built backwards.
              */
            def fromState[T <: RolloutTx](
                builder: Builder[T],
                state: State[T]
            ): PartialResult[T] = {
                import state.*
                NonEmptyVector.fromVector(payoutObligationsRemaining) match
                    case None      => PartialResult.First(builder, ctx, inputValueNeeded)
                    case Some(nev) => PartialResult.NotFirst(builder, ctx, inputValueNeeded, nev)
            }
        }

        enum Args extends Payout.Obligation.L1.Many.Remaining.NonEmpty {
            case Last(override val payoutObligationsRemaining: NonEmptyVector[Payout.Obligation.L1])
            case NotLast(
                override val payoutObligationsRemaining: NonEmptyVector[Payout.Obligation.L1],
                rolloutOutputValue: Value
            ) extends Args

            def mbRolloutOutputValue: Option[Value] = this match {
                case a: Args.NotLast => Some(a.rolloutOutputValue)
                case _: Args.Last    => None
            }
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
            override val payoutObligationsRemaining: Vector[Payout.Obligation.L1]
        ) extends Tx.Builder.HasCtx,
              State.Fields.HasInputRequired,
              Payout.Obligation.L1.Many.Remaining

        object State {
            object Fields {
                sealed trait HasInputRequired {
                    def inputValueNeeded: Value
                }
            }
        }
    }

    trait Builder[T <: RolloutTx] extends Tx.Builder {
        type ArgsType <: Args

        /** After the [[Builder]] has finished building the transaction, apply post-processing to
          * the transaction builder context to get a [[RolloutTx]].
          *
          * The result is specific to [[Builder.Last]] and [[Builder.NotLast]].
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
        final def partialResult(args: ArgsType): BuildErrorOr[PartialResult[T]] =
            addPayouts(args)

        private final def addPayouts(args: ArgsType): BuildErrorOr[PartialResult[T]] = {
            for {
                ctx <- TransactionBuilder.build(
                  config.env.network,
                  BasePessimistic.commonSteps(config) ++ RolloutOutput
                      .mbSendRollout(config, args.mbRolloutOutputValue)
                      .toList
                )
                withPayout <- tryAddPayout(
                  ctx,
                  payoutObligation = args.payoutObligationsRemaining.head
                )
                basePessimistic = State[T](
                  ctx = withPayout._1,
                  inputValueNeeded = withPayout._2,
                  payoutObligationsRemaining = args.payoutObligationsRemaining.tail
                )
                res <- addPayoutsLoop(basePessimistic)
            } yield PartialResult.fromState(Builder.this, res)
        }

        @tailrec
        private final def addPayoutsLoop(
            state: State[T]
        ): BuildErrorOr[State[T]] = {
            import state.*
            payoutObligationsRemaining match {
                case obligation +: otherObligations =>
                    tryAddPayout(ctx, obligation) match {
                        case Right((newCtx, value)) =>
                            val newState: State[T] = state.copy(
                              ctx = newCtx,
                              inputValueNeeded = value,
                              payoutObligationsRemaining = otherObligations
                            )
                            addPayoutsLoop(newState)
                        case Left(err) =>
                            Tx.Builder.Incremental.replaceInvalidSizeException(err, state)
                    }
                case _Empty => Right(state)
            }
        }

        /** Try to add payouts one-by-one until adding another payout would exceed transaction size
          * limits.
          */

        /** Try to add a single payout to the rollout transaction, and then trial a finalization
          * with a placeholder rollout utxo as an input. If successful, returns the new [[Context]]
          * and the actual value needed for the prior rollout utxo.
          */
        private final def tryAddPayout(
            ctx: TransactionBuilder.Context,
            payoutObligation: Payout.Obligation.L1
        ): BuildErrorOr[(TransactionBuilder.Context, Value)] =
            val payoutStep = Send(payoutObligation.output)
            for {
                newCtx <- TransactionBuilder.modify(ctx, List(payoutStep))
                valueNeededWithFee <- Placeholder.trialFinish(this, newCtx)
            } yield (newCtx, valueNeededWithFee)
    }

    private object BuilderOps {
        object BasePessimistic {
            def commonSteps(config: Tx.Builder.Config): List[TransactionBuilderStep] =
                List(stepRolloutMetadata(config), stepReferenceHNS(config))

            def stepRolloutMetadata(config: Tx.Builder.Config): ModifyAuxiliaryData =
                ModifyAuxiliaryData(_ =>
                    Some(MD(MD.L1TxTypes.Rollout, headAddress = config.headAddress))
                )

            def stepReferenceHNS(config: Tx.Builder.Config) =
                ReferenceOutput(config.headNativeScriptReferenceInput)

        }

        object RolloutOutput {
            def mbSendRollout(
                config: Tx.Builder.Config,
                mbRolloutOutputValue: Option[Value]
            ): Option[Send] =
                mbRolloutOutputValue.map(x => Send(rolloutOutput(config, x)))

            def rolloutOutput(
                config: Tx.Builder.Config,
                rolloutOutputValue: Value
            ): TxOutput.Babbage = {
                TxOutput.Babbage(
                  address = config.headAddress,
                  value = rolloutOutputValue,
                  datumOption = None,
                  scriptRef = None
                )
            }
        }

        object SpendRollout {
            def spendRollout(
                config: Tx.Builder.Config,
                resolvedUtxo: TransactionUnspentOutput
            ): Spend =
                Spend(resolvedUtxo, config.headNativeScript.witness)
        }

        object Placeholder {

            /** Add the placeholder rollout input to ensure that the RolloutTx will fit within size
              * constraints after fee calculation + balancing.
              */
            def trialFinish[T <: RolloutTx](
                builder: Builder[T],
                ctx: TransactionBuilder.Context
            ): BuildErrorOr[Value] = {
                // The deficit in the inputs to the transaction prior to adding the placeholder
                val valueNeeded = Placeholder.inputValueNeeded(ctx)
                val valueNeededPlus = valueNeeded + Value(Coin.ada(1000))
                val placeholder = List(spendPlaceholderRollout(builder.config, valueNeededPlus))
                for {
                    addedPlaceholderRolloutInput <- TransactionBuilder.modify(ctx, placeholder)
                    finished <- builder.finish(addedPlaceholderRolloutInput)
                    valueNeededWithFee = valueNeeded + Value(
                      finished.transaction.body.value.fee - ctx.transaction.body.value.fee
                    )
                } yield valueNeededWithFee
            }

            private def spendPlaceholderRollout(config: Tx.Builder.Config, value: Value): Spend =
                SpendRollout.spendRollout(config, placeholderRolloutResolvedUtxo(config, value))

            private def placeholderRolloutResolvedUtxo(
                config: Tx.Builder.Config,
                value: Value
            ): TransactionUnspentOutput =
                TransactionUnspentOutput(
                  Placeholder.utxoId,
                  TxOutput.Babbage(
                    address = config.headAddress,
                    value = value
                  )
                )

            // A UtxoID with the largest possible size in Flat encoding
            // - Transaction ID should be 32 bytes of 1111 1111 because Flat uses the least number of bytes.
            // - The index can be zero because Flat will still use a full byte
            // https://hackage.haskell.org/package/flat-0.6/docs/Flat-Class.html
            val utxoId = TransactionInput(
              transactionId = TransactionHash.fromByteString(
                ByteString.fromArray(Array.fill(32)(Byte.MinValue))
              ),
              index = 0
            )

            def inputValueNeeded(ctx: TransactionBuilder.Context): Value =
                TxBalance.produced(ctx.transaction)
        }

        object PostProcess {

            /** Post-process a transaction builder context into a [[RolloutTx.Last]].
              *
              * @throws AssertionError
              *   when the assumptions of [[PostProcess.unsafeGetRolloutSpent]] fail to hold.
              */
            @throws[AssertionError]
            def last(ctx: TransactionBuilder.Context): RolloutTx.Last = {
                RolloutTx.Last(
                  rolloutSpent = PostProcess.unsafeGetRolloutSpent(ctx),
                  tx = ctx.transaction
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

                val rolloutProduced = TransactionUnspentOutput(
                  TransactionInput(transactionId = tx.id, index = 0),
                  rolloutOutput
                )
                RolloutTx.NotLast(
                  rolloutSpent = PostProcess.unsafeGetRolloutSpent(ctx),
                  rolloutProduced = RolloutUtxo(rolloutProduced),
                  tx = tx
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
                    TransactionUnspentOutput(firstInput, ctx.resolvedUtxos.utxos(firstInput))

                RolloutUtxo(firstInputResolved)
            }
        }
    }
}
