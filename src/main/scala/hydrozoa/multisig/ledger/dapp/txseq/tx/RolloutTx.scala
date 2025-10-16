package hydrozoa.multisig.ledger.dapp.txseq.tx

import cats.data.NonEmptyVector
import hydrozoa.lib.tx.SomeBuildError.ValidationError
import hydrozoa.lib.tx.TransactionBuilderStep.{ModifyAuxiliaryData, ReferenceOutput, Send, Spend}
import hydrozoa.lib.tx.{SomeBuildError, TransactionBuilder, TransactionBuilderStep, TransactionUnspentOutput}
import hydrozoa.multisig.ledger.DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.txseq.tx.RolloutTx.Builder.State.Status.NeedsInput
import hydrozoa.multisig.ledger.dapp.utxo.RolloutUtxo
import hydrozoa.multisig.ledger.joint.utxo.Payout
import scalus.builtin.ByteString
import scalus.cardano.ledger.TransactionException.InvalidTransactionSizeException
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.txbuilder.Environment
import scalus.cardano.ledger.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler
import scalus.cardano.ledger.utils.TxBalance
import scalus.cardano.ledger.{TransactionOutput as TxOutput, *}

import scala.annotation.tailrec

enum RolloutTx extends Tx {
    def rolloutSpent: RolloutUtxo

    case Last(
        override val rolloutSpent: RolloutUtxo,
        override val tx: Transaction
    ) extends RolloutTx

    case NotLast(
        override val rolloutSpent: RolloutUtxo,
        /** Invariant: this rollout utxo MUST have the txId of the [[tx]] field and index 0. */
        override val rolloutProduced: RolloutUtxo,
        override val tx: Transaction
    ) extends RolloutTx, RolloutTx.HasRolloutProduced

    def mbRolloutProduced: Option[RolloutUtxo] = this match {
        case x: RolloutTx.HasRolloutProduced =>
            Some(x.rolloutProduced)
        case _ => None
    }
}

object RolloutTx {
    trait HasRolloutProduced {
        def rolloutProduced: RolloutUtxo
    }

    trait Builder:
        /** The current payout obligations when this rollout tx is built. The builder packs as many
          * as possible into this transaction (while preserving the order of the input sequence) and
          * returns the _remaining payouts_ (the ones that didn't fit) in its result.
          *
          * @return
          */
        def payouts: NonEmptyVector[Payout.Obligation.L1]
        def config: Builder.Config

        import Builder.*
        import Builder.State.Status

        type BuildPartialType
        final def buildPartial(): Either[Error, BuildPartialType] =
            for {
                needsInput <- progress()
                partialResult = classifyPartialResult(needsInput)
            } yield partialResult

        final def progress(): Either[Error, State[Status.NeedsInput]] = for {
            withPayouts <- AddPayouts.addPayouts()
        } yield State.NeedsInput.fromInProgress(withPayouts)

        def classifyPartialResult(state: State[Status.NeedsInput]): BuildPartialType

        type RolloutTxType
        final def complete(
            pr: PartialResult,
            rolloutSpent: RolloutUtxo
        ): Either[Error, RolloutTxType] =
            for {
                finished <- finish(pr, rolloutSpent)
            } yield getRolloutTx(finished)

        def getRolloutTx(ctx : TransactionBuilder.Context): RolloutTxType

        final def finish(
            pr : PartialResult,
            rolloutSpent: RolloutUtxo
        ): Either[Error, TransactionBuilder.Context] =
            for {
                addedRolloutInput <- TransactionBuilder.modify(
                  pr.ctx,
                  List(TxBuilder.spendRollout(rolloutSpent.utxo))
                )
                finished <- TxBuilder.finish(addedRolloutInput)
            } yield finished

        def mbSendRollout: Option[Send]

        /** This object serves to organize the construction of a "pessimistic" [[State]] that
          * assumes worst-case packing of payout obligations into the transaction by taking the
          * absolute minimum requirements of a rollout tx. These include:
          *   - Setting the metadata for the transaction
          *   - Adding the utxo with the head native script to the reference inputs of the tx body
          *   - If this rollout transaction is not the last one in the chain, creating the next
          *     rollout utxo.
          *
          * Then, it calculates the value that would satisfy the requirements of the transaction
          * (including balancing and fee).
          */
        object BasePessimistic {
//            // TODO: Add a single withdrawal
//            lazy val basePessimistic: Either[Error, State[Status.InProgress]] =
//                for {
//                    ctx <- TransactionBuilder.build(
//                      config.env.network,
//                      commonSteps ++ mbSendRollout.toList
//                    )
//                    valueNeededWithFee <- TxBuilder.trialFinishWithMock(ctx)
//                } yield State[Status.InProgress](
//                  ctx = ctx,
//                  inputValueNeeded = valueNeededWithFee,
//                    remainingPayoutObligations = payouts.toVector
//                )

            lazy val commonSteps: List[TransactionBuilderStep] =
                List(setRolloutMetadata, referenceHNS)

            lazy val setRolloutMetadata =
                ModifyAuxiliaryData(_ => Some(rolloutTxMetadata))

            lazy val rolloutTxMetadata: AuxiliaryData =
                MD(
                  MD.L1TxTypes.Rollout,
                  headAddress = config.headNativeScript.mkAddress(config.env.network)
                )

            lazy val referenceHNS = ReferenceOutput(config.headNativeScriptReferenceInput)
        }

        object AddPayouts {
            def addPayouts(): Either[Error, State[Status.InProgress]] = {
                for {
                    ctx <- TransactionBuilder.build(
                      config.env.network,
                      BasePessimistic.commonSteps ++ mbSendRollout.toList
                    )
                    withPayout <- tryAddPayout(ctx, payoutObligation = payouts.head)
                    basePessimistic = State[Status.InProgress](
                        ctx = withPayout._1,
                        inputValueNeeded = withPayout._2,
                        remainingPayoutObligations = payouts.tail
                    )
                    res <- loop(basePessimistic)
                } yield res

            }

            /** Try to add payouts one-by-one until adding another payout would exceed transaction
              * size limits.
              */
            @tailrec
            def loop(
                state: State[Status.InProgress]
            ): Either[Error, State[Status.InProgress]] = {
                import state.*
                remainingPayoutObligations match {
                    case obligation +: otherObligations =>
                        tryAddPayout(ctx, obligation) match {
                            case Right((newCtx, value)) =>
                                val newState: State[Status.InProgress] = state.copy(
                                  ctx = newCtx,
                                  inputValueNeeded = value,
                                  remainingPayoutObligations = otherObligations
                                )
                                loop(newState)
                            case Left(err) =>
                                TxBuilder.replaceInvalidSizeException(err, state)
                        }
                    case _Empty => Right(state)
                }
            }

            /** Try to add a single payout to the rollout transaction, and then trial a finalization
              * with a mock rollout utxo as an input. If successful, returns the new [[Context]] and
              * the actual value needed for the prior rollout utxo.
              */
            def tryAddPayout(
                ctx: TransactionBuilder.Context,
                payoutObligation: Payout.Obligation.L1
            ): Either[Error, (TransactionBuilder.Context, Value)] =
                val payoutStep = Send(payoutObligation.output)
                for {
                    newCtx <- TransactionBuilder.modify(ctx, List(payoutStep))
                    valueNeededWithFee <- TxBuilder.trialFinishWithMock(newCtx)
                } yield (newCtx, valueNeededWithFee)
        }

        object TxBuilder {
            def spendRollout(resolvedUtxo: TransactionUnspentOutput): Spend =
                Spend(resolvedUtxo, config.headNativeScript.witness)

            def spendMockRollout(value: Value): Spend =
                spendRollout(mockRolloutResolvedUtxo(value))

            def mockRolloutResolvedUtxo(value: Value): TransactionUnspentOutput =
                TransactionUnspentOutput(
                  Mock.utxoId,
                  TxOutput.Babbage(
                    address = config.headNativeScript.mkAddress(config.env.network),
                    value = value
                  )
                )

            /** Add the mock rollout input to ensure that the RolloutTx will fit within size
              * constraints after fee calculation + balancing.
              */
            def trialFinishWithMock(ctx: TransactionBuilder.Context): Either[Error, Value] = {
                // The deficit in the inputs to the transaction prior to adding the mock
                val valueNeeded = Mock.inputValueNeeded(ctx)
                val valueNeededPlus = valueNeeded + Value(Coin.ada(1000))
                for {
                    addedMockRolloutInput <- TransactionBuilder.modify(
                      ctx,
                      List(spendMockRollout(valueNeededPlus))
                    )
                    finished <- finish(addedMockRolloutInput)
                    valueNeededWithFee = valueNeeded + Value(
                      finished.transaction.body.value.fee - ctx.transaction.body.value.fee
                    )
                } yield valueNeededWithFee
            }

            def finish(
                txBuilderContext: TransactionBuilder.Context
            ): Either[Error, TransactionBuilder.Context] =
                // Try to build, balance, and validate the resulting transaction
                txBuilderContext
                    .finalizeContext(
                      protocolParams = config.env.protocolParams,
                      diffHandler = ChangeOutputDiffHandler(
                        protocolParams = config.env.protocolParams,
                        changeOutputIdx = 0
                      ).changeOutputDiffHandler,
                      evaluator = config.env.evaluator,
                      validators = config.validators
                    )

            /** Replace a [[InvalidTransactionSizeException]] with some other value.
              *
              * @param err
              *   The error to replace.
              * @param replacement
              *   The replacement value, provided as a lazy argument.
              * @tparam A
              *   The type of the replacement value, usually inferred by Scala.
              * @return
              */
            def replaceInvalidSizeException[A](err: Error, replacement: => A): Either[Error, A] =
                err match
                    case ValidationError(ve) =>
                        ve match {
                            case _: InvalidTransactionSizeException =>
                                Right(replacement)
                            case _ => Left(err)
                        }
                    case _ => Left(err)
        }

        object PostProcess {

            /** Given a finished [[State]], get the spent rollout utxo from its transaction. Assumes
              * that the spent rollout exists as the only input in the transaction.
              *
              * @param state
              *   the finalized state
              * @throws AssertionError
              *   when the assumption is broken
              * @return
              *   the resolved spend rollout utxo
              */
            @throws[AssertionError]
            def unsafeGetRolloutSpent(ctx : TransactionBuilder.Context): RolloutUtxo = {
                val tx = ctx.transaction
                val inputs = tx.body.value.inputs.toSeq

                assert(inputs.nonEmpty)
                assert(inputs.tail.isEmpty)
                val firstInput = inputs.head

                assert(inputs.contains(firstInput))
                val firstInputResolved =
                    TransactionUnspentOutput(firstInput, ctx.resolvedUtxos.utxos(firstInput))

                RolloutUtxo(firstInputResolved)
            }
        }

    object Builder {

        import Fields.*
        import State.Status

        /** Partial results are enumerated based on their position within the rollout chain. The
          * builders for [[Intermediate]] and [[First]] produce transactions that produce rollout
          * utxos, while the builders for [[Last]] and [[Only]] do not.
          */
        enum PartialResult extends HasTxBuilderContext, HasInputRequired:
            def builder: Builder

            case Intermediate(
                                 override val builder: Builder.NotLast,
                                 override val ctx: TransactionBuilder.Context,
                                 override val inputValueNeeded: Value,
                                 val remainingPayoutObligations: NonEmptyVector[Payout.Obligation.L1],
                             ) extends PartialResult, HasRemainingPayoutObligations

            case Last(
                         override val builder: Builder.Last,
                         override val ctx: TransactionBuilder.Context,
                         override val inputValueNeeded: Value,
                         val remainingPayoutObligations: NonEmptyVector[Payout.Obligation.L1],
                     ) extends PartialResult, HasRemainingPayoutObligations

            case First(
                          override val builder: Builder.NotLast,
                          override val ctx: TransactionBuilder.Context,
                          override val inputValueNeeded: Value,
            )
            case Only(
                         override val builder: Builder.Last,
                         override val ctx: TransactionBuilder.Context,
                         override val inputValueNeeded: Value,
            )

            final def tx: Transaction = ctx.transaction
            final def finish(rolloutSpent: RolloutUtxo): Either[Error, TransactionBuilder.Context] = {
                builder.finish(this, rolloutSpent = rolloutSpent)
            }

        type Error = SomeBuildError

        type Result = RolloutTx

        /** The state associated with a single rollout tx. It is passed between functions that
          * modify it in different stages, and carries a [[Status]] phantom type to indicate which
          * operations have already been applied.
          *
          * @param ctx
          *   The [[Context]] "so far" on the state, including a partially-built transaction, s
          *   resolved utxos, etc.
          * @param inputValueNeeded
          *   The input value needed to process this rollout tx. If this is not the Only or Last
          *   rollout transaction, this includes the value necessary to establish the next rollout
          *   utxo and pay for its fees.
          * @param remainingPayoutObligations
          *   The payout obligations that were not processes by this rollout transaction. May be empty.
          * @tparam Status
          *   The phantom type indicating which operations the builder has already applied to the
          *   state.
          */
        final case class State[Status <: State.Status](
            override val ctx: TransactionBuilder.Context,
            override val inputValueNeeded: Value,
            remainingPayoutObligations: Vector[Payout.Obligation.L1]
                                                      ) extends HasTxBuilderContext, HasInputRequired

        /** A builder for the last (or only) rollout transaction in a chain. It doesn't produce a
          * subsequent rollout utxo. In practice, this rollout transaction is built _first_ and we
          * work backwards to construct the rest of the chain.
          */
        final case class Last(
            override val config: Config,
            override val payouts: NonEmptyVector[Payout.Obligation.L1]
        ) extends Builder {
            override lazy val mbSendRollout: None.type = None

            override type BuildPartialType = PartialResult.LastOrOnly

            override def classifyPartialResult(needsInput: State[Status.NeedsInput]): BuildPartialType = {
                import needsInput.*
                NonEmptyVector.fromVector(needsInput.remainingPayoutObligations) match
                    case None => PartialResult.Only(
                        builder = this,
                        ctx = ctx,
                        inputValueNeeded = inputValueNeeded
                    )
                    case Some(nev) => PartialResult.Last(
                        builder = this,
                        ctx = ctx,
                        inputValueNeeded = inputValueNeeded,
                        remainingPayoutObligations = nev
                    )
            }

            override type RolloutTxType = RolloutTx.Last

            @throws[AssertionError]
            override def getRolloutTx(ctx : TransactionBuilder.Context): RolloutTxType = {
                RolloutTx.Last(
                  rolloutSpent = PostProcess.unsafeGetRolloutSpent(ctx),
                  tx = ctx.transaction
                )
            }
        }

        /** A builder for non-terminal rollout transaction in a chain. It produces a subsequent
          * rollout utxo with the correct amount value required for the _next_ rollout transaction
          * in the chain, which is _built_ prior to this transaction.
          * @param payouts
          *   the payout obligations that need to be discharged
          * @param rolloutOutputValue
          *   The rollout output this transaction produces, which must be consumed as an input in
          *   the next rollout transaction.
          */
        final case class NotLast(
            override val config: Config,
            override val payouts: NonEmptyVector[Payout.Obligation.L1],
            rolloutOutputValue: Value
        ) extends Builder {
            override lazy val mbSendRollout: Option[Send] = Some(sendRollout)

            lazy val sendRollout = Send(rolloutOutput)

            lazy val rolloutOutput: TxOutput.Babbage =
                TxOutput.Babbage(
                    address = config.headNativeScript.mkAddress(config.env.network),
                    value = rolloutOutputValue,
                    datumOption = None,
                    scriptRef = None
                )

            override type BuildPartialType = PartialResult.NotLast

            override def classifyPartialResult(needsInput: State[Status.NeedsInput]): BuildPartialType = {
                import needsInput.*
                NonEmptyVector.fromVector(needsInput.remainingPayoutObligations) match
                    case None => PartialResult.First(
                        builder = this,
                        ctx = ctx,
                        inputValueNeeded = inputValueNeeded
                    )
                    case Some(nev) => PartialResult.Intermediate(
                        builder = this,
                        ctx = ctx,
                        inputValueNeeded = inputValueNeeded,
                        remainingPayoutObligations = nev
                    )
            }

            override type RolloutTxType = RolloutTx.NotLast

            @throws[AssertionError]
            override def getRolloutTx(
                ctx : TransactionBuilder.Context
            ): RolloutTxType = {
                val tx = ctx.transaction
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
        }

        object State {


            type Status = Status.InProgress | Status.NeedsInput | Status.Finished

            object Status {
                type InProgress
                type NeedsInput
                type Finished
            }

            object NeedsInput {
                def fromInProgress(ip : State[Status.InProgress]): State[Status.NeedsInput] = {
                    import ip.*
                    State(ctx, inputValueNeeded, remainingPayoutObligations)
                }
            }

            object Finished {
                def fromNeedsInput(ni : State[Status.NeedsInput]): State[Status.Finished] = {
                    import ni.*
                    State(ctx, inputValueNeeded, remainingPayoutObligations)
                }
            }
        }

        object Mock {
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

        // TODO: Find a better place for this
        final case class Config(
            headNativeScript: HeadMultisigScript,
            headNativeScriptReferenceInput: TransactionUnspentOutput,
            env: Environment,
            validators: Seq[Validator]
        )

        /** Common fields shared between different parts of the builder
         *
         */
        object Fields {
            sealed trait HasTxBuilderContext {
                def ctx: TransactionBuilder.Context
            }

            sealed trait HasInputRequired {
                def inputValueNeeded: Value
            }

            sealed trait HasRemainingPayoutObligations {
                def remainingPayoutObligations: NonEmptyVector[Payout.Obligation.L1]
            }

            sealed trait HasRolloutOutput {
                def rolloutOutput: TxOutput.Babbage
            }
        }

        object PartialResult {
            type FirstOrOnly = PartialResult.First | PartialResult.Only

            type NotLast = Intermediate | First

            type LastOrOnly = PartialResult.Last | Only


            extension (pr: PartialResult.Intermediate)
                /** Used to "promote" an intermediate transaction to a "first" if the settlement tx merged in the
                 * first-of-many rollouts
                 */

                def asFirst: PartialResult.First =
                    import pr.*
                    PartialResult.First(builder, ctx ,inputValueNeeded)

            extension (pr: PartialResult.Last)
                /**
                 * Used to "promote" a  transaction to a "first" if the settlement tx merged in the
                 */
                def asOnly: PartialResult.Only =
                    import pr.*
                    PartialResult.Only(builder, ctx, inputValueNeeded)
        }
    }

}
