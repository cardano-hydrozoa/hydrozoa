package hydrozoa.multisig.ledger.dapp.txseq.tx

import hydrozoa.lib.tx.SomeBuildError.ValidationError
import hydrozoa.lib.tx.TransactionBuilderStep.{ModifyAuxiliaryData, ReferenceOutput, Send, Spend}
import hydrozoa.lib.tx.{
    SomeBuildError,
    TransactionBuilder,
    TransactionBuilderStep,
    TransactionUnspentOutput
}
import hydrozoa.multisig.ledger.DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.txseq.tx.RolloutTx.Builder.State.Status.NeedsInput
import hydrozoa.multisig.ledger.dapp.utxo.RolloutUtxo
import hydrozoa.multisig.ledger.joint.utxo.Payout
import scalus.builtin.ByteString
import scalus.cardano.ledger.TransactionException.InvalidTransactionSizeException
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler
import scalus.cardano.ledger.txbuilder.Environment
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

    object Builder {
        import State.Fields.*

        type Error = SomeBuildError

        type Result = RolloutTx

        enum PartialResult:
            def state: State[State.Status.NeedsInput]
            def builder: Builder

            case Intermediate(
                override val state: State[State.Status.NeedsInput],
                override val builder: Builder.NotLast
            )
            case Last(
                override val state: State[State.Status.NeedsInput],
                override val builder: Builder.Last
            )
            case First(
                override val state: State[State.Status.NeedsInput],
                override val builder: Builder.NotLast
            )
            case Only(
                override val state: State[State.Status.NeedsInput],
                override val builder: Builder.Last
            )

            def inputValueNeeded: Value = state.inputValueNeeded

            def tx: Transaction = state.ctx.transaction

        object PartialResult {
            type FirstOrOnly = PartialResult.First | PartialResult.Only

            type NotLast = Intermediate | First

            type LastOrOnly = PartialResult.Last | Only
        }

        final case class State[Status <: State.Status](
            override val ctx: TransactionBuilder.Context,
            override val inputValueNeeded: Value,
            override val remainingPayoutObligations: Vector[Payout.Obligation.L1]
        ) extends HasTxBuilderContext,
              HasInputRequired,
              HasRemainingPayoutObligations

        object State {
            object Fields {
                sealed trait HasTxBuilderContext {
                    def ctx: TransactionBuilder.Context
                }

                sealed trait HasInputRequired {
                    def inputValueNeeded: Value
                }

                sealed trait HasRemainingPayoutObligations {
                    def remainingPayoutObligations: Vector[Payout.Obligation.L1]
                }

                sealed trait HasRolloutOutput {
                    def rolloutOutput: TxOutput.Babbage
                }
            }

            type Status = Status.InProgress | Status.NeedsInput | Status.Finished

            object Status {
                type InProgress
                type NeedsInput
                type Finished
            }

            object NeedsInput {
                given fromInProgress
                    : Conversion[State[Status.InProgress], State[Status.NeedsInput]] = identity
            }

            object Finished {
                given fromNeedsInput: Conversion[State[Status.NeedsInput], State[Status.Finished]] =
                    identity
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
        final case class Config (
            headNativeScript: HeadMultisigScript,
            headNativeScriptReferenceInput: TransactionUnspentOutput,
            env: Environment,
            validators: Seq[Validator]
        )

        final case class Last(
            override val config: Config,
            override val payouts: Vector[Payout.Obligation.L1],
        ) extends Builder {
            def buildPartial(): Either[Error, PartialResult.LastOrOnly] = {
                for {
                    bp <- BasePessimistic.basePessimistic
                    withPayouts <- AddPayouts.addPayouts(bp)
                    needsInput = State.NeedsInput.fromInProgress(withPayouts)
                    partialResult: PartialResult.LastOrOnly =
                        if needsInput.remainingPayoutObligations.isEmpty then
                            PartialResult.Only(needsInput, this)
                        else PartialResult.Last(needsInput, this)
                } yield partialResult
            }
        }

        final case class NotLast(
            override val config: Config,
            override val payouts: Vector[Payout.Obligation.L1],
            rolloutOutputValue: Value
        ) extends Builder {
            def buildPartial(): Either[Error, PartialResult.NotLast] = {
                for {
                    bp <- BasePessimistic.basePessimistic
                    withPayouts <- AddPayouts.addPayouts(bp)
                    needsInput = State.NeedsInput.fromInProgress(withPayouts)
                    partialResult: PartialResult.NotLast =
                        if needsInput.remainingPayoutObligations.isEmpty then
                            PartialResult.First(needsInput, this)
                        else PartialResult.Intermediate(needsInput, this)
                } yield partialResult
            }
        }
    }

    trait Builder:
        def payouts: Vector[Payout.Obligation.L1]
        def config: Builder.Config

        import Builder.*
        import Builder.State.Status.*

        object BasePessimistic {
            lazy val basePessimistic: Either[Error, State[InProgress]] =
                for {
                    ctx <- TransactionBuilder.build(
                      config.env.network,
                      commonSteps ++ mbSendRollout.toList
                    )
                    valueNeededWithFee <- TxBuilder.trialFinishWithMock(ctx)
                } yield State[InProgress](
                  ctx = ctx,
                  inputValueNeeded = valueNeededWithFee,
                  remainingPayoutObligations = payouts.toVector
                )

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

            lazy val mbSendRollout: Option[Send] =
                Builder.this match {
                    case thisBuilder: Builder.NotLast =>
                        Some(Send(mkRolloutOutput(thisBuilder)))
                    case thisBuilder: Builder.Last => None
                }

            def mkRolloutOutput(thisBuilder: Builder.NotLast): TxOutput.Babbage =
                TxOutput.Babbage(
                  address = config.headNativeScript.mkAddress(config.env.network),
                  value = thisBuilder.rolloutOutputValue,
                  datumOption = None,
                  scriptRef = None
                )
        }

        object AddPayouts {
            @tailrec
            def addPayouts(
                state: State[InProgress]
            ): Either[Error, State[InProgress]] = {
                import state.*
                remainingPayoutObligations match {
                    case obligation +: otherObligations =>
                        tryAddPayout(ctx, obligation) match {
                            case Right((newCtx, value)) =>
                                val newState: State[InProgress] = state.copy(
                                  ctx = newCtx,
                                  inputValueNeeded = value,
                                  remainingPayoutObligations = otherObligations
                                )
                                addPayouts(newState)
                            case Left(err) =>
                                TxBuilder.replaceInvalidSizeException(err, state)
                        }
                    case _Empty => Right(state)
                }
            }

            def tryAddPayout(
                ctx: TransactionBuilder.Context,
                payoutObligation: Payout.Obligation.L1
            ): Either[Error, (TransactionBuilder.Context, Value)] =
                val payoutStep = Send(payoutObligation.output)
                for {
                    newCtx <- TransactionBuilder.modify(ctx, List(payoutStep))
                    valueNeededWithFee <- TxBuilder.trialFinishWithMock(ctx)
                } yield (newCtx, valueNeededWithFee)
        }

        object Finish {
            def finish(
                state: State[NeedsInput],
                rolloutSpent: TransactionUnspentOutput
            ): Either[Error, State[Finished]] =
                for {
                    addedRolloutInput <- TransactionBuilder.modify(
                      state.ctx,
                      List(TxBuilder.spendRollout(rolloutSpent))
                    )
                    finished <- TxBuilder.finish(addedRolloutInput)
                } yield State.Finished.fromNeedsInput(state.copy(ctx = finished))
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
            @throws[AssertionError]
            def getRolloutTx(
                state: State[Finished]
            ): RolloutTx =
                Builder.this match {
                    case thisBuilder: Builder.NotLast =>
                        NotLast.getRolloutTx(state, thisBuilder)
                    case thisBuilder: Builder.Last =>
                        Last.getRolloutTx(state, thisBuilder)
                }

            object Last {
                @throws[AssertionError]
                def getRolloutTx(
                    state: State[Finished],
                    thisBuilder: Builder.Last
                ): RolloutTx.Last = {
                    RolloutTx.Last(
                      rolloutSpent = unsafeGetRolloutSpent(state),
                      tx = state.ctx.transaction
                    )
                }
            }

            object NotLast {
                @throws[AssertionError]
                def getRolloutTx(
                    state: State[Finished],
                    thisBuilder: Builder.NotLast
                ): RolloutTx.NotLast = {
                    val tx = state.ctx.transaction
                    val rolloutProduced = TransactionUnspentOutput(
                      TransactionInput(transactionId = tx.id, index = 0),
                      BasePessimistic.mkRolloutOutput(thisBuilder)
                    )
                    RolloutTx.NotLast(
                      rolloutSpent = unsafeGetRolloutSpent(state),
                      rolloutProduced = RolloutUtxo(rolloutProduced),
                      tx = tx
                    )
                }
            }

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
            def unsafeGetRolloutSpent(state: State[Finished]): RolloutUtxo = {
                import state.*
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

}
