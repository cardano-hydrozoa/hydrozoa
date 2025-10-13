package hydrozoa.multisig.ledger.dapp.txseq.tx

import hydrozoa.lib.tx.BuildError.{BalancingError, StepError, ValidationError}
import hydrozoa.lib.tx.TransactionBuilderStep.{ModifyAuxiliaryData, ReferenceOutput, Send, Spend}
import hydrozoa.lib.tx.{
    BuildError,
    TransactionBuilder,
    TransactionBuilderStep,
    TransactionUnspentOutput
}
import hydrozoa.multisig.ledger.DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.txseq.tx.RolloutTx.Builder.State.First
import hydrozoa.multisig.ledger.dapp.utxo.RolloutUtxo
import hydrozoa.multisig.ledger.joint.utxo.Payout
import hydrozoa.reportDiffHandler
import scalus.builtin.ByteString
import scalus.cardano.ledger.TransactionException.InvalidTransactionSizeException
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.rules.TransactionSizeValidator
import scalus.cardano.ledger.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler
import scalus.cardano.ledger.txbuilder.TxBalancingError.CantBalance
import scalus.cardano.ledger.txbuilder.{Environment, TxBalancingError}
import scalus.cardano.ledger.utils.TxBalance
import scalus.cardano.ledger.{TransactionOutput as TxOutput, *}

import scala.annotation.tailrec

enum RolloutTx extends Tx {
    def rolloutSpent: RolloutUtxo

    case Last(
        override val rolloutSpent: RolloutUtxo,
        override val tx: Transaction
    ) extends RolloutTx

    case Intermediate(
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

        /**
         * An error that is thrown when we fail a pattern match on the result of [[reportDiffHandler]], which
         * should _always_ return a Left(CantBalance(...)). Thus, this should never be seen.
         */
        object IncoherentBalancingError

        type Result = RolloutTx


        type Error = BuildError | IncoherentBalancingError.type


        object PartialResult {
            type NeedsInput = NeedsInput.FirstOrOnly | NeedsInput.NotFirst

            object NeedsInput {
                import State.Status
                type FirstOrOnly = State.First[Status.NeedsInput] | State.Only[Status.NeedsInput]
                type LastOrOnly = State.Last[Status.NeedsInput] | State.Only[Status.NeedsInput]

                type NotFirst = State.Intermediate[Status.NeedsInput] |
                    State.Last[Status.NeedsInput]
                type NotLast = State.Intermediate[Status.NeedsInput] |
                    State.First[Status.NeedsInput]
            }
        }

        enum State extends HasTxBuilderContext, HasInputRequired:
            case Intermediate[Status <: State.Status](
                override val txBuilderContext: TransactionBuilder.Context,
                override val inputValueNeeded: Value,
                override val remainingPayoutObligations: Vector[Payout.Obligation.L1],
                override val rolloutOutput: TxOutput.Babbage
            ) extends State, HasRemainingPayoutObligations, HasRolloutOutput

            case Last[Status <: State.Status](
                override val txBuilderContext: TransactionBuilder.Context,
                override val inputValueNeeded: Value,
                override val remainingPayoutObligations: Vector[Payout.Obligation.L1]
            ) extends State, HasRemainingPayoutObligations

            case First[Status <: State.Status.NotInProgress](
                override val txBuilderContext: TransactionBuilder.Context,
                override val inputValueNeeded: Value,
                override val rolloutOutput: TxOutput.Babbage
            ) extends State, HasRolloutOutput

            case Only[Status <: State.Status.NotInProgress](
                override val txBuilderContext: TransactionBuilder.Context,
                override val inputValueNeeded: Value
            ) extends State

            def mbRolloutOutput: Option[TxOutput.Babbage] = this match {
                case x: HasRolloutOutput =>
                    Some(x.rolloutOutput)
                case _ => None
            }

        object State {
            type InProgress = State.Intermediate[Status.InProgress] | State.Last[Status.InProgress]

            type Status = Status.InProgress | Status.NeedsInput | Status.Finished

            object Status {
                type NotInProgress = Status.NeedsInput | Status.Finished

                type InProgress
                type NeedsInput
                type Finished
            }

            object Fields {
                sealed trait HasTxBuilderContext {
                    def txBuilderContext: TransactionBuilder.Context
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
    }

    final case class Builder(
        payouts: Vector[Payout.Obligation.L1],
        headNativeScript: HeadMultisigScript,
        headNativeScriptReferenceInput: TransactionUnspentOutput,
        env: Environment,
        validators: Seq[Validator],
        mbRolloutOutputValue: Option[Value]
    ) {

        import Builder.*
        import Builder.State.Status.*



        // Given:
        //    PartialResult.NeedsInput
        // Substitute type synonym:
        //    NeedsInput.FirstOrOnly | NeedsInput.NotFirst
        // Substitute type synonym on left + associativity of |:
        //    State.First[Status.NeedsInput] | State.Only[State.NeedsInput] | NeedsInput.NotFirst
        // Substitute type synoynm and associativity of |:
        //    State.First[Status.NeedsInput]
        //    | State.Only[State.NeedsInput]
        //    | State.Intermediate[Status.NeedsInput]
        //    | State.Last[Status.NeedsInput]
        def buildPartial(): Either[Error, PartialResult.NeedsInput] = {

            // Just a small helper to make the branching more readable.
            // I think there might be a way to do this is _.type? But I'm not certain
            enum Branch:
                case Only
                case First
                case Last
                case Intermediate

            for {
                bp <- BasePessimistic.basePessimistic
                withPayouts <- AddPayouts.addPayouts(bp)

                branch = if withPayouts.remainingPayoutObligations.isEmpty && mbRolloutOutputValue.isEmpty then Branch.Only
                else if withPayouts.remainingPayoutObligations.isEmpty then Branch.First
                else if mbRolloutOutputValue.isEmpty then Branch.Last
                else Branch.Intermediate

                res: PartialResult.NeedsInput = withPayouts match
                    case _ : State.Last[NeedsInput] => State.Last[State.Status.NeedsInput](
                        txBuilderContext = withPayouts.txBuilderContext,
                        inputValueNeeded = withPayouts.inputValueNeeded,
                        remainingPayoutObligations = withPayouts.remainingPayoutObligations
                    )
                    case _ : State.Intermediate[NeedsInput] => State.Intermediate[State.Status.NeedsInput](
                        txBuilderContext = withPayouts.txBuilderContext,
                        inputValueNeeded = withPayouts.inputValueNeeded,
                        remainingPayoutObligations = withPayouts.remainingPayoutObligations,
                        rolloutOutput = BasePessimistic.mkRolloutOutput(mbRolloutOutputValue.get)
                    )
            }
            yield res
        }

        object BasePessimistic {
            lazy val basePessimistic: Either[Error, State.InProgress] = {
                mbRolloutOutputValue match {
                    case Some(value) =>
                        val rolloutOutput = mkRolloutOutput(value)
                        for {
                            ctx <- TxBuilder.build(Send(rolloutOutput) +: commonSteps)
                            _ <- TxBuilder.finish(ctx)
                        } yield State.Intermediate[InProgress](ctx, value, payouts, rolloutOutput)
                    case None =>
                        val value = Value(Coin.zero)
                        for {
                            ctx <- TxBuilder.build(commonSteps)
                            // There's no point trying to finalize because there are no inputs/outputs
                            // _ <- TxBuilder.finish(ctx)
                        } yield State.Last[InProgress](ctx, value, payouts)
                }
            }

            lazy val commonSteps: List[TransactionBuilderStep] =
                List(setRolloutMetadata, referenceHNS)

            def spendMockRollout(value: Value): Spend =
                Spend(
                  TransactionUnspentOutput(
                    Mock.utxoId,
                    TxOutput.Babbage(
                      address = headNativeScript.mkAddress(env.network),
                        value = value
                    )
                  ),
                  headNativeScript.witness
                )

            def mkRolloutOutput(rolloutOutputValue: Value): TxOutput.Babbage =
                TxOutput.Babbage(
                  address = headNativeScript.mkAddress(env.network),
                  value = rolloutOutputValue,
                  datumOption = None,
                  scriptRef = None
                )

            lazy val setRolloutMetadata =
                ModifyAuxiliaryData(_ => Some(rolloutTxMetadata))

            lazy val rolloutTxMetadata: AuxiliaryData =
                MD(
                  MD.L1TxTypes.Rollout,
                  headAddress = headNativeScript.mkAddress(env.network)
                )

            lazy val referenceHNS = ReferenceOutput(headNativeScriptReferenceInput)
        }

        object AddPayouts {
            @tailrec
            def addPayouts(state: State.InProgress): Either[Error, State.InProgress] = {
                import state.*
                remainingPayoutObligations match {
                    case obligation +: otherObligations =>
                        tryAddPayout(txBuilderContext, obligation) match {
                            case Right((newCtx, value)) =>
                                val newState: State.InProgress = state match {
                                    case s: State.Intermediate[InProgress] => s.copy(
                                        txBuilderContext = newCtx,
                                        inputValueNeeded = value,
                                        remainingPayoutObligations = otherObligations,
                                        rolloutOutput = s.rolloutOutput
                                    )
                                    case s: State.Last[InProgress] => s.copy(
                                        txBuilderContext = newCtx,
                                        inputValueNeeded = value,
                                        remainingPayoutObligations = otherObligations
                                    )
                                }
                                addPayouts(newState)
                            case Left(e) => Right(state)
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
                    newCtx <- TxBuilder.modify(ctx, List(payoutStep))
                    value <- TxBuilder.trialFinishWithMock(ctx)
                } yield (newCtx, Value(Coin(value)))
        }

        object TxBuilder {
            def build(
                steps: List[TransactionBuilderStep]
            ): Either[Error, TransactionBuilder.Context] =
                TransactionBuilder.build(env.network, steps).left.map(StepError(_))

            def modify(
                ctx: TransactionBuilder.Context,
                steps: List[TransactionBuilderStep]
            ): Either[Error, TransactionBuilder.Context] =
                TransactionBuilder.modify(ctx, steps).left.map(StepError(_))

            /**
             * Add the mock rollout input to ensure that the RolloutTx will fit within size constraints
             * after fee calculation + balancing.
             *
             * @param ctx
             * @return Right(requiredAdaForPreviousRolloutUtxo) on success
             */
            def trialFinishWithMock(ctx: TransactionBuilder.Context): Either[Error, Long] =
                for {
                    // Check with 0 ada mock to get the diff
                    with0AdaMock <- modify(ctx, List(BasePessimistic.spendMockRollout(Value.zero)))
                    requiredAda <- with0AdaMock.finalizeContext(
                        protocolParams = env.protocolParams,
                        diffHandler = reportDiffHandler,
                        evaluator = env.evaluator,
                        validators = List.empty) match
                        case Left(CantBalance(diff)) => Right(diff)
                        // Not possible -- our diff handler only returns CantBalance
                        // and we don't run any validators
                        case Left(e) => Left(e).left
                            .map({
                                case balanceError: TxBalancingError =>
                                    BalancingError(balanceError)
                                case validationError: TransactionException =>
                                    ValidationError(validationError)
                            })
                        case Right(_) => Left(IncoherentBalancingError)

                    // Check with full Ada mock to get a _successful_ balance, which includes calculating the
                    // correct fee (which can increase the size of the transaction)
                    withFullAdaMock <- modify(ctx, List(BasePessimistic.spendMockRollout(Value(Coin(requiredAda)))))
                    _ <- withFullAdaMock.finalizeContext(
                            protocolParams = env.protocolParams,
                            diffHandler = new ChangeOutputDiffHandler(env.protocolParams, 0).changeOutputDiffHandler,
                            evaluator = env.evaluator,
                            validators = List(TransactionSizeValidator))
                        .left
                        .map({
                            case balanceError: TxBalancingError =>
                                BalancingError(balanceError)
                            case validationError: TransactionException =>
                                ValidationError(validationError)
                        })
                } yield (requiredAda)

            def finish(
                txBuilderContext: TransactionBuilder.Context
            ): Either[Error, TransactionBuilder.Context] =
                for {
                    // Try to build, balance, and validate the resulting transaction
                    finished <- txBuilderContext
                        .finalizeContext(
                          protocolParams = env.protocolParams,
                          diffHandler = ChangeOutputDiffHandler(
                            protocolParams = env.protocolParams,
                            changeOutputIdx = 0
                          ).changeOutputDiffHandler,
                          evaluator = env.evaluator,
                          validators = validators
                        )
                        .left
                        .map({
                            case balanceError: TxBalancingError =>
                                BalancingError(balanceError)
                            case validationError: TransactionException =>
                                ValidationError(validationError)
                        })
                } yield finished

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
    }
}
