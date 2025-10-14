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
import hydrozoa.multisig.ledger.dapp.txseq.tx.RolloutTx.Builder.State.First
import hydrozoa.multisig.ledger.dapp.utxo.RolloutUtxo
import hydrozoa.multisig.ledger.joint.utxo.Payout
import scalus.builtin.ByteString
import scalus.cardano.ledger.TransactionException.InvalidTransactionSizeException
import scalus.cardano.ledger.rules.STS.Validator
// import scalus.cardano.ledger.rules.TransactionSizeValidator
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

        /** An error that is thrown when we fail a pattern match on the result of
          * [[reportDiffHandler]], which should _always_ return a Left(CantBalance(...)). Thus, this
          * should never be seen.
          */
        object IncoherentBalancingError

        type Result = RolloutTx

        type Error = SomeBuildError | IncoherentBalancingError.type

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

            type Status = Status.InProgress | Status.NeedsInput | Status.Finished

            object Status {
                type NotInProgress = Status.NeedsInput | Status.Finished

                type InProgress
                type NeedsInput
                type Finished
            }

            type InProgress = State.Intermediate[Status.InProgress] | State.Last[Status.InProgress]

            type NeedsInput = State.Intermediate[Status.NeedsInput] |
                State.Last[Status.NeedsInput] | State.First[Status.NeedsInput] |
                State.Only[Status.NeedsInput]

            object NeedsInput {

                /** Convert a state from [[Status.InProgress]] to [[Status.NeedsInput]]. If its
                  * [[remainingPayoutObligations]] is empty, then:
                  *
                  *   - A [[State.Intermediate]] becomes a [[State.First]]
                  *   - A [[State.Last]] becomes a [[State.Only]].
                  */
                given fromInProgress: Conversion[State.InProgress, State.NeedsInput] = {
                    case s: State.Intermediate[Status.InProgress] =>
                        if s.remainingPayoutObligations.isEmpty then {
                            State.First[Status.NeedsInput](
                              txBuilderContext = s.txBuilderContext,
                              inputValueNeeded = s.inputValueNeeded,
                              rolloutOutput = s.rolloutOutput
                            )
                        } else s.convert
                    case s: State.Last[Status.InProgress] =>
                        if s.remainingPayoutObligations.isEmpty then {
                            State.Only[Status.NeedsInput](
                              txBuilderContext = s.txBuilderContext,
                              inputValueNeeded = s.inputValueNeeded
                            )
                        } else s.convert
                }

                private given Conversion[Intermediate[Status.InProgress], Intermediate[
                  Status.NeedsInput
                ]] = identity

                private given Conversion[Last[Status.InProgress], Last[Status.NeedsInput]] =
                    identity

                type FirstOrOnly = State.First[Status.NeedsInput] | State.Only[Status.NeedsInput]
                type LastOrOnly = State.Last[Status.NeedsInput] | State.Only[Status.NeedsInput]

                type NotFirst = State.Intermediate[Status.NeedsInput] |
                    State.Last[Status.NeedsInput]
                type NotLast = State.Intermediate[Status.NeedsInput] |
                    State.First[Status.NeedsInput]
            }

            type Finished = State.Intermediate[Status.Finished] | State.Last[Status.Finished] |
                State.First[Status.Finished] | State.Only[Status.Finished]

            object Finished {

                /** This conversion is trivial. */
                given fromNeedsInput: Conversion[State.NeedsInput, State.Finished] = {
                    case x: Intermediate[Status.NeedsInput] => x.convert
                    case x: Last[Status.NeedsInput]         => x.convert
                    case x: First[Status.NeedsInput]        => x.convert
                    case x: Only[Status.NeedsInput]         => x.convert
                }

                private given Conversion[Intermediate[Status.NeedsInput], Intermediate[
                  Status.Finished
                ]] = identity

                private given Conversion[Last[Status.NeedsInput], Last[Status.Finished]] = identity

                private given Conversion[First[Status.NeedsInput], First[Status.Finished]] =
                    identity

                private given Conversion[Only[Status.NeedsInput], Only[Status.Finished]] = identity
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

        def buildPartial(): Either[Error, State.NeedsInput] = {
            for {
                bp <- BasePessimistic.basePessimistic
                withPayouts <- AddPayouts.addPayouts(bp)
            } yield State.NeedsInput.fromInProgress(withPayouts)
        }

        object BasePessimistic {
            lazy val basePessimistic: Either[Error, State.InProgress] = {
                mbRolloutOutputValue match {
                    case Some(value) =>
                        val rolloutOutput = mkRolloutOutput(value)
                        for {
                            ctx <- TransactionBuilder.build(
                              env.network,
                              Send(rolloutOutput) +: commonSteps
                            )
                            valueNeededWithFee <- TxBuilder.trialFinishWithMock(ctx)
                        } yield State.Intermediate[InProgress](
                          txBuilderContext = ctx,
                          inputValueNeeded = valueNeededWithFee,
                          remainingPayoutObligations = payouts,
                          rolloutOutput = rolloutOutput
                        )
                    case None =>
                        val value = Value(Coin.zero)
                        for {
                            ctx <- TransactionBuilder.build(env.network, commonSteps)
                            // There's no point attempting to finalized because there are no inputs/outputs
                        } yield State.Last[InProgress](ctx, value, payouts)
                }
            }

            lazy val commonSteps: List[TransactionBuilderStep] =
                List(setRolloutMetadata, referenceHNS)

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
                                    case s: State.Intermediate[InProgress] =>
                                        s.copy(
                                          txBuilderContext = newCtx,
                                          inputValueNeeded = value,
                                          remainingPayoutObligations = otherObligations,
                                          rolloutOutput = s.rolloutOutput
                                        )
                                    case s: State.Last[InProgress] =>
                                        s.copy(
                                          txBuilderContext = newCtx,
                                          inputValueNeeded = value,
                                          remainingPayoutObligations = otherObligations
                                        )
                                }
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

        object TxBuilder {
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
                      protocolParams = env.protocolParams,
                      diffHandler = ChangeOutputDiffHandler(
                        protocolParams = env.protocolParams,
                        changeOutputIdx = 0
                      ).changeOutputDiffHandler,
                      evaluator = env.evaluator,
                      validators = validators
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
    }
}
