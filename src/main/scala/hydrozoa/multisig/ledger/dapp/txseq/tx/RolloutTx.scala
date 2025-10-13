package hydrozoa.multisig.ledger.dapp.txseq.tx

import hydrozoa.lib.tx.BuildError.{BalancingError, StepError, ValidationError}
import hydrozoa.lib.tx.TransactionBuilderStep.{ModifyAuxiliaryData, ReferenceOutput, Send, Spend}
import hydrozoa.lib.tx.{BuildError, NativeScriptWitness, TransactionBuilder, TransactionBuilderStep, TransactionUnspentOutput}
import hydrozoa.multisig.ledger.DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.utxo.RolloutUtxo
import hydrozoa.multisig.ledger.joint.utxo.Payout
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.txbuilder.{Environment, TxBalancingError}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionOutput as TxOutput
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import scalus.builtin.ByteString
import scalus.cardano.address.Address
import scalus.cardano.ledger.TransactionException.InvalidTransactionSizeException
import scalus.cardano.ledger.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler
import scalus.cardano.ledger.utils.TxBalance

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

        type Result = RolloutTx

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

        type Error = BuildError

        def buildPartial(): Either[Error, PartialResult.NeedsInput] = ???

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

            def spendRollout(value: Value): Spend =
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
                  MD.L1TxTypes.Settlement,
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
                        ???
                    case _Empty =>
                        ???
                }
            }

            def tryAddPayout(
                ctx: TransactionBuilder.Context,
                payoutObligation: Payout.Obligation.L1
            ): Either[Error, (TransactionBuilder.Context, Value)] =
                val payoutStep = Send(payoutObligation.output)
                for {
                    newCtx <- TxBuilder.modify(ctx, List(payoutStep))
                    value = ???

                } yield ???
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

            def finishWithMockInput() = ???
            
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
