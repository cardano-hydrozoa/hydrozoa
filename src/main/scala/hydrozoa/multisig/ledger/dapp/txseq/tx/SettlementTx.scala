package hydrozoa.multisig.ledger.dapp.txseq.tx

import hydrozoa.lib.tx.SomeBuildError.ValidationError
import hydrozoa.lib.tx.TransactionBuilderStep.*
import hydrozoa.lib.tx.{
    SomeBuildError,
    TransactionBuilder,
    TransactionBuilderStep,
    TransactionUnspentOutput
}
import hydrozoa.multisig.ledger.DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo.mkMultisigTreasuryDatum
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, RolloutUtxo, TreasuryUtxo}
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionException.InvalidTransactionSizeException
import scalus.cardano.ledger.TransactionOutput as TxOutput
import scalus.cardano.ledger.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler
import scalus.cardano.ledger.txbuilder.Environment
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.STS.Validator

import scala.annotation.tailrec
import scala.collection.immutable.Queue

enum SettlementTx extends Tx {
    def treasurySpent: TreasuryUtxo
    def treasuryProduced: TreasuryUtxo
    def depositsSpent: Queue[DepositUtxo]
    override def tx: Transaction

    case NoPayouts(
        override val treasurySpent: TreasuryUtxo,
        override val treasuryProduced: TreasuryUtxo,
        override val depositsSpent: Queue[DepositUtxo],
        override val tx: Transaction
    ) extends SettlementTx

    case WithPayouts(
        override val treasurySpent: TreasuryUtxo,
        override val treasuryProduced: TreasuryUtxo,
        override val depositsSpent: Queue[DepositUtxo],
        // TODO: Might be an option
        override val rolloutProduced: RolloutUtxo,
        override val tx: Transaction
    ) extends SettlementTx, RolloutTx.HasRolloutProduced

    def mbRolloutProduced: Option[RolloutUtxo] = this match {
        case thisBuilder: WithPayouts =>
            Some(thisBuilder.rolloutProduced)
        case _ => None
    }
}

object SettlementTx {
    object Builder {
        import State.Fields.*

        type Error = SomeBuildError | RolloutFeesWithoutRolloutOutputError.type

        // TODO: Obsolete?
        case object RolloutFeesWithoutRolloutOutputError

        sealed trait HasFirstRolloutTxPartial {
            def firstRolloutTxPartial: RolloutTx.Builder.State.NeedsInput.FirstOrOnly
        }

        enum Result extends State.Fields.HasDepositsPartition:
            def settlementTx: SettlementTx
            case NoPayouts(
                override val settlementTx: SettlementTx.NoPayouts,
                override val absorbedDeposits: Queue[DepositUtxo],
                override val remainingDeposits: Queue[DepositUtxo]
            )

            case WithPayouts(
                override val settlementTx: SettlementTx.WithPayouts,
                override val absorbedDeposits: Queue[DepositUtxo],
                override val remainingDeposits: Queue[DepositUtxo],
                isFirstRolloutMerged: IsFirstRolloutMerged
            )

        enum IsFirstRolloutMerged:
            case Merged, NotMerged

        final case class State[Status <: State.Status](
            override val txBuilderContext: TransactionBuilder.Context,
            override val absorbedDeposits: Queue[DepositUtxo],
            override val remainingDeposits: Queue[DepositUtxo]
        ) extends HasTxBuilderContext,
              HasDepositsPartition

        object State {
            object Fields {
                sealed trait HasTxBuilderContext {
                    def txBuilderContext: TransactionBuilder.Context
                }

                sealed trait HasDepositsPartition {
                    def absorbedDeposits: Queue[DepositUtxo]
                    def remainingDeposits: Queue[DepositUtxo]
                }
            }

            type Status = Status.InProgress | Status.Finished

            object Status {
                type InProgress
                type Finished
            }

        }
    }

    enum Builder:
        def majorVersion: Int
        def deposits: Queue[DepositUtxo]
        def treasuryUtxo: TreasuryUtxo
        def headNativeScript: HeadMultisigScript
        // The reference script for the HNS should live inside the multisig regime witness UTxO
        def headNativeScriptReferenceInput: TransactionUnspentOutput
        def env: Environment
        def validators: Seq[Validator]

        case NoPayouts(
            override val majorVersion: Int,
            override val deposits: Queue[DepositUtxo],
            override val treasuryUtxo: TreasuryUtxo,
            override val headNativeScript: HeadMultisigScript,
            override val headNativeScriptReferenceInput: TransactionUnspentOutput,
            override val env: Environment,
            override val validators: Seq[Validator]
        ) extends Builder

        case WithPayouts(
            override val majorVersion: Int,
            override val deposits: Queue[DepositUtxo],
            override val firstRolloutTxPartial: RolloutTx.Builder.State.NeedsInput.FirstOrOnly,
            override val treasuryUtxo: TreasuryUtxo,
            override val headNativeScript: HeadMultisigScript,
            override val headNativeScriptReferenceInput: TransactionUnspentOutput,
            override val env: Environment,
            override val validators: Seq[Validator]
        ) extends Builder, Builder.HasFirstRolloutTxPartial

        import Builder.*
        import Builder.State.Status.*

        def build(): Either[Error, Result] = for {
            pessimistic <- BasePessimistic.basePessimistic
            addedDeposits <- AddDeposits.addDeposits(pessimistic)
            result <- this match {
                case thisBuilder: Builder.WithPayouts =>
                    for {
                        mergeTrial <- Finish.tryMergeFirstRolloutTx(addedDeposits, thisBuilder)
                        (finished, isFirstRolloutMerged) = mergeTrial
                        settlementTx = PostProcess.WithPayouts.getSettlementTx(
                          finished,
                          thisBuilder
                        )
                    } yield Result.WithPayouts(
                      settlementTx = settlementTx,
                      absorbedDeposits = finished.absorbedDeposits,
                      remainingDeposits = finished.remainingDeposits,
                      isFirstRolloutMerged = isFirstRolloutMerged
                    )
                case thisBuilder: Builder.NoPayouts =>
                    for {
                        finished <- Finish.noPayouts(addedDeposits, thisBuilder)
                        settlementTx = PostProcess.NoPayouts.getSettlementTx(finished, thisBuilder)
                    } yield Result.NoPayouts(
                      settlementTx = settlementTx,
                      absorbedDeposits = finished.absorbedDeposits,
                      remainingDeposits = finished.remainingDeposits
                    )
            }
        } yield result

        object BasePessimistic {
            lazy val basePessimistic: Either[Error, State[InProgress]] = for {
                ctx <- TransactionBuilder.build(env.network, basePessimisticSteps)
                addedPessimisticRollout <- addPessimisticRollout(ctx)
                _ <- TxBuilder.finish(addedPessimisticRollout)
            } yield State[InProgress](
              txBuilderContext = ctx,
              absorbedDeposits = Queue.empty,
              remainingDeposits = deposits
            )

            lazy val basePessimisticSteps: List[TransactionBuilderStep] =
                List(setSettlementMetadata, referenceHNS, consumeTreasury, sendTreasury)

            def addPessimisticRollout(
                ctx: TransactionBuilder.Context
            ): Either[Error, TransactionBuilder.Context] = {
                val extraStep = BasePessimistic.mbPessimisticSendRollout.toList
                for { newCtx <- TransactionBuilder.modify(ctx, extraStep) } yield newCtx
            }

            lazy val mbPessimisticSendRollout: Option[Send] = Builder.this match {
                case thisBuilder: Builder.WithPayouts =>
                    Some(Send(pessimisticRolloutOutput(thisBuilder)))
                case _ => None
            }

            def pessimisticRolloutOutput(thisBuilder: Builder.WithPayouts): TxOutput.Babbage =
                TxOutput.Babbage(
                  address = headNativeScript.mkAddress(env.network),
                  value = thisBuilder.firstRolloutTxPartial.inputValueNeeded,
                  datumOption = None,
                  scriptRef = None
                )

            lazy val setSettlementMetadata =
                ModifyAuxiliaryData(_ => Some(settlementTxMetadata))

            lazy val settlementTxMetadata: AuxiliaryData =
                MD(
                  MD.L1TxTypes.Settlement,
                  headAddress = headNativeScript.mkAddress(env.network)
                )

            lazy val referenceHNS = ReferenceOutput(headNativeScriptReferenceInput)

            lazy val consumeTreasury: Spend =
                Spend(treasuryUtxo.asUtxo, headNativeScript.witness)

            lazy val sendTreasury: Send = Send(treasuryOutput)

            lazy val treasuryOutput: TxOutput.Babbage =
                TxOutput.Babbage(
                  address = headNativeScript.mkAddress(env.network),
                  value = treasuryUtxo.value,
                  datumOption = Some(Inline(treasuryOutputDatum.toData)),
                  scriptRef = None
                )

            lazy val treasuryOutputValue: Value = Builder.this match {
                case thisBuilder: Builder.WithPayouts =>
                    treasuryUtxo.value - thisBuilder.firstRolloutTxPartial.inputValueNeeded
                case _ => treasuryUtxo.value
            }

            lazy val treasuryOutputDatum: TreasuryUtxo.Datum =
                mkMultisigTreasuryDatum(majorVersion, ByteString.empty)
        }

        object AddDeposits {
            @tailrec
            def addDeposits(state: State[InProgress]): Either[Error, State[InProgress]] = {
                import state.*
                remainingDeposits match {
                    case deposit +: otherDeposits =>
                        tryAddDeposit(txBuilderContext, deposit) match {
                            case Right(x) =>
                                val newState: State[InProgress] = state.copy(
                                  txBuilderContext = x,
                                  absorbedDeposits = absorbedDeposits :+ deposit,
                                  remainingDeposits = otherDeposits
                                )
                                addDeposits(newState)
                            case Left(err) =>
                                TxBuilder.replaceInvalidSizeException(err, state)
                        }
                    case _Empty => Right(state)
                }
            }

            def tryAddDeposit(
                ctx: TransactionBuilder.Context,
                deposit: DepositUtxo
            ): Either[Error, TransactionBuilder.Context] =
                val depositStep = Spend(TransactionUnspentOutput(deposit.toUtxo))
                for {
                    newCtx <- TransactionBuilder.modify(ctx, List(depositStep))
                    // TODO: update the non-ADA assets in the treasury output, based on the absorbed deposits
                    //
                    // Ensure that at least the pessimistic rollout output fits into the transaction.
                    addedPessimisticRollout <- BasePessimistic.addPessimisticRollout(newCtx)
                    _ <- TxBuilder.finish(addedPessimisticRollout)
                } yield newCtx
        }

        object Finish {
            def noPayouts(
                state: State[InProgress],
                _thisBuilder: Builder.NoPayouts
            ): Either[Error, State[Finished]] = {
                import state.*
                for {
                    finished <- TxBuilder.finish(txBuilderContext)
                } yield State[Finished](
                  txBuilderContext = finished,
                  absorbedDeposits = absorbedDeposits,
                  remainingDeposits = remainingDeposits
                )
            }

            def tryMergeFirstRolloutTx(
                state: State[InProgress],
                _this: Builder.WithPayouts
            ): Either[Error, (State[Finished], IsFirstRolloutMerged)] = {
                import state.*
                import IsFirstRolloutMerged.*
                val rolloutTx: Transaction =
                    _this.firstRolloutTxPartial.txBuilderContext.transaction

                val optimisticSteps: List[Send] = rolloutTx.body.value.outputs
                    .map((x: Sized[TransactionOutput]) => Send(x.value))
                    .toList

                val optimisticTrial: Either[Error, TransactionBuilder.Context] = for {
                    newCtx <- TransactionBuilder.modify(txBuilderContext, optimisticSteps)
                    finished <- TxBuilder.finish(newCtx)
                } yield finished

                lazy val pessimisticBackup: Either[Error, TransactionBuilder.Context] = for {
                    newCtx <- BasePessimistic.addPessimisticRollout(txBuilderContext)
                    finished <- TxBuilder.finish(newCtx)
                } yield finished

                // Keep the optimistic transaction (which merged the settlement tx with the first rollout tx)
                // if it worked out. Otherwise, use the pessimistic transaction.
                for {
                    newCtx <- optimisticTrial.orElse(pessimisticBackup)
                    isFirstRolloutMerged = if optimisticTrial.isRight then Merged else NotMerged
                } yield (
                  State[Finished](
                    txBuilderContext = newCtx,
                    absorbedDeposits = absorbedDeposits,
                    remainingDeposits = remainingDeposits
                  ),
                  isFirstRolloutMerged
                )
            }
        }

        object PostProcess {
            object NoPayouts {

                /** Given a finished [[State]] for a [[Builder.NoPayouts]], apply post-processing to
                  * get the [[SettlementTx.NoPayouts]].
                  *
                  * @param state
                  *   The [[State]] of a [[Builder.NoPayouts]].
                  * @param thisBuilder
                  *   Proof that this builder is a [[Builder.NoPayouts]].
                  * @return
                  */
                def getSettlementTx(
                    state: State[Finished],
                    thisBuilder: Builder.NoPayouts
                ): SettlementTx.NoPayouts = {
                    SettlementTx.NoPayouts(
                      treasurySpent = treasuryUtxo,
                      treasuryProduced = getTreasuryProduced(state),
                      depositsSpent = state.absorbedDeposits,
                      tx = state.txBuilderContext.transaction
                    )
                }
            }

            object WithPayouts {

                /** Given a finished [[State]] for a [[Builder.WithPayouts]], apply post-processing
                  * to get the [[SettlementTx.WithPayouts]].
                  *
                  * @param state
                  *   The [[State[Finished]]] of this [[Builder.WithPayouts]].
                  * @param thisBuilder
                  *   Proof that this builder is a [[Builder.WithPayouts]].
                  * @return
                  */
                def getSettlementTx(
                    state: State[Finished],
                    thisBuilder: Builder.WithPayouts
                ): SettlementTx.WithPayouts = {
                    SettlementTx.WithPayouts(
                      treasurySpent = treasuryUtxo,
                      treasuryProduced = getTreasuryProduced(state),
                      depositsSpent = state.absorbedDeposits,
                      rolloutProduced = getRolloutUtxo(state, thisBuilder),
                      tx = state.txBuilderContext.transaction
                    )
                }

                /** Given a finished [[State]] of a [[Builder.WithPayouts]], apply post-processing
                  * to get the [[RolloutUtxo]] produced by the [[SettlementTx.WithPayouts]].
                  *
                  * @param state
                  *   The finished [[State]] of this [[Builder.WithPayouts]].
                  * @param thisBuilder
                  *   Proof that this builder is a [[Builder.WithPayouts]].
                  * @return
                  */
                def getRolloutUtxo(
                    state: State[Finished],
                    thisBuilder: Builder.WithPayouts
                ): RolloutUtxo = {
                    val tx = state.txBuilderContext.transaction
                    assert(tx.body.value.outputs.length >= 2)

                    val rolloutOutput = tx.body.value.outputs.tail.head.value

                    RolloutUtxo((TransactionInput(transactionId = tx.id, index = 1), rolloutOutput))
                }
            }

            /** Given a finished [[State]] of a [[Builder]], apply post-processing to get the
              * [[TreasuryUtxo]] produced by the [[SettlementTx]].
              *
              * @param state
              *   The finished [[State]] of this [[Builder]].
              * @return
              */
            def getTreasuryProduced(state: State[Finished]): TreasuryUtxo = {
                val tx = state.txBuilderContext.transaction
                val treasuryOutput = tx.body.value.outputs.head.value

                treasuryUtxo.copy(
                  txId = TransactionInput(transactionId = tx.id, index = 0),
                  datum = BasePessimistic.treasuryOutputDatum,
                  value = treasuryOutput.value
                )
            }
        }

        object TxBuilder {
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
