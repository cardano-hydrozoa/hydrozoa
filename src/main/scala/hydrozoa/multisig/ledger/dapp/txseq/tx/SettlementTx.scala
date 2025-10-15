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
        override val tx: Transaction,
        mbRolloutProduced: Option[RolloutUtxo]
    ) extends SettlementTx
}

object SettlementTx {
    type FirstRolloutTxPartial = RolloutTx.Builder.PartialResult.FirstOrOnly

    object Builder {
        import State.Fields.*
        import State.Status

        type Error = SomeBuildError

        sealed trait HasFirstRolloutTxPartial {
            def firstRolloutTxPartial: FirstRolloutTxPartial
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
            override val ctx: TransactionBuilder.Context,
            override val absorbedDeposits: Queue[DepositUtxo],
            override val remainingDeposits: Queue[DepositUtxo]
        ) extends HasTxBuilderContext,
              HasDepositsPartition

        object State {
            object Fields {
                sealed trait HasTxBuilderContext {
                    def ctx: TransactionBuilder.Context
                }

                trait HasDepositsPartition {
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

        final case class NoPayouts(
            override val majorVersion: Int,
            override val deposits: Queue[DepositUtxo],
            override val treasuryUtxo: TreasuryUtxo,
            override val headNativeScript: HeadMultisigScript,
            override val headNativeScriptReferenceInput: TransactionUnspentOutput,
            override val env: Environment,
            override val validators: Seq[Validator]
        ) extends Builder {
            override val mbPessimisticSendRollout: None.type = None

            override val pessimisticTreasuryOutputValue: Value = treasuryUtxo.value

            override type BuildResult = Result.NoPayouts

            override def build(): Either[Error, BuildResult] = for {
                progressed <- progress()
                completed <- complete(progressed)
            } yield completed

            override def complete(
                progressed: State[Status.InProgress]
            ): Either[Error, BuildResult] =
                for {
                    finished <- finish(progressed)
                } yield getResult(finished)

            override type FinishResult = State[Status.Finished]

            override def finish(state: State[Status.InProgress]): Either[Error, FinishResult] = {
                import state.*
                for {
                    finished <- TxBuilder.finish(ctx)
                } yield State[Status.Finished](
                  ctx = finished,
                  absorbedDeposits = absorbedDeposits,
                  remainingDeposits = remainingDeposits
                )
            }

            def getResult(finished: State[Status.Finished]): BuildResult = {
                val settlementTx = getSettlementTx(finished)
                Result.NoPayouts(
                  settlementTx = settlementTx,
                  absorbedDeposits = finished.absorbedDeposits,
                  remainingDeposits = finished.remainingDeposits
                )
            }

            /** Given a finished [[State]] for a [[Builder.NoPayouts]], apply post-processing to get
              * the [[SettlementTx.NoPayouts]]. Assumes that:
              *
              *   - The spent treasury utxo is the first input (unchecked).
              *   - The produced treasury utxo is the first output (asserted).
              *
              * @param finished
              *   The [[State]] of a [[Builder.NoPayouts]].
              * @throws AssertionError
              *   when the asserted assumption is broken.
              * @return
              */
            @throws[AssertionError]
            def getSettlementTx(
                finished: State[Status.Finished]
            ): SettlementTx.NoPayouts = {
                SettlementTx.NoPayouts(
                  treasurySpent = treasuryUtxo,
                  treasuryProduced = PostProcess.getTreasuryProduced(finished),
                  depositsSpent = finished.absorbedDeposits,
                  tx = finished.ctx.transaction
                )
            }
        }

        final case class WithPayouts(
            override val majorVersion: Int,
            override val deposits: Queue[DepositUtxo],
            override val firstRolloutTxPartial: FirstRolloutTxPartial,
            override val treasuryUtxo: TreasuryUtxo,
            override val headNativeScript: HeadMultisigScript,
            override val headNativeScriptReferenceInput: TransactionUnspentOutput,
            override val env: Environment,
            override val validators: Seq[Validator]
        ) extends Builder,
              Builder.HasFirstRolloutTxPartial {
            val pessimisticRolloutOutput: TxOutput.Babbage =
                TxOutput.Babbage(
                  address = headNativeScript.mkAddress(env.network),
                  value = firstRolloutTxPartial.inputValueNeeded,
                  datumOption = None,
                  scriptRef = None
                )

            override val mbPessimisticSendRollout: Some[Send] = Some(Send(pessimisticRolloutOutput))

            override val pessimisticTreasuryOutputValue: Value =
                treasuryUtxo.value - firstRolloutTxPartial.inputValueNeeded

            override type BuildResult = Result.WithPayouts

            override def build(): Either[Error, BuildResult] = for {
                progressed <- progress()
                completed <- complete(progressed)
            } yield completed

            override def complete(
                progressed: State[Status.InProgress]
            ): Either[Error, BuildResult] = {
                for {
                    mergeTrial <- finish(progressed)
                    (finished, isFirstRolloutMerged, mbRolloutUtxo) = mergeTrial
                } yield getResult(
                  finished,
                  isFirstRolloutMerged,
                  mbRolloutUtxo
                )
            }

            override type FinishResult =
                (State[Status.Finished], IsFirstRolloutMerged, Option[RolloutUtxo])

            override def finish(state: State[Status.InProgress]): Either[Error, FinishResult] = {
                import state.*
                import IsFirstRolloutMerged.*
                val rolloutTx: Transaction = firstRolloutTxPartial.tx

                val optimisticSteps: List[Send] = rolloutTx.body.value.outputs
                    .map((x: Sized[TransactionOutput]) => Send(x.value))
                    .toList

                val optimisticTrial: Either[Error, TransactionBuilder.Context] = for {
                    newCtx <- TransactionBuilder.modify(ctx, optimisticSteps)
                    finished <- TxBuilder.finish(newCtx)
                } yield finished

                lazy val pessimisticBackup: Either[Error, TransactionBuilder.Context] = for {
                    newCtx <- BasePessimistic.addPessimisticRollout(ctx)
                    finished <- TxBuilder.finish(newCtx)
                } yield finished

                // Keep the optimistic transaction (which merged the settlement tx with the first rollout tx)
                // if it worked out. Otherwise, use the pessimistic transaction.
                for {
                    newCtx <- optimisticTrial.orElse(pessimisticBackup)

                    finishedState = State[Status.Finished](
                      ctx = newCtx,
                      absorbedDeposits = absorbedDeposits,
                      remainingDeposits = remainingDeposits
                    )

                    isFirstRolloutMerged = if optimisticTrial.isRight then Merged else NotMerged

                    mbRolloutOutput =
                        if optimisticTrial.isRight then
                            Some(getPessimisticRolloutUtxo(finishedState))
                        else
                            rolloutTx.body.value.outputs.headOption.map(txOutput =>
                                RolloutUtxo(
                                  TransactionUnspentOutput(
                                    TransactionInput(rolloutTx.id, 0),
                                    txOutput.value
                                  )
                                )
                            )
                } yield (
                  finishedState,
                  isFirstRolloutMerged,
                  mbRolloutOutput
                )
            }

            /** Given a finished [[State]] of a [[Builder.WithPayouts]], apply post-processing to
              * get the [[RolloutUtxo]] produced by the [[SettlementTx.WithPayouts]], if it was
              * produced. Assumes that it the rollout produced is the second output of the
              * transaction.
              *
              * @param finished
              *   The finished builder state.
              * @throws AssertionError
              *   when the assumption is broken.
              * @return
              */
            def getPessimisticRolloutUtxo(finished: State[Status.Finished]): RolloutUtxo = {
                val tx = finished.ctx.transaction
                val outputs = tx.body.value.outputs

                assert(outputs.nonEmpty)
                val outputsTail = outputs.tail

                assert(outputsTail.nonEmpty)
                val rolloutOutput = outputsTail.head.value

                RolloutUtxo(
                  TransactionUnspentOutput(
                    TransactionInput(transactionId = tx.id, index = 1),
                    rolloutOutput
                  )
                )
            }

            /** Given a finished [[State]] for a [[Builder.WithPayouts]], apply post-processing to
              * assemble the [[Result.WithPayouts]]. Assumes that:
              *
              *   - The spent treasury utxo is the first input (unchecked).
              *   - The produced treasury utxo is the first output (asserted).
              *   - The produced rollout utxo is the second output (asserted).
              *
              * @param finished
              *   The finished builder state.
              * @throws AssertionError
              *   when the asserted assumptions are broken.
              * @return
              */
            @throws[AssertionError]
            def getResult(
                finished: State[Status.Finished],
                isFirstRolloutMerged: IsFirstRolloutMerged,
                mbRolloutUtxo: Option[RolloutUtxo]
            ): BuildResult = {
                val settlementTx = getSettlementTx(
                  finished,
                  mbRolloutUtxo
                )

                Result.WithPayouts(
                  settlementTx = settlementTx,
                  absorbedDeposits = finished.absorbedDeposits,
                  remainingDeposits = finished.remainingDeposits,
                  isFirstRolloutMerged = isFirstRolloutMerged
                )
            }

            /** Given a finished [[State]] for a [[Builder.WithPayouts]], apply post-processing to
              * get the [[SettlementTx.WithPayouts]]. Assumes that:
              *
              *   - The spent treasury utxo is the first input (unchecked).
              *   - The produced treasury utxo is the first output (asserted).
              *   - The produced rollout utxo is the second output (asserted).
              *
              * @param finished
              *   The finished builder state.
              * @throws AssertionError
              *   when the asserted assumptions are broken.
              * @return
              */
            @throws[AssertionError]
            def getSettlementTx(
                finished: State[Status.Finished],
                mbRolloutOutput: Option[RolloutUtxo]
            ): SettlementTx.WithPayouts = {
                SettlementTx.WithPayouts(
                  treasurySpent = treasuryUtxo,
                  treasuryProduced = PostProcess.getTreasuryProduced(finished),
                  depositsSpent = finished.absorbedDeposits,
                  mbRolloutProduced = mbRolloutOutput,
                  tx = finished.ctx.transaction
                )
            }
        }
    }

    trait Builder:
        def majorVersion: Int
        def deposits: Queue[DepositUtxo]
        def treasuryUtxo: TreasuryUtxo
        def headNativeScript: HeadMultisigScript
        // The reference script for the HNS should live inside the multisig regime witness UTxO
        def headNativeScriptReferenceInput: TransactionUnspentOutput
        def env: Environment
        def validators: Seq[Validator]

        import Builder.*
        import Builder.State.Status

        type BuildResult
        def build(): Either[Error, BuildResult]

        type FinishResult
        def finish(state: State[Status.InProgress]): Either[Error, FinishResult]

        type SettlementTxResult

        def progress(): Either[Error, State[Status.InProgress]] =
            for {
                pessimistic <- BasePessimistic.basePessimistic
                addedDeposits <- AddDeposits.addDeposits(pessimistic)

            } yield addedDeposits

        def complete(progressed: State[Status.InProgress]): Either[Error, BuildResult]

        def mbPessimisticSendRollout: Option[Send]

        def pessimisticTreasuryOutputValue: Value

        object BasePessimistic {
            lazy val basePessimistic: Either[Error, State[Status.InProgress]] = for {
                ctx <- TransactionBuilder.build(env.network, basePessimisticSteps)
                addedPessimisticRollout <- addPessimisticRollout(ctx)
                _ <- TxBuilder.finish(addedPessimisticRollout)
            } yield State[Status.InProgress](
              ctx = ctx,
              absorbedDeposits = Queue.empty,
              remainingDeposits = deposits
            )

            lazy val basePessimisticSteps: List[TransactionBuilderStep] =
                List(setSettlementMetadata, referenceHNS, consumeTreasury, sendTreasury)

            def addPessimisticRollout(
                ctx: TransactionBuilder.Context
            ): Either[Error, TransactionBuilder.Context] = {
                val extraStep = mbPessimisticSendRollout.toList
                for { newCtx <- TransactionBuilder.modify(ctx, extraStep) } yield newCtx
            }

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
                  value = pessimisticTreasuryOutputValue,
                  datumOption = Some(Inline(treasuryOutputDatum.toData)),
                  scriptRef = None
                )

            lazy val treasuryOutputDatum: TreasuryUtxo.Datum =
                mkMultisigTreasuryDatum(majorVersion, ByteString.empty)
        }

        object AddDeposits {
            @tailrec
            def addDeposits(
                state: State[Status.InProgress]
            ): Either[Error, State[Status.InProgress]] = {
                import state.*
                remainingDeposits match {
                    case deposit +: otherDeposits =>
                        tryAddDeposit(ctx, deposit) match {
                            case Right(x) =>
                                val newState: State[Status.InProgress] = state.copy(
                                  ctx = x,
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

        object PostProcess {

            /** Given a finished [[State]] of a [[Builder]], apply post-processing to get the
              * [[TreasuryUtxo]] produced by the [[SettlementTx]]. Assumes that the treasury output
              * is present in the transaction and is the first output.
              *
              * @param finished
              *   The finished [[State]]
              * @throws AssertionError
              *   when the assumption is broken.
              * @return
              */
            @throws[AssertionError]
            def getTreasuryProduced(
                finished: State[Status.Finished]
            ): TreasuryUtxo = {
                val tx = finished.ctx.transaction
                val outputs = tx.body.value.outputs

                assert(outputs.nonEmpty)
                val treasuryOutput = outputs.head.value

                treasuryUtxo.copy(
                  txId = TransactionInput(transactionId = tx.id, index = 0),
                  datum = BasePessimistic.treasuryOutputDatum,
                  value = treasuryOutput.value
                )
            }
        }
}
