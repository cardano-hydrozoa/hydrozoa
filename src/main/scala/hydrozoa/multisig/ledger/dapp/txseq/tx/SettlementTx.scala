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
import hydrozoa.multisig.ledger.dapp.txseq.RolloutTxSeq.Builder.PartialResult as RolloutTxSeqPartial
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

    case WithOnlyDirectPayouts(
        override val treasurySpent: TreasuryUtxo,
        override val treasuryProduced: TreasuryUtxo,
        override val depositsSpent: Queue[DepositUtxo],
        override val tx: Transaction
    ) extends SettlementTx

    case WithRollouts(
        override val treasurySpent: TreasuryUtxo,
        override val treasuryProduced: TreasuryUtxo,
        override val depositsSpent: Queue[DepositUtxo],
        override val tx: Transaction,
        rolloutProduced: RolloutUtxo
    ) extends SettlementTx
}

object SettlementTx {
    type NoRollouts = WithOnlyDirectPayouts | NoPayouts
    type WithPayouts = WithOnlyDirectPayouts | WithRollouts

    object Builder {
        import State.Fields.*
        import State.Status

        type Error = SomeBuildError

        sealed trait HasRolloutTxSeqPartial {
            def rolloutTxSeqPartial: RolloutTxSeqPartial
        }

        enum Result extends State.Fields.HasDepositsPartition:
            def settlementTx: SettlementTx
            case NoPayouts(
                override val settlementTx: SettlementTx.NoPayouts,
                override val absorbedDeposits: Queue[DepositUtxo],
                override val remainingDeposits: Queue[DepositUtxo]
            )

            case WithOnlyDirectPayouts(
                override val settlementTx: SettlementTx.WithOnlyDirectPayouts,
                override val absorbedDeposits: Queue[DepositUtxo],
                override val remainingDeposits: Queue[DepositUtxo]
            )

            case WithRollouts(
                override val settlementTx: SettlementTx.WithRollouts,
                override val absorbedDeposits: Queue[DepositUtxo],
                override val remainingDeposits: Queue[DepositUtxo],
                rolloutTxSeqPartial: RolloutTxSeqPartial
            )

        object Result {
            type WithPayouts = WithOnlyDirectPayouts | WithRollouts
        }

        enum MergeStatus:
            case NotMerged
            case Merged(mbRolloutTxSeqPartialSkipped: Option[RolloutTxSeqPartial.SkipFirst])

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

            override type SettlementTxType = SettlementTx.NoPayouts

            override type BuildType = Result.NoPayouts

            override def complete(
                progressed: State[Status.InProgress]
            ): Either[Error, BuildType] =
                for {
                    finished <- finish(progressed)
                } yield getResult(finished)

            override type FinishType = State[Status.Finished]

            override def finish(state: State[Status.InProgress]): Either[Error, FinishType] = {
                import state.*
                for {
                    finished <- TxBuilder.finish(ctx)
                } yield State[Status.Finished](
                  ctx = finished,
                  absorbedDeposits = absorbedDeposits,
                  remainingDeposits = remainingDeposits
                )
            }

            override type GetResultType = (finished: State[Status.Finished]) => BuildType

            override def getResult: GetResultType = (finished: State[Status.Finished]) => {
                val settlementTx = getSettlementTx(finished)
                Result.NoPayouts(
                  settlementTx = settlementTx,
                  absorbedDeposits = finished.absorbedDeposits,
                  remainingDeposits = finished.remainingDeposits
                )
            }

            override type GetSettlementTxType =
                (finished: State[Status.Finished]) => SettlementTx.NoPayouts

            /** Given a finished [[State]] for a [[Builder.NoPayouts]], apply post-processing to get
              * the [[SettlementTx.NoPayouts]]. Assumes that:
              *
              *   - The spent treasury utxo is the first input (unchecked).
              *   - The produced treasury utxo is the first output (asserted).
              *
              * @throws AssertionError
              *   when the asserted assumption is broken.
              * @return
              */
            @throws[AssertionError]
            override def getSettlementTx: GetSettlementTxType =
                (finished: State[Status.Finished]) => {
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
            override val treasuryUtxo: TreasuryUtxo,
            override val headNativeScript: HeadMultisigScript,
            override val headNativeScriptReferenceInput: TransactionUnspentOutput,
            override val env: Environment,
            override val validators: Seq[Validator],
            override val rolloutTxSeqPartial: RolloutTxSeqPartial
        ) extends Builder,
              HasRolloutTxSeqPartial {
            val firstRolloutTxPartial: RolloutTx.Builder.PartialResult.FirstOrOnly =
                rolloutTxSeqPartial.firstOrOnly

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

            override type SettlementTxType = SettlementTx.WithPayouts

            override type BuildType = Result.WithPayouts

            override def complete(
                progressed: State[Status.InProgress]
            ): Either[Error, BuildType] = {
                for {
                    mergeTrial <- finish(progressed)
                    (finished, mergeStatus) = mergeTrial
                } yield getResult(finished, mergeStatus)
            }

            override type FinishType =
                (State[Status.Finished], MergeStatus)

            override def finish(state: State[Status.InProgress]): Either[Error, FinishType] = {
                import state.*
                import MergeStatus.*
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

                    mergeStatus =
                        if optimisticTrial.isLeft then NotMerged
                        else Merged(rolloutTxSeqPartial.skipFirst)

                } yield (finishedState, mergeStatus)
            }

            override type GetResultType = (
                finished: State[Status.Finished],
                mergeStatus: MergeStatus
            ) => BuildType

            /** Given a finished [[State]] for a [[Builder.WithPayouts]], apply post-processing to
              * assemble the [[Result.WithRollouts]]. Assumes that:
              *
              *   - The spent treasury utxo is the first input (unchecked).
              *   - The produced treasury utxo is the first output (asserted).
              *   - The produced rollout utxo is the second output (asserted).
              *
              * @throws AssertionError
              *   when the asserted assumptions are broken.
              * @return
              */
            @throws[AssertionError]
            override def getResult: GetResultType = (
                finished: State[Status.Finished],
                mergeStatus: MergeStatus
            ) => {
                getSettlementTx(finished, mergeStatus) match {
                    case Left(tx) =>
                        Result.WithOnlyDirectPayouts(
                          settlementTx = tx,
                          absorbedDeposits = finished.absorbedDeposits,
                          remainingDeposits = finished.remainingDeposits
                        )
                    case Right((tx, rollouts)) =>
                        Result.WithRollouts(
                          settlementTx = tx,
                          absorbedDeposits = finished.absorbedDeposits,
                          remainingDeposits = finished.remainingDeposits,
                          rolloutTxSeqPartial = rollouts
                        )
                }
            }

            override type GetSettlementTxType = (
                finished: State[Status.Finished],
                mergeStatus: MergeStatus
            ) => Either[
              SettlementTx.WithOnlyDirectPayouts,
              (SettlementTx.WithRollouts, RolloutTxSeqPartial)
            ]

            /** Given a finished [[State]] for a [[Builder.WithPayouts]], apply post-processing to
              * get the [[SettlementTx.WithRollouts]]. Assumes that:
              *
              *   - The spent treasury utxo is the first input (unchecked).
              *   - The produced treasury utxo is the first output (asserted).
              *   - The produced rollout utxo is the second output (asserted).
              *
              * @throws AssertionError
              *   when the asserted assumptions are broken.
              * @return
              */
            @throws[AssertionError]
            override def getSettlementTx: GetSettlementTxType = (
                finished: State[Status.Finished],
                mergeStatus: MergeStatus
            ) => {
                import MergeStatus.*
                val tx = finished.ctx.transaction

                lazy val withRollouts: SettlementTx.WithRollouts = SettlementTx.WithRollouts(
                  treasurySpent = treasuryUtxo,
                  treasuryProduced = PostProcess.getTreasuryProduced(finished),
                  depositsSpent = finished.absorbedDeposits,
                  rolloutProduced = unsafeGetRolloutProduced(finished),
                  tx = tx
                )

                lazy val withOnlyDirectPayouts: SettlementTx.WithOnlyDirectPayouts =
                    SettlementTx.WithOnlyDirectPayouts(
                      treasurySpent = treasuryUtxo,
                      treasuryProduced = PostProcess.getTreasuryProduced(finished),
                      depositsSpent = finished.absorbedDeposits,
                      tx = tx
                    )

                mergeStatus match {
                    case NotMerged => Right((withRollouts, rolloutTxSeqPartial))
                    case Merged(mbFirstSkipped) =>
                        mbFirstSkipped match {
                            case None => Left(withOnlyDirectPayouts)
                            case Some(firstSkipped) =>
                                Right((withRollouts, firstSkipped.partialResult))
                        }
                }
            }

            /** Given a finished [[State]] of a [[Builder.WithPayouts]], apply post-processing to
              * get the [[RolloutUtxo]] produced by the [[SettlementTx.WithRollouts]], if it was
              * produced. Assumes that the rollout produced is the second output of the transaction.
              *
              * @param finished
              *   The finished builder state.
              * @throws AssertionError
              *   when the assumption is broken.
              * @return
              */
            private def unsafeGetRolloutProduced(finished: State[Status.Finished]): RolloutUtxo = {
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

        type BuildType
        def build(): Either[Error, BuildType] = for {
            progressed <- progress()
            completed <- complete(progressed)
        } yield completed

        def complete(progressed: State[Status.InProgress]): Either[Error, BuildType]

        type FinishType
        def finish(state: State[Status.InProgress]): Either[Error, FinishType]

        type SettlementTxType

        type GetResultType
        def getResult: GetResultType

        type GetSettlementTxType
        def getSettlementTx: GetSettlementTxType

        def progress(): Either[Error, State[Status.InProgress]] =
            for {
                pessimistic <- BasePessimistic.basePessimistic
                addedDeposits <- AddDeposits.addDeposits(pessimistic)

            } yield addedDeposits

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
