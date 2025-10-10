package hydrozoa.multisig.ledger.dapp.txseq.tx

import hydrozoa.lib.tx.BuildError.{BalancingError, StepError, ValidationError}
import hydrozoa.lib.tx.TransactionBuilderStep.*
import hydrozoa.lib.tx.{
    BuildError,
    TransactionBuilder,
    TransactionBuilderStep,
    TransactionUnspentOutput
}
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.{SettlementTx, Metadata as MD}
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo.mkMultisigTreasuryDatum
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, RolloutUtxo, TreasuryUtxo}
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionException.InvalidTransactionSizeException
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler
import scalus.cardano.ledger.txbuilder.{TxBalancingError, BuilderContext as ScalusBuilderContext}
import scalus.cardano.ledger.*

import scala.annotation.tailrec

object SettlementTx {
    object Builder {
        final case class Result(
            settlementTx: SettlementTx,
            override val absorbedDeposits: List[DepositUtxo],
            override val remainingDeposits: List[DepositUtxo]
        ) extends HasDepositsPartition

        trait HasDepositsPartition {
            def absorbedDeposits: List[DepositUtxo]
            def remainingDeposits: List[DepositUtxo]
        }

        trait HasTxBuilderContext {
            def txBuilderContext: TransactionBuilder.Context
        }

        trait HasMbRolloutOutput {
            def mbRolloutOutput: Option[Babbage]
        }

        final case class BasePessimistic(
            override val txBuilderContext: TransactionBuilder.Context,
            override val mbRolloutOutput: Option[Babbage]
        ) extends HasTxBuilderContext,
              HasMbRolloutOutput

        final case class AddedDeposits(
            override val absorbedDeposits: List[DepositUtxo],
            override val remainingDeposits: List[DepositUtxo],
            override val txBuilderContext: TransactionBuilder.Context,
            override val mbRolloutOutput: Option[Babbage]
        ) extends HasTxBuilderContext,
              HasMbRolloutOutput,
              HasDepositsPartition

        final case class AddedPayouts(
            remainingPayouts: List[Babbage],
            override val txBuilderContext: TransactionBuilder.Context,
            override val mbRolloutOutput: Option[Babbage]
        ) extends HasTxBuilderContext,
              HasMbRolloutOutput

        object AddedDeposits {

            /** Convert from [[BasePessimistic]], providing a list of deposits to spend.
              * @param state
              *   The base pessimistic state to which we want to add deposits.
              * @param deposits
              *   The deposits to add to the state.
              * @return
              */
            def apply(state: BasePessimistic, deposits: List[DepositUtxo]): AddedDeposits = {
                import state.*
                AddedDeposits(
                  absorbedDeposits = List.empty,
                  remainingDeposits = deposits,
                  txBuilderContext = txBuilderContext,
                  mbRolloutOutput = mbRolloutOutput
                )
            }
        }

        object AddedPayouts {

            /** Convert from [[AddedDeposits]], providing a list of payouts to send.
              * @param state
              *   The base pessimistic state to which we want to add deposits.
              * @param payouts
              *   The payouts to add to the state.
              * @return
              */
            def apply(state: AddedDeposits, payouts: List[Babbage]): AddedPayouts = {
                import state.*
                AddedPayouts(
                  remainingPayouts = payouts,
                  txBuilderContext = txBuilderContext,
                  mbRolloutOutput = mbRolloutOutput
                )
            }
        }
    }

    final case class Builder(
        majorVersion: Int,
        deposits: List[DepositUtxo],
        payouts: List[Babbage],
        treasuryUtxo: TreasuryUtxo,
        rolloutTokenName: AssetName,
        headNativeScript: HeadMultisigScript,
        // The reference script for the HNS should live inside the multisig regime witness UTxO
        headNativeScriptReferenceInput: TransactionUnspentOutput,
        context: ScalusBuilderContext
    ) {
        import Builder.*
        import BuilderUtils.*

        type Error = BuildError | RolloutFeesWithoutRolloutOutputError.type

        case object RolloutFeesWithoutRolloutOutputError

        object PartialResult {
            def needsRolloutFees: Either[Error, PartialResult.NeedsRolloutFees] = {
                for {
                    pessimistic <- basePessimistic
                    addedDeposits <- addDeposits(pessimistic)
                    addedPayouts <- addPayouts(addedDeposits)
                    partialResult = NeedsRolloutFees(addedDeposits, addedPayouts)
                } yield partialResult
            }

            final case class NeedsRolloutFees(
                override val txBuilderContext: TransactionBuilder.Context,
                override val mbRolloutOutput: Option[Babbage],
                override val absorbedDeposits: List[DepositUtxo],
                override val remainingDeposits: List[DepositUtxo]
            ) extends HasTxBuilderContext,
                  HasMbRolloutOutput,
                  HasDepositsPartition {
                def complete(rolloutFees: Coin): Either[Error, Result] =
                    addRolloutFees(this, rolloutFees)
            }

            object NeedsRolloutFees {

                /** Combine the [[AddedDeposits]] and [[AddedPayouts]] into a [[PartialResult]] that
                  * only needs the rollout transactions' cumulative fees to be turned into a
                  * [[Result]].
                  * @param addedDeposits
                  *   The [[AddedDeposits]] state after adding all deposit inputs that fit into the
                  *   settlement tx.
                  * @param addedPayouts
                  *   The [[AddedPayouts]] state after adding all payout outputs that fit into the
                  *   settlement tx.
                  * @return
                  */
                def apply(
                    addedDeposits: AddedDeposits,
                    addedPayouts: AddedPayouts
                ): PartialResult.NeedsRolloutFees =
                    PartialResult.NeedsRolloutFees(
                      txBuilderContext = addedPayouts.txBuilderContext,
                      mbRolloutOutput = addedPayouts.mbRolloutOutput,
                      absorbedDeposits = addedDeposits.absorbedDeposits,
                      remainingDeposits = addedDeposits.remainingDeposits
                    )
            }
        }

        def basePessimistic: Either[Error, BasePessimistic] =
            import BuilderUtils.*
            import BuilderUtils.PessimisticBase.*

            for {
                txBuilderContext <- TxBuilder.build(pessimisticSteps)
                _ <- TxBuilder.finalizeTxBuilderContext(txBuilderContext, mbRolloutOutput)
            } yield BasePessimistic(
              txBuilderContext = txBuilderContext,
              mbRolloutOutput = mbRolloutOutput
            )

        def addDeposits(state: BasePessimistic): Either[Error, AddedDeposits] =
            import BuilderUtils.AddDeposits.*
            val newState = AddedDeposits(state, deposits)
            loop(newState)

        def addPayouts(state: AddedDeposits): Either[Error, AddedPayouts] =
            import BuilderUtils.AddPayouts.*
            val newState = AddedPayouts(state, payouts)
            loop(newState)

        def addRolloutFees(
            state: PartialResult.NeedsRolloutFees,
            rolloutFees: Coin
        ): Either[Error, Result] =
            import BuilderUtils.*
            import state.*
            for {
                newMbRolloutOutput <- NeedsRolloutFees.updateMbRolloutOutput(
                  mbRolloutOutput,
                  rolloutFees
                )

                newState = state.copy(mbRolloutOutput = newMbRolloutOutput)

                settlementTx: SettlementTx <- PostProcess.finalizeStateAndGetSettlementTx(newState)
            } yield Result(
              settlementTx = settlementTx,
              absorbedDeposits = absorbedDeposits,
              remainingDeposits = remainingDeposits
            )

        object BuilderUtils {
            object PessimisticBase {
                lazy val mbRolloutOutput: Option[Babbage] = {
                    val valueRollout = payouts.map(_.value).foldLeft(Value.zero)(_ + _)
                    if valueRollout.isZero then None
                    else
                        Some(
                          Babbage(
                            headNativeScript.mkAddress(context.network),
                            valueRollout,
                            None,
                            None
                          )
                        )
                }

                val newTreasuryDatum: TreasuryUtxo.Datum =
                    mkMultisigTreasuryDatum(majorVersion, ByteString.empty)

                lazy val treasuryOutput: Babbage = {
                    val newTreasuryVal =
                        mbRolloutOutput.fold(treasuryUtxo.value)(treasuryUtxo.value - _.value)

                    Babbage(
                      address = headNativeScript.mkAddress(context.network),
                      value = newTreasuryVal,
                      datumOption = Some(Inline(newTreasuryDatum.toData)),
                      scriptRef = None
                    )
                }

                lazy val settlementTxMetadata: AuxiliaryData =
                    MD(
                      MD.L1TxTypes.Settlement,
                      headAddress = headNativeScript.mkAddress(context.network)
                    )

                lazy val referenceHNS = ReferenceOutput(headNativeScriptReferenceInput)

                lazy val consumeTreasury: Spend =
                    Spend(treasuryUtxo.asUtxo, headNativeScript.witness)

                lazy val sendTreasury: Send = Send(treasuryOutput)

                lazy val setSettlementMetadata =
                    ModifyAuxiliaryData(_ => Some(settlementTxMetadata))

                lazy val pessimisticSteps =
                    List(referenceHNS, consumeTreasury, sendTreasury, setSettlementMetadata)
            }

            object AddDeposits {
                @tailrec
                def loop(state: AddedDeposits): Either[Error, AddedDeposits] = {
                    import state.*
                    remainingDeposits match {
                        case deposit :: otherDeposits =>
                            tryAddDeposit(txBuilderContext, mbRolloutOutput, deposit) match {
                                case Right(x) =>
                                    val newState = state.copy(
                                      txBuilderContext = x,
                                      absorbedDeposits = absorbedDeposits :+ deposit,
                                      remainingDeposits = otherDeposits
                                    )
                                    loop(newState)
                                case Left(err) =>
                                    TxBuilder.replaceInvalidSizeException(err, state)
                            }
                        case Nil => Right(state)
                    }
                }

                def tryAddDeposit(
                    ctx: TransactionBuilder.Context,
                    mbRolloutOutput: Option[Babbage],
                    deposit: DepositUtxo
                ): Either[Error, TransactionBuilder.Context] =
                    val depositStep = Spend(TransactionUnspentOutput(deposit.toUtxo))
                    for {
                        newCtx <- TxBuilder.modify(ctx, List(depositStep))
                        _ <- TxBuilder.finalizeTxBuilderContext(newCtx, mbRolloutOutput)
                    } yield newCtx
            }

            object AddPayouts {
                @tailrec
                def loop(state: AddedPayouts): Either[Error, AddedPayouts] = {
                    import state.*
                    remainingPayouts match {
                        case payout :: otherPayouts =>
                            tryAddPayout(txBuilderContext, mbRolloutOutput, payout) match {
                                case Right(x) =>
                                    val newState = state.copy(
                                      txBuilderContext = x,
                                      remainingPayouts = otherPayouts
                                    )
                                    loop(newState)
                                case Left(err) =>
                                    TxBuilder.replaceInvalidSizeException(err, state)
                            }
                        case Nil => Right(state)
                    }
                }

                def tryAddPayout(
                    ctx: TransactionBuilder.Context,
                    mbRolloutOutput: Option[Babbage],
                    payout: Babbage
                ): Either[Error, TransactionBuilder.Context] =
                    val payoutStep = Send(payout)
                    for {
                        newCtx <- TxBuilder.modify(ctx, List(payoutStep))
                        _ <- TxBuilder.finalizeTxBuilderContext(newCtx, mbRolloutOutput)
                    } yield newCtx
            }

            object NeedsRolloutFees {
                def updateMbRolloutOutput(
                    mbRolloutOutput: Option[Babbage],
                    rolloutFees: Coin
                ): Either[Error, Option[Babbage]] =
                    mbRolloutOutput match {
                        case None =>
                            if rolloutFees == Coin(0) then Right(None)
                            else Left(RolloutFeesWithoutRolloutOutputError)
                        case Some(sendRollout) =>
                            import sendRollout.*
                            Right(Some(sendRollout.copy(value = value + Value(rolloutFees))))
                    }
            }

            object TxBuilder {
                def build(
                    steps: List[TransactionBuilderStep]
                ): Either[Error, TransactionBuilder.Context] =
                    TransactionBuilder.build(context.network, steps).left.map(StepError(_))

                def modify(
                    ctx: TransactionBuilder.Context,
                    steps: List[TransactionBuilderStep]
                ): Either[Error, TransactionBuilder.Context] =
                    TransactionBuilder.modify(ctx, steps).left.map(StepError(_))

                // Avoid naming methods [[finalize]] because it is a built-in Java method of [[Object]].
                // It's deprecated, but it still exists for now.
                def finalizeTxBuilderContext(
                    txBuilderContext: TransactionBuilder.Context,
                    mbRolloutOutput: Option[Babbage]
                ): Either[Error, TransactionBuilder.Context] = {
                    for {
                        // Add the rollout output step to tx-builder context
                        addedRolloutOutput <- modify(
                          txBuilderContext,
                          mbRolloutOutput.map(Send(_)).toList
                        )

                        // Try to build, balance, and validate the resulting transaction
                        finalized <- addedRolloutOutput
                            .finalizeContext(
                              protocolParams = context.protocolParams,
                              diffHandler = ChangeOutputDiffHandler(
                                protocolParams = context.protocolParams,
                                changeOutputIdx = 0
                              ).changeOutputDiffHandler,
                              evaluator = context.evaluator,
                              validators = context.validators
                            )
                            .left
                            .map({
                                case balanceError: TxBalancingError =>
                                    BalancingError(balanceError)
                                case validationError: TransactionException =>
                                    ValidationError(validationError)
                            })
                    } yield finalized
                }

                def replaceInvalidSizeException[A](err: Error, x: A): Either[Error, A] =
                    err match
                        case ValidationError(ve) =>
                            ve match {
                                case _: InvalidTransactionSizeException =>
                                    Right(x)
                                case _ => Left(err)
                            }
                        case _ => Left(err)
            }

            object PostProcess {

                /** Finalize the [[Builder]] state, and get the finalized transaction parsed as a
                  * [[SettlementTx]].
                  *
                  * @param state
                  *   A finalized [[Builder]] state that contains a [[TransactionBuilder.Context]].
                  * @return
                  *
                  * IMPORTANT: This function assumes that:
                  *
                  *   - The treasury output exists and is the first output of the transaction.
                  *     [[PessimisticBase.pessimisticSteps]] must satisfy this assumption.
                  *
                  *   - The rollout output is the last output of the transaction, if it exists.
                  *     [[TxBuilder.finalizeTxBuilderContext]] must satisfy this assumption.
                  */
                def finalizeStateAndGetSettlementTx(
                    state: HasTxBuilderContext & HasMbRolloutOutput & HasDepositsPartition
                ): Either[Error, SettlementTx] = {
                    import state.*
                    for {
                        stateFinalized <- TxBuilder.finalizeTxBuilderContext(
                          txBuilderContext,
                          mbRolloutOutput
                        )
                    } yield hydrozoa.multisig.ledger.dapp.tx.SettlementTx(
                      treasurySpent = treasuryUtxo,
                      treasuryProduced = unsafeGetTreasuryProduced(state),
                      depositsSpent = state.absorbedDeposits,
                      rolloutProduced = unsafeGetRolloutUtxo(state),
                      tx = state.txBuilderContext.transaction
                    )
                }

                /** Get the treasury output of the finalized transaction in the [[Builder]] state,
                  * parsed as a [[TreasuryUtxo]].
                  *
                  * @param state
                  *   A finalized [[Builder]] state that contains a [[TransactionBuilder.Context]].
                  * @return
                  *
                  * IMPORTANT: This function assumes that:
                  *   - The treasury output exists and is the first output of the transaction.
                  *     [[PessimisticBase.pessimisticSteps]] must satisfy this assumption.
                  */
                def unsafeGetTreasuryProduced(state: HasTxBuilderContext): TreasuryUtxo = {
                    val tx = state.txBuilderContext.transaction
                    val treasuryOutput = tx.body.value.outputs.head.value

                    treasuryUtxo.copy(
                      txId = TransactionInput(transactionId = tx.id, index = 0),
                      datum = PessimisticBase.newTreasuryDatum,
                      value = treasuryOutput.value
                    )
                }

                /** Get the rollout output of the finalized transaction in the [[Builder]] state,
                  * parsed as a [[RolloutUtxo]].
                  *
                  * @param state
                  *   A finalized [[Builder]] state that contains a [[TransactionBuilder.Context]]
                  *   and optionally a rollout output.
                  * @return
                  *
                  * WARNING: This function assumes that:
                  *
                  *   - The rollout output is the last output of the transaction, if it exists.
                  *     [[TxBuilder.finalizeTxBuilderContext]] must satisfy this assumption.
                  */
                def unsafeGetRolloutUtxo(
                    state: HasTxBuilderContext & HasMbRolloutOutput
                ): Option[RolloutUtxo] = {
                    val tx = state.txBuilderContext.transaction
                    val lastOutputIdx = tx.body.value.outputs.length - 1
                    assert(lastOutputIdx >= 1)

                    state.mbRolloutOutput.map(rolloutOutput =>
                        RolloutUtxo((TransactionInput(tx.id, lastOutputIdx), rolloutOutput))
                    )
                }
            }
        }
    }
}
