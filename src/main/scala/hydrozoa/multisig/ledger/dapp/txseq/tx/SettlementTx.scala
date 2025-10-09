package hydrozoa.multisig.ledger.dapp.txseq.tx

import hydrozoa.lib.tx.BuildError.{BalancingError, StepError, ValidationError}
import hydrozoa.lib.tx.TransactionBuilder.Context
import hydrozoa.lib.tx.TransactionBuilderStep.{Mint, *}
import hydrozoa.lib.tx.{
    BuildError,
    TransactionBuilder,
    TransactionBuilderStep,
    TransactionUnspentOutput
}
import hydrozoa.multisig.ledger.dapp.tx.SettlementTx.Recipe
import hydrozoa.multisig.ledger.dapp.tx.{RolloutTx, SettlementTx, Metadata as MD}
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo.mkMultisigTreasuryDatum
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, TreasuryUtxo}
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.ledger.AuxiliaryData.Metadata
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionException.InvalidTransactionSizeException
import scalus.cardano.ledger.TransactionMetadatum.Int
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler
import scalus.cardano.ledger.txbuilder.TxBalancingError
import scalus.cardano.ledger.{Mint as SMint, *}

import scala.annotation.tailrec

object SettlementTx {

    /** Deposit split.
      *
      * @param depositsAbsorbed
      *   fortune deposits that made it to the settlement tx
      * @param depositsPostponed
      *   the rest
      */
    case class Deposits(
        depositsAbsorbed: List[DepositUtxo],
        depositsPostponed: List[DepositUtxo]
    )

    /** @param directPayouts
      *   Payouts that fit directly as outputs to the settlement tx
      * @param rolledoutPayouts
      *   Payouts that must go to the rollout tx
      */
    case class Withdrawals(
        directPayouts: List[Babbage],
        rolledoutPayouts: List[Babbage]
    )

    type Cont = Coin => SettlementTx

    case class PartialResult(
        remainingPayouts: List[Babbage],
        deposits: Deposits,
        cont: Cont
    )

    /** This is a specialized wrapper around tx builder context that keeps _some_ steps separately,
      * so we can change them afterward.
      */
    case class SettlementContext(
        contextSoFar: Context,
        mkNewTreasury: Send,
        sendRolloutOutputStep: Option[MintSendRollout]
    ) {
        def extraSteps: Seq[TransactionBuilderStep] =
            Seq(
              // NB: treasury output must be in position 0
              mkNewTreasury,
              reserveSpaceForFees
            ) ++ MintSendRollout.toSeq(sendRolloutOutputStep)
    }

    val reserveSpaceForFees = ModifyAuxiliaryData(currentData =>
        Some(
          Metadata(
            currentData
                .getOrElse(Metadata(Map.empty))
                .getMetadata
                // NB: this magic number is just to add extra bytes to account for tx fees
                .updated(TransactionMetadatumLabel(99999L), Int(Long.MaxValue))
          )
        )
    )

    case class MintSendRollout(
        mint: Mint,
        send: Send
    ) {
        def toSeq: Seq[TransactionBuilderStep] = Seq(mint, send)
    }

    object MintSendRollout {
        def toSeq(x: Option[MintSendRollout]): Seq[TransactionBuilderStep] =
            x.fold(Seq.empty)(_.toSeq)
    }

    def build(args: Recipe): Either[BuildError, PartialResult] = {
        val builder = Builder(args)

        for {
            // Step 1. Pessimistic settlement transaction
            pessimistic <- builder.mkSettlementTxPessimistic
            // Step 2. Try to add as much deposits as we can
            // Outcomes:
            //  - list of deposits is depleted -> try to add direct payouts
            //  - some deposits don't fit the tx -> return
            depositsAdded <- builder.addDeposits(pessimistic, args.deposits)
            (withDeposits, deposits, finished) = depositsAdded
            // TODO: George: i think finished === deposits.depositsPostponed.nonEmpty
            // Step 3. Add direct payouts. Outcomes:
            //  - list of withdrawals is depleted -> no need to mint the token, no rollout utxo (effectively the end of the whole algorithm).
            //  - some withdrawals don't fit
            directPayoutsAdded <-
                if finished then Right((withDeposits, args.utxosWithdrawn))
                // FIXME: call addDirectPayouts
                else builder.addDirectPayouts(withDeposits, args.utxosWithdrawn)
            (withPayouts, remainingPayouts) = directPayoutsAdded
//            ret: PartialResult <- Right(
//              PartialResult(
//                remainingPayouts,
//                deposits,
//                builder.finalizeSettlementTx(withPayouts)
//              )
//            )
        } yield ???

    }

    class Builder(args: Recipe) {
        import args.*

        // -------------------------------------------------------------------------
        // 1. pessimistic settlement
        // -------------------------------------------------------------------------

        /** We assume this transaction is always possible to build. */
        val mkSettlementTxPessimistic: Either[BuildError, SettlementContext] = {

            val totalValueWithdrawn =
                utxosWithdrawn.map(_.value).foldLeft(Value.zero)(_ + _)

            /////////////////////////////////////////////////////
            // Step Definition
            val referenceHNS = ReferenceOutput(headNativeScriptReferenceInput)

            val consumeTreasury: Spend =
                Spend(
                  treasuryUtxo.asUtxo,
                  headNativeScript.witness
                )

            val sendTreasury: Send = Send(
              mkTreasuryOutput(treasuryUtxo, totalValueWithdrawn)
            )

            val mbMintSendRollout: Option[MintSendRollout] =
                if utxosWithdrawn.nonEmpty
                then {
                    val mintRollout: Mint =
                        Mint(
                          scriptHash = headNativeScript.policyId,
                          assetName = rolloutTokenName,
                          amount = 1L,
                          witness = headNativeScript.witness
                        )
                    val sendRollout: Send = Send(mkRolloutOutput(totalValueWithdrawn))
                    Some(MintSendRollout(mintRollout, sendRollout))
                } else None

            val modifyAuxiliaryData =
                ModifyAuxiliaryData(_ =>
                    Some(
                      MD(
                        MD.L1TxTypes.Settlement,
                        headAddress = headNativeScript.mkAddress(context.network)
                      )
                    )
                )

            val staticSteps: Seq[TransactionBuilderStep] = Seq(
              referenceHNS, // Stays the same
              consumeTreasury, // Stays the same
              modifyAuxiliaryData // Stays the same
            )

            /////////////////////////////////////////////////////////
            // Build and finalize
            for {
                builderContextToKeep <- TransactionBuilder
                    .build(context.network, staticSteps)
                    .left
                    .map(StepError(_))

                result = SettlementContext(
                  contextSoFar = builderContextToKeep,
                  mkNewTreasury = sendTreasury,
                  sendRolloutOutputStep = mbMintSendRollout
                )

                // Fee Calculation
                builderContextTemp <- TransactionBuilder
                    .modify(builderContextToKeep, result.extraSteps)
                    .left
                    .map(StepError(_))

                _ <- builderContextTemp
                    .finalizeContext(
                      context.protocolParams,
                      diffHandler = new ChangeOutputDiffHandler(
                        context.protocolParams,
                        0
                      ).changeOutputDiffHandler,
                      evaluator = context.evaluator,
                      validators = context.validators
                    )
                    .left
                    .map({
                        case balanceError: TxBalancingError => BalancingError(balanceError)
                        case validationError: TransactionException =>
                            ValidationError(validationError)
                    })

                /////////////////////////////////////////////////////////////////////////
                // Post-process result
            } yield result
        }

        def mkTreasuryOutput(treasuryUtxo: TreasuryUtxo, totalValueWithdrawn: Value) =
            // TODO: Pass the hash of the protocol parameters in the datum
            val newTreasuryDatum =
                mkMultisigTreasuryDatum(majorVersion, ByteString.empty)

            val newTreasuryVal = treasuryUtxo.value - totalValueWithdrawn

            Babbage(
              address = headNativeScript.mkAddress(context.network),
              value = newTreasuryVal,
              datumOption = Some(Inline(newTreasuryDatum.toData)),
              scriptRef = None
            )

        /** Combines all withdrawals into one TransactionOutput, omitting datums. As long as we have
          * one utxo for the _whole_ treasury, this is total.
          */
        def mkRolloutOutput(value: Value): TransactionOutput =
            Babbage(headNativeScript.mkAddress(context.network), value, None, None)

        // -------------------------------------------------------------------------
        // 2. adding deposits
        // -------------------------------------------------------------------------

        /** Tries to add a deposit one by one until the list is exhausted or tx size limit is hit.
          *
          * Comment (From Peter): Other options are a heuristic + binary search (take all utxos up
          * to a certain number, then work backwards from there) or a closed-form solution if we
          * know the size of the remainder of the transaction. One-by-one is a good start for
          * validity checking, though.
          *
          * @param state0
          *   initial state
          * @param depositsList
          *   deposits list
          * @return
          *   the last successfully built transaction and the deposits split. in case there are no
          *   deposit the original settlementTxPessimistic
          *
          * The boolean return indicates whether we've run out of space in the tx while adding
          * deposits
          */
        def addDeposits(
            state0: SettlementContext,
            depositsList: List[DepositUtxo]
        ): Either[BuildError, (SettlementContext, Deposits, Boolean)] = {
            val deposits0 =
                Deposits(depositsAbsorbed = List.empty, depositsPostponed = depositsList)

            @tailrec
            def loop(
                state: SettlementContext,
                deposits: Deposits
            ): Either[BuildError, (SettlementContext, Deposits, Boolean)] = {
                deposits.depositsPostponed match {
                    case deposit :: otherDeposits =>
                        val newDeposits = Deposits(
                          depositsAbsorbed = deposits.depositsAbsorbed :+ deposit,
                          depositsPostponed = otherDeposits
                        )
                        tryAddDeposit(state, deposit) match {
                            case Right(x) =>
                                loop(x, newDeposits)
                            case Left(err) =>
                                err match
                                    case ValidationError(ve) =>
                                        ve match {
                                            case _: InvalidTransactionSizeException =>
                                                Right((state, deposits, true))
                                            case _ => Left(err)
                                        }
                                    case _ => Left(err)
                        }
                    case Nil => Right((state, deposits, false))
                }
            }

            loop(state0, deposits0)
        }

        def tryAddDeposit(
            ctx: SettlementContext,
            deposit: DepositUtxo
        ): Either[BuildError, SettlementContext] = {
            val depositStep: Spend = Spend(TransactionUnspentOutput(deposit.toUtxo))
            for {
                // Start with the context so far and our additional deposit step, trialing this deposit.
                // We do this separately, because we yield this if the trial passes.
                builderContextToKeep <-
                    TransactionBuilder
                        .modify(
                          ctx.contextSoFar,
                          Seq(depositStep)
                        )
                        .left
                        .map(StepError(_))

                result = ctx.copy(contextSoFar = builderContextToKeep)

                // These are additional fake steps needed to see if this deposit "fits" after
                // balancing
                builderContextTemp <-
                    TransactionBuilder
                        .modify(
                          builderContextToKeep,
                          ctx.extraSteps
                        )
                        .left
                        .map(StepError(_))

                // Perform a trial balance, see what happens
                _ <- builderContextTemp
                    .finalizeContext(
                      context.protocolParams,
                      diffHandler = new ChangeOutputDiffHandler(
                        context.protocolParams,
                        0
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
            } yield result
        }

        // -------------------------------------------------------------------------
        // 3. adding direct payouts
        // -------------------------------------------------------------------------

        /** Attempt to add as many direct payouts as possible to the transaction.
          * @param context
          *   The context with deposits already added
          * @param utxosToWithdraw
          *   A list of Babbage transaction outputs that must appear on L1. The order in which these
          *   are provided is set by the dApp ledger (I think?) and must be formed of monotonic
          *   subsequence in terms of event numbering for individual peers.
          * @return
          *   Left if ???, Right with a tuple containing the context with as many direct payouts as
          *   possible and the remaining UTxOs that need to be rolled-out.
          */
        def addDirectPayouts(
            context: SettlementContext,
            utxosToWithdraw: List[Babbage]
        ): Either[BuildError, (SettlementContext, Withdrawals)] = {
            val withdrawals0 =
                Withdrawals(directPayouts = List.empty, rolledoutPayouts = utxosToWithdraw)

            /** Recursively loop over all UTxOs to withdraw, moving them from the rollout tx into
              * the direct payouts via "trial payouts".
              *
              * @return
              */
            @tailrec
            def loop(
                state: SettlementContext,
                withdrawals: Withdrawals
            ): Either[BuildError, (SettlementContext, Withdrawals)] = {
                withdrawals.rolledoutPayouts match {
                    // There are still withdrawals getting rolled out, process one-by-one
                    case w :: ws =>
                        val newWithdrawals = Withdrawals(
                          directPayouts = withdrawals.directPayouts :+ w,
                          rolledoutPayouts = ws
                        )
                        // Trial payout
                        tryAddPayout(state, w) match {
                            // Trial successful, loop
                            case Right(x) => loop(x, newWithdrawals)
                            // Trial failed, either because the Tx became to big or for some other build failure
                            case Left(err) =>
                                err match
                                    // Transaction became too big, this means that the state and withdrawals are
                                    // correct.
                                    case ValidationError(_: InvalidTransactionSizeException) =>
                                        Right(state, withdrawals)
                                    // Trial failed for some other reason -- return it.
                                    case _ => Left(err)
                        }
                    // No more withdrawals to process, rollout UTxO is not needed
                    case Nil => Right(state, withdrawals)
                }
            }
            loop(context, withdrawals0)
        }

        def tryAddPayout(
            ctx: SettlementContext,
            toWithdraw: Babbage
        ): Either[BuildError, SettlementContext] =
            for {
                // Trial this payout, see if it fits. We yield this if we are successful.
                builderContextToKeep <- TransactionBuilder
                    .modify(ctx.contextSoFar, Seq(Send(toWithdraw)))
                    .left
                    .map(StepError(_))

                result = ctx.copy(contextSoFar = builderContextToKeep)

                // Fake steps added for balancing
                builderContextTemp <- TransactionBuilder
                    .modify(builderContextToKeep, ctx.extraSteps)
                    .left
                    .map(StepError(_))

                // Perform a trial balance, see what happens
                _ <- builderContextTemp
                    .finalizeContext(
                      context.protocolParams,
                      diffHandler = new ChangeOutputDiffHandler(
                        context.protocolParams,
                        0
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
            } yield result

        /** Finalize the transaction if all withdrawals fit as direct payouts. In this case, there
          * is no rollout UTxO created or token minted.
          *
          * See also [[prefinalizeWithRollout()]]
          *
          * @param ctx
          * @return
          */
        def finalizeNoRollout(ctx: SettlementContext): Either[BuildError, Context] =
            for {
                withTreasury <- TransactionBuilder
                    .modify(ctx.contextSoFar, Seq(ctx.mkNewTreasury))
                    .left
                    .map(StepError(_))

                treasuryIndex = withTreasury.transaction.body.value.outputs.size - 1
                finalized <- withTreasury
                    .finalizeContext(
                      context.protocolParams,
                      diffHandler = new ChangeOutputDiffHandler(
                        context.protocolParams,
                        treasuryIndex
                      ).changeOutputDiffHandler,
                      evaluator = context.evaluator,
                      validators = context.validators
                    )
                    .left
                    .map({
                        case balanceError: TxBalancingError => BalancingError(balanceError)
                        case validationError: TransactionException =>
                            ValidationError(validationError)
                    })
            } yield finalized

        /** If we need to add rollouts, then we need to create a _rollout chain_. We do this in the
          * RolloutTx.scala module. finalizeWithRollout will produce a _function_ that takes the
          * value of the _first_ rollout UTxO as an argument. This is then passed in to the
          * arguments to [[RolloutTx.finalizeWithRollout]] as `mkSettlement`.
          *
          * See all [[finalizeNoRollout()]]
          * @param ctx
          * @param toRollout
          * @return
          *   A function awaiting the value for the first rollout transaction, see
          *   [[makeRolloutChain]]
          */
        def prefinalizeWithRollout(
            ctx: SettlementContext
        ): Value => Either[BuildError, (Context, TransactionInput)] = firstRolloutValue => {

            val sendFirstRollout: Send = Send(
              Babbage(
                address = headNativeScript.mkAddress(context.network),
                value = firstRolloutValue,
                datumOption = None,
                scriptRef = None
              )
            )
            for {
                unbalanced <- TransactionBuilder
                    .modify(ctx.contextSoFar, Seq(sendFirstRollout, ctx.mkNewTreasury))
                    .left
                    .map(StepError(_))

                treasuryIndex = unbalanced.transaction.body.value.outputs.size - 1
                rolloutIndex = unbalanced.transaction.body.value.outputs.size - 2

                finalized <- unbalanced
                    .finalizeContext(
                      context.protocolParams,
                      diffHandler = new ChangeOutputDiffHandler(
                        context.protocolParams,
                        treasuryIndex
                      ).changeOutputDiffHandler,
                      evaluator = context.evaluator,
                      validators = context.validators
                    )
                    .left
                    .map({
                        case balanceError: TxBalancingError => BalancingError(balanceError)
                        case validationError: TransactionException =>
                            ValidationError(validationError)
                    })

            } yield (finalized, TransactionInput(finalized.transaction.id, rolloutIndex))
        }

        // -------------------------------------------------------------------------
        // ...
        // -------------------------------------------------------------------------

        def finalizeSettlementTx(ctx: SettlementContext)(
            coin: Coin
        ): SettlementTx = {

            // Changes sendRolloutOutputStep, convert to Context and build the final transaction.
            ???
        }

    }
}
