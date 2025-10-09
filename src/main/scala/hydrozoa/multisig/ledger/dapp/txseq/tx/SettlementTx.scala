package hydrozoa.multisig.ledger.dapp.txseq.tx

import hydrozoa.lib.tx.BuildError.{BalancingError, StepError, ValidationError}
import hydrozoa.lib.tx.TransactionBuilder.Context
import hydrozoa.lib.tx.TransactionBuilderStep.{Mint, ModifyAuxiliaryData, ReferenceOutput, Send, Spend}
import hydrozoa.lib.tx.{BuildError, TransactionBuilder, TransactionBuilderStep, TransactionUnspentOutput}
import hydrozoa.multisig.ledger.dapp.tx.SettlementTx.Recipe
import hydrozoa.multisig.ledger.dapp.tx.{Metadata as MD, SettlementTx}
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo.mkMultisigTreasuryDatum
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, TreasuryUtxo}
import scala.annotation.tailrec
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.ledger.AuxiliaryData.Metadata
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionException.InvalidTransactionSizeException
import scalus.cardano.ledger.TransactionMetadatum.Int
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler
import scalus.cardano.ledger.txbuilder.TxBalancingError
import scalus.cardano.ledger.{Coin, TransactionException, TransactionInput, TransactionMetadatumLabel, TransactionOutput, Value}

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

    type Cont = Coin => SettlementTx

    case class PartialResult(
        remainingPayouts: Map[TransactionInput, TransactionOutput],
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
                else Right((withDeposits, args.utxosWithdrawn))
            (withPayouts, remainingPayouts) = directPayoutsAdded
            ret: PartialResult <- Right(
              PartialResult(
                remainingPayouts,
                deposits,
                builder.finalizeSettlementTx(withPayouts)
              )
            )
        } yield ret

    }

    class Builder(args: Recipe) {
        import args.*

        // -------------------------------------------------------------------------
        // 1. pessimistic settlement
        // -------------------------------------------------------------------------

        /** We assume this transaction is always possible to build. */
        val mkSettlementTxPessimistic: Either[BuildError, SettlementContext] = {

            val totalValueWithdrawn =
                utxosWithdrawn.values.map(_.value).foldLeft(Value.zero)(_ + _)

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

        // TODO
        def addDirectPayouts = ???

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
