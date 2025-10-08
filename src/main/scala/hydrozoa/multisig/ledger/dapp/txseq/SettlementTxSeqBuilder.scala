package hydrozoa.multisig.ledger.dapp.txseq

import cats.implicits.*
import hydrozoa.multisig.ledger.dapp.tx.RolloutTx
import hydrozoa.multisig.ledger.dapp.tx.SettlementTx
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.multisig.ledger.dapp.utxo.RolloutUtxo
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo
import scalus.cardano.ledger.{
    Coin,
    TransactionException,
    TransactionInput,
    TransactionMetadatumLabel,
    TransactionOutput,
    Value
}
import SettlementTx.Recipe
import hydrozoa.lib.tx.BuildError.{BalancingError, StepError, ValidationError}
import hydrozoa.lib.tx.ScriptSource.NativeScriptAttached
import hydrozoa.lib.tx.TransactionBuilder.Context
import hydrozoa.lib.tx.{
    BuildError,
    ExpectedSigner,
    NativeScriptWitness,
    TransactionBuilder,
    TransactionBuilderStep,
    TransactionUnspentOutput
}
import hydrozoa.lib.tx.TransactionBuilderStep.{Mint, ModifyAuxiliaryData, Send}
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo.mkMultisigTreasuryDatum
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import scalus.cardano.ledger.AuxiliaryData.Metadata
import scalus.cardano.ledger.TransactionMetadatum.Int
import scalus.cardano.ledger.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler
import scalus.cardano.ledger.txbuilder.TxBalancingError

/** This is the builder for the whole settlement sequence of transaction.
  *   - I remove all mentions of the FallbackTx, since now, when we decided to have a multisig
  *     witness utxo (with fees for FallbackTx), SettlementTx doesn't account for them.
  */

object SettlementTxSeqBuilder {

    /** The entry-point.
      *
      * Here is George decided to use [[SettlementTx.Recipe]], ask him why. I would have a separate
      * datatype even if they are the same.
      *
      * NB: I wouldn't call it recipe either, since it works more like a request: `Recipe.deposits`
      * is not something that must be included into the settlement tx, but rather a petition, so
      * some may end up in `depositsPostponed` in [[Result]] type.
      *
      * @param args
      * @return
      */
    def build(args: Recipe): Result =
        val (i1, deposits) = unfold(args)
        val i2 = traverseFee(i1)
        val seq = traverseInput(i2)
        Result(seq, deposits)

    case class Result(
        txSeq: SettlementTxSeq,
        deposits: Deposits
    )

    case class SettlementTxSeq(
        settlementTx: SettlementTx,
        rolloutTxs: List[RolloutTx]
    )

    /** Deposit split.
      * @param depositsAbsorbed
      *   fortune deposits that made it to the settlement tx
      * @param depositsPostponed
      *   the rest
      */
    case class Deposits(
        depositsAbsorbed: List[DepositUtxo],
        depositsPostponed: List[DepositUtxo]
    )

    // -------------------------------------------------------------------------
    // 1. unfold
    // -------------------------------------------------------------------------

    def unfold(args: Recipe): (UnfoldResult, Deposits) = {
        // Build a settlement tx as big as it can be
        val settlement: NewBuild.SettlementTx.PartialResult = NewBuild.SettlementTx.build(args)
        // Unfold rollouts
        val rollouts = List.unfold(settlement.remainingPayouts)(NewBuild.RolloutTx.build)

        (
          UnfoldResult(settlement.cont, rollouts),
          settlement.deposits
        )
    }

    case class UnfoldResult(
        settlementTx: NewBuild.SettlementTx.Cont,
        rolloutTxs: List[NewBuild.RolloutTx.Cont1]
    )

    // -------------------------------------------------------------------------
    // 2. traverse fee
    // -------------------------------------------------------------------------

    /** traverse from the right:
      *   - add accumulated fees to to the rollout output
      *   - plus some technical adjustemtns (see [[PartialResult2]])
      *   - recalculate fee
      *
      * @param unfoldResult
      * @return
      */
    private def traverseFee(unfoldResult: UnfoldResult): TraverseFeeResult = {
        import unfoldResult.*

        val (totalCoin, newRolloutTxs) =
            rolloutTxs.reverse.mapAccumulate(Coin(0))((x, cont) => cont(x))

        TraverseFeeResult(
          settlementTx = settlementTx(totalCoin),
          rolloutTxs = newRolloutTxs.reverse
        )
    }

    case class TraverseFeeResult(
        settlementTx: SettlementTx,
        rolloutTxs: List[NewBuild.RolloutTx.Cont2]
    )

    // -------------------------------------------------------------------------
    // 3. traverse input
    // -------------------------------------------------------------------------

    /** Traverse from the left:
      *   - set real rollout input
      * @param traverseFeeResult
      * @return
      */
    private def traverseInput(traverseFeeResult: TraverseFeeResult): SettlementTxSeq = {
        import traverseFeeResult.*

        // TODO: getting from option throws if empty. Need to make `traverseInput` total.
        val (_, newRolloutTxs) =
            rolloutTxs.mapAccumulate(settlementTx.rolloutProduced)((x, cont) => cont(x.get))

        SettlementTxSeq(
          settlementTx = settlementTx,
          rolloutTxs = newRolloutTxs
        )
    }

    // -------------------------------------------------------------------------

    /** See below. */
    trait HasContext:
        def getContext: Context

    object NewBuild {
        object SettlementTx {
            type Cont = Coin => SettlementTx

            case class PartialResult(
                remainingPayouts: Map[TransactionInput, TransactionOutput],
                deposits: Deposits,
                cont: Cont
            )

            /** This is a specialized wrapper around tx builder context that keeps _some_ steps
              * separately, so we can change them afterward.
              */
            case class SettlementContext(
                contextSoFar: Context,
                mkNewTreasury: Send,
                sendRolloutOutputStep: Option[(Send, Mint)]
            )
            // May be a given instance to keep SettlementContext just pure data?
            // Not sure
                extends HasContext {
                override def getContext = ???
            }

            def build(args: Recipe): PartialResult = {

                // --- auxiliary functions

                /** Combines all withdrawals into one TransactionOutput, omitting datums. As long as
                  * we have one utxo for the _whole_ treasury, this is total.
                  */
                def mkRolloutOutput(
                    utxosWithdrawn: Map[TransactionInput, TransactionOutput]
                ): Babbage = {
                    ???
                }

                val reserveSpaceForFees = ModifyAuxiliaryData(currentData =>
                    Some(
                      Metadata(
                        currentData
                            .getOrElse(Metadata(Map.empty))
                            .getMetadata
                            .updated(TransactionMetadatumLabel(99999L), Int(Long.MaxValue))
                      )
                    )
                )

                /** We assume this transaction is always possible to build. */
                val settlementTxPessimistic: Either[BuildError, SettlementContext] = {
                    import args.*
                    import hydrozoa.lib.tx.TransactionBuilderStep.*
                    import hydrozoa.lib.tx.BuildError.*

                    val totalValueWithdrawn =
                        utxosWithdrawn.values.map(_.value).foldLeft(Value.zero)(_ + _)

                    // TODO: Pass the hash of the protocol parameters in the datum
                    val newTreasuryDatum =
                        mkMultisigTreasuryDatum(majorVersion, ByteString.empty)

                    val newTreasuryVal = treasuryUtxo.value - totalValueWithdrawn

                    /////////////////////////////////////////////////////
                    // Step Definition
                    val referenceHNS: ReferenceOutput = ReferenceOutput(
                      headNativeScriptReferenceInput
                    )

                    val consumeTreasury: Spend =
                        Spend(
                          treasuryUtxo.asUtxo,
                          NativeScriptWitness(
                            NativeScriptAttached,
                            headNativeScript.requiredSigners
                          )
                        )

                    val mkNewTreasury: Send = Send(
                      Babbage(
                        address = headNativeScript.address(context.network),
                        value = newTreasuryVal,
                        datumOption = Some(Inline(newTreasuryDatum.toData)),
                        scriptRef = None
                      )
                    )

                    // Using a list instead of Option[(Send, Mint)] for ease of accumulation
                    val mbMkRollout: Option[(Send, Mint)] =
                        if utxosWithdrawn.nonEmpty
                        then {
                            val mintRollout: Mint =
                                Mint(
                                  scriptHash = headNativeScript.policyId,
                                  assetName = rolloutTokenName,
                                  amount = 1L,
                                  witness = NativeScriptWitness(
                                    scriptSource = NativeScriptAttached,
                                    additionalSigners = headNativeScript.requiredSigners
                                  )
                                )
                            val sendRollout: Send = Send(mkRolloutOutput(utxosWithdrawn))
                            Some((sendRollout, mintRollout))
                        } else None

                    val modifyAuxiliaryData =
                        ModifyAuxiliaryData(_ =>
                            Some(
                              MD(
                                MD.L1TxTypes.Settlement,
                                headAddress = headNativeScript.address(context.network)
                                // FIXME: this magic number is just to add extra bytes to account for tx fees,
                              )
                            )
                        )

                    val staticSteps: Seq[TransactionBuilderStep] = Seq(
                      referenceHNS, // Stays the same
                      consumeTreasury, // Stays the same
                      modifyAuxiliaryData // Stays the same
                    )

                    val stepsThatChange = Seq(
                      reserveSpaceForFees,
                      mkNewTreasury // Changes: treasury value changes based on deposits, withdrawals/rollouts, and fees
                    ) ++ mbMkRollout.fold(Seq.empty)(t => Seq(t._1, t._2)) // Changes during unfold

                    /////////////////////////////////////////////////////////
                    // Build and finalize
                    for {
                        unbalanced <- TransactionBuilder
                            .build(context.network, staticSteps)
                            .left
                            .map(StepError(_))

                        // Fee Calculation
                        foo <- TransactionBuilder
                            .modify(unbalanced, stepsThatChange)
                            .left
                            .map(StepError(_))

                        _ <- foo
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
                    } yield SettlementContext(
                      contextSoFar = unbalanced,
                      mkNewTreasury = mkNewTreasury,
                      sendRolloutOutputStep = mbMkRollout
                    )
                }

                /** @param settlementTxPessimistic
                  * @param deposits
                  * @return
                  *   the last successfully built transaction and the deposits split. in case there
                  *   are no deposit the original settlementTxPessimistic
                  */
                def addDeposits(
                    settlementTxPessimistic: SettlementContext,
                    deposits: List[DepositUtxo]
                ): (SettlementContext, Deposits) = {
                    import hydrozoa.lib.tx.TransactionBuilderStep.*
                    import args.*

                    // Returns None if the deposit doesn't fit.
                    def addDeposit(
                        ctx: SettlementContext,
                        deposit: DepositUtxo
                    ): Option[Either[BuildError, Context]] = {
                        val depositStep: Spend = Spend(TransactionUnspentOutput(deposit.toUtxo))
                        val eRes: Either[BuildError, Context] = for {
                            // Start with the context so far and our additional "fake" steps, trialing this deposit.
                            // We do this separately, because we yield this if the trial passes.
                            withDeposit <-
                                TransactionBuilder
                                    .modify(
                                      ctx.contextSoFar,
                                      Seq(depositStep)
                                    )
                                    .left
                                    .map(StepError(_))

                            // These are additional fake steps needed to see if this deposit "fits" after
                            // balancing    
                            withFake <-
                                TransactionBuilder
                                    .modify(
                                      withDeposit,
                                      ctx.sendRolloutOutputStep
                                          .fold(Seq.empty)(t => Seq(t._1, t._2))
                                          .appended(ctx.mkNewTreasury)
                                    )
                                    .left
                                    .map(StepError(_))
                                
                            // Perform a trial balance, see what happens    
                            _ <- withFake
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
                        } yield withDeposit
                        
//                        List.unfold(settlementTxPessimistic)(pess => ???)
                        ???
                    }

                    // Tries to add a deposit one by one until the list is exhausted or tx size limit is hit.
                    // The easiest way: create Context every time and rebuild the whole tx
                    // More optimal: use tx editor / edit the tx directly?
                    //
                    //  Question (from Peter): "one by one" -- how do we decide order? Time posted? Do we enforce it?
                    //
                    //  Comment (From Peter): Other options are a heursitc + binary search (take all utxos up to a
                    //    certain number, then work backwards from there) or a closed-form solution if we know the size
                    //    of the remainder of the transaction. One-by-one is a good start for validity checking, though.
                    ???
                }

                def finalizeSettlementTx(settlementTxPessimistic: SettlementContext)(
                    coin: Coin
                ): SettlementTx = {
                    // Changes sendRolloutOutputStep, convert to Context and build the final transaction.
                    ???
                }

                // --- end auxiliary functions

                // Step 1. Make the rollout uto for all withdrawals.
                // May absent is there are no withdrawals whatsoever.
//                val rolloutOutput: Option[TransactionOutput] = mkRolloutOutput(args.utxosWithdrawn)

                // Step 2. Make a "pessimistic" settlement tx that:
                //  - spends the treasury input
                //  - sends the treasury output back to the head script
                //  - mint the rollout token
                //  - rollout utxo
//                val settlementTxPessimistic: SettlementContext = mkSettlementTxPessimistic(
//                  args.treasuryUtxo,
//                  args.headNativeScript,
//                  rolloutOutput
//                )

                // Step 2. Adding deposits (if they exist). Outcomes:
                //  - list of deposits is depleted -> try to add direct payouts
                //  - some deposits don't fit the tx  -> return
                val (settlementTxWithDeposits, deposits): (SettlementContext, Deposits) = ???
//                    addDeposits(settlementTxPessimistic, args.deposits)

                val ret: PartialResult =
                    if deposits.depositsPostponed.nonEmpty
                    then
                        // some deposits don't fit the tx  -> return
                        PartialResult(
                          args.utxosWithdrawn,
                          deposits,
                          finalizeSettlementTx(settlementTxWithDeposits)
                        )
                    else {
                        // list of deposits is depleted -> try to add direct payouts
                        // Step 3. Add direct payouts. Outcomes:
                        //  - list of withdrawals is depleted -> no need to mint the token, no rollout utxo (effectively the end of the whole algorithm).
                        //  - some withdrawals don't fit
                        val (settlementTxWithPayouts, remainingPayouts) = ??? // def addPayouts
                        PartialResult(
                          remainingPayouts,
                          deposits,
                          finalizeSettlementTx(settlementTxWithPayouts)
                        )
                    }
                ret
            }
        }

        object RolloutTx {
            type Cont1 = Coin => PartialResult2

            type PartialResult1 = (
                Cont1,
                Map[TransactionInput, TransactionOutput]
            )

            type Cont2 = RolloutUtxo => (Option[RolloutUtxo], RolloutTx)

            /** This Cont2 during `traverseInput` should:
              *   - update the "spend" step for rollout utxo
              *   - this won't change fees
              *   - rebuild the tx
              */
            type PartialResult2 = (
                Coin,
                Cont2,
            )

            /** Tries to add as many direct payouts as possible and returns the result and the rest
              * of withdrawals.
              *
              *   - Rollout output on every step should contain the exact amount of ada to pay out
              *     the rest of withdrawals.
              *   - Transactions should reserve some space (in form of a fake metadata) to account
              *     the increase of rollout output when running `traverseFee`. We can't use ada
              *     value for that since later we want to add fees, so we need to preserve the
              *     amount.
              *
              * Cont1 = Coin => PartialResult2 should:
              *   - add Coin to ada in the rollout output
              *   - change that "send" step
              *   - remove phony metadata step
              *   - rebalance to get the fees, return in the PartialResult2
              *
              * Outcomes:
              *   - list of withdrawals is depleted
              *   - some withdrawals don't fit
              *
              * @param remainingPayouts
              * @return
              */
            def build(
                remainingPayouts: Map[TransactionInput, TransactionOutput]
            ): Option[PartialResult1] =
                ???
        }
    }
}
