package hydrozoa.multisig.ledger.dapp.txseq

import cats.implicits.*
import hydrozoa.multisig.ledger.dapp.tx.RolloutTx
import hydrozoa.multisig.ledger.dapp.tx.SettlementTx
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.multisig.ledger.dapp.utxo.RolloutUtxo
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo
import scalus.cardano.ledger.Coin
import scalus.cardano.ledger.TransactionInput
import scalus.cardano.ledger.TransactionOutput
import SettlementTx.Recipe
import hydrozoa.lib.tx.TransactionBuilder.Context
import hydrozoa.lib.tx.TransactionBuilderStep
import hydrozoa.lib.tx.TransactionBuilderStep.Send
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript

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
              * separately, so we can change them afterwards.
              */
            case class SettlementContext(
                context: Context,
                finalSteps: List[TransactionBuilderStep],
                sendRolloutOutputStep: Option[Send]
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
                def mkRolloutOutput(utxosWithdrawn: Map[TransactionInput, TransactionOutput]) = {
                    ???
                }

                /** We assume this transaction is always possible to build. */
                def mkSettlementTxPessimistic(
                    treasuryUtxo: TreasuryUtxo,
                    headNativeScript: HeadMultisigScript,
                    rolloutOutput: Option[TransactionOutput]
                ): SettlementContext = {
                    ???
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
                    // Tries to add a deposit one by one until the list is exhausted or tx size limit is hit.
                    // The easiest way: create Context every time and rebuild the whole tx
                    // More optimal: use tx editor / edit the tx directly?
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
                val rolloutOutput: Option[TransactionOutput] = mkRolloutOutput(args.utxosWithdrawn)

                // Step 2. Make a "pessimistic" settlement tx that:
                //  - spends the treasury input
                //  - sends the treasury output back to the head script
                //  - mint the rollout token
                //  - rollout utxo
                val settlementTxPessimistic: SettlementContext = mkSettlementTxPessimistic(
                  args.treasuryUtxo,
                  args.headNativeScript,
                  rolloutOutput
                )

                // Step 2. Adding deposits (is they exist). Outcomes:
                //  - list of deposits is depleted -> try to add direct payouts
                //  - some deposits don't fit the tx  -> return
                val (settlementTxWithDeposits, deposits): (SettlementContext, Deposits) =
                    addDeposits(settlementTxPessimistic, args.deposits)

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
