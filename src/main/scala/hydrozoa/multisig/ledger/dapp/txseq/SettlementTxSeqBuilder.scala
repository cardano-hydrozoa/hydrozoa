package hydrozoa.multisig.ledger.dapp.txseq

import cats.implicits.*
import hydrozoa.multisig.ledger.dapp.tx.RolloutTx
import hydrozoa.multisig.ledger.dapp.tx.SettlementTx
import hydrozoa.multisig.ledger.dapp.utxo.RolloutUtxo
import scalus.cardano.ledger.Coin
import scalus.cardano.ledger.TransactionInput
import scalus.cardano.ledger.TransactionOutput
import SettlementTx.Recipe
import hydrozoa.lib.tx.TransactionBuilder.Context
import hydrozoa.lib.tx.TransactionBuilder
import hydrozoa.multisig.ledger.dapp.txseq.tx as newtx

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
      * NB: I wouldn't call it recipe either, since it works more like a request:
      * [[Recipe.deposits]] is not something that must be included into the settlement tx, but
      * rather a petition, so some may end up in `depositsPostponed` in [[Result]] type.
      *
      * @param args
      *   the recipe for building the settlement/rollout tx sequence
      * @return
      */
    def build(args: Recipe): Result =
        val (i1, deposits) = unfold(args)
        val i2 = traverseFee(i1)
        val seq = traverseInput(i2)
        Result(seq, deposits)

    case class Result(
        txSeq: SettlementTxSeq,
        deposits: newtx.SettlementTx.Deposits
    )

    case class SettlementTxSeq(
        settlementTx: SettlementTx,
        rolloutTxs: List[RolloutTx]
    )

    // -------------------------------------------------------------------------
    // 1. unfold
    // -------------------------------------------------------------------------

    def unfold(args: Recipe): (UnfoldResult, newtx.SettlementTx.Deposits) = {
        // Build a settlement tx as big as it can be
        val settlement: newtx.SettlementTx.PartialResult = newtx.SettlementTx.build(args)
        // Unfold rollouts
        val rollouts = List.unfold(settlement.remainingPayouts)(newtx.RolloutTx.build)

        (
          UnfoldResult(settlement.cont, rollouts),
          settlement.deposits
        )
    }

    case class UnfoldResult(
        settlementTx: newtx.SettlementTx.Cont,
        rolloutTxs: List[newtx.RolloutTx.Cont1]
    )

    // -------------------------------------------------------------------------
    // 2. traverse fee
    // -------------------------------------------------------------------------

    /** traverse from the right:
      *   - add accumulated fees to the rollout output
      *   - plus some technical adjustments (see [[PartialResult2]])
      *   - recalculate fee
      *
      * @param unfoldResult
      *   result from the unfolding of the settlement and rollout txs
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
        rolloutTxs: List[hydrozoa.multisig.ledger.dapp.txseq.tx.RolloutTx.Cont2]
    )

    // -------------------------------------------------------------------------
    // 3. traverse input
    // -------------------------------------------------------------------------

    /** Traverse from the left:
      *   - set real rollout input
      * @param traverseFeeResult
      *   result from the previous traversal (where we calculated the real fees)
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

    /** Something that has a tx builder context. */
    trait HasContext:
        def getContext: Context
}
