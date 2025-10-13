//package hydrozoa.multisig.ledger.dapp.txseq
//
//import cats.implicits.*
//import hydrozoa.lib.tx.SomeBuildError
//import hydrozoa.multisig.ledger.dapp.tx.SettlementTx.Recipe
//import hydrozoa.multisig.ledger.dapp.tx.{RolloutTx, SettlementTx}
//import hydrozoa.multisig.ledger.dapp.txseq.tx as newtx
//import hydrozoa.multisig.ledger.dapp.utxo.RolloutUtxo
//import scalus.cardano.ledger.Coin
//
///** This is the builder for the whole settlement sequence of transaction.
//  *   - I remove all mentions of the FallbackTx, since now, when we decided to have a multisig
//  *     witness utxo (with fees for FallbackTx), SettlementTx doesn't account for them.
//  */
//
//object SettlementTxSeqBuilder {
//
//    /** The entry-point.
//      *
//      * Here is George decided to use [[SettlementTx.Recipe]], ask him why. I would have a separate
//      * datatype even if they are the same.
//      *
//      * NB: I wouldn't call it recipe either, since it works more like a request:
//      * [[Recipe.deposits]] is not something that must be included into the settlement tx, but
//      * rather a petition, so some may end up in `depositsPostponed` in [[Result]] type.
//      *
//      * @param args
//      *   the recipe for building the settlement/rollout tx sequence
//      * @return
//      */
//    def build(args: Recipe): Either[SomeBuildError, Result] = for {
//        unfolded <- unfold(args)
//        (i1, deposits) = unfolded
//        i2 = traverseFee(i1)
//        seq = traverseInput(i2)
//    } yield Result(seq, deposits)
//
//    case class Result(
//        txSeq: SettlementTxSeq,
//        deposits: newtx.SettlementTx.Deposits
//    )
//
//    case class SettlementTxSeq(
//        settlementTx: SettlementTx,
//        rolloutTxs: List[RolloutTx]
//    )
//
//    // -------------------------------------------------------------------------
//    // 1. unfold
//    // -------------------------------------------------------------------------
//
//    /** @param args
//      * @return
//      */
//    def unfold(args: Recipe): Either[SomeBuildError, (UnfoldResult, newtx.SettlementTx.Deposits)] =
//        for {
//            // Build a settlementPartialResult with the settlement tx (i.e. its building context)
//            // as big as it can be
//            settlementPartialResult: newtx.SettlementTx.PartialResult <- newtx.SettlementTx.build(
//              args
//            )
//
//            //// TODO: Unfold rollouts
//            // val rollouts =
//            //    List.empty // List.unfold(settlementPartialResult.remainingPayouts)(newtx.RolloutTx.build)
//            rollouts = List.empty
//
//        } yield (
//          UnfoldResult(settlementPartialResult.cont, rollouts),
//          settlementPartialResult.deposits
//        )
//
//    case class UnfoldResult(
//        settlementTx: newtx.SettlementTx.Cont,
//        rolloutTxs: List[newtx.RolloutTx.Cont1]
//    )
//
//    // -------------------------------------------------------------------------
//    // 2. traverse fee
//    // -------------------------------------------------------------------------
//
//    /** traverse from the right:
//      *   - add accumulated fees to the rollout output
//      *   - plus some technical adjustments (see [[PartialResult2]])
//      *   - recalculate fee
//      *
//      * TODO: add error handling
//      *
//      * @param unfoldResult
//      *   result from the unfolding of the settlement and rollout txs
//      * @return
//      */
//    private def traverseFee(unfoldResult: UnfoldResult): TraverseFeeResult = {
//        import unfoldResult.*
//
//        val (totalCoin, newRolloutTxs) =
//            rolloutTxs.reverse.mapAccumulate(Coin(0))((x, cont) => cont(x))
//
//        TraverseFeeResult(
//          settlementTx = settlementTx(totalCoin),
//          rolloutTxs = newRolloutTxs.reverse
//        )
//    }
//
//    case class TraverseFeeResult(
//        settlementTx: SettlementTx,
//        rolloutTxs: List[hydrozoa.multisig.ledger.dapp.txseq.tx.RolloutTx.Cont2]
//    )
//
//    // -------------------------------------------------------------------------
//    // 3. traverse input
//    // -------------------------------------------------------------------------
//
//    /** Traverse from the left:
//      *   - set real rollout input
//      *
//      * TODO: add error handling
//      *
//      * @param traverseFeeResult
//      *   result from the previous traversal (where we calculated the real fees)
//      * @return
//      */
//    private def traverseInput(traverseFeeResult: TraverseFeeResult): SettlementTxSeq = {
//        import traverseFeeResult.*
//
//        // TODO: getting from option throws if empty. Need to make `traverseInput` total.
//        val (_, newRolloutTxs) =
//            rolloutTxs.mapAccumulate(settlementTx.rolloutProduced)((x, cont) => cont(x.get))
//
//        SettlementTxSeq(
//          settlementTx = settlementTx,
//          rolloutTxs = newRolloutTxs
//        )
//    }
//
//    // -------------------------------------------------------------------------
//
//}
