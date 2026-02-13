package hydrozoa.lib.cardano.scalus.txbuilder

import scalus.cardano.ledger.Coin
import scalus.cardano.txbuilder.TxBalancingError.Failed

object DiffHandler {
    import scalus.cardano.txbuilder.DiffHandler

    /** A wrapper for [[reportLovelaceDiffHandler]] and [[prebalancedLovelaceDiffHandler]]
      */
    final case class WrappedCoin(coin: Coin) extends Throwable

    /** A diff handler for [[LowLevelTxBalancer]] that simply reports the difference as a
      * Left(CantBalance(diff)). Note that this handler returns left _even if the diff is zero_.
      *
      * This is a useful hack for doing two things:
      *
      *   - Speculative balancing, where you're trying to determine what the minimum size of an
      *     _input_ would need to be
      *   - Getting out the balance as seen by the diff handler
      */
    def reportLovelaceDiffHandler: DiffHandler = (diff, _) => Left(Failed(WrappedCoin(diff.coin)))

    /** A diff handler for [[LowLevelTxBalancer]] that only succeeds if the transaction is
      * pre-balanced, otherwise returning a Left(InsufficientFunds(diff, tx)).
      *
      * @return
      */
    def prebalancedLovelaceDiffHandler: DiffHandler =
        (diff, tx) =>
            if diff.coin.value == 0 then Right(tx) else Left(Failed(WrappedCoin(diff.coin)))
}
