package hydrozoa.multisig.ledger.dapp.txseq.tx

import hydrozoa.multisig.ledger.dapp.tx.RolloutTx
import hydrozoa.multisig.ledger.dapp.utxo.RolloutUtxo
import scalus.cardano.ledger.{Coin, TransactionInput, TransactionOutput}

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

    /** Tries to add as many direct payouts as possible and returns the result and the rest of
      * withdrawals.
      *
      *   - Rollout output on every step should contain the exact amount of ada to pay out the rest
      *     of withdrawals.
      *   - Transactions should reserve some space (in form of a fake metadata) to account the
      *     increase of rollout output when running `traverseFee`. We can't use ada value for that
      *     since later we want to add fees, so we need to preserve the amount.
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
      *   payouts that we still need to allocate into rollout txs
      * @return
      */
    def build(
        remainingPayouts: Map[TransactionInput, TransactionOutput]
    ): Option[PartialResult1] =
        ???
}
