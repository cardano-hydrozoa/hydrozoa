package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo
import scalus.cardano.ledger.Transaction

/** The Deinit tx in the multisig regime acts mostly the same way the eponymous tx in the rule-based
  * regime:
  *   - It spends the empty treasury utxo (combined with the multisig utxo by the finalization tx)
  *     that contains peers' collateral and voting deposits, collective deposits, and the head
  *     equity.
  *   - It pays out to every peer as one output (the deposits guarantee that every payout is at
  *     least > 2 * minAda):
  *     - Their collateral deposit
  *     - Their voting deposit
  *     - Their share of collective deposits (according to equity shares)
  *     - Their share of equity
  *   - All those payouts will fit the single tx. This holds, because the number of peers is limited
  *     by the fallback tx that should be able _simultaneously_ distribute collaterals and (N+1)
  *     vote utxos in one go, the 2x+1 number of outputs.
  */
case class DeinitTx(
    override val tx: Transaction,
    override val treasurySpent: TreasuryUtxo
) extends Tx,
      TreasuryUtxo.Spent

object DeinitTx:

    final case class Builder(config: Tx.Builder.Config)

end DeinitTx
