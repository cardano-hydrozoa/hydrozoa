package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.multisig.ledger.DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo

import scalus.cardano.ledger.Transaction

final case class FallbackTx(
    treasurySpent: TreasuryUtxo,
    override val tx: Transaction
) extends Tx

object FallbackTx {}
