package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.multisig.ledger.DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.utxo.RolloutUtxo

import scalus.cardano.ledger.Transaction

final case class RolloutTx(
    rolloutSpent: RolloutUtxo,
    rolloutProduced: Option[RolloutUtxo],
    override val tx: Transaction
) extends Tx

object RolloutTx {}
