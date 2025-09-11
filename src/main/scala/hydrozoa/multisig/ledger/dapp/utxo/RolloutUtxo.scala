package hydrozoa.multisig.ledger.dapp.utxo

import scalus.cardano.ledger.{TransactionInput, TransactionOutput}

final case class RolloutUtxo(utxo: (TransactionInput, TransactionOutput))

object RolloutUtxo {}
