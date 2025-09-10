package hydrozoa.multisig.ledger.l1.utxo

import scalus.cardano.ledger.{TransactionInput, TransactionOutput}

final case class RolloutUtxo(utxo: (TransactionInput, TransactionOutput))

object RolloutUtxo {}
