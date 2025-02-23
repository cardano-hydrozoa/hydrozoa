package hydrozoa.head.wallet

import hydrozoa.head.{TxKeyWitness, L1Tx}

trait Wallet {
  def sign(tx: L1Tx): TxKeyWitness
}
