package hydrozoa.l1.wallet

import hydrozoa.{L1Tx, TxKeyWitness}

trait Wallet {
    def sign(tx: L1Tx): TxKeyWitness
}
