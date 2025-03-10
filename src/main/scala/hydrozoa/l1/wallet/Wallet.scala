package hydrozoa.l1.wallet

import hydrozoa.{L1Tx, TxKeyWitness}

trait Wallet {
    def createTxKeyWitness(tx: L1Tx): TxKeyWitness
}
