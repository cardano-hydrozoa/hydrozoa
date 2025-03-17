package hydrozoa.l1.wallet

import hydrozoa.{TxAny, TxKeyWitness}

trait Wallet {
    def createTxKeyWitness(tx: TxAny): TxKeyWitness
}
