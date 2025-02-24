package hydrozoa.head.wallet

import hydrozoa.head.{TxKeyWitness, L1Tx}

class MockWallet extends Wallet {
    override def sign(tx: L1Tx): TxKeyWitness = ???
}
