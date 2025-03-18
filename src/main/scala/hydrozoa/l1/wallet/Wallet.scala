package hydrozoa.l1.wallet

import hydrozoa.l1.multisig.tx.{MultisigTx, MultisigTxTag}
import hydrozoa.{TxAny, TxKeyWitness}

trait Wallet {
    def createTxKeyWitness[T <: MultisigTxTag](tx: MultisigTx[T]): TxKeyWitness
}
