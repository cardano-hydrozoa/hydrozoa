package hydrozoa.multisig.ledger.dapp.utxo

import scalus.cardano.ledger.txbuilder.TransactionUnspentOutput

final case class RolloutUtxo(utxo: TransactionUnspentOutput)

object RolloutUtxo {
    trait Spent {
        def rolloutSpent: RolloutUtxo
    }
    
    trait Produced {
        def rolloutProduced: RolloutUtxo
    }

    trait MbProduced {
        final def mbRolloutProduced: Option[RolloutUtxo] = this match {
            case t: (this.type & Produced) => Some(t.rolloutProduced)
            case _ => None
        }
    }
}
