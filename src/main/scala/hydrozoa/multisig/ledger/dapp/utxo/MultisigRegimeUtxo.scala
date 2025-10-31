package hydrozoa.multisig.ledger.dapp.utxo

import scalus.*
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{AssetName, TransactionInput, TransactionOutput, Value}
import scalus.cardano.txbuilder.TransactionUnspentOutput

final case class MultisigRegimeUtxo(
    multisigRegimeTokenName: AssetName,
    txId: TransactionInput,
    address: ShelleyAddress,
    value: Value
) {
    val asUtxo: TransactionUnspentOutput =
        TransactionUnspentOutput(
          txId,
          TransactionOutput.apply(
            address = address,
            value = value
          )
        )
}

object MultisigRegimeUtxo {

    /** If some tx extends this, it means that tx is producing it. */
    trait Produced {
        def multisigRegimeUtxoProduced: MultisigRegimeUtxo
    }

    /** If some tx extends this, it means that tx is spending it. */
    trait Spent {
        def multisigRegimeUtxoSpent: MultisigRegimeUtxo
    }

    /// ** If some args extend this, it means that args contain it. */
    // trait ToSpend {
    //    def multisigUtxoToSpend: MultisigUtxo
    // }
}
