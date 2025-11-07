package hydrozoa.multisig.ledger.dapp.utxo

import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import scalus.*
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{AssetName, ScriptRef, TransactionInput, TransactionOutput, Value}
import scalus.cardano.txbuilder.TransactionUnspentOutput

final case class MultisigRegimeUtxo(
    multisigRegimeTokenName: AssetName,
    utxoId: TransactionInput,
    address: ShelleyAddress,
    value: Value,
    script: HeadMultisigScript
) {
    val asUtxo: TransactionUnspentOutput =
        TransactionUnspentOutput(
          utxoId,
          TransactionOutput.apply(
            address = address,
            value = value,
              datumOption = None,
              scriptRef = Some(ScriptRef.apply(script.script))
          )
        )
}

object MultisigRegimeUtxo {

    def apply(multisigRegimeTokenName: AssetName, utxoId: TransactionInput, output: TransactionOutput, script: HeadMultisigScript): MultisigRegimeUtxo =
        MultisigRegimeUtxo(
            multisigRegimeTokenName,
            utxoId,
            output.address.asInstanceOf[ShelleyAddress],
            output.value,
            script
        )
    
    /** If some tx extends this, it means that tx is producing it. */
    trait Produced {
        def multisigRegimeUtxoProduced: MultisigRegimeUtxo
    }

    /** If some tx extends this, it means that tx is spending it. */
    trait Spent {
        def multisigRegimeUtxoSpent: MultisigRegimeUtxo
    }

}
