package hydrozoa.multisig.ledger.dapp.utxo

import scalus.*
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{AssetName, TransactionInput, TransactionOutput, Value}
import scalus.cardano.txbuilder.TransactionUnspentOutput

/** A treasury utxo output of the finalization tx. Contains fallback deposits from mltisig utxo,
  * residual equity from the treasury, and two head tokens. Doesn't contain datum since it's not
  * needed anymore and also to guarantee the size of upgraded settlement tx won't increase.
  */
final case class ResidualTreasuryUtxo(
    headTokenName: AssetName,
    multisigTokenName: AssetName,
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

object ResidualTreasuryUtxo {

    /** If SomeTx extends ResidualTreasuryUtxo.Spent it means that tx is spending it. */
    trait Spent {
        def residualTreasurySpent: ResidualTreasuryUtxo
    }

    /** If SomeTx extends ResidualTreasuryUtxo.Produced it means that tx is producing it. */
    trait Produced {
        def residualTreasuryProduced: ResidualTreasuryUtxo
    }

    /** If SomeTx extends ResidualTreasuryUtxo.MbProduced it means that tx produced it optionally.
      */
    trait MbProduced {
        final def mbResidualTreasuryProduced: Option[ResidualTreasuryUtxo] = this match
            case produced: (this.type & Produced) => Some(produced.residualTreasuryProduced)
            case _                                => None
    }

    /** If some args extend this, it means that args contain it. */
    trait ToSpend {
        def residualTreasuryToSpend: ResidualTreasuryUtxo
    }
}
