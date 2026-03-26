package hydrozoa.lib.cardano.scalus.ledger

import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.txbuilder.PubKeyWitness
import scalus.cardano.txbuilder.TransactionBuilderStep.*

case class CollateralUtxo(
    input: TransactionInput,
    network: Network,
    addrKeyHash: AddrKeyHash,
    delegationPart: ShelleyDelegationPart,
    coin: Coin,
    datumOption: Option[DatumOption],
    scriptRef: Option[ScriptRef]
) {
    final def toUtxo: Utxo = Utxo(input, output)
    final def output: Babbage = Babbage(
      address = ShelleyAddress(
        network = network,
        payment = Key(addrKeyHash),
        delegation = delegationPart
      ),
      value = Value(coin),
      datumOption = datumOption,
      scriptRef = scriptRef
    )

    final val add: AddCollateral = AddCollateral(this.toUtxo)
    final val spend: Spend = Spend(this.toUtxo, PubKeyWitness)
    final val sendContinuing: Send = Send(this.output)
}

// TODO
//object CollateralUtxo {
//    def apply(utxo: Utxo) : Either[ ???, CollateralUtxo]
//}
