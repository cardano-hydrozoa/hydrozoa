package hydrozoa.lib.cardano.scalus.ledger

import hydrozoa.config.head.network.CardanoNetwork
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.address.{ShelleyAddress, ShelleyDelegationPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.txbuilder.PubKeyWitness
import scalus.cardano.txbuilder.TransactionBuilderStep.*

/** This type exposes a (partially) type safe wrapper around a [[Utxo]] that allows it to be used as
  * collateral.
  *
  * A collateral utxo:
  *   - Cannot contain any tokens (only ADA)
  *   - Cannot be a script address
  *   - Must be a UTXO input
  *
  * Additional criteria that are not enforced by this type are:
  *   - Must be at least some percentage of the fee in the tx (concrete percentage decided by a
  *     protocol parameter) -
  *   - Can be the same UTXO entry as used in non-collateral tx input
  *   - Is consumed entirely (no change) if the contract execution fails during phase 2 validation
  *   - Is not consumed if phase 2 validation succeeds
  */
case class CollateralUtxo(input: TransactionInput, collateralOutput: CollateralOutput) {

    // TODO: These methods could probably be extracted into a "UtxoLike" trait
    final def add(using network: CardanoNetwork.Section): AddCollateral = AddCollateral(this.toUtxo)

    final def spend(using network: CardanoNetwork.Section): Spend =
        Spend(this.toUtxo, PubKeyWitness)

    def toUtxo(using network: CardanoNetwork.Section): Utxo =
        Utxo(input, collateralOutput.toOutput)
}

case class CollateralOutput(
    addrKeyHash: AddrKeyHash,
    delegationPart: ShelleyDelegationPart,
    coin: Coin,
    datumOption: Option[DatumOption],
    scriptRef: Option[ScriptRef]
) {

    def toOutput(using network: CardanoNetwork.Section): Babbage =
        Babbage(
          address = ShelleyAddress(
            network = network.network,
            payment = Key(addrKeyHash),
            delegation = delegationPart
          ),
          value = Value(coin),
          datumOption = datumOption,
          scriptRef = scriptRef
        )

    final def send(using network: CardanoNetwork.Section): Send = Send(toOutput)
}
