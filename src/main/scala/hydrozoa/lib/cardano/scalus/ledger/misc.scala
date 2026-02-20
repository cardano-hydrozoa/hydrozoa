package hydrozoa.lib.cardano.scalus.ledger

import scalus.cardano.ledger.{ProtocolParams, Utxo, Utxos}

extension (self: List[Utxo]) def asUtxos: Utxos = self.map(u => u.input -> u.output).toMap

extension (self: Utxos) def asUtxoList: List[Utxo] = self.toList.map(Utxo.apply)

extension (self: ProtocolParams)
    def withZeroFees: ProtocolParams =
        self.copy(txFeeFixed = 0, txFeePerByte = 0)

//

import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.{AddrKeyHash, StakeKeyHash, ScriptHash}
import scalus.cardano.onchain.plutus.v3.Address
import scalus.cardano.onchain.plutus.v1.{Credential, StakingCredential}
import scalus.cardano.onchain.plutus.prelude.Option as ScalusOption

def plutusAddressToShelley(addr: Address, network: Network): ShelleyAddress =
    val payment = addr.credential match
        case Credential.PubKeyCredential(pkh) =>
            ShelleyPaymentPart.Key(pkh.hash.asInstanceOf[AddrKeyHash])
        case Credential.ScriptCredential(sh) =>
            ShelleyPaymentPart.Script(sh.asInstanceOf[ScriptHash])

    val delegation = addr.stakingCredential match
        case ScalusOption.Some(StakingCredential.StakingHash(Credential.PubKeyCredential(pkh))) =>
            ShelleyDelegationPart.Key(pkh.hash.asInstanceOf[StakeKeyHash])
        case ScalusOption.Some(StakingCredential.StakingHash(Credential.ScriptCredential(sh))) =>
            ShelleyDelegationPart.Script(sh.asInstanceOf[ScriptHash])
        case _ => ShelleyDelegationPart.Null

    ShelleyAddress(network, payment, delegation)
