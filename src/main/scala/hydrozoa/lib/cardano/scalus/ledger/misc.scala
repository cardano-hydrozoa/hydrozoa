package hydrozoa.lib.cardano.scalus.ledger

import monocle.Focus
import monocle.syntax.all.focus
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.{AddrKeyHash, ProtocolParams, ScriptHash, StakeKeyHash, TaggedSortedSet, Transaction, TransactionWitnessSet, Utxo, Utxos, VKeyWitness}
import scalus.cardano.onchain.plutus.prelude.Option as ScalusOption
import scalus.cardano.onchain.plutus.v1.{Credential, StakingCredential}
import scalus.cardano.onchain.plutus.v3.Address
import scalus.cardano.txbuilder.keepRawL

extension (self: List[Utxo]) def asUtxos: Utxos = self.map(u => u.input -> u.output).toMap

extension (self: Utxos) def asUtxoList: List[Utxo] = self.toList.map(Utxo.apply)

extension (self: Transaction)
    /** Useful for pre-processing before equality checking between signed and unsigned transactions
      * @param self
      */
    def stripVKeyWitnesses: Transaction = self
        .focus(_.witnessSetRaw)
        .andThen(keepRawL[TransactionWitnessSet]())
        .andThen(Focus[TransactionWitnessSet](_.vkeyWitnesses))
        .replace(TaggedSortedSet.empty[VKeyWitness])

extension (self: ProtocolParams)
    def withZeroFees: ProtocolParams =
        self.copy(txFeeFixed = 0, txFeePerByte = 0)

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
