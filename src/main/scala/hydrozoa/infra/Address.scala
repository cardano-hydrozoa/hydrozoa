package hydrozoa.infra

import com.bloxbean.cardano.client.address.AddressProvider.{getBaseAddress, getEntAddress}
import com.bloxbean.cardano.client.address.Credential.{fromKey, fromScript}
import com.bloxbean.cardano.client.address.{Address, Credential}
import com.bloxbean.cardano.client.common.model.Network
import hydrozoa.{AddressBechL1, AddressBechL2, hydrozoaL2Network}
import scalus.bloxbean.Interop.getAddress
import scalus.ledger.api.v1
import scalus.ledger.api.v1.Credential.{PubKeyCredential, ScriptCredential}
import scalus.ledger.api.v1.StakingCredential.{StakingHash, StakingPtr}
import scalus.prelude.Maybe.{Just, Nothing}

def decodeBech32AddressL1(address: AddressBechL1): v1.Address =
    getAddress(Address(address.bech32))

def decodeBech32AddressL2(address: AddressBechL2): v1.Address =
    getAddress(Address(address.bech32))


/** Takes Plutus Address from datum and makes a Hydrozoa L2 address.
 */
def plutusAddressAsL2(address: v1.Address): AddressBechL2 =
    AddressBechL2(addressToBloxbean(hydrozoaL2Network.toBloxbean, address).getAddress)

def addressToBloxbean(network: Network, address: v1.Address): Address =

    val credential: Credential = credentialToBloxbean(address.credential)

    address.stakingCredential match
        case Nothing => getEntAddress(credential, network)
        case Just(stakingCredential) =>
            stakingCredential match
                case StakingHash(stakingHash) =>
                    val stakingCredential: Credential = credentialToBloxbean(stakingHash)
                    getBaseAddress(credential, stakingCredential, network)
                case StakingPtr(_, _, _) => ??? // TODO do we need pointers?

def credentialToBloxbean(credential: v1.Credential): Credential =
    credential match
        case PubKeyCredential(hash) => fromKey(hash.hash.bytes)
        case ScriptCredential(hash) => fromScript(hash.bytes)
