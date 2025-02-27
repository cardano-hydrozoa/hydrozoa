package hydrozoa.infra

import com.bloxbean.cardano.client.address as bloxbean
import hydrozoa.{AddressBechL1, AddressBechL2}
import scalus.bloxbean.Interop.getAddress
import scalus.ledger.api.v1

def decodeBech32AddressL1(address: AddressBechL1): v1.Address =
    getAddress(bloxbean.Address(address.bech32))

def decodeBech32AddressL2(address: AddressBechL2): v1.Address =
    getAddress(bloxbean.Address(address.bech32))
