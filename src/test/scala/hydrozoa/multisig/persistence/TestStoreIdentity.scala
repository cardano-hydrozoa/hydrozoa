package hydrozoa.multisig.persistence

import hydrozoa.config.head.initialization.InitializationParameters.HeadId
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.{AssetName, Blake2b_256, Hash, ScriptHash}
import scalus.uplc.builtin.ByteString

/** The [[StoreIdentity]] the persistence tests stamp their stores with.
  *
  * Every store open takes one, so a suite that only cares about bytes in and bytes out still needs
  * a value; these tests are not exercising configuration agreement. Suites that *are* — see
  * `RocksDbBackendStoreTest` — vary one field at a time via [[mkIdentity]].
  */
object TestStoreIdentity {

    def mkIdentity(
        headParamsHashByte: Byte = 0x11,
        headIdByte: Byte = 0x22,
        headAddressByte: Byte = 0x33,
        ownPeerId: PeerId = PeerId.Head(HeadPeerNumber(0))
    ): StoreIdentity = StoreIdentity(
      headParamsHash = Hash[Blake2b_256, Any](
        ByteString.fromArray(Array.fill[Byte](32)(headParamsHashByte))
      ),
      headId = HeadId(AssetName(ByteString.fromArray(Array.fill[Byte](16)(headIdByte)))),
      // A script address, as a real head's is: the head multisig script hash under Testnet.
      headAddress = ShelleyAddress(
        network = Network.Testnet,
        payment = ShelleyPaymentPart.Script(
          ScriptHash.fromArray(Array.fill[Byte](28)(headAddressByte))
        ),
        delegation = ShelleyDelegationPart.Null
      ),
      ownPeerId = ownPeerId
    )

    val default: StoreIdentity = mkIdentity()
}
