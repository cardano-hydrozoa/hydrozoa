package hydrozoa.lib.cardano.scalus

import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.Hash
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.Builtins.blake2b_224

object ShelleyAddressExtra:

    /** Creates enterprise Shelley address from a verification key.
      *
      * @param verificationKey
      * @param network
      * @return
      */
    def mkShelleyAddress(verificationKey: VerificationKey, network: Network): ShelleyAddress = {
        val spp = ShelleyPaymentPart.Key(Hash(blake2b_224(verificationKey)))
        val sdp = ShelleyDelegationPart.Null
        ShelleyAddress(network = network, payment = spp, delegation = sdp)
    }
