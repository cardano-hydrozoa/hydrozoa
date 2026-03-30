package hydrozoa.lib.cardano.scalus

import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart}
import scalus.cardano.ledger.AddrKeyHash
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.Builtins.blake2b_224
import scalus.uplc.builtin.ByteString
import scalus.|>

object VerificationKeyExtra:

    extension (self: VerificationKey)

        /** Creates a Shelley address from a verification key, optionally attaching a delegation
          * part.
          *
          * @param network
          * @param delegationPart
          * @return
          */
        def shelleyAddress(
            network: Network,
            delegationPart: ShelleyDelegationPart = ShelleyDelegationPart.Null
        ): ShelleyAddress =
            ShelleyAddress(
              network = network,
              payment = Key(addrKeyHash),
              delegation = delegationPart
            )

        def pubKeyHash: PubKeyHash =
            PubKeyHash(hash)

        def addrKeyHash: AddrKeyHash = AddrKeyHash(hash)

        private def hash: ByteString = self.bytes
            |> ByteString.fromArray
            |> blake2b_224
