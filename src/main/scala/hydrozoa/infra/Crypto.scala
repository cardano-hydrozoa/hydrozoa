package hydrozoa.infra

import com.bloxbean.cardano.client.crypto.Blake2bUtil.{blake2bHash224, blake2bHash256}
import com.bloxbean.cardano.client.crypto.KeyGenUtil
import hydrozoa.{ParticipantSecretKey, ParticipantVerificationKey}

// Opaque types and functions for cryptographic hashes used in Hydrozoa.

object CryptoHash:

    // H28 - Blake2b224
    opaque type H28[A] = IArray[Byte]

    object H28 {
        private def apply[A](bytes: IArray[Byte]): H28[A] =
            require(bytes.length == 28, "Blake2b224 hash must be exactly 28 bytes long")
            bytes

        def hash[A](input: IArray[Byte]): H28[A] =
            apply(IArray.from[Byte](blake2bHash224(IArray.genericWrapArray(input).toArray)))

        // TODO: Remove once we migrate to IArray
        def hash_[A](input: Array[Byte]): H28[A] =
            apply(IArray.from[Byte](blake2bHash224(input)))

        extension [A](x: H28[A]) {
            def bytes: IArray[Byte] = x
        }
    }

    // H32 - Blake2b256
    opaque type H32[A] = IArray[Byte]

    object H32 {
        private def apply[A](bytes: IArray[Byte]): H32[A] =
            require(bytes.length == 32, "Blake2b256 hash must be exactly 32 bytes long")
            bytes

        def hash[A](input: IArray[Byte]): H32[A] =
            apply(IArray.from[Byte](blake2bHash256(IArray.genericWrapArray(input).toArray)))

        extension [A](x: H32[A]) {
            def bytes: IArray[Byte] = x
        }
    }

// Generating nodes keys

// Generating keys for a node
def genNodeKey(): (ParticipantSecretKey, ParticipantVerificationKey) = {
    val keys = KeyGenUtil.generateKey
    (ParticipantSecretKey(keys.getSkey.getBytes), ParticipantVerificationKey(keys.getVkey.getBytes))
}
