package hydrozoa.infra

import com.bloxbean.cardano.client.crypto.Blake2bUtil.{blake2bHash224, blake2bHash256}
import com.bloxbean.cardano.client.crypto.KeyGenUtil
import hydrozoa.{ParticipantSecretKey, ParticipantVerificationKey}

// Opaque types and functions for cryptographic hashes used in Hydrozoa.

object CryptoHash:

    // H28 - Blake2b224
    opaque type H28 = IArray[Byte]

    object H28 {
        def apply(bytes: IArray[Byte]): H28 =
            require(bytes.length == 28, "Blake2b224 hash must be exactly 28 bytes long")
            bytes

        def hash(input: IArray[Byte]): H28 =
            apply(IArray.from[Byte](blake2bHash224(IArray.genericWrapArray(input).toArray)))

        // Remove once we have
        def hash_(input: Array[Byte]): H28 =
            apply(IArray.from[Byte](blake2bHash224(input)))

        extension (x: H28) {
            def bytes: IArray[Byte] = x
        }
    }

    // H32 - Blake2b256
    opaque type H32 = IArray[Byte]

    object H32 {
        def apply(bytes: IArray[Byte]): H32 =
            require(bytes.length == 32, "Blake2b256 hash must be exactly 32 bytes long")
            bytes

        def hash(input: IArray[Byte]): H28 =
            apply(IArray.from[Byte](blake2bHash256(IArray.genericWrapArray(input).toArray)))

        extension (x: H32) {
            def bytes: IArray[Byte] = x
        }
    }

// Generating nodes keys

// Generating keys for a node
def genNodeKey(): (ParticipantSecretKey, ParticipantVerificationKey) = {
    val keys = KeyGenUtil.generateKey
    (ParticipantSecretKey(keys.getSkey.getBytes), ParticipantVerificationKey(keys.getVkey.getBytes))
}
