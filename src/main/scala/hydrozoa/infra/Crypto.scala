package hydrozoa.infra

import com.bloxbean.cardano.client.crypto.Blake2bUtil.{blake2bHash224, blake2bHash256}
import com.bloxbean.cardano.client.crypto.KeyGenUtil
import hydrozoa.{ParticipantSecretKey, ParticipantVerificationKey}

// Types and functions for cryptographic hashes used in Hydrozoa

// H28 - Blake2b224

case class H28 private (bytes: Array[Byte])

object H28 {
    def apply(bytes: Array[Byte]): H28 =
        require(bytes.length == 28, "Blake2b224 hash should be exactly 28 bytes long")
        new H28(bytes)
}

val hydrozoaH28: Array[Byte] => H28 = H28.apply.compose(blake2bHash224)

// H32 - Blake2b256

case class H32 private (bytes: Array[Byte])

object H32 {
    def apply(bytes: Array[Byte]): H32 =
        require(bytes.length == 32, "Blake2b256 hash should be exactly 32 bytes long")
        new H32(bytes)
}

// H32 hash function - Blake2b-256
val hydrozoaH32: Array[Byte] => H32 = H32.apply.compose(blake2bHash256)

// Generating nodes keys

// Generating keys for a node
def genNodeKey(): (ParticipantSecretKey, ParticipantVerificationKey) = {
    val keys = KeyGenUtil.generateKey
    (ParticipantSecretKey(keys.getSkey.getBytes), ParticipantVerificationKey(keys.getVkey.getBytes))
}
