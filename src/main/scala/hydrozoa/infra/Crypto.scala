package hydrozoa.infra

import com.bloxbean.cardano.client.crypto.Blake2bUtil.{blake2bHash224, blake2bHash256}
import com.bloxbean.cardano.client.crypto.KeyGenUtil
import hydrozoa.{ParticipantSecretKey, ParticipantVerificationKey}

// Cryptographic hash functions used in Hydrozoa

// H28

// FIXME: how to protect the constructor?
case class H28(bytes: Array[Byte])

def mkH28(bytes: Array[Byte]): H28 =
    bytes.length match
        case 28 => H28(bytes)
// FIXME:  _ => what's the proper way?

// H32

// FIXME: how to protect the constructor?
case class H32(bytes: Array[Byte])

def mkH32(bytes: Array[Byte]): H32 = bytes.length match
    case 32 => H32(bytes)
// FIXME:  _ => what's the proper way?

// H28 hash function = Blake2b-224
val hydrozoaH28: Array[Byte] => H28 = mkH28.compose(blake2bHash224)

// H32 hash function - Blake2b-256
val hydrozoaH32: Array[Byte] => H32 = mkH32.compose(blake2bHash256)

// Generating keys for a node
def genNodeKey(): (ParticipantSecretKey, ParticipantVerificationKey) = {
    val keys = KeyGenUtil.generateKey
    (ParticipantSecretKey(keys.getSkey.getBytes), ParticipantVerificationKey(keys.getVkey.getBytes))
}
