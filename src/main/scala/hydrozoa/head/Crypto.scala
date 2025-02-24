package hydrozoa.head

import com.bloxbean.cardano.client.crypto.Blake2bUtil.{blake2bHash224, blake2bHash256}

// Cryptographic hash functions used in Hydrozoa

// H24

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
