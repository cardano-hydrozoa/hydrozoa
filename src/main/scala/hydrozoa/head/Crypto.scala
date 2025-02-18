package hydrozoa.head

import com.bloxbean.cardano.client.crypto.Blake2bUtil.{blake2bHash224, blake2bHash256}

// Cryptographic hash functions used in Hydrozoa

// H24

// FIXME: how to protect the constructor?
case class H24(bytes: Array[Byte])

def mkH24(bytes: Array[Byte]): H24 = bytes.length match
  case 24 => H24(bytes)
// FIXME:  _ => what's the proper way?

// H32

// FIXME: how to protect the constructor?
case class H32(bytes: Array[Byte])

def mkH32(bytes: Array[Byte]): H32 = bytes.length match
  case 32 => H32(bytes)
// FIXME:  _ => what's the proper way?

// H28 hash function = Blake2b-224
val hydrozoaH28: Array[Byte] => H24 = mkH24.compose(blake2bHash224)

// H32 hash function - Blake2b-256
val hydrozoaH32: Array[Byte] => H32 = mkH32.compose(blake2bHash256)
