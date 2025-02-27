package hydrozoa

// Serialized L1 Cardano tx
case class L1Tx(bytes: Array[Byte])

// Serialized Address
case class L1Address(bytes: Array[Byte])

// Bech32 addresses
case class AddressBechL1(bech32: String)
case class AddressBechL2(bech32: String)

// Transaction key witness
case class TxKeyWitness(signature: Array[Byte], vkey: Array[Byte])

// Transaction ID
case class TxId(hash: String)

// Transacion output index
case class TxIx(ix: Long)

// Policy ID
case class PolicyId(policyId: String)

// A verification key of a participant, used on both L1 and L2
case class ParticipantVerificationKey(bytes: Array[Byte])

// A signing key of a participant, used on both L1 and L2
case class ParticipantSecretKey(bytes: Array[Byte])

case class Network(networkId: Int, protocolMagic: Long)

case class NativeScript(bytes: Array[Byte])

case class Datum(bytes: Array[Byte])
