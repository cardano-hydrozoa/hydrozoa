package hydrozoa.head

// Serialized Cardano tx
case class Tx(bytes: Array[Byte])

// Transaction ID
case class TxId(bytes: Array[Byte])

// Policy ID
case class PolicyId(policyId: String)

// Verification keys of participant, used on both L1 and L2
case class ParticipantVerificationKey(bytes: Array[Byte])

// Output reference
case class UtxoRef()