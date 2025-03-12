package hydrozoa

import hydrozoa.node.server.MultisigHeadStateL1

import scala.collection.mutable

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

// Transaction output index
// TODO: use BigInt
case class TxIx(ix: Long)

case class OutputRef[L <: Level](id: TxId, ix: TxIx)

def mkOutputRef[L <: Level](id: TxId, ix: TxIx): OutputRef[L] = OutputRef[L](id, ix)

sealed trait Level
final class L1 extends Level
final class L2 extends Level

// FIXME: parameterize AddressBech
// FIXME: migrate to Value
case class Output[L <: Level](address: AddressBechL1, coins: BigInt)

case class Utxo[L <: Level, F](ref: OutputRef[L], output: Output[L])

def mkUtxo[L <: Level, F](txId: TxId, txIx: TxIx, address: AddressBechL1, coins: BigInt) =
    Utxo[L, F](OutputRef[L](txId, txIx), Output(address, coins))

case class MutableUtxoSet[L <: Level, F](map: mutable.Map[OutputRef[L], Output[L]])

case class UtxoSet[L <: Level, F](map: Map[OutputRef[L], Output[L]])

object UtxoSet:
    def apply[L <: Level, F](mutableUtxoSet: MutableUtxoSet[L, F]): UtxoSet[L, F] =
        UtxoSet(mutableUtxoSet.map.toMap)

// Policy ID
case class PolicyId(policyId: String)

// A verification key of a participant, used on both L1 and L2
case class ParticipantVerificationKey(bytes: Array[Byte])

// A signing key of a participant, used on both L1 and L2
case class ParticipantSecretKey(bytes: Array[Byte])

case class Network(networkId: Int, protocolMagic: Long)

case class NativeScript(bytes: Array[Byte])

case class Datum(bytes: Array[Byte])

// UDiffTime
opaque type UDiffTime = BigInt

object UDiffTime:
    inline def apply(i: Int): UDiffTime = BigInt.apply(i)
extension (x: UDiffTime) def +(i: UDiffTime): UDiffTime = i + x

opaque type PosixTime = BigInt

// FIXME: move to another module
def timeCurrent: PosixTime = java.time.Instant.now.getEpochSecond
