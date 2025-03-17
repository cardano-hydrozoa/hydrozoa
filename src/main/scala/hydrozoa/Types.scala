package hydrozoa

import hydrozoa.node.server.MultisigHeadStateL1

import scala.collection.mutable

/** Cardano txs in serialized form
  * @param bytes
  *   CBOR bytes FIXME: use IArray
  * @tparam L
  *   phantom parameter to distinguish levels (L1, L2, Any)
  */
case class Tx[+L <: AnyLevel](bytes: Array[Byte])

type TxAny = Tx[AnyLevel]
type TxL1 = Tx[L1]

object TxL1:
    def apply(bytes: Array[Byte]): TxL1 = Tx[L1](bytes)

type TxL2 = Tx[L2]

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
// TODO: use Int, Long is too long
case class TxIx(ix: Long)

case class OutputRef[L <: AnyLevel](id: TxId, ix: TxIx)

def mkOutputRef[L <: AnyLevel](id: TxId, ix: TxIx): OutputRef[L] = OutputRef[L](id, ix)

sealed trait AnyLevel
sealed trait L1 extends AnyLevel
sealed trait L2 extends L1

// FIXME: parameterize AddressBech
// FIXME: migrate to Value
case class Output[L <: AnyLevel](address: AddressBechL1, coins: BigInt)

case class Utxo[L <: AnyLevel, F](ref: OutputRef[L], output: Output[L])

def mkUtxo[L <: AnyLevel, F](txId: TxId, txIx: TxIx, address: AddressBechL1, coins: BigInt) =
    Utxo[L, F](OutputRef[L](txId, txIx), Output(address, coins))

case class MutableUtxoSet[L <: AnyLevel, F](map: mutable.Map[OutputRef[L], Output[L]])

case class UtxoSet[L <: AnyLevel, F](map: Map[OutputRef[L], Output[L]])

object UtxoSet:
    def apply[L <: AnyLevel, F](mutableUtxoSet: MutableUtxoSet[L, F]): UtxoSet[L, F] =
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
