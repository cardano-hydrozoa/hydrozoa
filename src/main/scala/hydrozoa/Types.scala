package hydrozoa

import hydrozoa.l1.multisig.state.MultisigUtxoTag

import scala.collection.mutable

/** Cardano network layers.
  */
sealed trait AnyLevel
sealed trait L1 extends AnyLevel
sealed trait L2 extends L1

/** Cardano txs in serialized form.
  * @param bytes
  *   CBOR bytes FIXME: use IArray
  * @tparam L
  *   phantom parameter to distinguish tx level (L1, L2, Any)
  */
case class Tx[+L <: AnyLevel](bytes: Array[Byte])

type TxAny = Tx[AnyLevel]
type TxL1 = Tx[L1]

object TxL1:
    def apply(bytes: Array[Byte]): TxL1 = Tx[L1](bytes)

type TxL2 = Tx[L2]

object TxL2:
    def apply(bytes: Array[Byte]): TxL2 = Tx[L2](bytes)

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

final case class OutputRef[L <: AnyLevel](txId: TxId, outputIx: TxIx)

type OutputRefL1 = OutputRef[L1]
type OutputRefL2 = OutputRef[L2]

object OutputRefL1:
    def apply(id: TxId, ix: TxIx): OutputRef[L1] = OutputRef[L1](id, ix)

object OutputRefL2:
    def apply(id: TxId, ix: TxIx): OutputRef[L2] = OutputRef[L2](id, ix)

// FIXME: parameterize AddressBech
// FIXME: migrate to Value
case class Output[L <: AnyLevel](address: AddressBechL1, coins: BigInt)

case class Utxo[L <: AnyLevel, F <: MultisigUtxoTag](ref: OutputRef[L], output: Output[L])

def mkUtxo[L <: AnyLevel, T <: MultisigUtxoTag](
    txId: TxId,
    txIx: TxIx,
    address: AddressBechL1,
    coins: BigInt
) =
    Utxo[L, T](OutputRef[L](txId, txIx), Output(address, coins))

case class UtxoSetMutable[L <: AnyLevel, F](map: mutable.Map[OutputRef[L], Output[L]])

case class UtxoSet[L <: AnyLevel, F](map: Map[OutputRef[L], Output[L]])

object UtxoSet:
    def apply[L <: AnyLevel, F](mutableUtxoSet: UtxoSetMutable[L, F]): UtxoSet[L, F] =
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
opaque type UDiffTimeMilli = BigInt

object UDiffTimeMilli:
    inline def apply(i: Int): UDiffTimeMilli = BigInt.apply(i)
extension (x: UDiffTimeMilli) def +(i: UDiffTimeMilli): UDiffTimeMilli = i + x

opaque type PosixTime = BigInt

// FIXME: move to another module
def timeCurrent: PosixTime = java.time.Instant.now.getEpochSecond
