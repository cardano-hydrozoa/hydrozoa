package hydrozoa

import com.bloxbean.cardano.client.common.model.Network
import hydrozoa.l1.multisig.state.MultisigUtxoTag

import scala.collection.mutable

/** Cardano network layers.
  */
sealed trait AnyLevel
sealed trait L1 extends AnyLevel
sealed trait L2 extends AnyLevel

/** Cardano txs in a serialized form.
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
case class AddressBechL2(bech32: String) {
    def asL1: AddressBechL1 = AddressBechL1(bech32)
}

// Transaction key witness
case class TxKeyWitness(signature: Array[Byte], vkey: Array[Byte])

// Transaction hash
case class TxId(hash: String)

// Transaction output index
// TODO: use Int, Long is too long
// transaction_index = uint .size 2
case class TxIx(ix: Long)

final case class UtxoId[L <: AnyLevel](txId: TxId, outputIx: TxIx)

type UtxoIdL1 = UtxoId[L1]
type UtxoIdL2 = UtxoId[L2]

object UtxoIdL1:
    def apply(id: TxId, ix: TxIx): UtxoId[L1] = UtxoId(id, ix)

object UtxoIdL2:
    def apply(id: TxId, ix: TxIx): UtxoId[L2] = UtxoId(id, ix)

// FIXME: parameterize AddressBech
// FIXME: migrate to Value
case class Output[L <: AnyLevel](address: AddressBechL1, coins: BigInt)

type OutputL1 = Output[L1]
type OutputL2 = Output[L2]

// FIXME: We also neew Utxo without MltisigUtxoTag
case class Utxo[L <: AnyLevel, F <: MultisigUtxoTag](ref: UtxoId[L], output: Output[L])

def mkUtxo[L <: AnyLevel, T <: MultisigUtxoTag](
    txId: TxId,
    txIx: TxIx,
    address: AddressBechL1,
    coins: BigInt
) =
    Utxo[L, T](UtxoId[L](txId, txIx), Output(address, coins))

case class UtxoSetMutable[L <: AnyLevel, F](map: mutable.Map[UtxoId[L], Output[L]])

case class UtxoSet[L <: AnyLevel, F](map: Map[UtxoId[L], Output[L]])

object UtxoSet:
    def apply[L <: AnyLevel, F](mutableUtxoSet: UtxoSetMutable[L, F]): UtxoSet[L, F] =
        UtxoSet(mutableUtxoSet.map.toMap)

// Policy ID
case class PolicyId(policyId: String)

// A verification key of a participant, used on both L1 and L2
case class PeerPublicKeyBytes(bytes: Array[Byte])

// A signing key of a participant, used on both L1 and L2
case class PeerPrivateKeyBytes(bytes: Array[Byte])

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

// FIXME: should be parameter
val networkL1static = Network(0, 42)

val hydrozoaL2Network = Network(0, 42)
