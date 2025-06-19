package hydrozoa

import hydrozoa.l1.multisig.state.MultisigUtxoTag

import scala.collection.mutable

/** Cardano network layers.
  */
sealed trait AnyLevel derives CanEqual
sealed trait L1 extends AnyLevel derives CanEqual
sealed trait L2 extends AnyLevel derives CanEqual

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
    inline def apply(bytes: Array[Byte]): TxL1 = Tx[L1](bytes)

type TxL2 = Tx[L2]

object TxL2:
    def apply(bytes: Array[Byte]): TxL2 = Tx[L2](bytes)

// Bech32 addresses
case class AddressBech[+L <: AnyLevel](bech32: String) derives CanEqual:
    def asL1: AddressBech[L1] = AddressBech[L1](bech32)
    def asL2: AddressBech[L2] = AddressBech[L2](bech32)

type AddressBechL1 = AddressBech[L1]
type AddressBechL2 = AddressBech[L2]

// Transaction key witness
case class TxKeyWitness(signature: Array[Byte], vkey: Array[Byte])

// Transaction hash
case class TxId(hash: String) derives CanEqual

// Transaction output index

// transaction_index = uint .size 2, so Int, which is 32 signed is just on the mark.
// TODO: we can also use Char probably, it's unsigned and it's 16-bit long
//  Currently, the absence of Schema for Char prevents us from doing so.
case class TxIx(ix: Int) derives CanEqual

final case class UtxoId[L <: AnyLevel](txId: TxId, outputIx: TxIx) derives CanEqual

type UtxoIdL1 = UtxoId[L1]
type UtxoIdL2 = UtxoId[L2]

object UtxoIdL1:
    def apply(id: TxId, ix: TxIx): UtxoId[L1] = UtxoId(id, ix)

object UtxoIdL2:
    def apply(id: TxId, ix: TxIx): UtxoId[L2] = UtxoId(id, ix)

type Tokens = Map[PolicyId, Map[TokenName, BigInt]]

val emptyTokens: Tokens = Map.empty

// TODO: migrate to Value
case class Output[L <: AnyLevel](
    address: AddressBech[L],
    coins: BigInt,
    tokens: Tokens,
    mbInlineDatum: Option[String] = None
)

// TODO: Unsound in general
object Output:
    def apply[L <: AnyLevel](o: OutputNoTokens[L]): Output[L] =
        new Output[L](o.address, o.coins, emptyTokens, o.mbInlineDatum)

type OutputL1 = Output[L1]
type OutputL2 = Output[L2]

/* This is useful since I was not able to make Tapir generate schema for Output:

[error] -- Error: /home/euonymos/src/cardano-hydrozoa/hydrozoa/src/main/scala/hydrozoa/node/rest/NodeRestApi.scala:215:4
[error] 215 |    Schema.derived[StateL2Response]
[error]     |    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
[error]     |    method deriveSubtype is declared as `inline`, but was not inlined
[error]     |
[error]     |    Try increasing `-Xmax-inlines` above 32
[error]     |---------------------------------------------------------------------------
[error]     |Inline stack trace
[error]     |- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
[error]     |This location contains code that was inlined from impl.scala:208
[error]     |- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
[error]     |This location contains code that was inlined from impl.scala:208
[error]     |- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
[error]     |This location contains code that was inlined from impl.scala:208
[error]     |- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
[error]     |This location contains code that was inlined from impl.scala:208
[error]     |- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
[error]     |This location contains code that was inlined from impl.scala:208
[error]     |- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
[error]     |This location contains code that was inlined from impl.scala:208
[error]      ---------------------------------------------------------------------------
 */

case class OutputNoTokens[L <: AnyLevel](
    address: AddressBech[L],
    coins: BigInt,
    mbInlineDatum: Option[String] = None
)

/** ---------------------------------------------------------------------------------------------
  * UTxO
  * ---------------------------------------------------------------------------------------------
  */

case class Utxo[L <: AnyLevel](ref: UtxoId[L], output: Output[L])

// TODO: Lossy conversion
object OutputNoTokens:
    def apply[L <: AnyLevel](o: Output[L]): OutputNoTokens[L] =
        new OutputNoTokens[L](o.address, o.coins, o.mbInlineDatum)

object Utxo:
    def apply[L <: AnyLevel](
        txId: TxId,
        txIx: TxIx,
        address: AddressBech[L],
        coins: BigInt,
        tokens: Tokens,
        mbInlineDatum: Option[String] = None
    ) =
        new Utxo[L](UtxoId[L](txId, txIx), Output(address, coins, tokens, mbInlineDatum))

case class TaggedUtxo[L <: AnyLevel, F <: MultisigUtxoTag](unTag: Utxo[L])

object TaggedUtxo:
    def apply[L <: AnyLevel, T <: MultisigUtxoTag](
        txId: TxId,
        txIx: TxIx,
        address: AddressBech[L],
        coins: BigInt,
        tokens: Tokens, // = Map.empty,
        mbInlineDatum: Option[String] = None
    ) = new TaggedUtxo[L, T](Utxo.apply(txId, txIx, address, coins, tokens, mbInlineDatum))

/** ---------------------------------------------------------------------------------------------
  * UTxO Set
  * ---------------------------------------------------------------------------------------------
  */

type UtxoMap[L <: AnyLevel] = Map[UtxoId[L], Output[L]]

case class UtxoSet[L <: AnyLevel](utxoMap: UtxoMap[L])

type UtxoSetL1 = UtxoSet[L1]
type UtxoSetL2 = UtxoSet[L2]

object UtxoSet:
    def apply[L <: AnyLevel](): UtxoSet[L] =
        new UtxoSet(Map.empty)

    def apply[L <: AnyLevel](map: UtxoMap[L]): UtxoSet[L] =
        new UtxoSet(map)

    def apply[L <: AnyLevel, F](mutableUtxoSet: TaggedUtxoSetMutable[L, F]): UtxoSet[L] =
        new UtxoSet(mutableUtxoSet.utxoMap.toMap)

case class TaggedUtxoSet[L <: AnyLevel, F <: MultisigUtxoTag](unTag: UtxoSet[L])

object TaggedUtxoSet:
    def apply[L <: AnyLevel, F <: MultisigUtxoTag](): TaggedUtxoSet[L, F] =
        new TaggedUtxoSet[L, F](UtxoSet.apply())

    def apply[L <: AnyLevel, F <: MultisigUtxoTag](map: UtxoMap[L]): TaggedUtxoSet[L, F] =
        new TaggedUtxoSet[L, F](UtxoSet.apply(map))

    def apply[L <: AnyLevel, F <: MultisigUtxoTag](
        mutableUtxoSet: TaggedUtxoSetMutable[L, F]
    ): TaggedUtxoSet[L, F] =
        new TaggedUtxoSet[L, F](UtxoSet.apply(mutableUtxoSet.utxoMap.toMap))

case class TaggedUtxoSetMutable[L <: AnyLevel, F](utxoMap: mutable.Map[UtxoId[L], Output[L]])

// Policy ID
case class PolicyId(policyId: String)

// A verification key of a peer, used on both L1 and L2
case class VerificationKeyBytes(bytes: Array[Byte])

object VerificationKeyBytes:
    def applyI(bytes: IArray[Byte]): VerificationKeyBytes =
        new VerificationKeyBytes(IArray.genericWrapArray(bytes).toArray)

// A signing key of a peer, used on both L1 and L2
case class SigningKeyBytes(bytes: Array[Byte])

case class Network(networkId: Int, protocolMagic: Long)

case class NativeScript(bytes: Array[Byte])

case class CurrencySymbol(bytes: IArray[Byte])

case class Datum(bytes: Array[Byte])

// Shall we use bytes as in other types?
case class TokenName(tokenNameHex: String)

// UDiffTime
opaque type UDiffTimeMilli = BigInt

object UDiffTimeMilli:
    inline def apply(i: Int): UDiffTimeMilli = BigInt.apply(i)
extension (x: UDiffTimeMilli) def +(i: UDiffTimeMilli): UDiffTimeMilli = i + x

type PosixTime = BigInt

// FIXME: move to another module
def timeCurrent: PosixTime = java.time.Instant.now.getEpochSecond

// FIXME: should be parameter
val networkL1static = Network(0, 42)

val hydrozoaL2Network = Network(0, 42)
