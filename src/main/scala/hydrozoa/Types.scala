package hydrozoa

/** This package defines wrapped types that primarily help to distinguish between similar values
  * that can appear at both L1 and L2, but where the semantics of those values change depending on
  * the layer. The underlying types are, where possible, using the upstream Scalus types.
  *
  * The mechanism to define the types was derived by a response from by Claude Sonnet 4 on
  * 2025-08-08. It is as follows:
  *   - Each wrapped type is defined as an opaque type synonym associated with an object. Use a type
  *     synonym should ensure zero runtime overhead, similar to a haskell newtype (as contrasted
  *     with a case class, which would be akin to wrapping a type using a `data` declaration in
  *     haskell)
  *   - an `apply` method is defined for each type
  *   - a `Conversion[$type, $wrappedType]` given is defined. This allows using methods from the
  *     underlying type directly.
  *     - Note that in many circumstances, this will require enabling implicit conversions in order
  *       to avoid compiler warnings. This is intended. See
  *       https://docs.scala-lang.org/scala3/book/ca-implicit-conversions.html
  *   - Outside of the object, a type synonym is given to allow declarations like `val myTx : Tx =
  *     (...)` instead of `val myTx : Tx.Tx = (...)`
  *   - Extension methods are defined within the object on the type where appropriate, include an
  *     `untagged` method to get the wrapped value.
  *
  * The result of all of this should be that you are able to pass the newtype where you would expect
  * a value of the underlying type, but not vice versa.
  */

/* TODO/Peter's Note: Full type safety with these types would require controlling introduction of their terms.
Right now, for example, we allow unrestricted production of a "UtxoIdL2" from any TransactionInput. The way I would
prefer to do this is by:
  - renaming the methods that are currently exposed in `apply` as something like
    `unsafeAsL2 : TransactionInput => UtxoId[L2]` in order to give an indication that this is the place where the
    developer/reviewer/auditor is responsible for verifying the pre-conditions that aren't captured in the type system
  - Examining the places in our code where we do actual produce values of these types and trying as much as possible
    to a limited set of boundaries. In particular, our tx builders would need to produce L1 tagged types; serialization
    boundaries need to produce both L1/L2 tagged types; bloxbean/blockfrost chain queries produce L1; L2 ledger produces
    L2; etc.
 */

import scala.language.implicitConversions
import scalus.builtin.Builtins.blake2b_224
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data, ToData}
import scalus.cardano.address.{Address as SAddress, ShelleyAddress}
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.{Context, State, UtxoEnv}
import scalus.cardano.ledger.{Utxo as SUtxo, *}
import scalus.ledger.api.v3
import scalus.ledger.api.v3.PubKeyHash

/** Cardano network layers.
  */
sealed trait AnyLayer derives CanEqual
sealed trait L1 extends AnyLayer derives CanEqual
sealed trait L2 extends AnyLayer derives CanEqual

//////////////////////////////////////////////////////////
// Address

object Address:
    opaque type Address[+L <: AnyLayer] = ShelleyAddress

    given [L <: AnyLayer]: Conversion[Address[L], ShelleyAddress] = identity

    def apply[L <: AnyLayer](addr: ShelleyAddress): Address[L] = addr

    given fromDataAddress[L <: AnyLayer]: ToData[Address[L]] = (addr: Address[L]) =>
        toData(LedgerToPlutusTranslation.getAddress(addr.untagged))

    /** Assumes a Shelley address and that the developer is asserting the correct layer */
    def unsafeFromBech32[L <: AnyLayer](addr: String): Address[L] =
        Address[L](SAddress.fromBech32(addr).asInstanceOf[ShelleyAddress])

    extension [L <: AnyLayer](addr: Address[L])
        def untagged: ShelleyAddress = identity[ShelleyAddress](addr)

    def fromByteString[L <: AnyLayer](bytes: ByteString): Option[Address[L]] =
        SAddress.fromByteString(bytes) match {
            case sa: ShelleyAddress => Some(Address[L](sa))
            case _                  => None
        }

type Address[L <: AnyLayer] = Address.Address[L]
type AddressL1 = Address.Address[L1]
type AddressL2 = Address.Address[L2]

///////////////////////////////////////////////////////
// Ed25519 Signatures

object Ed25519Signature:
    opaque type Ed25519Signature = IArray[Byte]
    def apply(signature: IArray[Byte]): Ed25519Signature = signature
    given Conversion[Ed25519Signature, IArray[Byte]] = identity
    given Conversion[Ed25519Signature, Array[Byte]] = sig => IArray.genericWrapArray(sig).toArray
    extension (signature: Ed25519Signature) def untagged: IArray[Byte] = identity(signature)

type Ed25519Signature = Ed25519Signature.Ed25519Signature

object Ed25519SignatureHex:
    opaque type Ed25519SignatureHex = String
    def apply(signature: String): Ed25519SignatureHex = signature
    given Conversion[Ed25519SignatureHex, String] = identity
    extension (signature: Ed25519SignatureHex) def untagged: String = identity(signature)

type Ed25519SignatureHex = Ed25519SignatureHex.Ed25519SignatureHex

//////////////////////////////////////////////////////////////////
// UtxoId, TxIx

object UtxoId:
    opaque type UtxoId[L <: AnyLayer] = TransactionInput

    def apply[L <: AnyLayer](transactionId: TransactionHash, index: Int): UtxoId[L] =
        UtxoId[L](TransactionInput(transactionId, index))

    def apply[L <: AnyLayer](utxoId: TransactionInput): UtxoId[L] = utxoId

    given [L <: AnyLayer]: CanEqual[UtxoId[L], UtxoId[L]] = CanEqual.derived
    given [L <: AnyLayer]: Conversion[UtxoId[L], TransactionInput] = identity
    extension [L <: AnyLayer](utxoId: UtxoId[L]) def untagged: TransactionInput = identity(utxoId)

type UtxoId[L <: AnyLayer] = UtxoId.UtxoId[L]
type UtxoIdL1 = UtxoId.UtxoId[L1]
type UtxoIdL2 = UtxoId.UtxoId[L2]

object UtxoIdL1:
    def apply(ti: TransactionInput): UtxoId[L1] = UtxoId(ti)

object UtxoIdL2:
    def apply(ti: TransactionInput): UtxoId[L2] = UtxoId(ti)

    case class TxIx(ix: Int) derives CanEqual

object TxIx:
    opaque type TxIx = Int
    def apply(i: Int): TxIx = i
    given Conversion[TxIx, Int] = _.toInt
    extension (txIx: TxIx) def untagged: Int = identity(txIx)

type TxIx = TxIx.TxIx

///////////////////////////////////////////////////////////////////
// Output

// TODO: Unsound in general
object Output:
    opaque type Output[L <: AnyLayer] = Babbage
    def apply[L <: AnyLayer](o: Babbage): Output[L] = o
    given [L <: AnyLayer]: Conversion[Output[L], Babbage] = identity
    extension [L <: AnyLayer](output: Output[L]) def untagged: Babbage = identity(output)

type Output[L <: AnyLayer] = Output.Output[L]
type OutputL1 = Output.Output[L1]
type OutputL2 = Output.Output[L2]

object OutputNoTokens:
    opaque type OutputNoTokens[L <: AnyLayer] = Babbage
    def apply[L <: AnyLayer](o: Babbage): OutputNoTokens[L] = {
        require(o.value.assets == MultiAsset.empty)
        o
    }
    given [L <: AnyLayer]: Conversion[OutputNoTokens[L], Babbage] = identity
    given [L <: AnyLayer]: Conversion[OutputNoTokens[L], Output[L]] = identity
    extension [L <: AnyLayer](output: OutputNoTokens[L]) def untagged: Babbage = identity(output)

type OutputNoTokens[L <: AnyLayer] = OutputNoTokens.OutputNoTokens[L]

/* ---------------------------------------------------------------------------------------------
 * UTxO
 * Question: It seems that we go between using this type and just a raw tuple `(UtxoId[L], Output[L])`.
 * If we use the tuple form, then we can ue an opaque type alias as above. Should we?
 * ---------------------------------------------------------------------------------------------
 */

/** A UTxO should reflect a _matching_ input and output. This condition is not presently enforced at
  * the type level.
  */
object Utxo:
    opaque type Utxo[L <: AnyLayer] = (UtxoId[L], Output[L])
    def apply[L <: AnyLayer](io: (UtxoId[L], Output[L])): Utxo[L] = io
    def apply[L <: AnyLayer](input: UtxoId[L], output: Output[L]): Utxo[L] = (input, output)
    def apply[L <: AnyLayer](
        txId: TransactionHash,
        txIx: Int,
        address: Address[L],
        value: Value,
        mbInlineDatum: Option[DatumOption.Inline] = None
    ): Utxo[L] =
        (
          UtxoId[L](TransactionInput(txId, txIx)),
          Output[L](
            Babbage(
              address = address,
              value = value,
              datumOption = mbInlineDatum,
              scriptRef = None
            )
          )
        )
    given [L <: AnyLayer]: Conversion[Utxo[L], (UtxoId[L], Output[L])] = identity
    // Still need this conversion because scalus doesn't have a full query thing yet
    extension [L <: AnyLayer](utxo: Utxo[L]) def untagged: (UtxoId[L], Output[L]) = identity(utxo)
    extension [L <: AnyLayer](utxo: Utxo[L]) def output: Output[L] = utxo._2
    extension [L <: AnyLayer](utxo: Utxo[L]) def input: UtxoId[L] = utxo._1
    extension [L <: AnyLayer](utxo: Utxo[L])
        def toScalus: SUtxo =
            SUtxo(utxo._1.untagged, utxo._2.convert)

type Utxo[L <: AnyLayer] = Utxo.Utxo[L]

/** ---------------------------------------------------------------------------------------------
  * UTxO Set
  * ---------------------------------------------------------------------------------------------
  */

object UtxoSet:
    opaque type UtxoSet[L <: AnyLayer] = Map[UtxoId[L], Output[L]]
    def apply[L <: AnyLayer](): UtxoSet[L] = Map.empty
    def apply[L <: AnyLayer](map: Map[UtxoId[L], Output[L]]): UtxoSet[L] =
        map
    given [L <: AnyLayer]: Conversion[UtxoSet[L], Map[UtxoId[L], Output[L]]] =
        identity

    /** We can only have one implict conversion that resolves to the same runtime (type-erased)
      * representation, otherwise the compiler gets confused.
      */
    extension [L <: AnyLayer](
        utxoSet: UtxoSet[L]
    )
        def asScalus: Map[TransactionInput, Babbage] =
            utxoSet.untagged.map((k, v) => (k.untagged, v.untagged))
    extension [L <: AnyLayer](utxoSet: UtxoSet[L])
        def untagged: Map[UtxoId[L], Output[L]] = identity(utxoSet)

    /** FIXME: This is a dummy value. The slot config and protocol params should be passed in
      */
    extension (utxoSetL2: UtxoSetL2)
        def getContextAndState: (Context, State) =
            (
              Context(fee = Coin(0L), env = UtxoEnv.default),
              State(
                utxos = utxoSetL2.untagged.map((k, v) => k.untagged -> v.untagged),
                certState = CertState.empty
              )
            )

type UtxoSet[L <: AnyLayer] = UtxoSet.UtxoSet[L]

type UtxoSetL1 = UtxoSet.UtxoSet[L1]
type UtxoSetL2 = UtxoSet.UtxoSet[L2]

extension (utxo: UTxO)
    /** Unsafe because it requires the developer to check:
      *   - That the UTxOs are indeed L2 UTxOs
      *   - Specifically, that the transaction outputs are Babbage outputs. This function will throw
      *     an exception if not.
      * @return
      */
    def unsafeAsL2: UtxoSet[L2] =
        UtxoSet[L2](utxo.map((ti, to) => UtxoId[L2](ti) -> Output[L2](to.asInstanceOf[Babbage])))

////////////////////////////////////////////////////////////////////////////////////////
// Keys

// A verification key of a peer, used on both L1 and L2
case class VerificationKeyBytes(bytes: ByteString) {
    def verKeyHash: AddrKeyHash = Hash(blake2b_224(bytes))
    def pubKeyHash: PubKeyHash = PubKeyHash(blake2b_224(bytes))
}

object VerificationKeyBytes:
    def applyI(bytes: IArray[Byte]): VerificationKeyBytes =
        new VerificationKeyBytes(ByteString.fromArray(IArray.genericWrapArray(bytes).toArray))

// A signing key of a peer, used on both L1 and L2
// TODO: why ByteString? This is never used onchain.
case class SigningKeyBytes(bytes: ByteString)

// UDiffTime
opaque type UDiffTimeMilli = BigInt

object UDiffTimeMilli:
    inline def apply(i: Int): UDiffTimeMilli = BigInt.apply(i)
extension (x: UDiffTimeMilli) def +(i: UDiffTimeMilli): UDiffTimeMilli = i + x
