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
Right now, for example, we allow unrestricted production of a "" from any TransactionInput. The way I would
prefer to do this is by:
  - renaming the methods that are currently exposed in `apply` as something like
    `unsafeAsL2 : TransactionInput => ` in order to give an indication that this is the place where the
    developer/reviewer/auditor is responsible for verifying the pre-conditions that aren't captured in the type system
  - Examining the places in our code where we do actual produce values of these types and trying as much as possible
    to a limited set of boundaries. In particular, our tx builders would need to produce L1 tagged types; serialization
    boundaries need to produce both L1/L2 tagged types; bloxbean/blockfrost chain queries produce L1; L2 ledger produces
    L2; etc.
 */

import scala.language.implicitConversions
import scalus.builtin.Builtins.blake2b_224
import scalus.builtin.ByteString
import scalus.cardano.ledger.{Utxo as _, *}
import scalus.ledger.api.v3
import scalus.ledger.api.v3.PubKeyHash

/** Cardano network layers.
  */
sealed trait AnyLayer derives CanEqual
sealed trait L1 extends AnyLayer derives CanEqual
sealed trait L2 extends AnyLayer derives CanEqual

////////////////////////////////////////////////////////////////////////////////////////
// Keys

// A verification key of a peer, used on both L1 and L2
// TODO: review, I think it's strange. Shall we use opaque type instead?
// I agree. What constructors would we need?
//
// The valid ways to generate one would be...
//
// Key generation
// (unsafe) key parsing
// anything else?
case class VerificationKeyBytes(bytes: ByteString) {
    def verKeyHash: AddrKeyHash = Hash(blake2b_224(bytes))
    def pubKeyHash: PubKeyHash = PubKeyHash(blake2b_224(bytes))
}

object VerificationKeyBytes:
    def applyI(bytes: IArray[Byte]): VerificationKeyBytes =
        new VerificationKeyBytes(ByteString.fromArray(IArray.genericWrapArray(bytes).toArray))
