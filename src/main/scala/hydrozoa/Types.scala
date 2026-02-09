package hydrozoa

import scala.language.implicitConversions
import scalus.builtin.Builtins.blake2b_224
import scalus.builtin.ByteString
import scalus.cardano.ledger.{Utxo as _, *}
import scalus.ledger.api.v3
import scalus.ledger.api.v3.PubKeyHash

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
