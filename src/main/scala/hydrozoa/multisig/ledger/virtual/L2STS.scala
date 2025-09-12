package hydrozoa.multisig.ledger.virtual

/** This module defines the "L2 STS". It is almost a direct clone of scalus's upstream "STS"
 * (state transition system), which is in turn modeled after the `IntersectMBO/cardano-ledger`
 * haskell repository.
 *
 * For a more formal treatment, see
 * https://github.com/intersectmbo/cardano-ledger/releases/latest/download/small-step-semantics.pdf.
 * The mapping from this document to our types is:
 *   - "States" = given by the `State` associated type, in our case the
 *     `scalus.cardano.ledger.rules.State` type
 *   - "Transitions" = implementations of the `transit` method on instances of STSL2.Mutator.
 *   - "Signals" = given by the `Event` associated type, in our case `L2Event` (as defined in
 *     `Event.scala`)
 *   - "Rules" = roughly, a set of calls to implementations of the `validate` method on instances
 *     of STSL2.Validator (the antecedents), followed by a calls to `transit` functions (the
 *     consequent).
 *   - "Environment" = the `Context` associated type, in our case the
 *     `scalus.cardano.ledger.rules.Context` type
 *
 * The overall principle is simple: `validate` checks a `(context, state, event)` tuple for
 * validity, `transit` takes `(context, state, event)` to a new `state`.
 *
 * Where possible, we use the upstream types for representing our own STS. This means that our
 * State, Context, and Transition types are "too big" -- for instance, our State type contains
 * information about the UTxO Map, which the L2 Ledger indeed makes use of, but ALSO contains
 * information about Certificate state, which the L2 Ledger does not support. We do this because we
 * want to re-use L1 ledger rules directly where possible without having to convert possibly large
 * data structures (such as the entire utxo map) each time; this is a performance vs type-safety
 * trade-off that we felt was worth it.
 *
 * The validation rules for our STSL2 that are native to hydrozoa (i.e., that do not apply to L1)
 * can be found in `L2ConformanceValidator.scala`.
 */

import hydrozoa.*
import hydrozoa.multisig.ledger.infG2Point
import scalus.builtin.Builtins.{blake2b_224, serialiseData}
import scalus.builtin.Data.toData
import scalus.builtin.{BLS12_381_G1_Element, BLS12_381_G2_Element, ByteString}
import scalus.cardano.ledger.*
import scalus.ledger.api.v3
import scalus.ledger.api.v3.TxInInfo
import scalus.prelude.{asScalus, List as SList}
import supranational.blst.{P1, P2, Scalar}

import java.math.BigInteger

////////////////////////////////////////
// Layer 2 state transition system

sealed trait STSL2 {
    final type Context = scalus.cardano.ledger.rules.Context
    final type State = scalus.cardano.ledger.rules.State
    // An L2 Event is a Transaction (re-using the L1 transaction type) that can
    // also absorb deposits or release withdrawals.
    final type Event = L2Event
    final type Error = String | TransactionException
    final type Result = Either[Error, Value]
    type Value

    def apply(context: Context, state: State, event: Event): Result

    protected final def failure(error: Error): Result = Left(error)
    

}

object STSL2 {
    trait Validator extends STSL2 {
        override final type Value = Unit
        protected final val success: Result = Right(())

        override final def apply(context: Context, state: State, event: Event): Result =
            validate(context, state, event)

        def validate(context: Context, state: State, event: Event): Result
    }

    trait Mutator extends STSL2 {
        override final type Value = State

        override final def apply(context: Context, state: State, event: Event): Result =
            transit(context, state, event)

        def transit(context: Context, state: State, event: Event): Result

        protected final def success(state: State): Result = Right(state)
    }
}

////////////////////////////////////////////
// BLS Stuff

// TODO: this will be gone as soon as we get a setup ceremony up and running.
val tau = Scalar(BigInteger("42"))

def mkDummySetupG2(n: Int): SList[P2] = {
    val setup =
        (1 to n + 1).map(i =>
            P2.generator().dup().mult(tau.dup().mul(Scalar(BigInteger(i.toString))))
        )
    SList.Cons(P2.generator(), setup.toList.asScalus)
}

def mkDummySetupG1(n: Int): SList[P1] = {
    val setup =
        (1 to n + 1).map(i =>
            P1.generator().dup().mult(tau.dup().mul(Scalar(BigInteger(i.toString))))
        )
    SList.Cons(P1.generator(), setup.toList.asScalus)
}

def getUtxosActiveCommitment(utxo: UTxO): IArray[Byte] = {
    def toPlutus(ti: TransactionInput, to: TransactionOutput): TxInInfo =
        LedgerToPlutusTranslation.getTxInInfoV3(ti, utxo)

    val elemsRaw = utxo.toList
        .map(e => blake2b_224(serialiseData(toPlutus(e._1, e._2).toData)).toHex)
        .asScalus
    println(s"utxos active hashes raw: $elemsRaw")

    val elems = utxo.toList
        .map(e =>
            Scalar().from_bendian(blake2b_224(serialiseData(toPlutus(e._1, e._2).toData)).bytes)
        )
        .asScalus
    println(s"utxos active hashes: ${elems.map(e => BigInt.apply(e.to_bendian()))}")

    val setup = mkDummySetupG2(elems.length.toInt)

    val setupBS = setup.map(e => BLS12_381_G2_Element.apply(e).toCompressedByteString)
    setupBS.foreach(println)

    val commitmentPoint = getG2Commitment(setup, elems)
    val commitment = IArray.unsafeFromArray(commitmentPoint.compress())
    println(s"Commitment: ${(ByteString.fromArray(commitment.toArray)).toHex}")
    commitment
}

/*
 * Multiply a list of n coefficients that belong to a binomial each to get a final polynomial of degree n+1
 * Example: for (x+2)(x+3)(x+5)(x+7)(x+11)=x^5 + 28 x^4 + 288 x^3 + 1358 x^2 + 2927 x + 2310
 */
def getFinalPoly(binomial_poly: SList[Scalar]): SList[Scalar] = {
    binomial_poly
        .foldLeft(SList.single(new Scalar(BigInteger("1")))): (acc, term) =>
            // We need to clone the whole `acc` since `mul` mutates it
            // and final adding gets mutated `shiftedPoly`
            val shiftedPoly: SList[Scalar] =
                SList.Cons(Scalar(BigInteger("0")), acc.map(_.dup))
            val multipliedPoly = acc.map(s => s.mul(term)).appended(Scalar(BigInteger("0")))
            SList.map2(shiftedPoly, multipliedPoly)((l, r) => l.add(r))
}

// TODO: use multi-scalar multiplication
def getG2Commitment(
                       setup: SList[P2],
                       subset: SList[Scalar]
                   ): P2 = {
    val subsetInG2 =
        SList.map2(getFinalPoly(subset), setup): (sb, st) =>
            st.mult(sb)

    val zero = infG2Point
    require(zero.is_inf())

    subsetInG2.foldLeft(zero.dup()): (a, b) =>
        a.add(b)
}

@main
def dumpSetupG1(): Unit = {
    val setup = mkDummySetupG1(6)
    val setupBS = setup.map(e => BLS12_381_G1_Element.apply(e).toCompressedByteString)
    setupBS.foreach(println)

    //    println(encodeHex(IArray.unsafeFromArray(P1.generator().compress())))
    //    println(G1.generator.toCompressedByteString)
}
