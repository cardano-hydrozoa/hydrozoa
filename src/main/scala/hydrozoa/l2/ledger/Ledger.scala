package hydrozoa.l2.ledger

import cats.Traverse
import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.{
    Piper,
    decodeBech32AddressL2,
    decodeHex,
    encodeHex,
    plutusAddressAsL2,
    txHash
}
import hydrozoa.l1.multisig.state.depositDatum
import hydrozoa.l1.rulebased.onchain.scalar.Scalar as ScalusScalar
import hydrozoa.l2.commitment.infG2Point
import hydrozoa.l2.ledger
import hydrozoa.l2.ledger.*
import scalus.builtin.Builtins.{blake2b_224, serialiseData}
import scalus.builtin.Data.toData
import scalus.builtin.{BLS12_381_G1_Element, BLS12_381_G2_Element, ByteString}
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{Address, Network, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.RedeemerTag.Spend
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.*
import scalus.ledger.api.v2.OutputDatum.{NoOutputDatum, OutputDatum}
import scalus.ledger.api.v2.TxOut
import scalus.ledger.api.v3
import scalus.ledger.api.v3.{TxInInfo, TxOutRef, Address as APIAddress}
import scalus.ledger.babbage.ProtocolParams
import scalus.prelude.List.{Cons, asScala, asScalus}
import scalus.prelude.Option.{None as SNone, Some as SSome}
import scalus.prelude.crypto.bls12_381.G1
import scalus.prelude.{AssocMap, List as SList, Option as SOption, given}
import supranational.blst.{P1, P2, Scalar}

import java.math.BigInteger
import scala.collection.mutable
import scala.jdk.CollectionConverters.*



////////////////////////////////////////
// Layer 2 state transition system

sealed trait STSL2 {
    final type Context = scalus.cardano.ledger.rules.Context
    final type State = scalus.cardano.ledger.rules.State
    // An L2 Event is a Transaction (re-using the L1 transaction type) that can
    // also absorb deposits or release withdrawals.
    final type Event = L2Event
    type Value
    final type Error = String | TransactionException
    final type Result = Either[Error, Value]

    def apply(context: Context, state: State, event: Event): Result

    protected final def failure(error: Error): Result = Left(error)
}

object STSL2 {
    trait Validator extends STSL2 {
        override final type Value = Unit

        def validate(context: Context, state: State, event: Event): Result

        override final def apply(context: Context, state: State, event: Event): Result =
            validate(context, state, event)

        protected final val success: Result = Right(())
    }

    trait Mutator extends STSL2 {
        override final type Value = State

        def transit(context: Context, state: State, event: Event): Result

        override final def apply(context: Context, state: State, event: Event): Result =
            transit(context, state, event)

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

    val elems = utxo.toList
        .map(e =>
            Scalar().from_bendian(blake2b_224(serialiseData(toPlutus(e._1, e._2).toData)).bytes)
        )
        .asScalus

    val setup = mkDummySetupG2(elems.length.toInt)

    val setupBS = setup.map(e => BLS12_381_G2_Element.apply(e).toCompressedByteString)
    setupBS.foreach(println)

    val commitmentPoint = getG2Commitment(setup, elems)
    val commitment = IArray.unsafeFromArray(commitmentPoint.compress())
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
