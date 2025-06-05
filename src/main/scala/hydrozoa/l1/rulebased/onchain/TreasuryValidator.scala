package hydrozoa.l1.rulebased.onchain

import hydrozoa.l1.multisig.state.L2ConsensusParamsH32
import hydrozoa.l1.rulebased.onchain.TreasuryValidator.TreasuryRedeemer.{Deinit, Resolve, Withdraw}
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.Builtins.*
import scalus.builtin.{
    BLS12_381_G1_Element,
    BLS12_381_G2_Element,
    ByteString,
    Data,
    FromData,
    ToData
}
import scalus.ledger.api.v3.*
import scalus.ledger.api.v3.TxOutRef.given
import scalus.prelude.{Option, Validator, orFail, *, given}
import scalus.uplc.Program
import supranational.blst.Scalar
import hydrozoa.l1.rulebased.onchain.scalar.Scalar as ScalusScalar

import java.math.BigInteger

@Compile
object TreasuryValidator extends Validator:

    // EdDSA / ed25519 Cardano verification key
    private type VerificationKey = ByteString

    // The result of `bls12_381_G2_compress` function
    private type BLSProof = ByteString

    // Datum
    enum TreasuryDatum:
        case Resolved(resolvedDatum: ResolvedDatum)
        case Unresolved(unresolvedDatum: UnresolvedDatum)

    case class ResolvedDatum(
        headMp: CurrencySymbol,
        utxosActive: BLSProof, // TODO: Accumulator commitment
        version: (BigInt, BigInt),
        params: L2ConsensusParamsH32
    )

    case class UnresolvedDatum(
        headMp: CurrencySymbol,
        disputeId: TokenName,
        peers: List[VerificationKey],
        peersN: BigInt,
        deadlineVoting: PosixTime,
        versionMajor: BigInt,
        params: L2ConsensusParamsH32
    )

    // Redeemer
    enum TreasuryRedeemer:
        case Resolve
        case Withdraw(withdrawRedeemer: WithdrawRedeemer)
        case Deinit

    given FromData[TreasuryRedeemer] = FromData.derived

    given ToData[TreasuryRedeemer] = ToData.derived

    case class WithdrawRedeemer(
        utxoIds: List[TxOutRef],
        // membership proof for utxoIds and the updated accumulator at the same time
        proof: BLSProof
    )

    given FromData[WithdrawRedeemer] = FromData.derived

    given ToData[WithdrawRedeemer] = ToData.derived

    // Errors
    // This doesn't inline
    enum Error(msg: String):
        case SomeErr1 extends Error("SomeErr1")

    // This inlines well
    inline val SomeErr2 = "SomeErr2"

    // Entry point
    override def spend(datum: Option[Data], redeemer: Data, tx: TxInfo, ownRef: TxOutRef): Unit =
        redeemer.to[TreasuryRedeemer] match
            case Resolve          => false orFail Error.SomeErr1.toString
            case Withdraw(_proof) => false orFail SomeErr2
            case Deinit =>
                ScalusScalar("123").getOrFail("Failed to get scalar")
                false orFail SomeErr2

    // Utility functions
    /*
     * Multiply a list of n coefficients that belong to a binomial each to get a final polynomial of degree n+1
     * Example: for (x+2)(x+3)(x+5)(x+7)(x+11)=x^5 + 28 x^4 + 288 x^3 + 1358 x^2 + 2927 x + 2310
     * */
    def getFinalPolyScalus(binomial_poly: List[ScalusScalar]): List[ScalusScalar] = {
        binomial_poly
            .foldLeft(List.single(ScalusScalar.one)): (acc, term) =>
                val shiftedPoly: List[ScalusScalar] = List.Cons(ScalusScalar.zero, acc)
                val multipliedPoly = acc.map(s => s * term).appended(ScalusScalar.zero)
                List.map2(shiftedPoly, multipliedPoly)((l, r) => l + r)
    }

    @Ignore
    /*
     * Multiply a list of n coefficients that belong to a binomial each to get a final polynomial of degree n+1
     * Example: for (x+2)(x+3)(x+5)(x+7)(x+11)=x^5 + 28 x^4 + 288 x^3 + 1358 x^2 + 2927 x + 2310
     * */
    def getFinalPoly(binomial_poly: List[BigInt]): List[Scalar] = {
        binomial_poly
            .map(bi => Scalar(bi.bigInteger))
            .foldLeft(List.single(new Scalar(BigInteger("1")))): (acc, term) =>
                // We need to clone the whole `acc` since `mul` mutates it
                // and final adding gets mutated `shiftedPoly`
                val shiftedPoly: List[Scalar] = List.Cons(Scalar(BigInteger("0")), acc.map(_.dup))
                val multipliedPoly = acc.map(s => s.mul(term)).appended(Scalar(BigInteger("0")))
                List.map2(shiftedPoly, multipliedPoly)((l, r) => l.add(r))
    }

    def getG1Commitment(
        setup: List[BLS12_381_G1_Element],
        subset: List[ScalusScalar]
    ): BLS12_381_G1_Element = {
        val g1Zero = bls12_381_G1_uncompress(bls12_381_G1_compressed_zero)

        val subsetInG1 =
            List.map2(getFinalPolyScalus(subset), setup): (sb, st) =>
                bls12_381_G1_scalarMul(sb.toInt, st)

        subsetInG1.foldLeft(g1Zero): (a, b) =>
            bls12_381_G1_add(a, b)
    }

    /** Checks the membership `proof` for a `subset` of elements against the given accumulator
      * `acc`.
      *
      * @param setup
      *   The setup of the accumulator.
      * @param acc
      *   The accumulator to check.
      * @param subset
      *   The subset of the setup.
      * @return
      *   True if the accumulator is valid, false otherwise.
      */
    def checkMembership(
        setup: List[BLS12_381_G1_Element],
        acc: BLS12_381_G2_Element,
        subset: List[ScalusScalar],
        proof: BLS12_381_G2_Element
    ): Boolean = {
        val g1 = setup !! 0
        val lhs = bls12_381_millerLoop(g1, acc)
        val rhs = bls12_381_millerLoop(getG1Commitment(setup, subset), proof)
        println(lhs)
        println(rhs)
        bls12_381_finalVerify(lhs, rhs)
    }

    // Builders and so on

    @Ignore
    val script: Program = compile(TreasuryValidator.validate)
        .toUplc(generateErrorTraces = true)
        .plutusV3

    @Ignore
    def showSir(): Unit = println(compile(TreasuryValidator.validate).showHighlighted)

end TreasuryValidator

object TreasuryScript {
    val sir = Compiler.compile(TreasuryValidator.validate)
    val uplc = sir.toUplcOptimized(true)
}

@main
def main(args: String): Unit = {
    println("Hi!")
    println(TreasuryScript.sir.showHighlighted)
    // val ret = Scalar().inverse() // works
    // println(ret.to_bendian.toString())

    //    val ret = Scalar(
    //      BigInteger("1")
    //    ).mul(
    //      Scalar(BigInteger("0"))
    //    )
    //    println(BigInteger(ret.to_bendian))

    //    val ret2 = TreasuryValidator.getFinalPoly(
    //      List(
    //        BigInt("2"),
    //        BigInt("3")
    //      )
    //    )
    //    println(ret2.map(s => BigInteger(s.to_bendian)))

    //    val ret3 = TreasuryValidator.getFinalPoly0(
    //      List(
    //        BigInt("2"),
    //        BigInt("3")
    //      )
    //    )
    //    println(ret3)

    //    val ret2 = TreasuryValidator.getFinalPoly(
    //      List(
    //        BigInt("23028688090729445163723509241052290199566563106418427159558324016743"),
    //        BigInt("11615207285513499188821867516815580552008598385639274683799580493893")
    //      )
    //    )
    //    println(ret2.map(s => BigInteger(s.to_bendian)))

    //    val ret3 = TreasuryValidator.getFinalPolyScalus(
    //      List(
    //        BigInt("23028688090729445163723509241052290199566563106418427159558324016743"),
    //        BigInt("11615207285513499188821867516815580552008598385639274683799580493893")
    //      ).map(ScalusScalar(_).get)
    //    )
    //    println(ret3)

}
