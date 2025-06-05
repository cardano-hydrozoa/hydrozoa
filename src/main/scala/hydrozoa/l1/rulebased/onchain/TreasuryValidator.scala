package hydrozoa.l1.rulebased.onchain

import hydrozoa.l1.multisig.state.L2ConsensusParamsH32
import hydrozoa.l1.rulebased.onchain.TreasuryValidator.TreasuryDatum.Resolved
import hydrozoa.l1.rulebased.onchain.TreasuryValidator.TreasuryRedeemer.{Deinit, Resolve, Withdraw}
import hydrozoa.l1.rulebased.onchain.scalar.Scalar as ScalusScalar
import scalus.Compiler.compile
import scalus.builtin.Builtins.*
import scalus.builtin.ToData.toData
import scalus.builtin.{BLS12_381_G1_Element, BLS12_381_G2_Element, ByteString, Data, FromData, ToData}
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v3.*
import scalus.ledger.api.v3.TxOutRef.given
import scalus.prelude.Option.{None, Some}
import scalus.prelude.{Option, Validator, orFail, *, given}
import scalus.uplc.Program
import scalus.{Compile, Ignore, |>, toUplc, plutusV3, showHighlighted}
import supranational.blst.Scalar

import java.math.BigInteger

@Compile
object TreasuryValidator extends Validator:

    // TODO: we don't know exactly how to handle this
    val setup: List[BLS12_381_G1_Element] = List.empty

    // EdDSA / ed25519 Cardano verification key
    private type VerificationKey = ByteString

    // The result of `bls12_381_G2_compress` function
    private type BLSProof = ByteString

    // Datum
    enum TreasuryDatum:
        case Resolved(resolvedDatum: ResolvedDatum)
        case Unresolved(unresolvedDatum: UnresolvedDatum)

    given FromData[TreasuryDatum] = FromData.derived
    given ToData[TreasuryDatum] = ToData.derived

    case class ResolvedDatum(
        headMp: CurrencySymbol,
        utxosActive: BLSProof,
        version: (BigInt, BigInt),
        params: L2ConsensusParamsH32
    )

    given FromData[ResolvedDatum] = FromData.derived
    given ToData[ResolvedDatum] = ToData.derived

    case class UnresolvedDatum(
        headMp: CurrencySymbol,
        disputeId: TokenName,
        peers: List[VerificationKey],
        peersN: BigInt,
        deadlineVoting: PosixTime,
        versionMajor: BigInt,
        params: L2ConsensusParamsH32
    )

    given FromData[UnresolvedDatum] = FromData.derived
    given ToData[UnresolvedDatum] = ToData.derived

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

    inline val DatumIsMissing = "Treasury datum should be present"
    inline val WithdrawNeedsResolvedDatum = "Withdraw redeemer requires resolved datum"
    inline val WrongNumberOfWithdrawals = "Number of outputs should match the number of utxo ids"
    inline val BeaconTokenFailure = "Treasury should contain exactly one beacon token"
    inline val MembershipValidationFailed = "Withdrawals membership check failed"
    inline val BeaconTokenShouldBePreserved = "Beacon token should be preserves in treasury output"
    inline val ValueShouldBePreserved =
        "Value invariant should hold: treasuryInput = treasuryOutput + Σ withdrawalOutput"

    // Entry point
    override def spend(datum: Option[Data], redeemer: Data, tx: TxInfo, ownRef: TxOutRef): Unit =
        val treasuryDatum = datum match
            case Some(d) => d.to[TreasuryDatum]
            case None    => fail(DatumIsMissing)

        redeemer.to[TreasuryRedeemer] match
            case Resolve                                    => false orFail "Not implemented"
            case Withdraw(WithdrawRedeemer(utxoIds, proof)) =>
                // Check treasury datum
                val resolvedDatum = treasuryDatum match
                    case Resolved(d) => d
                    case _           => fail(WithdrawNeedsResolvedDatum)

                // headMp and headId
                val headMp = resolvedDatum.headMp
                val treasuryInput = tx.inputs
                    .find(_.outRef === ownRef)
                    .getOrFail("Own input was not found")
                    .resolved
                val headId: TokenName =
                    treasuryInput.value
                        .get(headMp)
                        .getOrFail("Beacon token was not found")
                        .toList match
                        case List.Cons((tokenName, amount), tail) =>
                            require(tail.isEmpty && amount == BigInt(1), BeaconTokenFailure)
                            tokenName
                        case _ => fail(BeaconTokenFailure)

                // The beacon token should be preserved
                // By contract, we require the treasure utxo is always the head, and the tail is always withdrawals
                val treasuryOutput = tx.outputs.head
                require(
                  treasuryOutput.value
                      .get(headMp)
                      .getOrFail(BeaconTokenShouldBePreserved)
                      .get(headId)
                      .getOrFail(BeaconTokenShouldBePreserved) == BigInt(1),
                  BeaconTokenShouldBePreserved
                )

                // Withdrawals
                // By contract, we require the treasure utxo is always the head, and the tail is always withdrawals
                val withdrawalOutputs = tx.outputs.tail
                // The number of withdrawals should match the number of utxos ids in the redeemer
                require(withdrawalOutputs.size == utxoIds.size, WrongNumberOfWithdrawals)
                // Calculate the final poly for withdrawn subset
                val withdrawnUtxos = utxoIds
                    // Joint utxo ids and outputs
                    .zip(withdrawalOutputs)
                    // Convert to data, serialize, calculate a hash, convert to scalars, multiply binomials
                    .map(e =>
                        e.toData
                            |> serialiseData
                            |> blake2b_224
                            |> ScalusScalar.fromByteStringBigEndianUnsafe
                    ) |> getFinalPolyScalus
                // Decompress commitments and run the membership check
                val acc = bls12_381_G2_uncompress(resolvedDatum.utxosActive)
                val proof_ = bls12_381_G2_uncompress(proof)
                require(
                  checkMembership(setup, acc, withdrawnUtxos, proof_),
                  MembershipValidationFailed
                )

                // Accumulator updated commitment
                treasuryOutput.datum match
                    case OutputDatum.OutputDatum(inlineDatum) =>
                        val outputResolvedDatum = inlineDatum.to[ResolvedDatum]
                        require(
                          outputResolvedDatum.utxosActive == proof,
                          "Accumulator should be updated properly"
                        )
                    case _ => fail("Treasury output datum was not found")

                // Value check
                // TODO: combine with iterating for poly calculation up above?
                // TODO: consider fees delta?
                val withdrawnValue =
                    tx.outputs.tail.foldLeft(Value.zero)((acc, o) => Value.plus(acc, o.value))
                val valueIsPreserved =
                    treasuryInput.value === Value.plus(treasuryOutput.value, withdrawnValue)
                require(valueIsPreserved, ValueShouldBePreserved)

            case Deinit => false orFail "SomeErr"

    // Utility functions
    /*
     * Multiply a list of n coefficients that belong to a binomial each to get a final polynomial of degree n+1
     * Example: for (x+2)(x+3)(x+5)(x+7)(x+11)=x^5 + 28 x^4 + 288 x^3 + 1358 x^2 + 2927 x + 2310
     * */
    def getFinalPolyScalus(binomial_poly: List[ScalusScalar]): List[ScalusScalar] = {
        binomial_poly
            .foldLeft(List.single(ScalusScalar.one)): (acc, term) =>
                val shiftedPoly: List[ScalusScalar] = List.Cons(ScalusScalar.zero, acc)
                val multipliedPoly = acc.map(s => s.mul(term)).appended(ScalusScalar.zero)
                List.map2(shiftedPoly, multipliedPoly)((l, r) => l.add(r))
    }

    // TODO: move it away, this is useful only for offchain code
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

@main
def main(args: String): Unit = {
    println("Hi!")
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
