package hydrozoa.l1.rulebased.onchain

import hydrozoa.l1.multisig.state.L2ConsensusParamsH32
import hydrozoa.l1.rulebased.onchain.DisputeResolutionValidator.VoteDatum
import hydrozoa.l1.rulebased.onchain.DisputeResolutionValidator.VoteStatus.{NoVote, Vote}
import hydrozoa.l1.rulebased.onchain.TreasuryValidator.TreasuryDatum.{Resolved, Unresolved}
import hydrozoa.l1.rulebased.onchain.TreasuryValidator.TreasuryRedeemer.{Deinit, Resolve, Withdraw}
import hydrozoa.l1.rulebased.onchain.scalar.Scalar as ScalusScalar
import scalus.builtin.Builtins.*
import scalus.builtin.ToData.toData
import scalus.builtin.{
    BLS12_381_G1_Element,
    BLS12_381_G2_Element,
    ByteString,
    Data,
    FromData,
    ToData
}
import scalus.ledger.api.v1.Value.{+, -}
import scalus.ledger.api.v2.OutputDatum as TOutputDatum
import scalus.ledger.api.v2.OutputDatum.OutputDatum
import scalus.ledger.api.v3.*
import scalus.ledger.api.v3.TxOutRef.given
import scalus.prelude.Option.{None, Some}
import scalus.prelude.{Option, Validator, orFail, *, given}
import scalus.{Compile, Ignore, |>}
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
        case Unresolved(unresolvedDatum: UnresolvedDatum)
        case Resolved(resolvedDatum: ResolvedDatum)

    given FromData[TreasuryDatum] = FromData.derived
    given ToData[TreasuryDatum] = ToData.derived

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

    case class ResolvedDatum(
        headMp: CurrencySymbol,
        utxosActive: BLSProof,
        version: (BigInt, BigInt),
        params: L2ConsensusParamsH32
    )

    given FromData[ResolvedDatum] = FromData.derived
    given ToData[ResolvedDatum] = ToData.derived

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

    private inline val DatumIsMissing = "Treasury datum should be present"
    private inline val ResolveNeedsUnresolvedDatumInInput =
        "Resolve redeemer requires unresolved datum in treasury input"
    private inline val ResolveNeedsResolvedDatumInOutput =
        "Resolve redeemer requires resolved datum in treasury output"
    private inline val WithdrawNeedsResolvedDatum = "Withdraw redeemer requires resolved datum"
    private inline val WrongNumberOfWithdrawals =
        "Number of outputs should match the number of utxo ids"
    private inline val BeaconTokenFailure = "Treasury should contain exactly one beacon token"
    private inline val MembershipValidationFailed = "Withdrawals membership check failed"
    private inline val BeaconTokenShouldBePreserved =
        "Beacon token should be preserves in treasury output"
    private inline val ResolveValueShouldBePreserved =
        "Value invariant should hold: treasuryOutput = treasuryInput + voteInput"
    private inline val WithdrawValueShouldBePreserved =
        "Value invariant should hold: treasuryInput = treasuryOutput + Σ withdrawalOutput"
    private inline val VoteUtxoNotFound = "Vote UTxo was not found"
    private inline val VoteInputDatumShouldPresent = "Vote input datum should present"
    private inline val ResolveVersionCheck =
        "The version field of treasuryOutput must match (versionMajor, 0)"
    private inline val ResolveUtxoActiveCheck =
        "The activeUtxo in resolved treasury must match voting results"
    private inline val TreasuryInputOutputHeadMp =
        "headMp in treasuryInput and treasuryOutput must match"
    private inline val TreasuryInputOutputParams =
        "params in treasuryInput and treasuryOutput must match"

    // Entry point
    override def spend(datum: Option[Data], redeemer: Data, tx: TxInfo, ownRef: TxOutRef): Unit =
        // Parse datum
        val treasuryDatum = datum match
            case Some(d) => d.to[TreasuryDatum]
            case None    => fail(DatumIsMissing)

        extension (self: Value)
            // Check - contains only specified amount of same tokens and no other tokens
            private def containsExactlyOneAsset(
                cs: CurrencySymbol,
                tn: TokenName,
                amount: BigInt
            ): Boolean =
                self.toList.filter(!_._1.isEmpty) match
                    case List.Cons(asset, tail) =>
                        if tail.isEmpty then
                            if asset._1 == cs then
                                asset._2.toList match
                                    case List.Cons(token, tail) =>
                                        if tail.isEmpty then token._1 == tn && token._2 == amount
                                        else false
                                    case _ => false
                            else false
                        else false
                    case _ => false

            // Negate value, useful for burning operations
            private def unary_- :Value = Value.zero - self

        redeemer.to[TreasuryRedeemer] match
            case Resolve =>
                // Treasury datum should be "unresolved" one
                val unresolvedDatum = treasuryDatum match
                    case Unresolved(d) => d
                    case _             => fail(ResolveNeedsUnresolvedDatumInInput)

                // Vote input
                val voteInput = tx.inputs
                    .find(e =>
                        e.resolved.value.containsExactlyOneAsset(
                          unresolvedDatum.headMp,
                          unresolvedDatum.disputeId,
                          unresolvedDatum.peersN + 1
                        )
                    )
                    .getOrFail(VoteUtxoNotFound)
                    .resolved

                // Treasury input
                val treasuryInput = tx.inputs
                    .find(_.outRef === ownRef)
                    .getOrFail("Own input was not found")
                    .resolved

                // Total output
                val treasuryOutputExpected = voteInput.value + treasuryInput.value

                // The only treasury output
                val treasuryOutput = tx.outputs
                    .filter(e => e.address === treasuryInput.address) match
                    case List.Cons(o, tail) =>
                        require(tail.isEmpty, "Only one treasury output should be there")
                        o
                    case _ => fail("Treasury output should be present")

                // Check treasury output value
                require(
                  treasuryOutput.value === treasuryOutputExpected,
                  ResolveValueShouldBePreserved
                )

                val treasuryOutputDatum = treasuryOutput.datum match
                    case TOutputDatum.OutputDatum(inlineDatum) =>
                        inlineDatum.to[TreasuryDatum] match
                            case Unresolved(_) => fail(ResolveNeedsResolvedDatumInOutput)
                            case Resolved(d)   => d
                    case _ => fail(ResolveNeedsResolvedDatumInOutput)

                val voteDatum = voteInput.datum match
                    case TOutputDatum.OutputDatum(inlineDatum) => inlineDatum.to[VoteDatum]
                    case _                                     => fail(VoteInputDatumShouldPresent)

                voteDatum.voteStatus match
                    case NoVote            => fail("impossible")
                    case Vote(voteDetails) =>
                        // (a) Let versionMinor be the corresponding field in voteStatus.
                        // (b) The version field of treasuryOutput must match (versionMajor, versionMinor).
                        require(
                          treasuryOutputDatum.version._1 == unresolvedDatum.versionMajor &&
                              treasuryOutputDatum.version._2 == voteDetails.versionMinor,
                          ResolveVersionCheck
                        )
                        // (c) voteStatus and treasuryOutput must match on utxosActive.
                        require(
                          treasuryOutputDatum.utxosActive === voteDetails.utxosActive,
                          ResolveUtxoActiveCheck
                        )

                require(
                  unresolvedDatum.headMp === treasuryOutputDatum.headMp,
                  TreasuryInputOutputHeadMp
                )
                require(
                  unresolvedDatum.params === treasuryOutputDatum.params,
                  TreasuryInputOutputParams
                )

            case Withdraw(WithdrawRedeemer(utxoIds, proof)) =>
                // Treasury datum should be "resolved" one
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
                    case TOutputDatum.OutputDatum(inlineDatum) =>
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
                    treasuryInput.value === (treasuryOutput.value + withdrawnValue)
                require(valueIsPreserved, WithdrawValueShouldBePreserved)

            case Deinit =>
                // This redeemer does not require the treasury’s active utxo set to be empty,
                // but it implicitly requires the transaction to be multi-signed by all peers
                // to burn the headMp tokens.
                // Thus, the peers can use this redeemer to override the treasury’s
                // spending validator with their multi-signature.

                // Treasury datum might be "resolved" or "unresolved"
                val headMp = treasuryDatum match
                    case Resolved(d)   => d.headMp
                    case Unresolved(d) => d.headMp

                val treasuryInput = tx.inputs
                    .find(_.outRef === ownRef)
                    .getOrFail("Own input was not found")
                    .resolved

                val headTokensInput = treasuryInput.value
                    .get(headMp)
                    .getOrFail("Head tokens was not found in treasury input")

                require(
                  headTokensInput.size > 0,
                  "Deinit should burn at least one token, but there are no tokens in treasury input"
                )

                // All head tokens should be burned
                val headTokensMint = (-tx.mint)
                    .get(headMp)
                    .getOrFail("Head tokens was not found in mint value")

                require(headTokensInput === headTokensMint, "All head tokens should be burned")

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

//    // Builders and so on
//    @Ignore
//    val script: Program = compile(TreasuryValidator.validate)
//        .toUplc(generateErrorTraces = true)
//        .plutusV3
//
//    @Ignore
//    def showSir(): Unit = println(compile(TreasuryValidator.validate).showHighlighted)

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
