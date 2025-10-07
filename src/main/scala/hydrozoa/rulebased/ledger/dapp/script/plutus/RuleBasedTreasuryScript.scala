package hydrozoa.rulebased.ledger.dapp.script.plutus

import hydrozoa.AddressL1
import hydrozoa.lib.cardano.scalus.Scalar as ScalusScalar
import hydrozoa.lib.cardano.scalus.ledger.api.ByteStringExtension.take
import hydrozoa.lib.cardano.scalus.ledger.api.TxOutExtension.inlineDatumOfType
import hydrozoa.lib.cardano.scalus.ledger.api.ValueExtension.*
import hydrozoa.rulebased.ledger.dapp.script.plutus.RuleBasedTreasuryValidator.TreasuryRedeemer.{
    Deinit,
    Resolve,
    Withdraw
}
import hydrozoa.rulebased.ledger.dapp.state.TreasuryState.RuleBasedTreasuryDatum.{
    Resolved,
    Unresolved
}
import hydrozoa.rulebased.ledger.dapp.state.TreasuryState.{MembershipProof, RuleBasedTreasuryDatum}
import hydrozoa.rulebased.ledger.dapp.state.VoteState.VoteStatus.{NoVote, Vote}
import hydrozoa.rulebased.ledger.dapp.state.VoteState.{VoteDatum, VoteStatus}
import scalus.*
import scalus.builtin.Builtins.*
import scalus.builtin.ByteString.hex
import scalus.builtin.ToData.toData
import scalus.builtin.{
    BLS12_381_G1_Element,
    BLS12_381_G2_Element,
    ByteString,
    Data,
    FromData,
    ToData
}
import scalus.cardano.address.Network
import scalus.cardano.ledger.{Language, ScriptHash}
import scalus.ledger.api.v1.Value.+
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.prelude.Option.{None, Some}
import scalus.prelude.crypto.bls12_381.G2
import scalus.prelude.crypto.bls12_381.G2.scale
import scalus.uplc.DeBruijnedProgram

@Compile
object RuleBasedTreasuryValidator extends Validator {

    // Script redeemer
    enum TreasuryRedeemer:
        case Resolve
        case Withdraw(withdrawRedeemer: WithdrawRedeemer)
        case Deinit

    given FromData[TreasuryRedeemer] = FromData.derived

    given ToData[TreasuryRedeemer] = ToData.derived

    case class WithdrawRedeemer(
        utxoIds: List[TxOutRef],
        // membership proof for utxoIds and the updated accumulator at the same time
        proof: MembershipProof
    )

    given FromData[WithdrawRedeemer] = FromData.derived

    given ToData[WithdrawRedeemer] = ToData.derived

    // Common errors
    private inline val DatumIsMissing =
        "Treasury datum should be present"

    // Resolve redeemer
    private inline val ResolveNeedsUnresolvedDatumInInput =
        "Resolve redeemer requires unresolved datum in treasury input"
    private inline val ResolveNeedsResolvedDatumInOutput =
        "Resolve redeemer requires resolved datum in treasury output"
    private inline val ResolveValueShouldBePreserved =
        "Value invariant should hold: treasuryOutput = treasuryInput + voteInput"
    private inline val ResolveVoteInputNotFound =
        "Vote input was not found"
    private inline val ResolveVersionCheck =
        "The version field of treasuryOutput must match (versionMajor, 0)"
    private inline val ResolveUtxoActiveCheck =
        "The activeUtxo in resolved treasury must match voting results"
    private inline val ResolveTreasuryInputOutputHeadMp =
        "headMp in treasuryInput and treasuryOutput must match"
    private inline val ResolveTreasuryInputOutputParams =
        "params in treasuryInput and treasuryOutput must match"
    private inline val ResolveTreasuryInputOutputSetup =
        "setup in treasuryInput and treasuryOutput must match"
    private inline val ResolveTreasuryOutputFailure =
        "Exactly one treasury output should present"
    private inline val ResolveUnexpectedNoVote =
        "Unexpected NoVot when trying to resolve"

    // Withdraw redeemer
    private inline val WithdrawNeedsResolvedDatum =
        "Withdraw redeemer requires resolved datum"
    private inline val WithdrawWrongNumberOfWithdrawals =
        "Number of outputs should match the number of utxo ids"
    private inline val WithdrawBeaconTokenFailure =
        "Treasury should contain exactly one beacon token"
    private inline val WithdrawSetupIsNotBigEnough =
        "Trusted setup in the treasury is not big enough"
    private inline val WithdrawMembershipValidationFailed =
        "Withdrawals membership check failed"
    private inline val WithdrawBeaconTokenShouldBePreserved =
        "Beacon token should be preserves in treasury output"
    private inline val WithdrawValueShouldBePreserved =
        "Value invariant should hold: treasuryInput = treasuryOutput + Σ withdrawalOutput"
    private inline val WithdrawOutputAccumulatorUpdated =
        "Accumulator in the output should be properly updated"

    // Deinit redeemer
    private inline val DeinitTokensNotFound =
        "Head tokens was not found in treasury input"
    private inline val DeinitTokensNotBurned =
        "All head tokens should be burned"

    def cip67BeaconTokenPrefix = hex"01349900"

    // Entry point
    override def spend(datum: Option[Data], redeemer: Data, tx: TxInfo, ownRef: TxOutRef): Unit =

        log("TreasuryValidator")

        // Parse datum
        val treasuryDatum: RuleBasedTreasuryDatum = datum match
            case Some(d) => d.to[RuleBasedTreasuryDatum]
            case None    => fail(DatumIsMissing)

        redeemer.to[TreasuryRedeemer] match
            case Resolve =>
                log("Resolve")

                // Treasury datum should be an "unresolved" one
                val unresolvedDatum = treasuryDatum match
                    case Unresolved(d) => d
                    case _             => fail(ResolveNeedsUnresolvedDatumInInput)

                // TODO: pass vote input's outRef in the redeemer?
                // Vote input
                val voteInput = tx.inputs
                    .find(e =>
                        e.resolved.value.containsExactlyOneAsset(
                          unresolvedDatum.headMp,
                          unresolvedDatum.disputeId,
                          unresolvedDatum.peersN + 1
                        )
                    )
                    .getOrFail(ResolveVoteInputNotFound)
                    .resolved

                // Treasury (own) input
                // TODO: factor out
                val treasuryInput = tx.inputs
                    .find(_.outRef === ownRef)
                    .getOrFail("Impossible happened: own input was not found")
                    .resolved

                // Total expected output
                val treasuryOutputExpected = voteInput.value + treasuryInput.value

                // TODO: pass output index in redeemer?
                // The only treasury output
                val treasuryOutput = tx.outputs
                    .filter(e => e.address === treasuryInput.address) match
                    case List.Cons(o, tail) =>
                        require(tail.isEmpty, ResolveTreasuryOutputFailure)
                        o
                    case _ => fail(ResolveTreasuryOutputFailure)

                // Check treasury output value
                require(
                  treasuryOutput.value === treasuryOutputExpected,
                  ResolveValueShouldBePreserved
                )

                val treasuryOutputDatum =
                    treasuryOutput.inlineDatumOfType[RuleBasedTreasuryDatum] match
                        case Unresolved(_) => fail(ResolveNeedsResolvedDatumInOutput)
                        case Resolved(d)   => d

                val voteDatum = voteInput.inlineDatumOfType[VoteDatum]

                // 7. If voteStatus is Vote...
                voteDatum.voteStatus match
                    case NoVote            => fail(ResolveUnexpectedNoVote)
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
                          treasuryOutputDatum.utxosActive === voteDetails.commitment,
                          ResolveUtxoActiveCheck
                        )

                // 8. treasuryInput and treasuryOutput must match on all other fields.
                require(
                  unresolvedDatum.headMp === treasuryOutputDatum.headMp,
                  ResolveTreasuryInputOutputHeadMp
                )

                require(
                  unresolvedDatum.params === treasuryOutputDatum.params,
                  ResolveTreasuryInputOutputParams
                )

                require(
                  unresolvedDatum.setup === treasuryOutputDatum.setup,
                  ResolveTreasuryInputOutputSetup
                )

            case Withdraw(WithdrawRedeemer(utxoIds, proof)) =>
                log("Withdraw")

                // Treasury datum should be "resolved" one
                val resolvedDatum = treasuryDatum match
                    case Resolved(d) => d
                    case _           => fail(WithdrawNeedsResolvedDatum)

                // headMp and headId

                // Let headMp be the corresponding field in treasuryInput.
                val headMp = resolvedDatum.headMp

                // TODO: factor out
                val treasuryInput = tx.inputs
                    .find(_.outRef === ownRef)
                    .getOrFail("Impossible happened: own input was not found")
                    .resolved

                // Let headId be the asset name of the only headMp token in treasuryInput with CIP-67
                // prefix 4937.
                val headId: TokenName =
                    treasuryInput.value.toSortedMap
                        .get(headMp)
                        .getOrFail(WithdrawBeaconTokenFailure)
                        .toList
                        .filter((tn, _) => tn.take(4) == cip67BeaconTokenPrefix) match
                        case List.Cons((tokenName, amount), none) =>
                            require(none.isEmpty && amount == BigInt(1), WithdrawBeaconTokenFailure)
                            tokenName
                        case _ => fail(WithdrawBeaconTokenFailure)

                // The beacon token should be preserved
                // By contract, we require the treasure utxo is always be the head, and the tail be withdrawals
                // FIXME: in reality the change outputs gets in
                //   Ideally we need to use treasury for fees.
                val List.Cons(treasuryOutput, withdrawalOutputs) = tx.outputs: @unchecked
                require(
                  treasuryOutput.value.toSortedMap
                      .get(headMp)
                      .getOrFail(WithdrawBeaconTokenShouldBePreserved)
                      .get(headId)
                      .getOrFail(WithdrawBeaconTokenShouldBePreserved) == BigInt(1),
                  WithdrawBeaconTokenShouldBePreserved
                )

                // Withdrawals
                // The number of withdrawals should match the number of utxos ids in the redeemer
                // FIXME: in reality the change outputs gets in - hence +1 for now
                require(
                  withdrawalOutputs.size == utxoIds.size + 1,
                  WithdrawWrongNumberOfWithdrawals
                )
                // Calculate the final poly for withdrawn subset
                // FIXME: this fails due to the same error:
                // Caused by: java.lang.IllegalArgumentException: Expected case class type, got TypeVar(T,Some(218919)) in expression: match d with
                // I blame this lines in Scalus, though it's not clear how to fix that since it uses

                // TODO:
                val withdrawalOutputsNoChange = withdrawalOutputs.reverse.tail.reverse

                // Zip utxo ids and outputs
                val withdrawnUtxos: List[ScalusScalar] = utxoIds
                    // .zip(withdrawalOutputs)
                    .zip(withdrawalOutputsNoChange)
                    // Convert to data, serialize, calculate a hash, convert to scalars
                    .map(e =>
                        e.toData
                            |> serialiseData
                            |> blake2b_224
                            |> ScalusScalar.fromByteStringBigEndianUnsafe
                    )

                // Decompress commitments and run the membership check
                val acc = bls12_381_G1_uncompress(resolvedDatum.utxosActive)
                val proof_ = bls12_381_G1_uncompress(proof)

                require(
                  resolvedDatum.setup.length > withdrawnUtxos.length,
                  WithdrawSetupIsNotBigEnough
                )

                // Extract setup of needed length
                val setup = resolvedDatum.setup.take(withdrawnUtxos.length + 1).map(G2.uncompress)

                require(
                  checkMembership(setup, acc, withdrawnUtxos, proof_),
                  WithdrawMembershipValidationFailed
                )

                // Accumulator updated commitment
                val Resolved(outputResolvedDatum) =
                    treasuryOutput.inlineDatumOfType[RuleBasedTreasuryDatum]: @unchecked

                require(
                  outputResolvedDatum.utxosActive == proof,
                  WithdrawOutputAccumulatorUpdated
                )

                // treasuryInput must hold the sum of all tokens in treasuryOutput and the outputs of
                // withdrawals.
                // TODO: combine with iterating for poly calculation up above?
                val withdrawnValue =
                    withdrawalOutputsNoChange.foldLeft(Value.zero)((acc, o) => acc + o.value)

                val valueIsPreserved =
                    treasuryInput.value === (treasuryOutput.value + withdrawnValue)

                require(valueIsPreserved, WithdrawValueShouldBePreserved)

            case Deinit =>
                log("Deinit")

                // This redeemer does not require the treasury’s active utxo set to be empty,
                // but it implicitly requires the transaction to be multi-signed by all peers
                // to burn the headMp tokens.

                // Thus, the peers can use this redeemer to override the treasury’s
                // spending validator with their multi-signature.

                // Treasury datum might be "resolved" or "unresolved"
                val headMp = treasuryDatum match
                    case Resolved(d)   => d.headMp
                    case Unresolved(d) => d.headMp

                // TODO: factor out
                val treasuryInput = tx.inputs
                    .find(_.outRef === ownRef)
                    .getOrFail("Impossible happened: own input was not found")
                    .resolved

                val headTokensInput = treasuryInput.value.toSortedMap
                    .get(headMp)
                    .getOrFail(DeinitTokensNotFound)

                // TODO: this might be redundant
                require(
                  !headTokensInput.isEmpty,
                  DeinitTokensNotFound
                )

                // All head tokens should be burned
                val headTokensMint = (-tx.mint).toSortedMap
                    .get(headMp)
                    .getOrFail(DeinitTokensNotBurned)

                require(headTokensInput === headTokensMint, DeinitTokensNotBurned)

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

    def getG2Commitment(
        setup: List[BLS12_381_G2_Element],
        subset: List[ScalusScalar]
    ): BLS12_381_G2_Element = {
        val subsetInGroup =
            List.map2(getFinalPolyScalus(subset), setup): (sb, st) =>
                st.scale(sb.toInt)

        subsetInGroup.foldLeft(G2.zero): (a, b) =>
            bls12_381_G2_add(a, b)
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
        setup: List[BLS12_381_G2_Element],
        acc: BLS12_381_G1_Element,
        subset: List[ScalusScalar],
        proof: BLS12_381_G1_Element
    ): Boolean = {
        val g2 = setup !! 0
        val lhs = bls12_381_millerLoop(acc, g2)
        val rhs = bls12_381_millerLoop(proof, getG2Commitment(setup, subset))
        bls12_381_finalVerify(lhs, rhs)
    }
}

object RuleBasedTreasuryScript {
    // Compile the validator to Scalus Intermediate Representation (SIR)
    // Using def instead of lazy val to avoid stack overflow during tests
    private def compiledSir = Compiler.compile(RuleBasedTreasuryValidator.validate)

    // Convert to optimized UPLC with error traces for PlutusV3
    private def compiledUplc = compiledSir.toUplcOptimized(generateErrorTraces = true)

    private def compiledPlutusV3Program = compiledUplc.plutusV3

    // Native Scalus PlutusScript - no Bloxbean dependency needed
    private def compiledDeBruijnedProgram: DeBruijnedProgram =
        compiledPlutusV3Program.deBruijnedProgram

    // Various encoding formats available natively in Scalus
    // private def cborEncoded: Array[Byte] = compiledDeBruijnedProgram.cborEncoded
    def flatEncoded: Array[Byte] = compiledDeBruijnedProgram.flatEncoded

    private def compiledDoubleCborEncoded: Array[Byte] = compiledDeBruijnedProgram.doubleCborEncoded

    // Hex representations - use the main program methods
    private def compiledDoubleCborHex: String = compiledDeBruijnedProgram.doubleCborHex

    def compiledScriptHash =
        ScriptHash.fromByteString(blake2b_224(ByteString.fromArray(compiledDoubleCborEncoded)))

    // Generate .plutus file if needed
    def writePlutusFile(path: String): Unit = {
        compiledPlutusV3Program.writePlutusFile(path, Language.PlutusV3)
    }

    // For compatibility with existing code that expects hex representation
    def getScriptHex: String = compiledDoubleCborHex

    // For compatibility with code that expects script hash as byte array
    def getScriptHash: Array[Byte] = compiledScriptHash.bytes

    def address(n: Network): AddressL1 = ???
}
