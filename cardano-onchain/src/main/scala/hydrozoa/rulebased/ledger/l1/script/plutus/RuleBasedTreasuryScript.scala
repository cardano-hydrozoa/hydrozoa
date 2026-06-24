package hydrozoa.rulebased.ledger.l1.script.plutus
import hydrozoa.lib.cardano.scalus.Scalar as ScalusScalar
import hydrozoa.lib.cardano.scalus.cardano.onchain.plutus.ByteStringExtension.take
import hydrozoa.lib.cardano.scalus.cardano.onchain.plutus.TxOutExtension.inlineDatumOfType
import hydrozoa.lib.cardano.scalus.cardano.onchain.plutus.ValueExtension.*
import hydrozoa.multisig.ledger.joint.EvacuationKey
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.TreasuryRedeemer.{Deinit, Evacuate, Resolve}
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatumOnchain.{ResolvedOnchain, UnresolvedOnchain}
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.{MembershipProof, RuleBasedTreasuryDatumOnchain}
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus.*
import hydrozoa.rulebased.ledger.l1.state.VoteState.{VoteDatum, VoteStatus}
import scalus.*
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.prelude.Option.{None, Some}
import scalus.cardano.onchain.plutus.prelude.crypto.bls12_381.G2
import scalus.cardano.onchain.plutus.prelude.crypto.bls12_381.G2.scale
import scalus.cardano.onchain.plutus.v1.Value.+
import scalus.cardano.onchain.plutus.v2.TxOut
import scalus.cardano.onchain.plutus.v3.{Validator, *}
import scalus.compiler.Compile
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data.{fromData, toData}
import scalus.uplc.builtin.bls12_381.*

@Compile
object RuleBasedTreasuryValidator extends Validator {

    // Script redeemer
    enum TreasuryRedeemer:
        case Resolve
        case Evacuate(evacuateRedeemer: EvacuateRedeemer)
        case Deinit

    given FromData[TreasuryRedeemer] = FromData.derived

    given ToData[TreasuryRedeemer] = ToData.derived

    // TODO: inline these fields into TreasuryRedeemer to avoid extra data deconstruction?
    case class EvacuateRedeemer(
        evacuationKeys: List[EvacuationKey],
        // membership proof for evacuation keys and the updated accumulator at the same time
        proof: MembershipProof
    )

    given evacuationKeyToData: ToData[EvacuationKey] with {
        override def apply(v1: EvacuationKey): Data = toData(v1.byteString)
    }

    given evacuationKeyFromData: FromData[EvacuationKey] with {
        override def apply(data: Data): EvacuationKey = EvacuationKey(
          fromData[ByteString](data)
        ).get
    }

    given FromData[EvacuateRedeemer] = FromData.derived

    given ToData[EvacuateRedeemer] = ToData.derived

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
    private inline val ResolveTreasuryInputOutputSetupG2 =
        "setupG2 in treasuryInput and treasuryOutput must match"
    private inline val ResolveTreasuryOutputFailure =
        "Exactly one treasury output should present"
    private inline val ResolveUnexpectedAwaitingVote =
        "Unexpected AwaitingVote when trying to resolve"
    private inline val ResolveUnexpectedAbstain =
        "Unexpected Abstain when trying to resolve"

    // Evacuate redeemer
    private inline val EvacuateNeedsResolvedDatum =
        "Evacuate redeemer requires resolved datum"
    private inline val EvacuateWrongNumberOfEvacuatees =
        "Number of outputs should match the number of utxo ids"
    private inline val EvacuateBeaconTokenFailure =
        "Treasury should contain exactly one beacon token"
    private inline val EvacuateSetupIsNotBigEnough =
        "Trusted setup in the treasury is not big enough"
    private inline val EvacuateMembershipValidationFailed =
        "Evacuatees membership check failed"
    private inline val EvacuateBeaconTokenShouldBePreserved =
        "Beacon token should be preserved in treasury output"
    private inline val EvacuateValueShouldBePreserved =
        "Value invariant should hold: treasuryInput = treasuryOutput + Σ evacuationOutput"
    private inline val EvacuateOutputAccumulatorUpdated =
        "Accumulator in the output should be properly updated"
    private inline val EvacuateHeadMpShouldBePreserved =
        "headMp in treasuryInput and treasuryOutput must match"
    private inline val EvacuateVersionShouldBePreserved =
        "version in treasuryInput and treasuryOutput must match"
    private inline val EvacuateSetupG2ShouldBePreserved =
        "setupG2 in treasuryInput and treasuryOutput must match"
    private inline val EvacuateTreasuryWrongAddress =
        "Treasury output must remain at the treasury script address"

    // Deinit redeemer
    private inline val DeinitRequiresResolvedTreasury =
        "Deinitialization is not possible until the dispute is resolved"
    private inline val DeinitTokensNotFound =
        "Head tokens was not found in treasury input"
    private inline val DeinitTokensNotBurned =
        "All head tokens should be burned"
    private inline val DeinitTreasuryShouldBeEmpty =
        "All utxos should be evacuated before deinitializing"

    def cip67BeaconTokenPrefix = hex"01349900"

    // Entry point
    override inline def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit =

        log("TreasuryValidator")

        // Parse datum
        val treasuryDatum: RuleBasedTreasuryDatumOnchain = datum match
            case Some(d) => d.to[RuleBasedTreasuryDatumOnchain]
            case None    => fail(DatumIsMissing)

        // ===================================
        // Resolve redeemer
        // ===================================

        redeemer.to[TreasuryRedeemer] match
            case Resolve =>
                log("Resolve")

                // Treasury datum should be an "unresolved" one
                val unresolvedDatum = treasuryDatum match
                    case d: UnresolvedOnchain => d
                    case _                    => fail(ResolveNeedsUnresolvedDatumInInput)

                // TODO: pass vote input's outRef in the redeemer?
                // Vote input
                val voteInput = tx.inputs
                    .find(e =>
                        e.resolved.value.containsExactlyOneAsset(
                          unresolvedDatum.headMp,
                          unresolvedDatum.disputeId,
                          unresolvedDatum.headPeersN + 1
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
                    treasuryOutput.inlineDatumOfType[RuleBasedTreasuryDatumOnchain] match
                        case _: UnresolvedOnchain => fail(ResolveNeedsResolvedDatumInOutput)
                        case d: ResolvedOnchain   => d

                val voteDatum = voteInput.inlineDatumOfType[VoteDatum]

                // 7. If voteStatus is Vote...
                voteDatum.voteStatus match
                    // Both AwaitingVote and Abstain are "no commitment selected" — they cannot
                    // close out the dispute. In practice maxVote pushes Abstain below any Voted,
                    // and the FallbackTx's default vote utxo is always Voted with the implicit
                    // commitment, so reaching Resolve with a non-Voted status indicates an
                    // upstream tally bug.
                    case AwaitingVote(_) => fail(ResolveUnexpectedAwaitingVote)
                    case Abstain         => fail(ResolveUnexpectedAbstain)
                    case Voted(commitment, versionMinor) =>
                        val inputMajor = unresolvedDatum.versionMajor
                        val inputMinor = versionMinor
                        val outputMajor = treasuryOutputDatum.version._1
                        val outputMinor = treasuryOutputDatum.version._2
                        // (a) Let versionMinor be the corresponding field in voteStatus.
                        // (b) The version field of treasuryOutput must match (versionMajor, versionMinor).
                        require(
                          inputMajor == outputMajor &&
                              inputMinor == outputMinor,
                          ResolveVersionCheck
                        )
                        // (c) voteStatus and treasuryOutput must match on utxosActive.
                        require(
                          treasuryOutputDatum.evacuationActive === commitment,
                          ResolveUtxoActiveCheck
                        )

                // 8. treasuryInput and treasuryOutput must match on all other fields.
                require(
                  unresolvedDatum.headMp === treasuryOutputDatum.headMp,
                  ResolveTreasuryInputOutputHeadMp
                )

                require(
                  unresolvedDatum.setupG2 === treasuryOutputDatum.setupG2,
                  ResolveTreasuryInputOutputSetupG2
                )

            // ===================================
            // Evacuate redeemer
            // ===================================

            case Evacuate(EvacuateRedeemer(evacuationKeys, proof)) =>
                log("Evacuate")

                // Treasury datum should be "resolved" one
                val resolvedDatum = treasuryDatum match
                    case d: ResolvedOnchain => d
                    case _                  => fail(EvacuateNeedsResolvedDatum)

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
                        .getOrFail(EvacuateBeaconTokenFailure)
                        .toList
                        .filter((tn, _) => tn.take(4) == cip67BeaconTokenPrefix) match
                        case List.Cons(tokenNameAndAmount, none) =>
                            val tokenName = tokenNameAndAmount._1
                            val amount = tokenNameAndAmount._2
                            require(none.isEmpty && amount == BigInt(1), EvacuateBeaconTokenFailure)
                            tokenName
                        case _ => fail(EvacuateBeaconTokenFailure)

                // The beacon token should be preserved
                // By contract, we require:
                //   - The change utxo is position zero
                //   - the treasury utxo in position one
                //   - the tail be evacuatees
                val List.Cons(changeOutput, List.Cons(treasuryOutput, evacuationOutputs)) =
                    tx.outputs: @unchecked

                // The continuing treasury output must stay at the treasury script address. The
                // checks below only constrain its beacon, datum and total value — not where it
                // goes; without this an Evacuate could redirect the beacon and the entire treasury
                // value to an arbitrary address (cf. the Resolve branch, which pins the address).
                require(
                  treasuryOutput.address === treasuryInput.address,
                  EvacuateTreasuryWrongAddress
                )

                require(
                  treasuryOutput.value.toSortedMap
                      .get(headMp)
                      .getOrFail(EvacuateBeaconTokenShouldBePreserved)
                      .get(headId)
                      .getOrFail(EvacuateBeaconTokenShouldBePreserved) == BigInt(1),
                  EvacuateBeaconTokenShouldBePreserved
                )

                // Evacuatees
                // The number of evacuations should match the number of utxos ids in the redeemer
                require(
                  evacuationOutputs.size == evacuationKeys.size,
                  EvacuateWrongNumberOfEvacuatees
                )

                // Calculate the final poly for evacuated subset

                // Zip evacuation keys and outputs
                val evacuatedUtxos: List[ScalusScalar] = evacuationKeys
                    .zip(evacuationOutputs)
                    // Convert to data, serialize, calculate a hash, convert to scalars
                    .map(
                      _ |> ToData.tupleToData
                          |> serialiseData
                          |> blake2b_224
                          |> ScalusScalar.fromByteStringBigEndianUnsafe
                    )

                // Decompress commitments and run the membership check
                val acc = bls12_381_G1_uncompress(resolvedDatum.evacuationActive)
                val proof_ = bls12_381_G1_uncompress(proof)

                require(
                  resolvedDatum.setupG2.length > evacuatedUtxos.length,
                  EvacuateSetupIsNotBigEnough
                )

                // Extract the G2 setup prefix of needed length and decompress for pairing.
                val uncompressedSetup =
                    resolvedDatum.setupG2.take(evacuatedUtxos.length + 1).map(G2.uncompress)

                //// trace hashes
                // evacuatednUtxos.foreach( s =>
                //    trace(s.toInt.show)(())
                // )

                require(
                  checkMembership(uncompressedSetup, acc, evacuatedUtxos, proof_),
                  EvacuateMembershipValidationFailed
                )

                // Accumulator updated commitment
                val outputResolvedDatum =
                    treasuryOutput.inlineDatumOfType[RuleBasedTreasuryDatumOnchain] match
                        case _: UnresolvedOnchain => fail(ResolveNeedsResolvedDatumInOutput)
                        case d: ResolvedOnchain   => d

                require(
                  outputResolvedDatum.evacuationActive == proof,
                  EvacuateOutputAccumulatorUpdated
                )

                // Identity-binding fields must be preserved across the Evacuate transition
                // (foundation I6 Preservation). Without these, an attacker could land one
                // legitimate Evacuate, corrupt the output's setupG2 to a degenerate value
                // (e.g., all G2 identity points, which trivially satisfy any pairing check),
                // then drain the treasury via subsequent Evacuates that pass checkMembership
                // for arbitrary (subset, proof). Equivalent risks exist for headMp and version.
                require(
                  outputResolvedDatum.headMp === resolvedDatum.headMp,
                  EvacuateHeadMpShouldBePreserved
                )
                require(
                  outputResolvedDatum.version === resolvedDatum.version,
                  EvacuateVersionShouldBePreserved
                )
                require(
                  outputResolvedDatum.setupG2 === resolvedDatum.setupG2,
                  EvacuateSetupG2ShouldBePreserved
                )

                // treasuryInput must hold the sum of all tokens in treasuryOutput and the outputs of
                // evacuations.
                // TODO: combine with iterating for poly calculation up above?
                val evacuatedValue =
                    evacuationOutputs.foldLeft(Value.zero)((acc, o) => acc + o.value)

                val valueIsPreserved =
                    treasuryInput.value === (treasuryOutput.value + evacuatedValue)

                require(valueIsPreserved, EvacuateValueShouldBePreserved)

            // ===================================
            // Deinit redeemer
            // ===================================

            case Deinit =>
                log("Deinit")

                // Treasury should be resolved
                val (headMp, utxosActive) = treasuryDatum match
                    case d: ResolvedOnchain   => (d.headMp, d.evacuationActive)
                    case d: UnresolvedOnchain => fail(DeinitRequiresResolvedTreasury)

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

                // All head tokens should be burned, which means that all peers sign the tx.
                val headTokensMint = (-tx.mint).toSortedMap
                    .get(headMp)
                    .getOrFail(DeinitTokensNotBurned)

                require(headTokensInput === headTokensMint, DeinitTokensNotBurned)

                // All utxos should be evacuated
                // TODO: comparing as bytestrings is more efficient, we want to have this constant in Scalus
                require(
                  utxosActive === hex"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb",
                  DeinitTreasuryShouldBeEmpty
                )

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
        setupG2: List[G2Element],
        subset: List[ScalusScalar]
    ): G2Element = {
        val subsetInGroup =
            List.map2(getFinalPolyScalus(subset), setupG2): (sb, st) =>
                st.scale(sb.toInt)

        subsetInGroup.foldLeft(G2.zero): (a, b) =>
            bls12_381_G2_add(a, b)
    }

    /** Checks the membership `proof` for a `subset` of elements against the given accumulator
      * `acc`.
      *
      * @param setupG2
      *   The uncompressed G2 prefix of the KZG trusted setup, sized to `subset.length + 1`.
      * @param acc
      *   The accumulator to check (a G1 commitment over the full set).
      * @param subset
      *   The subset of evacuation-key scalars being proven.
      * @param proof
      *   The membership proof — a G1 commitment over the set's complement of `subset`.
      * @return
      *   True if the accumulator is valid, false otherwise.
      */
    def checkMembership(
        setupG2: List[G2Element],
        acc: G1Element,
        subset: List[ScalusScalar],
        proof: G1Element
    ): Boolean = {
        val g2 = setupG2 !! 0
        val lhs = bls12_381_millerLoop(acc, g2)
        val rhs = bls12_381_millerLoop(proof, getG2Commitment(setupG2, subset))
        bls12_381_finalVerify(lhs, rhs)
    }
}

object RuleBasedTreasuryScript {
    // Compile the validator using PlutusV3.compile
    given scalus.compiler.Options = scalus.compiler.Options.default

    val compiledPlutusV3Program: PlutusV3[Data => Unit] =
        PlutusV3.compile(RuleBasedTreasuryValidator.validate)

    private val compiledScriptHash: ScriptHash = compiledPlutusV3Program.script.scriptHash

    def address(n: Network): ShelleyAddress =
        ShelleyAddress(
          network = n,
          payment = ShelleyPaymentPart.Script(
            scalus.cardano.ledger.ScriptHash.fromArray(this.compiledScriptHash.bytes)
          ),
          delegation = Null
        )
}
