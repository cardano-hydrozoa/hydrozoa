package hydrozoa.l1.rulebased.onchain

import com.bloxbean.cardano.client.address
import com.bloxbean.cardano.client.address.AddressProvider
import com.bloxbean.cardano.client.plutus.spec.PlutusV3Script
import hydrozoa.infra.toBloxbean
import hydrozoa.{AddressBech, AddressBechL1, L1, Network, VerificationKeyBytes}
import hydrozoa.l1.rulebased.onchain.DisputeResolutionValidator.{VoteDatum, VoteDetails, VoteStatus}
import hydrozoa.l1.rulebased.onchain.TallyingValidator.TallyRedeemer
import hydrozoa.l1.rulebased.onchain.TreasuryValidator.TreasuryDatum.Unresolved
import hydrozoa.l1.rulebased.onchain.TreasuryValidator.{TreasuryDatum, cip67BeaconTokenPrefix}
import hydrozoa.l1.rulebased.onchain.lib.ByteStringExtensions.take
import hydrozoa.l1.rulebased.onchain.lib.TxOutExtensions.inlineDatumOfType
import hydrozoa.l1.rulebased.onchain.lib.ValueExtensions.{containsExactlyOneAsset, onlyNonAdaAsset}
import hydrozoa.l2.block.BlockTypeL2
import scalus.*
import scalus.builtin.Builtins.{blake2b_224, serialiseData, verifyEd25519Signature}
import scalus.builtin.ByteString.hex
import scalus.builtin.ToData.toData
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.ledger.api.v1.Credential.ScriptCredential
import scalus.ledger.api.v1.IntervalBoundType.Finite
import scalus.ledger.api.v3.*
import scalus.ledger.api.v3.ScriptPurpose.Rewarding
import scalus.prelude.Option.{None, Some}
import scalus.prelude.{
    ===,
    AssocMap,
    Eq,
    List,
    Option,
    ParameterizedValidator,
    fail,
    require,
    given
}

@Compile
object DisputeResolutionValidator extends ParameterizedValidator[ScriptHash]:

    // EdDSA / ed25519 signature
    private type Signature = ByteString

    // The result of `bls12_381_G2_compress` function
    private type UtxoSetCommitment = ByteString

    // Datum
    case class VoteDatum(
        key: BigInt,
        link: BigInt,
        peer: Option[PubKeyHash],
        voteStatus: VoteStatus
    )

    given FromData[VoteDatum] = FromData.derived
    given ToData[VoteDatum] = ToData.derived

    enum VoteStatus:
        case NoVote
        case Vote(voteDetails: VoteDetails)

    given Eq[VoteStatus] = (a: VoteStatus, b: VoteStatus) =>
        a match
            case VoteStatus.NoVote =>
                b match
                    case VoteStatus.NoVote  => true
                    case VoteStatus.Vote(_) => false
            case VoteStatus.Vote(as) =>
                a match {
                    case VoteStatus.NoVote   => false
                    case VoteStatus.Vote(bs) => as === bs
                }

    given FromData[VoteStatus] = FromData.derived
    given ToData[VoteStatus] = ToData.derived

    case class VoteDetails(
        utxosActive: UtxoSetCommitment,
        versionMinor: BigInt
    )

    given Eq[VoteDetails] = (a: VoteDetails, b: VoteDetails) =>
        a.utxosActive == b.utxosActive && a.versionMinor == b.versionMinor

    given FromData[VoteDetails] = FromData.derived
    given ToData[VoteDetails] = ToData.derived

    // Redeemer
    enum DisputeRedeemer:
        case Vote(voteRedeemer: MinorBlockL1Effect)
        case Tally
        case Resolve

    given FromData[DisputeRedeemer] = FromData.derived
    given ToData[DisputeRedeemer] = ToData.derived

    case class MinorBlockL1Effect(
        blockHeader: BlockHeader,
        multisig: List[Signature]
    )

    given FromData[MinorBlockL1Effect] = FromData.derived
    given ToData[MinorBlockL1Effect] = ToData.derived

    // TODO: should we re-use Hydrozoa's type more broadly?
    // issues: some types from scalus won't work well with Hydrozoa's transport
    case class BlockHeader(
        blockNum: BigInt,
        // TODO: should we re-use Hydrozoa's type more broadly?
        blockType: BlockTypeL2,
        timeCreation: PosixTime,
        versionMajor: BigInt,
        versionMinor: BigInt,
        utxosActive: UtxoSetCommitment
    )

    given FromData[BlockHeader] = FromData.derived
    given ToData[BlockHeader] = ToData.derived

    given FromData[BlockTypeL2] = FromData.derived
    given ToData[BlockTypeL2] = ToData.derived

    inline def cip67DisputeTokenPrefix = hex"00d950b0"

    // Common errors
    private inline val DatumIsMissing = "Vote datum should be present"

    // Vote redeemer
    private inline val VoteOnlyOneVoteUtxoIsSpent = "Only one vote utxo can be spent"
    private inline val VoteAlreadyCast = "Vote is already has been cast"
    private inline val VoteMustBeSignedByPeer = "Transaction must be signed by peer"
    private inline val VoteOneRefInputTreasury = "Only one ref input (treasury) is reuired"
    private inline val VoteTreasuryBeacon = "Treasury should contain beacon token"
    private inline val VoteTreasuryDatum = "Treasury datum is missing"
    private inline val VoteTreasuryDatumHeadMp = "Treasury datum headMp mismatch"
    private inline val VoteTreasuryDatumDisputeId = "Treasury datum disputeId mismatch"
    private inline val VoteTimeValidityCheck =
        "The transaction validity upper bound must not exceed the deadlineVoting"
    private inline val VoteMultisigCheck =
        "Redeemer should contain all valid signatures for the block voted"
    private inline val VoteMajorVersionCheck =
        "The versionMajor field must match between treasury and voteRedeemer"
    private inline val VoteVoteOutputExists =
        "There should exist one continuing vote output with the same set of tokens"
    private inline val VoteOutputDatumCheck =
        "voteStatus field of voteOutput must be a correct Vote"
    private inline val VoteOutputDatumAdditionalChecks =
        "other fields of voteOutput must match voteInput"

    // Tally redeemer
    private inline val TallyOnlyOne = "One (and only one) tallying per tx is required"
    private inline val TallyRedeemerMentionsSpentUtxo =
        "Tally redeemer should mention spent utxo in one of the fields"

    // Resolve redeemer
    private inline val ResolveTreasurySpent =
        "Treasury that holds head beacon must be spent"
    private inline val ResolveDatumIsUnresolved =
        "Treasury input datum should be unresolved"
    private inline val ResolveTreasuryVoteMatch =
        "Treasury datum should match vote datum on (headMp, disputeId)"
    //    private inline val Resolve
    //    private inline val Resolve

    // Entry point
    override def spend(
        tallyValidator: ScriptHash
    )(datum: Option[Data], redeemer: Data, tx: TxInfo, ownRef: TxOutRef): Unit =
        // Parse datum
        val voteDatum: VoteDatum = datum match
            case Some(d) => d.to[VoteDatum]
            case None    => fail(DatumIsMissing)

        redeemer.to[DisputeRedeemer] match
            case DisputeRedeemer.Vote(voteRedeemer) =>
                // There must not be any other spent input matching voteOutref on transaction hash
                val voteOutref = tx.inputs.filter(_.outRef.id === ownRef.id) match
                    case List.Cons(voteOutref, tail) =>
                        require(tail.isEmpty, VoteOnlyOneVoteUtxoIsSpent)
                        voteOutref
                    // Unreachable
                    case _ => fail()

                // Check vote status
                require(voteDatum.voteStatus === VoteStatus.NoVote, VoteAlreadyCast)
                // Check signature
                require(voteDatum.peer.forall(tx.signatories.contains(_)), VoteMustBeSignedByPeer)

                // Let(headMp, disputeId) be the minting policy and asset name of the only non-ADA
                // tokens in voteInput.
                val voteInput = voteOutref.resolved
                val (headMp, disputeId, voteTokenAmount) = voteInput.value.onlyNonAdaAsset

                // Verify the treasury reference input
                // Let treasury be the only reference input matching voteOutref on tx hash.
                val treasuryReference = tx.referenceInputs match {
                    case List.Cons(input, otherInputs) =>
                        require(
                          otherInputs.isEmpty && input.outRef.id === voteOutref.outRef.id,
                          VoteOneRefInputTreasury
                        )
                        input
                    case List.Nil => fail(VoteOneRefInputTreasury)
                }

                // A head beacon token of headMp and CIP-67 prefix 4937 must be in treasury.
                treasuryReference.resolved.value.get(headMp).getOrElse(AssocMap.empty).toList match
                    case List.Cons((tokenName, amount), none) =>
                        require(
                          none.isEmpty && tokenName.take(4) == cip67BeaconTokenPrefix,
                          VoteTreasuryBeacon
                        )
                    case _ => fail(VoteTreasuryBeacon)

                //  headMp and disputeId must match the corresponding fields of the Unresolved datum in treasury.
                val treasuryDatum =
                    treasuryReference.resolved.inlineDatumOfType[TreasuryDatum] match {
                        case Unresolved(unresolvedDatum) => unresolvedDatum
                        case _                           => fail(VoteTreasuryDatum)
                    }
                require(treasuryDatum.headMp === headMp, VoteTreasuryDatumHeadMp)
                require(treasuryDatum.disputeId === disputeId, VoteTreasuryDatumDisputeId)

                // The transaction’s time -validity upper bound must not exceed the deadlineVoting
                // field of treasury.
                tx.validRange.to.boundType match {
                    case Finite(toTime) =>
                        require(toTime <= treasuryDatum.deadlineVoting, VoteTimeValidityCheck)
                    case _ => fail(VoteTimeValidityCheck)
                }

                // The multisig field of voteRedeemer must have signatures of the blockHeader
                // field of voteRedeemer for all the public keys in the peers field of treasury.
                val msg = voteRedeemer.blockHeader.toData |> serialiseData
                require(
                  treasuryDatum.peers.length == voteRedeemer.multisig.length,
                  VoteMultisigCheck
                )
                List.map2(treasuryDatum.peers, voteRedeemer.multisig)((vk, sig) =>
                    require(verifyEd25519Signature(vk, msg, sig), VoteMultisigCheck)
                )
                // The versionMajor field must match between treasury and voteRedeemer.
                require(
                  voteRedeemer.blockHeader.versionMajor == treasuryDatum.versionMajor,
                  VoteMajorVersionCheck
                )

                // Vote output
                val voteOutput = tx.outputs.find(o =>
                    o.value.containsExactlyOneAsset(headMp, disputeId, voteTokenAmount)
                ) match
                    case Some(e) => e
                    case None    => fail(VoteVoteOutputExists)

                require(voteOutput.address === voteInput.address, VoteVoteOutputExists)

                val voteOutputDatum = voteOutput.inlineDatumOfType[VoteDatum]

                // voteStatus field of voteOutput must be a Vote matching voteRedeemer
                // on the utxosActive and versionMinor fields.
                voteOutputDatum.voteStatus match {
                    case VoteStatus.Vote(voteDetails) =>
                        require(
                          voteDetails.versionMinor == voteRedeemer.blockHeader.versionMinor,
                          VoteOutputDatumCheck
                        )
                        require(
                          voteDetails.utxosActive == voteRedeemer.blockHeader.utxosActive,
                          VoteOutputDatumCheck
                        )
                    case _ => fail(VoteOutputDatumCheck)
                }

                // All other fields of voteInput and voteOutput must match.
                require(voteDatum.key === voteOutputDatum.key, VoteOutputDatumAdditionalChecks)
                require(voteDatum.peer === voteOutputDatum.peer, VoteOutputDatumAdditionalChecks)
                require(voteDatum.link === voteOutputDatum.link, VoteOutputDatumAdditionalChecks)

            case DisputeRedeemer.Tally =>
                // Delegate checks to the stake validator
                // The transaction’s redeemers list must show tallyValidator being invoked to withdraw-zero
                // with a redeemer that mentions voteOutref in one of its fields.

                // We allow only one tallying per tx, so we can require there is
                // only one redeemer with `Reward` purpose.
                tx.redeemers.toList.filter((purpose, redeemer) =>
                    purpose === Rewarding(ScriptCredential(tallyValidator))
                ) match
                    case List.Cons((_, redeemer), none) =>
                        require(none.isEmpty, TallyOnlyOne)
                        val tallyRedeemer = redeemer.to[TallyRedeemer]
                        require(
                          tallyRedeemer.removedOutRef === ownRef
                              || tallyRedeemer.continuingOutRef === ownRef,
                          TallyRedeemerMentionsSpentUtxo
                        )
                    case _ => fail(TallyOnlyOne)

            case DisputeRedeemer.Resolve =>
                val voteInput = tx.inputs.find(_.outRef === ownRef).get
                val (headMp, disputeId, _) = voteInput.resolved.value.onlyNonAdaAsset

                // Let treasury be a spent input that holds a head beacon token of headMp and CIP-67
                // prefix 4937.
                val treasuryInput = tx.inputs
                    .find { i =>
                        i.resolved.value.get(headMp).getOrElse(AssocMap.empty).toList match
                            case List.Cons((tokenName, amount), none) =>
                                tokenName.take(4) == cip67BeaconTokenPrefix
                                && amount == BigInt(1)
                                && none.isEmpty
                            case _ => fail(ResolveTreasurySpent)
                    }
                    .getOrFail(ResolveTreasurySpent)

                val treasuryDatum =
                    treasuryInput.resolved.inlineDatumOfType[TreasuryDatum] match {
                        case Unresolved(unresolvedDatum) => unresolvedDatum
                        case _                           => fail(ResolveDatumIsUnresolved)
                    }

                // headMp and disputeId must match the corresponding fields of the Unresolved datum
                // in treasury.
                require(treasuryDatum.headMp === headMp, ResolveTreasuryVoteMatch)
                require(treasuryDatum.disputeId === disputeId, ResolveTreasuryVoteMatch)

end DisputeResolutionValidator

object DisputeResolutionScript {

    // FIXME: would be nice to calculate it on-the-fly not from the constant
    lazy val sir =
        Compiler.compile(
          DisputeResolutionValidator.validate(
            ByteString.fromHex("571337e0fb6de2617184fdc6c649f0b3d82f08c99601869288b9d2b8")
          )
        )
    lazy val script = sir.toUplcOptimized(generateErrorTraces = true).plutusV3

    // TODO: can we use Scalus for that?
    lazy val plutusScript: PlutusV3Script = PlutusV3Script
        .builder()
        .`type`("PlutusScriptV3")
        .cborHex(script.doubleCborHex)
        .build()
        .asInstanceOf[PlutusV3Script]

    lazy val scriptHash: ByteString = ByteString.fromArray(plutusScript.getScriptHash)

    def entAddress(n: Network): AddressBechL1 = {
        val address = AddressProvider.getEntAddress(plutusScript, n.toBloxbean)
        address.getAddress |> AddressBech[L1].apply
    }
}

// TODO: utxoActive
def mkDefVoteDatum(peersN: Int, _utxosActive: Unit): VoteDatum =
    VoteDatum(
      0,
      if peersN > 0 then 1 else 0,
      None,
      VoteStatus.Vote(VoteDetails(ByteString.empty, BigInt(0)))
    )

def hashVerificationKey(peer: VerificationKeyBytes): PubKeyHash =
    PubKeyHash(blake2b_224(ByteString.fromArray(peer.bytes)))

def mkVoteDatum(key: Int, peersN: Int, peer: VerificationKeyBytes): VoteDatum =
    VoteDatum(
      key = key,
      link = if peersN > key then key + 1 else 0,
      peer = Some(hashVerificationKey(peer)),
      voteStatus = VoteStatus.NoVote
    )

@main
def disputeResolutionValidatorSir(args: String): Unit = {
    println(DisputeResolutionScript.sir.showHighlighted)
    println(DisputeResolutionScript.scriptHash)
}
