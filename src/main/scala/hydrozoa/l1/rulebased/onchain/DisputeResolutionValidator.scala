package hydrozoa.l1.rulebased.onchain

import hydrozoa.l1.rulebased.onchain.TallyingValidator.TallyRedeemer
import hydrozoa.l1.rulebased.onchain.TreasuryValidator.TreasuryDatum.Unresolved
import hydrozoa.l1.rulebased.onchain.TreasuryValidator.{TreasuryDatum, cip67BeaconTokenPrefix}
import hydrozoa.l1.rulebased.onchain.lib.ByteStringExtensions.take
import hydrozoa.l1.rulebased.onchain.lib.TxOutExtensions.inlineDatumOfType
import hydrozoa.l1.rulebased.onchain.lib.ValueExtensions.{
    containsCurrencySymbol,
    containsExactlyOneAsset,
    onlyNonAdaAsset
}
import hydrozoa.l2.block.BlockTypeL2
import scalus.*
import scalus.builtin.Builtins.{serialiseData, verifyEd25519Signature}
import scalus.builtin.ByteString.hex
import scalus.builtin.ToData.toData
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.ledger.api.v1.Credential.ScriptCredential
import scalus.ledger.api.v1.IntervalBoundType.Finite
import scalus.ledger.api.v1.Value.+
import scalus.ledger.api.v3.*
import scalus.ledger.api.v3.ScriptPurpose.Rewarding
import scalus.prelude.Option.{None, Some}
import scalus.prelude.{
    !==,
    ===,
    AssocMap,
    Eq,
    List,
    Option,
    ParameterizedValidator,
    Validator,
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
                    case VoteStatus.NoVote => true
                    case _                 => false
            case VoteStatus.Vote(_) => false // TODO: fix

    given FromData[VoteStatus] = FromData.derived
    given ToData[VoteStatus] = ToData.derived

    case class VoteDetails(
        utxosActive: UtxoSetCommitment,
        versionMinor: BigInt
    )

    given FromData[VoteDetails] = FromData.derived
    given ToData[VoteDetails] = ToData.derived

    // Redeemer
    enum DisputeRedeemer:
        case Vote(voteRedeemer: Option[MinorBlockL1Effect])
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
    // private inline val OneVoteTokenInput = "Only one vote token should be in vote utxo input"

    // Tally redeemer

    // Resolve redeemer

    // Entry point
    override def spend(
        tallyValidator: ScriptHash
    )(datum: Option[Data], redeemer: Data, tx: TxInfo, ownRef: TxOutRef): Unit =
        // Parse datum
        val voteDatum = datum match
            case Some(d) => d.to[VoteDatum]
            case None    => fail(DatumIsMissing)

        redeemer.to[DisputeRedeemer] match
            case DisputeRedeemer.Vote(mbVoteRedeemer) =>
                // Own input
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

                // Let(headMp, disputeId) be the minting policy and asset name of the only non -ADA
                // tokens in voteInput.
                val voteInput = voteOutref.resolved
                val (headMp, disputeId, voteTokenAmount) = voteInput.value.onlyNonAdaAsset

                // Verify the treasury reference input
                // Let treasury be the only reference input matching voteOutref on tx hash.
                val treasuryReference = tx.referenceInputs match {
                    case List.Cons(input, otherInputs) =>
                        require(otherInputs.isEmpty)
                        require(input.outRef.id === voteOutref.outRef.id)
                        input
                    case List.Nil => fail()
                }

                // A head beacon token of headMp and CIP-67 prefix 4937 must be in treasury.
                treasuryReference.resolved.value.get(headMp).getOrElse(AssocMap.empty).toList match
                    case List.Cons((tokenName, amount), none) =>
                        require(none.isEmpty && tokenName.take(4) == cip67BeaconTokenPrefix)
                    case _ => fail()

                //  headMp and disputeId must match the corresponding fields of the Unresolved datum in treasury.
                val treasuryDatum =
                    treasuryReference.resolved.inlineDatumOfType[TreasuryDatum] match {
                        case Unresolved(unresolvedDatum) => unresolvedDatum
                        case _                           => fail()
                    }
                require(treasuryDatum.headMp === headMp)
                require(treasuryDatum.disputeId === disputeId)

                // The transaction’s time -validity upper bound must not exceed the deadlineVoting
                // field of treasury.
                tx.validRange.to.boundType match {
                    case Finite(toTime) => require(toTime <= treasuryDatum.deadlineVoting)
                    case _              => fail()
                }

                // The multisig field of voteRedeemer must have signatures of the blockHeader
                // field of voteRedeemer for all the public keys in the peers field of treasury.
                mbVoteRedeemer match
                    case Some(voteRedeemer) =>
                        val msg = voteRedeemer.blockHeader.toData |> serialiseData
                        require(treasuryDatum.peers.length == voteRedeemer.multisig.length)
                        List.map2(treasuryDatum.peers, voteRedeemer.multisig)((vk, sig) =>
                            require(verifyEd25519Signature(vk, msg, sig))
                        )
                        // The versionMajor field must match between treasury and voteRedeemer.
                        require(voteRedeemer.blockHeader.versionMajor == treasuryDatum.versionMajor)
                    case None => ()

                val voteOutput = tx.outputs.find(o =>
                    o.value.containsExactlyOneAsset(headMp, disputeId, voteTokenAmount)
                ) match
                    case Some(e) => e
                    case None    => fail()

                require(voteOutput.address === voteInput.address)

                val voteOutputDatum = voteOutput.inlineDatumOfType[VoteDatum]

                // If voteRedeemer is provided, the voteStatus field of voteOutput must be a Vote
                // matching voteRedeemer on the utxosActive and versionMinor fields.
                mbVoteRedeemer match
                    case Some(voteRedeemer) =>
                        voteOutputDatum.voteStatus match {
                            case VoteStatus.Vote(voteDetails) =>
                                require(
                                  voteDetails.versionMinor == voteRedeemer.blockHeader.versionMinor
                                )
                                require(
                                  voteDetails.utxosActive == voteRedeemer.blockHeader.utxosActive
                                )
                            case _ => fail()
                        }
                    // TODO: Otherwise, it must be Abstain.
                    case None =>
                        voteOutputDatum.voteStatus match
                            case VoteStatus.Vote(voteDetails) => fail()
                            case _                            => ()

                // All other fields of voteInput and voteOutput must match.
                require(voteDatum.key === voteOutputDatum.key)
                require(voteDatum.peer === voteOutputDatum.peer)
                require(voteDatum.link === voteOutputDatum.link)

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
                        require(none.isEmpty)
                        val tallyRedeemer = redeemer.to[TallyRedeemer]
                        require(
                          tallyRedeemer.removedOutRef === ownRef
                              || tallyRedeemer.continuingOutRef === ownRef
                        )
                    case _ => fail()

            case DisputeRedeemer.Resolve =>
                val voteInput = tx.inputs.find(_.outRef === ownRef).get
                val (headMp, disputeId, _) = voteInput.resolved.value.onlyNonAdaAsset

                // Let treasury be a spent input that holds a head beacon token of headMp and CIP-67
                // prefix 4937.
                val treasuryInput = tx.inputs
                    .find { i =>
                        i.resolved.value.get(headMp).getOrElse(AssocMap.empty).toList match
                            case List.Cons((tokenName, amount), none) =>
                                // TODO: check prefix
                                tokenName == cip67BeaconTokenPrefix
                                && amount == BigInt(1)
                                && none.isEmpty
                            case _ => fail()
                    }
                    .getOrFail("TBD")

                val treasuryDatum =
                    treasuryInput.resolved.inlineDatumOfType[TreasuryDatum] match {
                        case Unresolved(unresolvedDatum) => unresolvedDatum
                        case _                           => fail()
                    }

                // headMp and disputeId must match the corresponding fields of the Unresolved datum
                // in treasury.
                require(treasuryDatum.headMp === headMp)
                require(treasuryDatum.disputeId === disputeId)

                //
                fail()

end DisputeResolutionValidator

object DisputeResolutionScript {
    val sir = Compiler.compile(TallyingValidator.validate)
    val uplc = sir.toUplcOptimized(true)
}

//@main
//def main(args: String): Unit = {
//    println(TreasuryScript.sir.showHighlighted)
//}
