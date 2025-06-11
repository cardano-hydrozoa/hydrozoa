package hydrozoa.l1.rulebased.onchain

import hydrozoa.l1.rulebased.onchain.TreasuryValidator.TreasuryDatum.Unresolved
import hydrozoa.l1.rulebased.onchain.TreasuryValidator.{TreasuryDatum, cip67beaconPrefix}
import hydrozoa.l1.rulebased.onchain.lib.TxOutExtensions.inlineDatumOfType
import hydrozoa.l1.rulebased.onchain.lib.ValueExtensions.{
    containsCurrencySymbol,
    containsExactlyOneAsset,
    onlyNonAdaAsset,
    tokensUnder
}
import hydrozoa.l2.block.BlockTypeL2
import scalus.*
import scalus.builtin.Builtins.{serialiseData, verifyEd25519Signature}
import scalus.builtin.ByteString.hex
import scalus.builtin.ToData.toData
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.ledger.api.v1.IntervalBoundType.Finite
import scalus.ledger.api.v1.Value.+
import scalus.ledger.api.v3.*
import scalus.prelude.Option.{None, Some}
import scalus.prelude.{!==, ===, Eq, List, Option, Validator, fail, require, given}

@Compile
object DisputeResolutionValidator extends Validator:

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
        case Tally(tallyRedeemer: TallyRedeemer)
        case Resolve

    given FromData[DisputeRedeemer] = FromData.derived
    given ToData[DisputeRedeemer] = ToData.derived

    case class MinorBlockL1Effect(
        blockHeader: BlockHeader,
        multisig: List[Signature]
    )

    given FromData[MinorBlockL1Effect] = FromData.derived
    given ToData[MinorBlockL1Effect] = ToData.derived

    case class TallyRedeemer(
        continuingOutRef: TxOutRef,
        removedOutRef: TxOutRef
    )

    given FromData[TallyRedeemer] = FromData.derived
    given ToData[TallyRedeemer] = ToData.derived

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

    inline def disputeTokenPrefix = hex"00d950b0"

    private inline val DatumIsMissing = "Vote datum should be present"
    private inline val OnlyOneVoteUtxoIsSpent = "Only one vote utxo can be spent"
    private inline val VoteAlreadyCast = "Vote is already has been cast"
    private inline val MustBeSignedByPeer = "Transaction must be signed by peer"
    private inline val OneVoteTokenInput = "Only one vote token should be in vote utxo input"

    // Entry point
    override def spend(datum: Option[Data], redeemer: Data, tx: TxInfo, ownRef: TxOutRef): Unit =
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
                        require(tail.isEmpty, OnlyOneVoteUtxoIsSpent)
                        voteOutref
                    // Unreachable
                    case _ => fail()

                // Check vote status
                require(voteDatum.voteStatus === VoteStatus.NoVote, VoteAlreadyCast)
                // Check signature
                require(voteDatum.peer.forall(tx.signatories.contains(_)), MustBeSignedByPeer)

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

                // TODO: A head beacon token of headMp and CIP-67 prefix 4937 must be in treasury.

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

            case DisputeRedeemer.Tally(tallyRedeemer) =>
                // TODO: is it better than two .find()?
                val (continuingInput, removedInput) = {
                    tx.inputs.filter(i =>
                        i.outRef === tallyRedeemer.continuingOutRef || i.outRef === tallyRedeemer.removedOutRef
                    ) match
                        case List.Cons(i1, is) =>
                            is match {
                                case List.Cons(i2, none) =>
                                    require(none.isEmpty)
                                    if i1.outRef === tallyRedeemer.continuingOutRef then
                                        (i1.resolved, i2.resolved)
                                    else (i2.resolved, i1.resolved)
                                case _ => fail()
                            }
                        case _ => fail()
                }

                // continuingInput and removedInput must match on address
                require(continuingInput.address === removedInput.address)

                // continuingInput and removedInput must have non-ADA tokens of only one asset
                // class, which must match between them
                val (contCs, contTn, contAmount) = continuingInput.value.onlyNonAdaAsset
                val (removedCs, removedTn, removedAmount) = continuingInput.value.onlyNonAdaAsset
                require(contCs === removedCs && contTn === removedTn)

                // ... and satisfy both of the following:
                // (a) The minting policy ( headMp) must be a native script,
                // i.e. has prefix 'x00'
                // FIXME: check prefix, not the whole string
                require(contCs == hex"00")

                // (b) The asset name (disputeId) must have the CIP-67 prefix 3477.
                // TODO: should be something like tn.take(4)
                require(contTn == disputeTokenPrefix)

                // The key field of removedInput must be greater than the key field and equal to the
                // link field of continuingInput.
                val continuingDatum = continuingInput.inlineDatumOfType[VoteDatum]
                val removedDatum = removedInput.inlineDatumOfType[VoteDatum]
                require(
                  removedDatum.key > continuingDatum.key && removedDatum.key == continuingDatum.link
                )

                // There must be no other spent inputs from the same address as continuingInput
                // or holding any tokens of headMp.
                require(
                  tx.inputs
                      .filter(i =>
                          // Input other than we handle
                          (i.outRef !== tallyRedeemer.continuingOutRef) && (i.outRef !== tallyRedeemer.removedOutRef)
                          // with the same address
                              && (i.resolved.address === continuingInput.address
                              // or holding any tokens of headMp
                                  || i.resolved.value.containsCurrencySymbol(contCs))
                      )
                      .isEmpty
                )

                // Verify the treasury reference input
                // If the voteStatus of either continuingInput or removedInput is NoVote, all of
                // the following must be satisfied
                if (
                  continuingDatum.voteStatus === VoteStatus.NoVote
                  || removedDatum.voteStatus === VoteStatus.NoVote
                ) {
                    // Let treasury be a reference input holding the head beacon token of headMp
                    // and CIP-67 prefix 4937
                    val treasuryReference = tx.referenceInputs
                        .find { i =>
                            i.resolved.value.tokensUnder(contCs).toList match
                                case List.Cons((tokenName, amount), none) =>
                                    // TODO: check prefix
                                    tokenName == cip67beaconPrefix
                                    && amount == BigInt(1)
                                    && none.isEmpty
                                case _ => fail()
                        }
                        .getOrFail("TBD")

                    // TODO: duplicates a part of Vote redeemer
                    // headMp and disputeId must match the corresponding fields of the Unresolved
                    // datum in treasury
                    val treasuryDatum =
                        treasuryReference.resolved.inlineDatumOfType[TreasuryDatum] match {
                            case Unresolved(unresolvedDatum) => unresolvedDatum
                            case _                           => fail()
                        }
                    require(treasuryDatum.headMp === contCs)
                    require(treasuryDatum.disputeId === contTn)

                    // TODO: duplicates a part of Vote redeemer
                    // The transaction’s time -validity upper bound must not exceed the deadlineVoting
                    // field of treasury.
                    tx.validRange.to.boundType match {
                        case Finite(toTime) => require(toTime <= treasuryDatum.deadlineVoting)
                        case _              => fail()
                    }
                }

                // Verify the vote output

                // 8. Let continuingOutput be an output with the same address and the sum of all
                // tokens (including ADA) in continuingInput and removedInput.
                // TODO: check whether this is sufficient for the case of two tallying
                val continuingOutput = tx.outputs
                    .find(o =>
                        o.address === continuingInput.address
                            && o.value === continuingInput.value + removedInput.value
                    )
                    .get

                def maxVote(a: VoteStatus, b: VoteStatus): VoteStatus =
                    import VoteStatus.{Vote, NoVote}
                    a match {
                        case NoVote => b
                        case Vote(ad) =>
                            b match {
                                case NoVote   => a
                                case Vote(bd) => if ad.versionMinor > bd.versionMinor then a else b
                            }
                    }

                // The voteStatus field of continuingOutput must match the highest voteStatus
                // of continuingInput and removedInput
                val continuingOutputDatum = continuingOutput.inlineDatumOfType[VoteDatum]
                require(
                  continuingOutputDatum.voteStatus === maxVote(
                    continuingDatum.voteStatus,
                    removedDatum.voteStatus
                  )
                )

                // The link field of removedInput and continuingOutput must match.
                require(continuingOutputDatum.link == removedDatum.link)

                // All other fields of continuingInput and continuingOutput must match.
                require(continuingOutputDatum.key === removedDatum.key)
                require(continuingOutputDatum.peer === removedDatum.peer)

            case DisputeRedeemer.Resolve =>
                val voteInput = tx.inputs.find(_.outRef === ownRef).get
                val (headMp, disputeId, _) = voteInput.resolved.value.onlyNonAdaAsset

                // Let treasury be a spent input that holds a head beacon token of headMp and CIP-67
                // prefix 4937.
                val treasuryInput = tx.inputs
                    .find { i =>
                        i.resolved.value.tokensUnder(headMp).toList match
                            case List.Cons((tokenName, amount), none) =>
                                // TODO: check prefix
                                tokenName == cip67beaconPrefix
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
    val sir = Compiler.compile(DisputeResolutionValidator.validate)
    val uplc = sir.toUplcOptimized(true)
}

//@main
//def main(args: String): Unit = {
//    println(TreasuryScript.sir.showHighlighted)
//}
