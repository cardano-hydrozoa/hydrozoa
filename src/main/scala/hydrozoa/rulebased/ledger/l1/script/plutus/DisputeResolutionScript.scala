package hydrozoa.rulebased.ledger.l1.script.plutus

import hydrozoa.*
import hydrozoa.lib.cardano.scalus.cardano.onchain.plutus.ByteStringExtension.take
import hydrozoa.lib.cardano.scalus.cardano.onchain.plutus.TxOutExtension.inlineDatumOfType
import hydrozoa.lib.cardano.scalus.cardano.onchain.plutus.ValueExtension.*
import hydrozoa.multisig.ledger.stack.StandaloneEvacuationCommitment
import hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionValidator.TallyRedeemer.{Continuing, Removed}
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.cip67BeaconTokenPrefix
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatumOnchain
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatumOnchain.UnresolvedOnchain
import hydrozoa.rulebased.ledger.l1.state.VoteState
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus.AwaitingVote
import hydrozoa.rulebased.ledger.l1.state.VoteState.{VoteDatum, VoteStatus}
import scala.annotation.tailrec
import scalus.*
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.onchain.plutus.prelude.Option.{None, Some}
import scalus.cardano.onchain.plutus.prelude.{!==, ===, List, Option, SortedMap, fail, log, require}
import scalus.cardano.onchain.plutus.v1.Value.+
import scalus.cardano.onchain.plutus.v3.*
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.Builtins.{serialiseData, verifyEd25519Signature}
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}

@Compile
object DisputeResolutionValidator extends Validator {

    // Redeemer
    enum DisputeRedeemer:
        case Vote(voteRedeemer: VoteRedeemer)
        case Tally(tallyRedeemer: TallyRedeemer)
        case Resolve
        case Abstain

    given FromData[DisputeRedeemer] = FromData.derived
    given ToData[DisputeRedeemer] = ToData.derived

    case class VoteRedeemer(
        sec: StandaloneEvacuationCommitment.Onchain,
        headMultisig: List[Signature],
        coilMultisig: List[Option[Signature]]
    )

    given FromData[VoteRedeemer] = FromData.derived
    given ToData[VoteRedeemer] = ToData.derived

    // EdDSA / ed25519 signature
    private type Signature = ByteString

    enum BlockTypeL2:
        case Minor
        case Major
        case Final

    given FromData[BlockTypeL2] = FromData.derived
    given ToData[BlockTypeL2] = ToData.derived

    enum TallyRedeemer:
        case Continuing
        case Removed

    given FromData[TallyRedeemer] = FromData.derived
    given ToData[TallyRedeemer] = ToData.derived

    inline def cip67DisputeTokenPrefix = hex"021eb240"

    // Common errors
    private inline val DatumIsMissing = "Vote datum should be present"

    // Vote redeemer
    private inline val VoteOnlyOneVoteUtxoIsSpent = "Only one vote utxo can be spent"
    private inline val VoteRatchetNotMonotonic =
        "Open-phase ratchet must strictly increase versionMinor"
    private inline val VoteMustBeSignedByPeer = "Transaction must be signed by peer"
    private inline val VoteOneRefInputTreasury = "Only one ref input (treasury) is required"
    private inline val VoteTreasuryBeacon = "Treasury should contain beacon token"
    private inline val VoteHeadIdMismatch =
        "sec.headId must equal the treasury reference input's HYDR token name"
    private inline val VoteTreasuryDatum = "Treasury datum is missing"
    private inline val VoteTreasuryDatumHeadMp = "Treasury datum headMp mismatch"
    private inline val VoteTreasuryDatumDisputeId = "Treasury datum disputeId mismatch"
    private inline val VoteTimeValidityCheck =
        "The transaction validity upper bound must not exceed the deadlineVoting"
    private inline val VoteMultisigCheck =
        "Redeemer should contain all valid signatures for the block voted"
    private inline val VoteCoilQuorumCheck =
        "coilMultisig must contain at least coilQuorum valid signatures"
    private inline val VoteCoilSigInvalid =
        "Invalid coil-peer signature in coilMultisig"
    private inline val VoteMajorVersionCheck =
        "The versionMajor field must match between treasury and voteRedeemer"
    private inline val VoteVoteOutputExists =
        "There should exist one continuing vote output with the same value"
    private inline val VoteOutputDatumCheck =
        "voteStatus field of voteOutput must be a correct Vote"
    private inline val VoteOutputDatumAdditionalChecks =
        "other fields of voteOutput must match voteInput"
    private inline val VoteOutputNoScriptRef =
        "Vote output must not carry a reference script"

    // Tally redeemer
    private inline val VotingInputsNotFound =
        "Continuing and removed inputs not found"
    private inline val TwoVotingInputsExpected =
        "Exactly two voting inputs are expected"
    private inline val VotingInputsDoNotMatch =
        "Voting inputs must match on voting tokens"
    private inline val KeyLinkFieldsDoNotMatch =
        "Key and link fields in voting inputs must match"
    private inline val NoOtherInputs =
        "No other inputs are allowed"
    private inline val TreasuryReferenceInputExists =
        "Treasury reference should present"
    private inline val TreasuryDatumIsUnresolved =
        "Treasury datum should be unresolved"
    private inline val TreasuryDatumMatchesHeadMp =
        "Treasury datum should match voting inputs on head policy"
    private inline val TreasuryDatumMatchesDisputeId =
        "Treasury datum should match voting inputs on dispute id"
    private inline val TallyValidityStartRequired =
        "Tally tx must specify the validity start (to prove deadline has elapsed)"
    private inline val TallyOnlyAfterVotingDeadline =
        "Tally tx may only fire after deadlineVoting has elapsed"
    private inline val HighestVoteCheck =
        "continuingOutput must match the highest voteStatus"
    private inline val AbsentOrWrongContinuingOutput =
        "The continuing output was not found or contains wrong value"
    private inline val LinkCheck =
        "The link field of removedInput and continuingOutput must match"
    private inline val KeyCheck =
        "Key field of continuingInput and continuingOutput must match"
    private inline val TallyOutputNoScriptRef =
        "Tally continuing output must not carry a reference script"

    // Resolve redeemer
    private inline val ResolveTreasurySpent =
        "Treasury that holds head beacon must be spent"
    private inline val ResolveDatumIsUnresolved =
        "Treasury input datum should be unresolved"
    private inline val ResolveTreasuryVoteMatch =
        "Treasury datum should match vote datum on (headMp, disputeId)"

    // Abstain redeemer
    private inline val AbstainOnlyOneVoteUtxoIsSpent =
        "Only one vote utxo can be spent"
    private inline val AbstainVoteInputNotFound =
        "Spent input matching ownRef not found"
    private inline val AbstainOnlyFromAwaitingVote =
        "Abstain is only valid from AwaitingVote"
    private inline val AbstainMustBeSignedByPeer =
        "Abstain must be signed by the peer whose vote utxo is being abstained"
    private inline val AbstainVoteOutputExists =
        "There should exist one continuing vote output with the same value and address"
    private inline val AbstainVoteOutputNoScriptRef =
        "Vote output must not carry a reference script"
    private inline val AbstainOutputDatumCheck =
        "voteStatus field of voteOutput must be Abstain"
    private inline val AbstainOutputDatumAdditionalChecks =
        "other fields of voteOutput must match voteInput"

    // Entry point
    override inline def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit =

        log("DisputeResolution")

        // Parse datum
        val voteDatum: VoteDatum = datum match
            case Some(d) => d.to[VoteDatum]
            case None    => fail(DatumIsMissing)

        redeemer.to[DisputeRedeemer] match
            case DisputeRedeemer.Vote(voteRedeemer) =>
                log("Vote")

                // There must not be any other spent input matching voteOutref on transaction hash
                val voteOutref = tx.inputs.filter(_.outRef.id === ownRef.id) match
                    case List.Cons(voteOutref, tail) =>
                        require(tail.isEmpty, VoteOnlyOneVoteUtxoIsSpent)
                        voteOutref
                    // Unreachable
                    case _ => fail()

                voteDatum.voteStatus match {
                    case AwaitingVote(pkh) =>
                        // Reserved phase: only the named peer can transition this box (one-shot).
                        require(tx.signatories.contains(pkh), VoteMustBeSignedByPeer)
                    case VoteStatus.Voted(_, prevVersionMinor) =>
                        // Open phase: any multisigned SEC can ratchet this box, but only with
                        // strictly higher versionMinor (foundation I8).
                        require(
                          voteRedeemer.sec.versionMinor > prevVersionMinor,
                          VoteRatchetNotMonotonic
                        )
                    case VoteStatus.Abstain =>
                        // Open phase: any multisigned SEC can ratchet from Abstain to Voted.
                        // Implicit prior versionMinor is 0; SEC must have versionMinor > 0.
                        require(
                          voteRedeemer.sec.versionMinor > BigInt(0),
                          VoteRatchetNotMonotonic
                        )
                }

                // Let(headMp, disputeId) be the minting policy and asset name of the only non-ADA
                // tokens in voteInput.
                val voteInput = voteOutref.resolved
                val (headMp, disputeId, voteTokenAmount) = voteInput.value.onlyNonAdaAsset

                // Verify the treasury reference input
                // Find a reference input that holds a CIP-67-HYDR-prefixed token under the same
                // headMp policy as the vote utxo. (The previous txid-match filter only worked for
                // ratchets on fallback-created boxes — under foundation I8 we must support
                // arbitrary post-AwaitingVote ratchet chains where the vote utxo's source-tx
                // txid diverges from the treasury's.)
                val treasuryReference = tx.referenceInputs
                    .find { i =>
                        i.resolved.value.toSortedMap
                            .get(headMp)
                            .getOrElse(SortedMap.empty)
                            .toList
                            .find((tokenName, amount) =>
                                tokenName.take(4) == cip67BeaconTokenPrefix
                                    && amount == BigInt(1)
                            ) match
                            case Some(_) => true
                            case _       => false
                    }
                    .getOrFail(VoteOneRefInputTreasury)

                // A head beacon token of headMp and CIP-67 prefix 4937 must be the only
                // headMp-policy token in treasury, and its full asset name must equal
                // voteRedeemer.sec.headId (foundation I5 — no cross-head contamination).
                treasuryReference.resolved.value.toSortedMap
                    .get(headMp)
                    .getOrElse(SortedMap.empty)
                    .toList match
                    case List.Cons(tokenNameAndAmount, none) =>
                        val tokenName = tokenNameAndAmount._1
                        require(
                          none.isEmpty && tokenName.take(4) == cip67BeaconTokenPrefix,
                          VoteTreasuryBeacon
                        )
                        require(voteRedeemer.sec.headId === tokenName, VoteHeadIdMismatch)
                    case _ => fail(VoteTreasuryBeacon)

                //  headMp and disputeId must match the corresponding fields of the Unresolved datum in treasury.
                val treasuryDatum =
                    treasuryReference.resolved
                        .inlineDatumOfType[RuleBasedTreasuryDatumOnchain] match {
                        case u: UnresolvedOnchain => u
                        case _                    => fail(VoteTreasuryDatum)
                    }
                require(treasuryDatum.headMp === headMp, VoteTreasuryDatumHeadMp)
                require(treasuryDatum.disputeId === disputeId, VoteTreasuryDatumDisputeId)

                // The transaction’s time -validity upper bound must not exceed the deadlineVoting
                // field of treasury.
                tx.validRange.to.boundType match {
                    case IntervalBoundType.Finite(toTime) =>
                        require(toTime <= treasuryDatum.deadlineVoting, VoteTimeValidityCheck)
                    case _ => fail(VoteTimeValidityCheck)
                }

                // The multisig field of voteRedeemer must have signatures of the blockHeader
                // field of voteRedeemer for all the public keys in the headPeers field of treasury.
                val msg = voteRedeemer.sec.toData |> serialiseData
                require(
                  treasuryDatum.headPeers.length == voteRedeemer.headMultisig.length,
                  VoteMultisigCheck
                )

                // Temporary workaround
                import scalus.cardano.onchain.plutus.prelude.List.{Cons, Nil}
                @tailrec
                def verifySignatures(a: List[ByteString], b: List[ByteString]): Unit =
                    a match
                        case Cons(h1, t1) =>
                            b match
                                case Cons(h2, t2) =>
                                    require(verifyEd25519Signature(h1, msg, h2))
                                    verifySignatures(t1, t2)
                                case Nil => ()
                        case Nil => ()

                verifySignatures(treasuryDatum.headPeers, voteRedeemer.headMultisig)

                // The coilMultisig field is sparse and position-aligned to coilPeers. It does NOT
                // need to contain the exact length of entries; no more than coilQuorum signatures
                // are verified, even if more are provided. Each Some(sig) must be a valid
                // signature — an invalid signature in any Some slot aborts the transaction.
                //
                // TODO (Scalus team): the List[Option[Signature]] encoding bloats wire size when
                // many coil peers abstain. A sparser encoding like List[(CoilPeerId, Signature)]
                // ascending in CoilPeerId would be cheaper on wire — at some UPLC cost for the
                // ascending-order check and the index lookup. Worth evaluating once cost
                // benchmarks are in place.
                @tailrec
                def verifyCoilSignatures(
                    keys: List[ByteString],
                    sigs: List[Option[ByteString]],
                    count: BigInt
                ): BigInt = {
                    if count == treasuryDatum.coilQuorum
                    then count
                    else
                        keys match
                            case Nil => count
                            case Cons(k, ks) =>
                                sigs match
                                    case Nil => count
                                    case Cons(s, ss) =>
                                        s match
                                            case scalus.cardano.onchain.plutus.prelude.Option
                                                    .Some(sig) =>
                                                require(
                                                  verifyEd25519Signature(k, msg, sig),
                                                  VoteCoilSigInvalid
                                                )
                                                verifyCoilSignatures(ks, ss, count + 1)
                                            case scalus.cardano.onchain.plutus.prelude.Option.None =>
                                                verifyCoilSignatures(ks, ss, count)
                }

                val coilSigCount = verifyCoilSignatures(
                  treasuryDatum.coilPeers,
                  voteRedeemer.coilMultisig,
                  BigInt(0)
                )
                require(coilSigCount == treasuryDatum.coilQuorum, VoteCoilQuorumCheck)

                // The versionMajor field must match between treasury and voteRedeemer.
                require(
                  voteRedeemer.sec.versionMajor == treasuryDatum.versionMajor,
                  VoteMajorVersionCheck
                )

                // Vote output
                val voteOutput = tx.outputs.find(o =>
                    // We decided to use collateral, now the value should be preserved.
                    o.value === voteInput.value
                ) match
                    case Some(e) => e
                    case None    => fail(VoteVoteOutputExists)

                require(voteOutput.address === voteInput.address, VoteVoteOutputExists)

                // Reject an attached reference script — would bloat the utxo and could push
                // downstream Tally / Resolve over the tx-size limit (denial of evacuation).
                voteOutput.referenceScript match
                    case None    => ()
                    case Some(_) => fail(VoteOutputNoScriptRef)

                val voteOutputDatum = voteOutput.inlineDatumOfType[VoteDatum]

                // voteStatus field of voteOutput must be a Vote matching voteRedeemer
                // on the utxosActive and versionMinor fields.
                voteOutputDatum.voteStatus match {
                    case VoteStatus.Voted(commitment, versionMinor) =>
                        require(
                          versionMinor == voteRedeemer.sec.versionMinor,
                          VoteOutputDatumCheck
                        )
                        require(
                          commitment == voteRedeemer.sec.commitment,
                          VoteOutputDatumCheck
                        )
                    case _ => fail(VoteOutputDatumCheck)
                }

                // All other fields of voteInput and voteOutput must match.
                require(voteDatum.key === voteOutputDatum.key, VoteOutputDatumAdditionalChecks)
                require(voteDatum.link === voteOutputDatum.link, VoteOutputDatumAdditionalChecks)

            /** Tallying is done as follows:
              *
              * +-----------+        +-----------+        +-----------+        +-----------+
              * |  key = 0  |        |  key = 1  |        |  key = 2  |        |  key = 3  |
              * | link = 1  |        | link = 2  |-+      | link = 3  |        | link = 0  |-+
              * |CONTINUING |        |  REMOVED  | |      |CONTINUING |        |  REMOVED  | |
              * |                                  |      |                                  |
              * v                                  |       v                                 |
              * +-----------+                      |       +-----------+                     |
              * |  key = 0  |                      |       |  key = 2  |                     |
              * | link = 2  | <------------------- +       | link = 0  |<-------+------------+
              * |CONTINUING |                              |  REMOVED  |        |
              * |                                                               |
              * v                                                               |
              * +-----------+                                                   |
              * |  key = 0  |                                                   |
              * | link = 0  |<--------------------------------------------------+
              * |   FINAL   |
              *
              * NB:
              * 1. `peer` is always set to None since it's not needed and since it's not relevant
              * 2. the higher `vote` is selected
              * 3. we start pair-wise from the beginning, but an arbitrary adjacent votes can be tallied
              */

            case DisputeRedeemer.Tally(tallyRedeemer) =>
                log("Tally")

                // TODO: hide `ownInput` and `otherInput` so they can't be used accidentally
                val ownInput = tx.inputs.find(_.outRef === ownRef).get
                val otherInput: TxInInfo =
                    tx.inputs.filter(i =>
                        i.resolved.address === ownInput.resolved.address && (i.outRef !== ownRef)
                    ) match
                        case List.Cons(other, empty) =>
                            require(empty.isEmpty, TwoVotingInputsExpected)
                            other
                        case _ => fail(VotingInputsNotFound)

                val ((continuingInputId, continuingInput), (removedInputId, removedInput)) =
                    tallyRedeemer match
                        case Continuing =>
                            // TODO: make a helper
                            (
                              (ownInput.outRef, ownInput.resolved),
                              (otherInput.outRef, otherInput.resolved)
                            )
                        case Removed =>
                            // TODO: make a helper
                            (
                              (otherInput.outRef, otherInput.resolved),
                              (ownInput.outRef, ownInput.resolved)
                            )

                // continuingInput and removedInput must have non-ADA tokens of only one asset
                // class, which must match between them
                val (contCs, contTn, _) = continuingInput.value.onlyNonAdaAsset
                val (removedCs, removedTn, _) = removedInput.value.onlyNonAdaAsset
                require(contCs === removedCs && contTn === removedTn, VotingInputsDoNotMatch)

                // The key field of removedInput must be greater than the key field and equal to the
                // link field of continuingInput.
                val continuingDatum = continuingInput.inlineDatumOfType[VoteDatum]
                val removedDatum = removedInput.inlineDatumOfType[VoteDatum]
                require(
                  removedDatum.key > continuingDatum.key && removedDatum.key == continuingDatum.link,
                  KeyLinkFieldsDoNotMatch
                )

                // FIXME: we don't have address anymore since address is how we find the other input
                // There must be no other spent inputs from the same address as continuingInput
                // or holding any tokens of headMp.
                require(
                  tx.inputs
                      .filter(i =>
                          // Input other than we handle
                          (i.outRef !== continuingInputId) && (i.outRef !== removedInputId)
                          // holding any tokens of headMp
                              && (i.resolved.value.containsCurrencySymbol(contCs))
                      )
                      .isEmpty,
                  NoOtherInputs
                )

                // Verify the treasury reference input

                // Always require the tally tx's validity start to be at or after deadlineVoting.
                // The key=0 ballot box starts in the Open phase and remains ratchet-able by any
                // multisigned SEC up until the deadline; since every dispute includes that box,
                // local short-cuts based on the two specific tally inputs cannot speed up the
                // global tally. Simplest rule: deadline applies uniformly.

                // Let treasury be a reference input holding the head beacon token of headMp
                // and CIP-67 prefix 4937
                val treasuryReference = tx.referenceInputs
                    .find { i =>
                        i.resolved.value.toSortedMap
                            .get(contCs)
                            .getOrElse(SortedMap.empty)
                            .toList
                            .find((tokenName, amount) =>
                                tokenName.take(4) == cip67BeaconTokenPrefix
                                    && amount == BigInt(1)
                            ) match
                            case Some(_) => true
                            case _       => false
                    }
                    .getOrFail(TreasuryReferenceInputExists)

                // headMp and disputeId must match the corresponding fields of the Unresolved
                // datum in treasury
                val treasuryDatum =
                    treasuryReference.resolved
                        .inlineDatumOfType[RuleBasedTreasuryDatumOnchain] match {
                        case u: UnresolvedOnchain => u
                        case _                    => fail(TreasuryDatumIsUnresolved)
                    }

                require(treasuryDatum.headMp === contCs, TreasuryDatumMatchesHeadMp)
                require(treasuryDatum.disputeId === contTn, TreasuryDatumMatchesDisputeId)

                tx.validRange.from.boundType match {
                    case IntervalBoundType.Finite(fromTime) =>
                        require(
                          treasuryDatum.deadlineVoting <= fromTime,
                          TallyOnlyAfterVotingDeadline
                        )
                    case _ => fail(TallyValidityStartRequired)
                }

                // Verify the vote output

                // 8. Let continuingOutput be an output with:
                // - same address
                // - combined tokens
                // - ada amount that is not less than ada in continuing input + max (0, (ada in removed input - tx fees))

                // This is better that require(removedInput.value.getLovelace - tx.fee), since this may prevent the
                // tx from getting through.
                val residualAda = {
                    val residualAda = removedInput.value.getLovelace - tx.fee
                    if residualAda > 0 then residualAda else BigInt(0)
                }

                val continuingOutput = tx.outputs
                    .find(o =>
                        o.address === continuingInput.address
                            && o.value.onlyNonAdaAsset === (continuingInput.value + removedInput.value).onlyNonAdaAsset
                            && o.value.getLovelace >= continuingInput.value.getLovelace + residualAda
                    )
                    .getOrFail(AbsentOrWrongContinuingOutput)

                // Reject an attached reference script — would bloat the utxo and could push
                // downstream Tally / Resolve over the tx-size limit (denial of evacuation).
                continuingOutput.referenceScript match
                    case None    => ()
                    case Some(_) => fail(TallyOutputNoScriptRef)

                // 9. The voteStatus field of continuingOutput must match the highest voteStatus
                // of continuingInput and removedInput
                val continuingOutputDatum = continuingOutput.inlineDatumOfType[VoteDatum]
                require(
                  continuingOutputDatum.voteStatus === maxVote(
                    continuingDatum.voteStatus,
                    removedDatum.voteStatus
                  ),
                  HighestVoteCheck
                )

                // 10. The link field of removedInput and continuingOutput must match.
                require(continuingOutputDatum.link == removedDatum.link, LinkCheck)

                // 11. All other fields of continuingInput and continuingOutput must match.
                require(continuingOutputDatum.key === continuingDatum.key, KeyCheck)

            case DisputeRedeemer.Resolve =>
                log("Resolve")

                val voteInput = tx.inputs.find(_.outRef === ownRef).get
                val (headMp, disputeId, _) = voteInput.resolved.value.onlyNonAdaAsset

                // Let treasury be a spent input that holds a head beacon token of headMp and CIP-67
                // prefix 4937.
                val treasuryInput = tx.inputs
                    .find { i =>
                        i.resolved.value.toSortedMap
                            .get(headMp)
                            .getOrElse(SortedMap.empty)
                            .toList match
                            case List.Cons(tokenNameAndAmount, none) =>
                                val tokenName = tokenNameAndAmount._1
                                val amount = tokenNameAndAmount._2
                                tokenName.take(4) == cip67BeaconTokenPrefix
                                && amount == BigInt(1)
                                && none.isEmpty
                            case _ => false
                    }
                    .getOrFail(ResolveTreasurySpent)

                // TODO: This is checked by the treasury validator
                val treasuryDatum =
                    treasuryInput.resolved.inlineDatumOfType[RuleBasedTreasuryDatumOnchain] match {
                        case u: UnresolvedOnchain => u
                        case _                    => fail(ResolveDatumIsUnresolved)
                    }

                // headMp and disputeId must match the corresponding fields of the Unresolved datum
                // in treasury.
                require(treasuryDatum.headMp === headMp, ResolveTreasuryVoteMatch)
                require(treasuryDatum.disputeId === disputeId, ResolveTreasuryVoteMatch)

            case DisputeRedeemer.Abstain =>
                log("Abstain")

                // 1. Exactly one input spent at this address (this utxo).
                val voteOutref = tx.inputs.filter(_.outRef.id === ownRef.id) match
                    case List.Cons(o, tail) =>
                        require(tail.isEmpty, AbstainOnlyOneVoteUtxoIsSpent)
                        o
                    case List.Nil => fail(AbstainVoteInputNotFound)

                // 2. Input status must be AwaitingVote(peer).
                val votePeer = voteDatum.voteStatus match {
                    case AwaitingVote(pkh) => pkh
                    case _                 => fail(AbstainOnlyFromAwaitingVote)
                }

                // 3. The peer whose utxo this is must sign.
                require(tx.signatories.contains(votePeer), AbstainMustBeSignedByPeer)

                // 4. Continuing output: same address + value (vote token + ada preserved),
                //    datum flipped to Abstain with key/link unchanged, no reference script.
                val voteInput = voteOutref.resolved
                val voteOutput = tx.outputs.find(o => o.value === voteInput.value) match
                    case Some(o) => o
                    case None    => fail(AbstainVoteOutputExists)
                require(voteOutput.address === voteInput.address, AbstainVoteOutputExists)

                // Reject an attached reference script — would bloat the utxo and could push
                // downstream Tally / Resolve over the tx-size limit (denial of evacuation).
                voteOutput.referenceScript match
                    case None    => ()
                    case Some(_) => fail(AbstainVoteOutputNoScriptRef)

                val voteOutputDatum = voteOutput.inlineDatumOfType[VoteDatum]
                voteOutputDatum.voteStatus match {
                    case VoteStatus.Abstain => ()
                    case _                  => fail(AbstainOutputDatumCheck)
                }
                require(voteDatum.key === voteOutputDatum.key, AbstainOutputDatumAdditionalChecks)
                require(voteDatum.link === voteOutputDatum.link, AbstainOutputDatumAdditionalChecks)

    /** Pick the higher-precedence vote status. Precedence: `Voted` > `AwaitingVote` > `Abstain`.
      */
    def maxVote(a: VoteStatus, b: VoteStatus): VoteStatus =
        import VoteStatus.{Abstain, AwaitingVote, Voted}
        a match {
            case Abstain => b
            case AwaitingVote(_) =>
                b match {
                    case Abstain => a
                    case _       => b
                }
            case Voted(_commitmentA, versionMinorA) =>
                b match {
                    case Voted(_commitmentB, versionMinorB) =>
                        if versionMinorA > versionMinorB then a else b
                    case _ => a
                }
        }

}

object DisputeResolutionScript {
    // Compile the validator using PlutusV3.compile
    given scalus.compiler.Options = scalus.compiler.Options.default

    val compiledPlutusV3Program: PlutusV3[Data => Unit] =
        PlutusV3.compile(DisputeResolutionValidator.validate)

    private val compiledScriptHash: ScriptHash = compiledPlutusV3Program.script.scriptHash

    def address(n: Network): ShelleyAddress = ShelleyAddress(
      network = n,
      payment = ShelleyPaymentPart.Script(
        scalus.cardano.ledger.ScriptHash.fromArray(this.compiledScriptHash.bytes)
      ),
      delegation = ShelleyDelegationPart.Null
    )

}
