package hydrozoa.l1.rulebased.onchain

import com.bloxbean.cardano.client.plutus.spec.PlutusV3Script
import hydrozoa.l1.rulebased.onchain.DisputeResolutionValidator.{VoteDatum, VoteStatus}
import hydrozoa.l1.rulebased.onchain.TreasuryValidator.TreasuryDatum.Unresolved
import hydrozoa.l1.rulebased.onchain.TreasuryValidator.{TreasuryDatum, cip67BeaconTokenPrefix}
import hydrozoa.l1.rulebased.onchain.lib.ByteStringExtensions.take
import hydrozoa.l1.rulebased.onchain.lib.TxOutExtensions.inlineDatumOfType
import hydrozoa.l1.rulebased.onchain.lib.ValueExtensions.{containsCurrencySymbol, onlyNonAdaAsset}
import scalus.*
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.ledger.api.v1.IntervalBoundType.Finite
import scalus.ledger.api.v1.Value.+
import scalus.ledger.api.v3.*
import scalus.prelude.Option.{None, Some}
import scalus.prelude.{!==, ===, AssocMap, Eq, List, Option, Validator, fail, require, given}
import scalus.sir.SIR

@Compile
object TallyingValidator extends Validator:

    case class TallyRedeemer(
        continuingOutRef: TxOutRef,
        removedOutRef: TxOutRef
    )

    given FromData[TallyRedeemer] = FromData.derived
    given ToData[TallyRedeemer] = ToData.derived

    private inline val VotingInputsNotFound =
        "Continuing and removed inputs not found"
    private inline val VotingInputsDoNotMatch =
        "Voting inputs must match on address and tokens"
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
    private inline val TimeValidityCheck =
        "The transaction validity upper bound must not exceed the deadlineVoting"
    private inline val HighestVoteCheck =
        "continuingOutput must match the highest voteStatus"
    private inline val LinkCheck =
        "The link field of removedInput and continuingOutput must match"
    private inline val KeyCheck =
        "Key field of continuingInput and continuingOutput must match"
    private inline val PeerCheck =
        "Peer field of continuingInput and continuingOutput must match"

    // Entry point
    override def reward(redeemer: Datum, _stakingKey: Credential, tx: TxInfo): Unit =

        val tallyRedeemer = redeemer.to[TallyRedeemer]

        // Get voting inputs
        val (continuingInput, removedInput) = {
            tx.inputs.filter(i =>
                i.outRef === tallyRedeemer.continuingOutRef || i.outRef === tallyRedeemer.removedOutRef
            ) match
                case List.Cons(i1, is) =>
                    is match {
                        case List.Cons(i2, none) =>
                            require(none.isEmpty, VotingInputsNotFound)
                            if i1.outRef === tallyRedeemer.continuingOutRef then
                                (i1.resolved, i2.resolved)
                            else (i2.resolved, i1.resolved)
                        case _ => fail(VotingInputsNotFound)
                    }
                case _ => fail(VotingInputsNotFound)
        }

        // continuingInput and removedInput must match on address
        require(continuingInput.address === removedInput.address, VotingInputsDoNotMatch)

        // continuingInput and removedInput must have non-ADA tokens of only one asset
        // class, which must match between them
        val (contCs, contTn, contAmount) = continuingInput.value.onlyNonAdaAsset
        val (removedCs, removedTn, removedAmount) = continuingInput.value.onlyNonAdaAsset
        require(contCs === removedCs && contTn === removedTn, VotingInputsDoNotMatch)

        // The key field of removedInput must be greater than the key field and equal to the
        // link field of continuingInput.
        val continuingDatum = continuingInput.inlineDatumOfType[VoteDatum]
        val removedDatum = removedInput.inlineDatumOfType[VoteDatum]
        require(
          removedDatum.key > continuingDatum.key && removedDatum.key == continuingDatum.link,
          KeyLinkFieldsDoNotMatch
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
              .isEmpty,
          NoOtherInputs
        )

        // Verify the treasury reference input

        // If the voteStatus of either continuingInput or removedInput is NoVote,
        // all the following must be satisfied
        if (
          continuingDatum.voteStatus === VoteStatus.NoVote
          || removedDatum.voteStatus === VoteStatus.NoVote
        ) {
            // Let treasury be a reference input holding the head beacon token of headMp
            // and CIP-67 prefix 4937
            val treasuryReference = tx.referenceInputs
                .find { i =>
                    i.resolved.value.get(contCs).getOrElse(AssocMap.empty).toList match
                        case List.Cons((tokenName, amount), none) =>
                            tokenName.take(4) == cip67BeaconTokenPrefix
                            && amount == BigInt(1)
                            && none.isEmpty
                        case _ => fail(TreasuryReferenceInputExists)
                }
                .getOrFail(TreasuryReferenceInputExists)

            // headMp and disputeId must match the corresponding fields of the Unresolved
            // datum in treasury
            val treasuryDatum =
                treasuryReference.resolved.inlineDatumOfType[TreasuryDatum] match {
                    case Unresolved(unresolvedDatum) => unresolvedDatum
                    case _                           => fail(TreasuryDatumIsUnresolved)
                }

            require(treasuryDatum.headMp === contCs, TreasuryDatumMatchesHeadMp)
            require(treasuryDatum.disputeId === contTn, TreasuryDatumMatchesDisputeId)

            // The transaction’s time -validity upper bound must not exceed the deadlineVoting
            // field of treasury.
            tx.validRange.to.boundType match {
                case Finite(toTime) =>
                    require(toTime <= treasuryDatum.deadlineVoting, TimeValidityCheck)
                case _ => fail(TimeValidityCheck)
            }
        }

        // Verify the vote output

        // 8. Let continuingOutput be an output with the same address and the sum of all
        // tokens (including ADA) in continuingInput and removedInput.
        val continuingOutput = tx.outputs
            .find(o =>
                o.address === continuingInput.address
                    && o.value === continuingInput.value + removedInput.value
            )
            .get

        def maxVote(a: VoteStatus, b: VoteStatus): VoteStatus =
            import VoteStatus.{NoVote, Vote}
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
          ),
          HighestVoteCheck
        )

        // The link field of removedInput and continuingOutput must match.
        require(continuingOutputDatum.link == removedDatum.link, LinkCheck)

        // All other fields of continuingInput and continuingOutput must match.
        require(continuingOutputDatum.key === removedDatum.key, KeyCheck)
        require(continuingOutputDatum.peer === removedDatum.peer, PeerCheck)

end TallyingValidator

object TallyingValidatorScript {
    lazy val sir: SIR = Compiler.compile(TallyingValidator.validate)
    private lazy val script = sir.toUplcOptimized(generateErrorTraces = true).plutusV3

    // TODO: can we use Scalus for that?
    private val plutusScript: PlutusV3Script = PlutusV3Script
        .builder()
        .`type`("PlutusScriptV3")
        .cborHex(script.doubleCborHex)
        .build()
        .asInstanceOf[PlutusV3Script]

    lazy val scriptHash: ByteString = ByteString.fromArray(plutusScript.getScriptHash)
}

@main
def tallyingValidatorSir(args: String): Unit = {
    println(TallyingValidatorScript.sir.showHighlighted)
    println(TallyingValidatorScript.scriptHash)
}
