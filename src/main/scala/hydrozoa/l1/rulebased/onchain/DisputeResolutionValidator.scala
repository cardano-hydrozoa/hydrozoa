package hydrozoa.l1.rulebased.onchain

import hydrozoa.l1.rulebased.onchain.DisputeResolutionValidator.DisputeRedeemer.{
    Resolve,
    Tally,
    Vote
}
import hydrozoa.l1.rulebased.onchain.value.ValueExtensions.onlyNonAdaToken
import hydrozoa.l2.block.BlockTypeL2
import scalus.*
import scalus.builtin.ByteString.hex
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.ledger.api.v3.*
import scalus.prelude.Option.{None, Some}
import scalus.prelude.{===, Eq, List, Option, Validator, fail}

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

    inline def disputeTokenPrefix = hex"00d950b0"

    private inline val DatumIsMissing = "Vote datum should be present"
    private inline val OnlyOneVoteUtxoIsSpent = "Only one vote utxo can be spent"
    private inline val VoteAlreadyCast = "Vote is already has been cast"

    // Entry point
    override def spend(datum: Option[Data], redeemer: Data, tx: TxInfo, ownRef: TxOutRef): Unit =
        // Parse datum
        val voteDatum = datum match
            case Some(d) => d.to[VoteDatum]
            case None    => fail(DatumIsMissing)

        redeemer.to[DisputeRedeemer] match
            case Vote(_voteRedeemer) =>
                // Own input
                // There must not be any other spent input matching voteOutref on transaction hash
                val voteOutref = tx.inputs.filter(_.outRef._1 === ownRef._1) match
                    case List.Cons(voteOutref, tail) =>
                        require(tail.isEmpty, OnlyOneVoteUtxoIsSpent)
                        voteOutref
                    // Unreachable
                    case _ => fail()

                require(voteDatum.voteStatus === VoteStatus.NoVote, VoteAlreadyCast)

                // The transaction must be signed by the peer field of voteInput, if it is non-empty.
                voteDatum.peer match
                    case Some(peer) => tx.signatories.contains(peer)
                    case None       => ()

                // Let(headMp, disputeId) be the minting policy and asset name of the only non -ADA
                // tokens in voteInput.
                val voteInput = voteOutref.resolved

                val (headMp, disputeId) = voteInput.value.onlyNonAdaToken
                //
                fail()
            case Tally   => fail()
            case Resolve => fail()

end DisputeResolutionValidator

object DisputeResolutionScript {
    // val sir = Compiler.compile(DisputeResolutionValidator.validate)
    // val uplc = sir.toUplcOptimized(true)
}

//@main
//def main(args: String): Unit = {
//    println(TreasuryScript.sir.showHighlighted)
//}
