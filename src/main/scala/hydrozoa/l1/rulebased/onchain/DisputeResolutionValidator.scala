package hydrozoa.l1.rulebased.onchain

import hydrozoa.l2.block.BlockTypeL2
import scalus.Compiler.compile
import scalus.builtin.{ByteString, FromData, ToData}
import scalus.ledger.api.v1.{PosixTime, PubKeyHash}
import scalus.prelude.Validator
import scalus.uplc.Program
import scalus.{Ignore, plutusV3, toUplc}
import scalus.prelude.Option

object DisputeResolutionValidator extends Validator:

    // EdDSA / ed25519 signature
    private type Signature = ByteString
    private type RH32UtxoSetL2 = ByteString

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

    given FromData[VoteStatus] = FromData.derived
    given ToData[VoteStatus] = ToData.derived

    case class VoteDetails(
        utxosActive: RH32UtxoSetL2,
        versionMinor: BigInt
    )

    given FromData[VoteDetails] = FromData.derived
    given ToData[VoteDetails] = ToData.derived

    // Redeemer
    enum DisputeRedeemer:
        case Vote(voteRedeemer: Option[MinorBlockL1Effect])
        case Tally
        case Resolve

    case class MinorBlockL1Effect(
        blockHeader: BlockHeader,
        multisig: List[Signature]
    )

    // TODO: should we re-use Hydrozoa's type more broadly?
    case class BlockHeader(
        blockNum: BigInt,
        // TODO: should we re-use Hydrozoa's type more broadly?
        blockType: BlockTypeL2,
        timeCreation: PosixTime,
        versionMajor: BigInt,
        versionMinor: BigInt,
        utxosActive: RH32UtxoSetL2
    )

//    @Ignore
//    val script: Program = compile(DisputeResolutionValidator.validate)
//        .toUplc(generateErrorTraces = true)
//        .plutusV3

end DisputeResolutionValidator
