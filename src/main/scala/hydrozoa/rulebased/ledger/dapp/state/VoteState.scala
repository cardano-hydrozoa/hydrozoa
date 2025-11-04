package hydrozoa.rulebased.ledger.dapp.state

import cats.data.NonEmptyList
import hydrozoa.rulebased.ledger.dapp.state.VoteState.VoteStatus.NoVote
import hydrozoa.rulebased.ledger.dapp.state.VoteState.{
    KzgCommitment,
    VoteDatum,
    VoteDetails,
    VoteStatus
}
import scalus.*
import scalus.builtin.Data.{FromData, ToData}
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.ledger.api.v3.PubKeyHash
import scalus.prelude.{===, Eq, Option}

object VoteDatum {
    def default(commitment: KzgCommitment): VoteDatum = VoteState.VoteDatum(
        key = 0,
        // N.B.: Version "Branch: (None) @ c28633a • Commit date: 2025-10-16" of the spec says
        // to set link to `0 < peersN ? 1 : 0`. But we have peers as a NonEmptyList, so this is just 1.
        link = 1,
        peer = Option.None,
        voteStatus = VoteStatus.Vote(VoteDetails(commitment = commitment, versionMinor = 0))
    )

    def apply(peers: NonEmptyList[PubKeyHash]): NonEmptyList[VoteState.VoteDatum] = {
        {
            val numPeers = peers.length
            val mapFunc = (x: (PubKeyHash, Int)) => {
                val i = x._2
                val pkh = x._1
                VoteState.VoteDatum(
                    key = i,
                    link = if i < numPeers then i + 1 else 0,
                    peer = Option.Some(pkh),
                    voteStatus = NoVote
                )
            }
            peers.zipWithIndex.map(mapFunc)
        }
    }
}


@Compile
object VoteState:

    case class VoteDatum(
        key: Key,
        link: Link,
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

    case class VoteDetails(
        commitment: KzgCommitment,
        versionMinor: BigInt
    )

    given FromData[VoteDetails] = FromData.derived
    given ToData[VoteDetails] = ToData.derived

    given Eq[VoteDetails] = (a: VoteDetails, b: VoteDetails) =>
        a.commitment == b.commitment && a.versionMinor == b.versionMinor

    type Key = BigInt

    type Link = BigInt

    // G1 compressed point
    type KzgCommitment = ByteString

end VoteState
