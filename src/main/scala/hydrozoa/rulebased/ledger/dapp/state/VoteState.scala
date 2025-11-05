package hydrozoa.rulebased.ledger.dapp.state

import cats.data.NonEmptyList
import hydrozoa.rulebased.ledger.dapp.state.VoteState.VoteStatus.{AwaitingVote, Voted}
import hydrozoa.rulebased.ledger.dapp.state.VoteState.{KzgCommitment, VoteDatum, VoteStatus}
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
        voteStatus = VoteStatus.Voted(commitment = commitment, versionMinor = 0)
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
                    voteStatus = AwaitingVote(pkh))}
            peers.zipWithIndex.map(mapFunc)
        }
    }
}

@Compile
object VoteState:
    case class VoteDatum(
        key: Key,
        link: Link,
        voteStatus: VoteStatus
    )

    given FromData[VoteDatum] = FromData.derived
    given ToData[VoteDatum] = ToData.derived

    enum VoteStatus:
        case AwaitingVote(peer : PubKeyHash)
        case Voted( commitment: KzgCommitment,
                   versionMinor: BigInt)

    given FromData[VoteStatus] = FromData.derived
    given ToData[VoteStatus] = ToData.derived

    given Eq[VoteStatus] = (a: VoteStatus, b: VoteStatus) =>
        a match
            case VoteStatus.AwaitingVote(peerA) =>
                b match
                    case VoteStatus.AwaitingVote(peerB) => peerA === peerB 
                    case VoteStatus.Voted(_, _) => false
            case VoteStatus.Voted(commitmentA, versionMinorA) =>
                a match {
                    case VoteStatus.AwaitingVote(_)   => false
                    case VoteStatus.Voted(commitmentB, versionMinorB) =>
                        commitmentA === commitmentB
                            && versionMinorA === versionMinorB
                }
    
    type Key = BigInt

    type Link = BigInt

    // G1 compressed point
    type KzgCommitment = ByteString

end VoteState
