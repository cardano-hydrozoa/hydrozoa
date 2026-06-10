package hydrozoa.rulebased.ledger.l1.state

import cats.data.NonEmptyList
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus.{AwaitingVote, Voted}
import hydrozoa.rulebased.ledger.l1.state.VoteState.{KzgCommitment, VoteDatum, VoteStatus}
import scalus.*
import scalus.cardano.onchain.plutus.prelude.{===, Eq}
import scalus.cardano.onchain.plutus.v3.PubKeyHash
import scalus.uplc.builtin.Data.{FromData, ToData}
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}

object VoteDatum {
    def default(commitment: KzgCommitment): VoteDatum = VoteState.VoteDatum(
      key = 0,
      // N.B.: Version "Branch: (None) @ c28633a • Commit date: 2025-10-16" of the spec says
      // to set link to `0 < peersN ? 1 : 0`. But we have peers as a NonEmptyList, so this is just 1.
      link = 1,
      voteStatus = VoteStatus.Voted(commitment = commitment, versionMinor = 0)
    )

    // TODO: This should probably also create the default, and should probably be renamed.
    /** Given a non empty list of peers, create a nonempty list of vote datums awaiting votes (one
      * for each peer)
      * @param peers
      * @return
      */
    def apply(peers: NonEmptyList[PubKeyHash]): NonEmptyList[VoteState.VoteDatum] = {
        {
            val numPeers = peers.length
            val mapFunc = (x: (PubKeyHash, Int)) => {
                val i = x._2
                val pkh = x._1
                VoteState.VoteDatum(
                  key = i + 1,
                  link = if i < numPeers - 1 then i + 2 else 0,
                  voteStatus = AwaitingVote(pkh)
                )
            }
            peers.zipWithIndex.map(mapFunc)
        }
    }
}

@Compile
object VoteState:
    // TODO: I'd like to turn this into `VoteDatum[Status <: VoteStatus]`, but then data derivation breaks
    case class VoteDatum(
        // Uniquely identifies a vote utxo. The default vote utxo has key number 0,
        // according to the spec draft 2025-11-07
        key: Key,
        // Uniquely references another vote utxo by its key
        link: Link,
        voteStatus: VoteStatus
    )

    given FromData[VoteDatum] = FromData.derived
    given ToData[VoteDatum] = ToData.derived

    /** Status of a single vote utxo at the dispute resolution address.
      *
      *   - [[AwaitingVote]] — peer has not yet acted; can transition to [[Voted]] (cast vote) or
      *     [[Abstain]] (peer publicly declines to vote, e.g. when no SEC is available because the
      *     latest hard-confirmed stack is Initial or a Major with no trailing minors — introduced
      *     to close the gap left by the KZG-out-of-BlockHeader refactor).
      *   - [[Voted]] — peer has cast a vote on a specific KZG commitment + minor block version.
      *   - [[Abstain]] — peer has publicly abandoned this ballot box. Tallies the same as
      *     [[AwaitingVote]] in [[Eq]]/precedence terms (any [[Voted]] supersedes it), but is
      *     terminal: the script lets the peer's own utxo be tallied immediately rather than waiting
      *     for `deadlineVoting`.
      */
    enum VoteStatus:
        case AwaitingVote(peer: PubKeyHash)
        case Voted(commitment: KzgCommitment, versionMinor: BigInt)
        case Abstain

    given FromData[VoteStatus] = FromData.derived
    given ToData[VoteStatus] = ToData.derived

    given Eq[VoteStatus] = (a: VoteStatus, b: VoteStatus) =>
        a match
            case VoteStatus.AwaitingVote(peerA) =>
                b match
                    case VoteStatus.AwaitingVote(peerB) => peerA === peerB
                    case _                              => false
            case VoteStatus.Voted(commitmentA, versionMinorA) =>
                b match
                    case VoteStatus.Voted(commitmentB, versionMinorB) =>
                        commitmentA === commitmentB && versionMinorA === versionMinorB
                    case _ => false
            case VoteStatus.Abstain =>
                b match
                    case VoteStatus.Abstain => true
                    case _                  => false

    type Key = BigInt

    type Link = BigInt

    // G1 compressed point
    type KzgCommitment = ByteString

end VoteState
