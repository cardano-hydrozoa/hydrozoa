package hydrozoa.rulebased.ledger.l1.state

import scalus.*
import scalus.builtin.Data.{FromData, ToData}
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.ledger.api.v3.PubKeyHash
import scalus.prelude.{===, Eq, Option}

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

    private type Key = BigInt

    private type Link = BigInt

    // G1 compressed point
    type KzgCommitment = ByteString

end VoteState
