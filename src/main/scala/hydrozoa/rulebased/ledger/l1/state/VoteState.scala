package hydrozoa.rulebased.ledger.l1.state

import hydrozoa.multisig.protocol.types.Block.Version.Minor
import scalus.*
import scalus.builtin.Data.{Constr, FromData, ToData}
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.ledger.api.v3.PubKeyHash
import scalus.prelude.{!==, ===, Eq, List, Option, SortedMap, Validator, fail, log, require, given}

@Compile
object VoteState:
    case class VoteDatum(
        key: Key,
        link: Link,
        peer: Option[PubKeyHash],
        voteStatus: VoteStatus
    ) derives FromData,
          ToData

    enum VoteStatus derives FromData, ToData:
        case NoVote
        case Vote(voteDetails: VoteDetails)

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
    ) derives FromData,
          ToData

    given Eq[VoteDetails] = (a: VoteDetails, b: VoteDetails) =>
        a.commitment == b.commitment && a.versionMinor == b.versionMinor

    private type Key = BigInt

    private type Link = BigInt

    // G1 compressed point
    type KzgCommitment = ByteString

end VoteState
