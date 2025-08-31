package hydrozoa.rulebased.cardano.state

import scalus.*
import scalus.builtin.Data.{FromData, ToData}
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.prelude.Option

case class VoteDatum (
    key: Key,
    link: Link,
    peer: Option[PubKeyHash],
    voteStatus: VoteStatus
  ) derives FromData, ToData

enum VoteStatus derives FromData, ToData:
    case NoVote
    case Vote(voteDetails: VoteDetails)

case class VoteDetails(
    commit: KzgCommit,
    versionMinor: VersionMinor,
  ) derives FromData, ToData


private type Key = BigInt

private type Link = BigInt

private type PubKeyHash = ByteString

private type KzgCommit = ByteString

private type VersionMinor = BigInt