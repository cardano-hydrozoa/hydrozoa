package hydrozoa.rulebased.ledger.cardano.real.state

import scalus._
import scalus.builtin.ByteString
import scalus.builtin.Data
import scalus.builtin.Data.FromData
import scalus.builtin.Data.ToData
import scalus.builtin.FromData
import scalus.builtin.ToData
import scalus.prelude.Option

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

case class VoteDetails(
    commit: KzgCommit,
    versionMinor: VersionMinor
) derives FromData,
      ToData

private type Key = BigInt

private type Link = BigInt

private type PubKeyHash = ByteString

private type KzgCommit = ByteString

private type VersionMinor = BigInt
