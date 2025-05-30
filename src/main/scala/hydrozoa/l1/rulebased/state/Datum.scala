package hydrozoa.l1.rulebased.state

import hydrozoa.l1.multisig.state.L2ConsensusParamsH32
import scalus.builtin.ByteString
import scalus.ledger.api.v1.{CurrencySymbol, PosixTime, PubKeyHash, TokenName}

enum RuleBasedTreasuryDatum:
    case Resolved(resolvedDatum: ResolvedDatum)
    case Unresolved(unresolvedDatum: UnresolvedDatum)

case class ResolvedDatum(
    headMp: CurrencySymbol,
    utxosActive: ByteString, // TODO:
    version: (BigInt, BigInt),
    params: L2ConsensusParamsH32
)

type VerificationKey = ByteString

case class UnresolvedDatum(
    headMp: CurrencySymbol,
    disputeId: TokenName,
    peers: List[VerificationKey],
    peersN: BigInt,
    deadlineVoting: PosixTime,
    versionMajor: BigInt,
    params: L2ConsensusParamsH32
)
case class VoteDatum(
    key: BigInt,
    link: BigInt,
    peer: Option[PubKeyHash],
    voteStatus: VoteStatus
)

enum VoteStatus:
    case NoVote
    case Abstain
    case Vote(voteDetails: VoteDetails)

type RH32UtxoSetL2 = ByteString

case class VoteDetails(
    utxosActive: RH32UtxoSetL2,
    versionMinor: BigInt
)
