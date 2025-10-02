package hydrozoa.rulebased.ledger.l1.state

import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.ledger.api.v3._

enum RuleBasedTreasuryDatum derives FromData, ToData:
    case UnresolvedDatum(
        headMp: PolicyId,
        disputeId: TokenName,
        peers: VerificationKey,
        peersN: BigInt,
        deadlineVoting: PosixTime,
        versionMajor: VersionMajor,
        params: H32
    )
    case ResolvedDatum(
        headMp: PolicyId,
        commit: KzgCommit
    )

private type KzgCommit = ByteString

private type VersionMajor = BigInt

private type VerificationKey = ByteString

private type H32 = ByteString
