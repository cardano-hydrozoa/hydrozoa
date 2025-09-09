package hydrozoa.rulebased.ledger.cardano.real.state

import scalus.builtin.ByteString
import scalus.builtin.Data
import scalus.builtin.FromData
import scalus.builtin.ToData
import scalus.ledger.api.v3._

enum RuleBasedTreasuryDatum derives FromData, ToData:
    case UnresolvedDatum(
        headMp: CurrencySymbol,
        disputeId: TokenName,
        peers: VerificationKey,
        peersN: BigInt,
        deadlineVoting: PosixTime,
        versionMajor: VersionMajor,
        params: H32
    )
    case ResolvedDatum(
        headMp: CurrencySymbol,
        commit: KzgCommit
    )

private type KzgCommit = ByteString

private type VersionMajor = BigInt

private type VerificationKey = ByteString

private type H32 = ByteString
