package hydrozoa.rulebased.ledger.dapp.state

import scalus.Compile
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.List

@Compile
// TODO: shall we move it to object RuleBasedTreasuryUtxo?
object TreasuryState:

    // Datum
    enum RuleBasedTreasuryDatum:
        case Unresolved(unresolvedDatum: UnresolvedDatum)
        case Resolved(resolvedDatum: ResolvedDatum)

    given FromData[RuleBasedTreasuryDatum] = FromData.derived

    given ToData[RuleBasedTreasuryDatum] = ToData.derived

    case class UnresolvedDatum(
        headMp: PolicyId,
        disputeId: TokenName,
        peers: List[VerificationKey],
        peersN: BigInt,
        deadlineVoting: PosixTime,
        versionMajor: BigInt,
        setup: List[ByteString]
    )

    given FromData[UnresolvedDatum] = FromData.derived

    given ToData[UnresolvedDatum] = ToData.derived

    case class ResolvedDatum(
        headMp: PolicyId,
        utxosActive: MembershipProof,
        // FIXME: missing in the refactored version
        version: (BigInt, BigInt),
        // FIXME: missing in the refactored version
        setup: List[ByteString]
    )

    given FromData[ResolvedDatum] = FromData.derived

    given ToData[ResolvedDatum] = ToData.derived

    // EdDSA / ed25519 Cardano verification key
    type VerificationKey = ByteString

    // The result of `bls12_381_G1_compress` function
    type MembershipProof = ByteString

//enum RuleBasedTreasuryDatum derives FromData, ToData:
//    case UnresolvedDatum(
//        headMp: PolicyId,
//        disputeId: TokenName,
//        peers: VerificationKey,
//        peersN: BigInt,
//        deadlineVoting: PosixTime,
//        versionMajor: VersionMajor,
//        params: H32
//    )
//    case ResolvedDatum(
//        headMp: PolicyId,
//        commit: KzgCommit
//    )
//
//private type KzgCommit = ByteString
//
//private type VersionMajor = BigInt
//
//private type VerificationKey = ByteString
//
//private type H32 = ByteString
