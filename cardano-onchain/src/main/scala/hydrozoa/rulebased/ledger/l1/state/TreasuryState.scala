package hydrozoa.rulebased.ledger.l1.state

import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatumOnchain.{ResolvedOnchain, UnresolvedOnchain}
import scalus.Compile
import scalus.cardano.onchain.plutus.prelude.List
import scalus.cardano.onchain.plutus.v3.*
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}

@Compile
// TODO: shall we move it to object RuleBasedTreasuryUtxo?
object TreasuryState:

    // Datum
    enum RuleBasedTreasuryDatum:
        case Unresolved(
            deadlineVoting: PosixTime,
            versionMajor: BigInt,
            setupG2: List[ByteString]
        )
        case Resolved(
            evacuationActive: MembershipProof,
            // FIXME: missing in the refactored version
            version: (BigInt, BigInt),
            // FIXME: missing in the refactored version
            setupG2: List[ByteString]
        )

    enum RuleBasedTreasuryDatumOnchain:
        case UnresolvedOnchain(
            headMp: PolicyId,
            disputeId: TokenName,
            headPeers: List[VerificationKey],
            headPeersN: BigInt,
            coilPeers: List[VerificationKey],
            coilQuorum: BigInt,
            deadlineVoting: PosixTime,
            versionMajor: BigInt,
            setupG2: List[ByteString]
        )
        case ResolvedOnchain(
            headMp: PolicyId,
            evacuationActive: MembershipProof,
            version: (BigInt, BigInt),
            setupG2: List[ByteString]
        )

    given FromData[RuleBasedTreasuryDatumOnchain] = FromData.derived

    given ToData[RuleBasedTreasuryDatumOnchain] = ToData.derived

    // EdDSA / ed25519 Cardano verification key
    type VerificationKey = ByteString

    // The result of `bls12_381_G1_compress` function
    type MembershipProof = ByteString
