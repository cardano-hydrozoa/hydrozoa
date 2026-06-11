package hydrozoa.rulebased.ledger.l1.state

import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.{Resolved, Unresolved}
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
        def toOnchain(using
            config: HeadConfig.Bootstrap.Section
        ): RuleBasedTreasuryDatumOnchain = this match {
            case Unresolved(deadlineVoting, versionMajor, setupG2) =>
                UnresolvedOnchain(
                  config.headMultisigScript.policyId,
                  config.headTokenNames.voteTokenName.bytes,
                  List.from(config.headPeerVKeys.toList),
                  BigInt(config.nHeadPeers.toInt),
                  List.from(config.coilPeerVKeys),
                  BigInt(config.coilQuorum),
                  deadlineVoting,
                  versionMajor,
                  setupG2
                )
            case Resolved(evacuationActive, version, setupG2) =>
                ResolvedOnchain(
                  config.headMultisigScript.policyId,
                  evacuationActive,
                  version,
                  setupG2
                )
        }

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
        // TODO: This should fully parse into a type tag so we don't
        //  need to pattern match again
        // TODO: Use either
        def toOffchain(using
            config: HasTokenNames & HeadPeers.Section
        ): Option[RuleBasedTreasuryDatum] = this match {
            case UnresolvedOnchain(
                  headMp,
                  disputeId,
                  headPeers,
                  headPeersN,
                  coilPeers,
                  coilQuorum,
                  deadlineVoting,
                  versionMajor,
                  setupG2
                ) =>
                if headMp == config.headMultisigScript.policyId
                    && disputeId == config.headTokenNames.voteTokenName.bytes
                    && headPeers == List.from(config.headPeerVKeys.toList)
                    && headPeersN == BigInt(config.nHeadPeers.toInt)
                then
                    Some(
                      Unresolved(deadlineVoting, versionMajor, setupG2)
                    )
                else None
            case ResolvedOnchain(headMp, evacuationActive, version, setupG2) =>
                if headMp == config.headMultisigScript.policyId
                then Some(Resolved(evacuationActive, version, setupG2))
                else None
        }

    given FromData[RuleBasedTreasuryDatumOnchain] = FromData.derived

    given ToData[RuleBasedTreasuryDatumOnchain] = ToData.derived

    // EdDSA / ed25519 Cardano verification key
    type VerificationKey = ByteString

    // The result of `bls12_381_G1_compress` function
    type MembershipProof = ByteString
