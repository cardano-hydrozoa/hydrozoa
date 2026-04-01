package hydrozoa.rulebased.ledger.l1.state

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
        case Unresolved(deadlineVoting: PosixTime, versionMajor: BigInt, setup: List[ByteString])
        case Resolved(
            evacuationActive: MembershipProof,
            // FIXME: missing in the refactored version
            version: (BigInt, BigInt),
            // FIXME: missing in the refactored version
            setup: List[ByteString]
        )
        def toOnchain(using
            config: HasTokenNames & HeadPeers.Section
        ): RuleBasedTreasuryDatumOnchain = this match {
            case Unresolved(deadlineVoting, versionMajor, setup) =>
                UnresolvedOnchain(
                  config.headMultisigScript.policyId,
                  config.headTokenNames.voteTokenName.bytes,
                  List.from(config.headPeerVKeys.toList),
                  BigInt(config.nHeadPeers.toInt),
                  deadlineVoting,
                  versionMajor,
                  setup
                )
            case Resolved(evacuationActive, version, setup) =>
                ResolvedOnchain(
                  config.headMultisigScript.policyId,
                  evacuationActive,
                  version,
                  setup
                )
        }

    enum RuleBasedTreasuryDatumOnchain:
        case UnresolvedOnchain(
            headMp: PolicyId,
            disputeId: TokenName,
            peers: List[VerificationKey],
            peersN: BigInt,
            deadlineVoting: PosixTime,
            versionMajor: BigInt,
            setup: List[ByteString]
        )
        case ResolvedOnchain(
            headMp: PolicyId,
            evacuationActive: MembershipProof,
            version: (BigInt, BigInt),
            setup: List[ByteString]
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
                  peers,
                  peersN,
                  deadlineVoting,
                  versionMajor,
                  setup
                ) =>
                if headMp == config.headMultisigScript.policyId
                    && disputeId == config.headTokenNames.voteTokenName.bytes
                    && peers == List.from(config.headPeerVKeys.toList)
                    && peersN == BigInt(config.nHeadPeers.toInt)
                then Some(Unresolved(deadlineVoting, versionMajor, setup))
                else None
            case ResolvedOnchain(headMp, evacuationActive, version, setup) =>
                if headMp == config.headMultisigScript.policyId
                then Some(Resolved(evacuationActive, version, setup))
                else None
        }

    given FromData[RuleBasedTreasuryDatumOnchain] = FromData.derived

    given ToData[RuleBasedTreasuryDatumOnchain] = ToData.derived

    // EdDSA / ed25519 Cardano verification key
    type VerificationKey = ByteString

    // The result of `bls12_381_G1_compress` function
    type MembershipProof = ByteString
