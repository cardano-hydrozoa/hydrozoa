package hydrozoa.rulebased.ledger.l1.state

import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.{Resolved, Unresolved}
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatumOnchain.{ResolvedOnchain, UnresolvedOnchain}
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.{RuleBasedTreasuryDatum, RuleBasedTreasuryDatumOnchain}
import scalus.cardano.onchain.plutus.prelude.List

object TreasuryStateConversions:

    extension (datum: RuleBasedTreasuryDatum)
        // TODO: This should fully parse into a type tag so we don't need to pattern match again
        // TODO: Use either
        def toOnchain(using config: HeadConfig.Bootstrap.Section): RuleBasedTreasuryDatumOnchain =
            datum match {
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

    extension (datum: RuleBasedTreasuryDatumOnchain)
        def toOffchain(using
            config: HasTokenNames & HeadPeers.Section
        ): Option[RuleBasedTreasuryDatum] =
            datum match {
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
                    then Some(Unresolved(deadlineVoting, versionMajor, setupG2))
                    else None
                case ResolvedOnchain(headMp, evacuationActive, version, setupG2) =>
                    if headMp == config.headMultisigScript.policyId
                    then Some(Resolved(evacuationActive, version, setupG2))
                    else None
            }
