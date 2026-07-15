package hydrozoa.rulebased.ledger.l1.state

import hydrozoa.rulebased.ledger.l1.state.TreasuryState.VerificationKey
import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.List
import scalus.cardano.onchain.plutus.v3.*
import scalus.uplc.builtin.{FromData, ToData}

@Compile
object RegimeState:

    /** Immutable head-identity fields, held once in the rule-based regime utxo (identified by the
      * HRWT beacon token) instead of being inlined in every treasury datum. The utxo's beacon
      * policy is the head multisig policy, so `headMp` is not repeated here.
      *
      * `setupG2Ladder` is the outRef of rung 0 of the deployed G2 setup ladder; the seven rungs are
      * outputs 0-6 of its transaction. It authenticates the setup reference input used by Evacuate
      * (the datum itself is trusted because the regime utxo is produced by the all-peers-signed
      * FallbackTx).
      */
    case class RuleBasedRegimeDatum(
        disputeId: TokenName,
        headPeers: List[VerificationKey],
        headPeersN: BigInt,
        coilPeers: List[VerificationKey],
        coilQuorum: BigInt,
        setupG2Ladder: TxOutRef
    )

    given FromData[RuleBasedRegimeDatum] = FromData.derived

    given ToData[RuleBasedRegimeDatum] = ToData.derived
