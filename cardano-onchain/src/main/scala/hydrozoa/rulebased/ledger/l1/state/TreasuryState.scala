package hydrozoa.rulebased.ledger.l1.state

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.v3.*
import scalus.uplc.builtin.{ByteString, FromData, ToData}

@Compile
// TODO: shall we move it to object RuleBasedTreasuryUtxo?
object TreasuryState:

    /** Treasury datum, used both on- and off-chain.
      *
      * `headMp` is the induction anchor for reference-input lookups: written by the
      * all-peers-signed FallbackTx and preserved across Resolve/Evacuate, it lets validators
      * locate the rule-based regime utxo (HRWT beacon under `headMp`) without trusting tokens in
      * the treasury's own value, which holds arbitrary L2 assets.
      *
      * The immutable head-identity fields live in the regime utxo ([[RegimeState]]); the KZG G2
      * setup lives in the deployed setup-ladder utxos.
      */
    enum RuleBasedTreasuryDatum:
        case Unresolved(
            headMp: PolicyId,
            deadlineVoting: PosixTime,
            versionMajor: BigInt
        )
        case Resolved(
            headMp: PolicyId,
            evacuationActive: MembershipProof,
            version: (BigInt, BigInt)
        )

    given FromData[RuleBasedTreasuryDatum] = FromData.derived

    given ToData[RuleBasedTreasuryDatum] = ToData.derived

    // EdDSA / ed25519 Cardano verification key
    type VerificationKey = ByteString

    // The result of `bls12_381_G1_compress` function
    type MembershipProof = ByteString
