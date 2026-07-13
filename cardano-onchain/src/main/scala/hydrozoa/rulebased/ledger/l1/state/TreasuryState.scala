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
        /** The induction anchor, common to both phases: it is preserved across every treasury
          * transition so validators can always locate the regime utxo, so it is exposed here as a
          * uniform accessor rather than re-matched at each use site.
          */
        def headMp: PolicyId

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

    /** The only valid treasury-datum transitions, single-sourcing which fields advance and which are
      * carried forward immutably. Validators build the expected next datum from these and check the
      * output against it; off-chain builders construct the next datum the same way.
      */
    extension (self: RuleBasedTreasuryDatum.Unresolved)
        /** Resolve advances the unresolved treasury to the winning vote's commitment and minor
          * version, preserving `headMp` and the major version.
          */
        def resolve(
            evacuationActive: MembershipProof,
            versionMinor: BigInt
        ): RuleBasedTreasuryDatum.Resolved =
            RuleBasedTreasuryDatum.Resolved(
              headMp = self.headMp,
              evacuationActive = evacuationActive,
              version = (self.versionMajor, versionMinor)
            )

    extension (self: RuleBasedTreasuryDatum.Resolved)
        /** Evacuate advances only the accumulator to the post-evacuation proof, preserving `headMp`
          * and the resolved version.
          */
        def evacuate(evacuationActive: MembershipProof): RuleBasedTreasuryDatum.Resolved =
            RuleBasedTreasuryDatum.Resolved(
              headMp = self.headMp,
              evacuationActive = evacuationActive,
              version = self.version
            )

    // EdDSA / ed25519 Cardano verification key
    type VerificationKey = ByteString

    // The result of `bls12_381_G1_compress` function
    type MembershipProof = ByteString
