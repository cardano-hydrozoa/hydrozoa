package hydrozoa.multisig.ledger.stack

import hydrozoa.multisig.ledger.block.{BlockHeader, BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.commitment.KzgCommitment.KzgCommitment
import hydrozoa.rulebased.ledger.l1.state.StandaloneEvacuationCommitmentOnchain
import scalus.cardano.onchain.plutus.v3.TokenName

/** A standalone evacuation commitment — the per-spec record a **minor** block carries (see
  * `replicated-state-machine/effects#standalone-evacuation-commitment`).
  *
  * Lives in the `stack` package (a slow-consensus stack-effect datum held by
  * [[StackEffects.Unsigned.Regular]] / [[StackEffects.HardConfirmed.Regular]]), deliberately NOT in
  * `l1/tx`: it is not a transaction.
  *
  * It is a **contingent / dormant L1 effect**: a fixed-size record that "lays dormant" and is
  * presented to Hydrozoa's L1 dispute-resolution scripts in the rules-based regime — only after a
  * fallback effect executes. It never reaches L1 immediately, never rotates the treasury. (For
  * initial/major blocks the evacuation commitment is implicit in the initialization/settlement
  * effect and goes to L1 immediately on execution; only minor blocks have a *standalone* one.)
  *
  * Per spec the on-L1 record is `(headId, blockVersion, kzgCommitment)`, with `blockVersion`
  * flattened to `(versionMajor, versionMinor)` in the datum encoding — so [[Onchain]] below carries
  * four fields: `(headId, versionMajor, versionMinor, commitment)`. `headId` is fixed per head (the
  * `HYDR` token asset name) and pins the SEC to this head for the dispute-resolution script's
  * cross-head-contamination check. It is supplied at SEC construction time from the head's
  * `headTokenNames.treasuryTokenName`. `blockNum` is kept on the *offchain* effect (below) so the
  * slow side can key the hard-ack header signature (the consensus artifact paired with this record
  * at dispute time) by block number, but it is NOT carried on-chain.
  *
  * `header` carries the committed minor block's serialized header — the exact bytes the SEC
  * hard-ack signs over. Keeping it here makes the SEC effect **self-contained for signing**: the
  * signer and verifier derive the SEC signing material straight off this effect, with no
  * `BlockResult` / `Stack.Unsigned.results` lookup (PR #446 review — `results` is a
  * construction-only input and is being removed). KZG lives on the header transitionally, so today
  * these bytes coincide with the soft-ack header domain.
  *
  * @param blockNum
  *   the committed minor block's number
  * @param blockVersion
  *   that block's full version
  * @param kzgCommitment
  *   the dormant record's KZG commitment (spec content)
  * @param header
  *   the committed minor block's serialized header — the SEC signing bytes
  */
final case class StandaloneEvacuationCommitment(
    blockNum: BlockNumber,
    blockVersion: BlockVersion.Full,
    kzgCommitment: KzgCommitment,
    header: StandaloneEvacuationCommitmentOnchain.Serialized
)

object StandaloneEvacuationCommitment {

    /** The hard-confirmed form: the dormant record plus every head peer's signature over the
      * committed minor block's header (the consensus artifact that makes the commitment usable —
      * presented in the rule-based regime's vote tx after a fallback). Mirrors
      * [[hydrozoa.multisig.ledger.block.BlockEffects.HardConfirmed.Minor]] `headerMultiSigned`.
      *
      * @param headerMultiSigned
      *   one header signature per head peer (deterministic peer order), over `blockNum`'s header —
      *   the same bytes every peer hard-acked.
      */
    final case class MultiSigned(
        commitment: StandaloneEvacuationCommitment,
        headerMultiSigned: List[BlockHeader.Minor.HeaderSignature]
    )

    /** The PlutusData shape the rule-based dispute-resolution script consumes as the vote
      * redeemer's `sec` field. Type alias for [[StandaloneEvacuationCommitmentOnchain]], which
      * lives in `cardano-onchain` so validators can reference it without a back-dependency on core.
      *
      * @param headId
      *   this head's `HYDR` token asset name. Pins the SEC to this head; the dispute script rejects
      *   any SEC whose `headId` does not match the treasury reference input's `HYDR` token name
      *   (foundation I5 — no cross-head contamination).
      * @param versionMajor
      *   committed minor block's major version (dispute script matches against the treasury's
      *   pinned major version)
      * @param versionMinor
      *   committed minor block's minor version — read by the dispute script to check the
      *   per-version vote tally.
      * @param commitment
      *   the SEC's KZG commitment (spec content) — read by the dispute script as the value to vote
      *   on.
      */
    type Onchain = StandaloneEvacuationCommitmentOnchain

    /** Factory and [[Serialized]] accessor, preserving the former `object Onchain` interface. */
    object Onchain {

        type Serialized = StandaloneEvacuationCommitmentOnchain.Serialized

        /** Delegates to [[StandaloneEvacuationCommitmentOnchain]] companion. */
        object Serialized {
            export StandaloneEvacuationCommitmentOnchain.*
        }

        /** Build the on-chain SEC datum from this head's `headId`, the offchain block header, and
          * the KZG commitment of the evacuation map at that block. KZG is passed explicitly (not
          * read from the header) because as of step 4 it's a slow-cycle concern, computed in
          * [[StackEffectsBuilder]] from the cumulative evacuation map state — the header itself no
          * longer carries it.
          */
        def apply(
            headId: TokenName,
            offchainHeader: BlockHeader.Section,
            kzgCommitment: KzgCommitment
        ): Onchain =
            StandaloneEvacuationCommitmentOnchain(
              headId = headId,
              versionMajor = BigInt(offchainHeader.blockVersion.major.convert),
              versionMinor = BigInt(offchainHeader.blockVersion.minor.convert),
              commitment = kzgCommitment
            )
    }
}
