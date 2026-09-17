package hydrozoa.multisig.ledger.stack

import hydrozoa.multisig.ledger.block.{BlockHeader, BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.ledger.l2.L2StateHash
import hydrozoa.rulebased.ledger.l1.state.StandaloneEvacuationCommitmentOnchain
import scalus.cardano.onchain.plutus.v3.TokenName
import scalus.uplc.builtin.ByteString

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
  * flattened to `(versionMajor, versionMinor)` in the datum encoding, and `l2StateHash` added
  * beside the commitment (`docs/spec/l2-state-certificate.md`) — so [[Onchain]] below carries five
  * fields: `(headId, versionMajor, versionMinor, commitment, l2StateHash)`. `headId` is fixed per
  * head (the `HYDR` token asset name) and pins the SEC to this head for the dispute-resolution
  * script's cross-head-contamination check. It is supplied at SEC construction time from the head's
  * `headTokenNames.treasuryTokenName`. `blockNum` is kept on the *offchain* effect (below) so the
  * slow side can key the hard-ack header signature (the consensus artifact paired with this record
  * at dispute time) by block number, but it is NOT carried on-chain.
  *
  * `header` carries the committed minor block's serialized header — the exact bytes the SEC
  * hard-ack signs over. Keeping it here makes the SEC effect **self-contained for signing**: the
  * signer and verifier derive the SEC signing material straight off this effect, with no
  * `BlockResult` / `Stack.Unsigned.results` lookup (PR #446 review — `results` is a
  * construction-only input and is being removed). The signature over them is a [[Signature]];
  * soft-acks sign something else entirely — a block's `BlockHash` — with a type of their own.
  *
  * @param blockNum
  *   the committed minor block's number
  * @param blockVersion
  *   that block's full version
  * @param kzgCommitment
  *   the dormant record's KZG commitment (spec content)
  * @param l2StateHash
  *   the L2 ledger's digest of the state that block leaves behind — what this SEC certifies. It is
  *   the minor-only stack's counterpart to the settlement datum's field of the same name.
  * @param header
  *   the committed minor block's serialized header — the SEC signing bytes
  */
final case class StandaloneEvacuationCommitment(
    blockNum: BlockNumber,
    blockVersion: BlockVersion.Full,
    kzgCommitment: KzgCommitment,
    l2StateHash: L2StateHash,
    header: StandaloneEvacuationCommitmentOnchain.Serialized
)

object StandaloneEvacuationCommitment {

    /** The hard-confirmed form: the dormant record plus the peers' signatures over the committed
      * minor block's header (the consensus artifact that makes the commitment usable — presented in
      * the rule-based regime's vote tx after a fallback).
      *
      * @param signatures
      *   an ordered, peer-position-aligned list over `allHeadPeers.sorted ++ allCoilPeers.sorted`:
      *   `Some(sig)` where that peer signed `blockNum`'s header, `None` otherwise. Head peers
      *   always sign (AllOf), so only coil slots ever carry `None`. The alignment is load-bearing:
      *   the dispute-resolution script verifies `coilMultisig[i]` against
      *   `regimeDatum.coilPeers[i]`, so a coil signature MUST sit at its own coil peer's index, not
      *   densely packed.
      */
    final case class MultiSigned(
        commitment: StandaloneEvacuationCommitment,
        signatures: List[Option[Signature]]
    )

    type Signature = Signature.Signature

    /** A peer's Ed25519 signature over an SEC's serialized on-chain record
      * ([[StandaloneEvacuationCommitment.header]]) — the signature the rule-based regime's vote tx
      * presents to the dispute-resolution script, which verifies it over the SEC and ratchets on
      * the SEC's own `versionMinor`.
      *
      * Built by `PeerWallet.mkSecSignature`; carried in hard-acks and aggregated on
      * [[MultiSigned]]. Signatures over L1 effect transactions are a separate type, `TxSignature`.
      */
    object Signature {
        opaque type Signature = IArray[Byte]

        def apply(signature: IArray[Byte]): Signature = signature

        given Conversion[Signature, IArray[Byte]] = identity

        given Conversion[Signature, Array[Byte]] = sig => IArray.genericWrapArray(sig).toArray

        given Conversion[Signature, ByteString] = sig => ByteString.fromArray(sig)

        extension (signature: Signature) def untagged: IArray[Byte] = identity(signature)
    }

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
      * @param l2StateHash
      *   the L2 state digest this SEC certifies. Not read by the dispute script; it is signed
      *   because the script verifies over `serialiseData(sec.toData)`.
      */
    type Onchain = StandaloneEvacuationCommitmentOnchain

    /** Factory and [[Serialized]] accessor, preserving the former `object Onchain` interface. */
    object Onchain {

        type Serialized = StandaloneEvacuationCommitmentOnchain.Serialized

        /** Delegates to [[StandaloneEvacuationCommitmentOnchain]] companion. */
        object Serialized {
            export StandaloneEvacuationCommitmentOnchain.*
        }

        /** Build the on-chain SEC datum from this head's `headId`, the offchain block header, the
          * KZG commitment of the evacuation map at that block, and the L2 state digest at it. KZG
          * is passed explicitly (not read from the header) because as of step 4 it's a slow-cycle
          * concern, computed in [[StackEffectsBuilder]] from the cumulative evacuation map state —
          * the header itself no longer carries it.
          */
        def apply(
            headId: TokenName,
            offchainHeader: BlockHeader.Section,
            kzgCommitment: KzgCommitment,
            l2StateHash: L2StateHash
        ): Onchain =
            StandaloneEvacuationCommitmentOnchain(
              headId = headId,
              versionMajor = BigInt(offchainHeader.blockVersion.major.convert),
              versionMinor = BigInt(offchainHeader.blockVersion.minor.convert),
              commitment = kzgCommitment,
              l2StateHash = l2StateHash.byteString
            )
    }
}
