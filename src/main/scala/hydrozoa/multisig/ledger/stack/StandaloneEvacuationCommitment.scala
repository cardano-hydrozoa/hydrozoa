package hydrozoa.multisig.ledger.stack

import hydrozoa.multisig.ledger.block.{BlockHeader, BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.commitment.KzgCommitment.KzgCommitment
import hydrozoa.rulebased.ledger.l1.state.VoteState
import scalus.cardano.onchain.plutus.v3.PosixTime
import scalus.uplc.builtin.Builtins.serialiseData
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{ByteString, FromData, ToData}

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
  * Per spec the on-L1 record is `(headId, blockVersion, kzgCommitment)`. `headId` is constant per
  * head and supplied at dispute-presentation time (the storage/dispute layer is future work), so it
  * is not carried in this in-memory effect. `blockNum` is kept so the slow side can key the
  * hard-ack header signature (the consensus artifact paired with this record at dispute time) by
  * block number.
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
    header: StandaloneEvacuationCommitment.Onchain.Serialized
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
      * redeemer's `blockHeader` field. This is the off-chain ⇄ on-chain interface for the SEC: an
      * SEC effect produced off-chain is serialized into [[Onchain.Serialized]] for signing, and on
      * dispute presentation the same shape (with the multisig) is fed to
      * [[hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionValidator]] as a
      * `VoteRedeemer.blockHeader`.
      *
      * @param blockNum
      *   committed minor block's number
      * @param startTime
      *   committed minor block's start time. Currently NOT read by the dispute script — kept here
      *   because it is part of the committed minor block's identity and may be useful for future
      *   script extensions (e.g. time-window-bounded disputes). Drop only after spec confirms it is
      *   no longer load-bearing.
      * @param versionMajor
      *   committed minor block's major version (dispute script reads via `versionMinor` neighbor
      *   for treasury matching)
      * @param versionMinor
      *   committed minor block's minor version — read by the dispute script to check the
      *   per-version vote tally.
      * @param commitment
      *   the SEC's KZG commitment (spec content) — read by the dispute script as the value to vote
      *   on.
      */
    final case class Onchain(
        blockNum: BigInt,
        startTime: PosixTime,
        versionMajor: BigInt,
        versionMinor: BigInt,
        commitment: VoteState.KzgCommitment
    ) derives FromData,
          ToData

    object Onchain {

        /** Build the on-chain SEC datum from a minor block header + the KZG commitment of the
          * evacuation map at that block. KZG is passed explicitly (not read from the header)
          * because as of step 4 it's a slow-cycle concern, computed in [[StackEffectsBuilder]] from
          * the cumulative evacuation map state — the header itself no longer carries it.
          */
        def apply(offchainHeader: BlockHeader.Section, kzgCommitment: KzgCommitment): Onchain =
            new Onchain(
              blockNum = BigInt(offchainHeader.blockNum.convert),
              startTime = offchainHeader.startTime.instant.toEpochMilli,
              versionMajor = BigInt(offchainHeader.blockVersion.major.convert),
              versionMinor = BigInt(offchainHeader.blockVersion.minor.convert),
              commitment = kzgCommitment
            )

        type Serialized = Serialized.Serialized

        object Serialized {
            opaque type Serialized = IArray[Byte]

            def apply(onchainHeader: Onchain): Serialized =
                IArray.from(serialiseData(onchainHeader.toData).bytes)

            given Conversion[Serialized, IArray[Byte]] = identity

            given Conversion[Serialized, Array[Byte]] = msg => IArray.genericWrapArray(msg).toArray

            given Conversion[Serialized, ByteString] = msg => ByteString.fromArray(msg)

            extension (msg: Serialized) def untagged: IArray[Byte] = identity(msg)

            trait Section {
                def headerSerialized: Onchain.Serialized
            }
        }
    }
}
