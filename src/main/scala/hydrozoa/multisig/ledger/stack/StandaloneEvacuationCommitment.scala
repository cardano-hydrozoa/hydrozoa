package hydrozoa.multisig.ledger.stack

import hydrozoa.multisig.ledger.block.{BlockHeader, BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.commitment.KzgCommitment.KzgCommitment

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
  * is not carried in this in-memory effect. `committedBlockNum` is kept so the slow side can key
  * the hard-ack header signature (the consensus artifact paired with this record at dispute time)
  * by block number.
  */
final case class StandaloneEvacuationCommitment(
    committedBlockNum: BlockNumber,
    blockVersion: BlockVersion.Full,
    kzgCommitment: KzgCommitment
)

object StandaloneEvacuationCommitment {

    /** The hard-confirmed form: the dormant record plus every head peer's signature over the
      * committed minor block's header (the consensus artifact that makes the commitment usable —
      * presented in the rule-based regime's vote tx after a fallback). Mirrors
      * [[hydrozoa.multisig.ledger.block.BlockEffects.HardConfirmed.Minor]] `headerMultiSigned`.
      *
      * @param headerMultiSigned
      *   one header signature per head peer (deterministic peer order), over `committedBlockNum`'s
      *   header — the same bytes every peer hard-acked.
      */
    final case class MultiSigned(
        commitment: StandaloneEvacuationCommitment,
        headerMultiSigned: List[BlockHeader.Minor.HeaderSignature]
    )
}
