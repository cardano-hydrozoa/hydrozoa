package hydrozoa.multisig.ledger.stack

import hydrozoa.multisig.ledger.block.{BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.commitment.KzgCommitment.KzgCommitment

/** A standalone evacuation commitment — the per-spec record a **minor** block carries (see
  * `replicated-state-machine/effects#standalone-evacuation-commitment`).
  *
  * Lives in the `stack` package (a slow-consensus stack-effect datum held by
  * [[StackEffects.Regular]]), deliberately NOT in `l1/tx`: it is not a transaction.
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
