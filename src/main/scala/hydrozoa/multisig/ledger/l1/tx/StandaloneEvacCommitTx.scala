package hydrozoa.multisig.ledger.l1.tx

import hydrozoa.multisig.ledger.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.ledger.l1.utxo.MultisigTreasuryUtxo
import scalus.cardano.ledger.Transaction

/** A "standalone" evacuation-commitment transaction that pins the cumulative KZG commitment of the
  * evacuation map onto L1 without performing a settlement. Used in stacks that don't contain a
  * major block to absorb the commitment into a settlement tx — i.e. minor-only stacks (or the
  * minor-only trailing partition of a multi-major stack).
  *
  * Semantically: spend the current treasury utxo, produce a new treasury utxo with the SAME
  * `versionMajor` (no major bump — this is not a settlement) but with `commit = nextKzg`. No
  * deposit absorption, no L2 payout realization; treasury value is preserved.
  *
  * **Implementation status (slow-consensus PR2):** the data type below carries the inputs and the
  * outputs the builder will need; the actual transaction body construction is deferred because the
  * on-chain `MultisigTreasuryValidator` script does not yet have a "datum-only update" path. The
  * validator currently expects settlement / fallback / finalization spending patterns — adding a
  * no-payouts-no-bump-major case is a coordinated change with the on-chain code. See
  * [[hydrozoa.multisig.ledger.l1.L1LedgerM.mkStandaloneEvacCommitTx]].
  */
final case class StandaloneEvacCommitTx(
    tx: Transaction,
    treasurySpent: MultisigTreasuryUtxo,
    treasuryProduced: MultisigTreasuryUtxo,
    nextKzg: KzgCommitment
)
