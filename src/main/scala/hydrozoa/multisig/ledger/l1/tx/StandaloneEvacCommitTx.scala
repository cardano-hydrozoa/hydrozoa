package hydrozoa.multisig.ledger.l1.tx

import scalus.cardano.ledger.Transaction

/** A "standalone" evacuation-commitment transaction that pins the cumulative KZG commitment of the
  * evacuation map onto L1 without performing a settlement. Used in stacks that don't contain a
  * major block to absorb the commitment into a settlement tx — i.e. minor-only stacks (or the
  * minor-only trailing partition of a multi-major stack).
  *
  * TODO(slow-consensus): construction logic in `multisig/ledger/effects/StackEffects.scala`.
  */
final case class StandaloneEvacCommitTx(tx: Transaction)
