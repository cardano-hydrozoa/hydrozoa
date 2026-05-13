package hydrozoa.multisig.consensus

/** Placeholder for the head-side block-stack actor.
  *
  * Mirrors [[BlockWeaver]] but for the slow cycle: accumulates per-block effect inputs
  * (evacuation-map diffs, payout obligations) supplied by
  * [[hydrozoa.multisig.ledger.joint.JointLedger]], decides block-stack boundaries via the
  * slow-consensus leadership schedule, and drives [[SlowConsensusActor]] to multisign each stack's
  * necessary effects.
  *
  * This file is the parking lot for the effect-construction code (`mkBlockEffectsIntermediate`,
  * settlement / fallback / rollout / refund tx assembly) extracted from
  * [[hydrozoa.multisig.ledger.joint.JointLedger]].
  *
  * The parked code is kept here so it compiles but is currently '''never instantiated'''.
  */
object StackActor {
    // Intentionally empty for now. See the file-level docstring.
}
