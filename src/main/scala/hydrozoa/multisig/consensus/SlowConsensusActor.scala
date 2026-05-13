package hydrozoa.multisig.consensus

/** Placeholder for the slow-consensus actor.
  *
  * In the fast/slow split (see whitepaper §consensus), slow consensus collects hard
  * acknowledgments over block-stack effects and lives between the head and coil peers. This file
  * is the parking lot for code dragged out of the original [[ConsensusActor]] that handled
  * per-block effect-tx signature collection (Major1/Major2/Final1/Final2 cells, settlement /
  * fallback / rollout / finalization witness aggregation).
  *
  * The parked code is kept here so it compiles but is currently '''never instantiated''' — fast
  * consensus is sufficient for the integration milestones we care about now (stage4). When we
  * later wire up the slow cycle, this is where the per-stack two-round signing state machine will
  * live.
  */
object SlowConsensusActor {
    // Intentionally empty for now. See the file-level docstring.
}
