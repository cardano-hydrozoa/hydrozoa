package hydrozoa.integration

/** =Stage 4 integration test=
  *
  * ==What stage 4 tests==
  *
  * Runs N≥2 peers' full multisig actor stack (`BlockWeaver`, `JointLedger`, `FastConsensusActor`,
  * `StackComposer`, `SlowConsensusActor`, `CardanoLiaison`, `RequestSequencer`, plus inter-peer
  * liaisons; optional coil followers) in one JVM against a shared
  * [[hydrozoa.multisig.backend.cardano.CardanoBackendMock]] L1. A ScalaCheck-driven scenario
  * submits randomized L2 transactions, deposits, and delays at the per-peer `RequestSequencer`s;
  * the suite asserts the protocol behaves correctly on the happy path.
  *
  * ==Scenario==
  *
  * Commands ([[Commands]]) sampled by [[Generator.Stage4ScenarioGen]] from a Poisson superposition
  * of N per-peer processes: each `(peer, delay)` dispatches to that peer's `L2TxCommand`,
  * `RegisterAndSubmitDepositCommand`, or `DelayCommand`. A single global model clock advances by
  * `cmd.interArrivalDelay` and the harness sleeps the same amount before submission, so the model
  * clock and the SUT clock coincide at every command boundary.
  *
  * ==Properties (asserted in `Suite.beforeFinalize`)==
  *
  *   - '''propLiveness''' — every submitted `RequestId` appears in some block brief.
  *   - '''propDepositTiming''' — no deposit absorbed before its protocol maturity (`brief.endTime
  *     >= deposit.absorptionStartTime`).
  *   - '''propValidRatio''' — SUT L2 valid/total ratio ≤ model's (SUT no more permissive than the
  *     mutator).
  *   - '''propStackCoverage''' — every observed block lies in some hard-confirmed stack on the
  *     canonical peer.
  *   - '''propEffectsLanded''' — every backbone tx (settlement / finalization / init plus dependent
  *     rollouts) the slow cycle produced lands on L1.
  *   - '''propCoilParticipation''' — coil followers hard-confirm the same stacks the head does
  *     (no-op for pure-head runs).
  *   - '''analyzePersistence''' — §6/§7 producer-side writes (Treasury, EvacuationMap,
  *     HardConfirmation, BlockResult, SoftConfirmation, DepositMap, SoftAck/HardAck, Request)
  *     landed in each peer's backend store.
  *   - '''No actor errors''' — the error-stream drainer captured no uncaught actor exception.
  *
  * ==Contract: happy-path only==
  *
  * Stage 4 has no semantics in the rule-based regime. Any peer's `CardanoLiaison` dispatching a
  * `FallbackToRuleBased` short-circuits the scenario with `Prop.exception` via
  * `Stage4Sut.fallbackEnteredSignal`. Fallback firing is a generator / timing regression.
  *
  * ==TestControl==
  *
  * Direct-transport runs default to `useTestControl = true`: command delays advance virtual time,
  * and the scenario simulates minutes-to-hours of head lifetime in seconds. WebSocket runs are
  * real-clock (TestControl can't drive real sockets).
  *
  * ==Files==
  *
  *   - [[Commands]] — SUT command ADT.
  *   - [[Generator]] — Poisson-superposition scenario generator.
  *   - [[Model]] — model state, mutator, per-command state advancement.
  *   - [[Sut]] — `Stage4Sut`, `Stage4PeerHandle`, capture observers.
  *   - [[Suite]] — `Stage4Suite` (extends `ModelBasedSuite`), `beforeFinalize`, properties.
  *   - [[Runner]] — `Stage4Properties` (test entry point) and the `@main` print helper.
  */
package object stage4 {}
