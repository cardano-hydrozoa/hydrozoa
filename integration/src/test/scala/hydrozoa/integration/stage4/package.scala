package hydrozoa.integration

/** =Stage 4 integration test=
  *
  * ==Goal==
  *
  * Run the full multisig actor stack for `N` peers (`N >= 2`) inside a single JVM and one
  * `cats-actors` `ActorSystem`, and verify that the protocol behaves correctly under
  * concurrent user requests, deposits, and inevitable per-peer message reordering.
  *
  * Each peer's `PeerLiaison` actors are wired directly to their counterparts in-process
  * via `cats-actors` `ActorRef`s — there is no serialization and no network.
 *
  * ==SUT architecture==
  *
  *   - For each peer: `BlockWeaver`, `CardanoLiaison`, `EventSequencer`, `JointLedger`,
  *     `ConsensusActor`, plus one `PeerLiaison` per (local, remote) pair, all wired in-
  *     process. There is no real network transport.
  *   - One shared [[hydrozoa.multisig.backend.cardano.CardanoBackendMock]] for all peers
  *     (shared L1 state).
  *   - Factory pattern: build all actors against a `Deferred[IO,
  *     MultisigRegimeManager.Connections]`, then complete each peer's deferred after all
  *     actors exist so the cross-peer `PeerLiaison` graph can be wired.
  *   - A [[Sut.BlockBriefObserver]] proxy actor wraps each peer's `ConsensusActor` to
  *     capture both leader-produced and follower-reproduced block briefs into per-peer
  *     `Ref[IO, Vector[BlockBrief.Intermediate]]`.
  *
  * ==Three-phase test flow==
  *
  * 1. '''Generation''' — [[Generator.Stage4ScenarioGen]] produces a single flat
  *    interleaved sequence of commands tagged with [[hydrozoa.multisig.consensus.peer.HeadPeerNumber]].
  *    [[Model]] maintains per-peer clocks plus the shared `utxosL2Active` set. Each
  *    [[Commands.L2TxCommand]] /
  *    [[Commands.RegisterAndSubmitDepositCommand]] / [[Commands.DelayCommand]] is
  *    scheduled with an `interArrivalDelay` derived from a Poisson process per peer.
  * 2. '''Execution''' — `SutCommand.run` submits each request directly to the owning
  *    peer's `EventSequencer`. Validity is recorded per request in
  *    [[Model.ModelState.modelFlags]]; the SUT's verdicts come later from the block
  *    briefs.
  * 3. '''Shutdown / analysis''' — `Stage4Suite.shutdownSut` terminates the actor system,
  *    drains any actor errors, prints the per-block table, and combines three properties
  *    into the test verdict.
  *
  * ==Model state==
  *
  * [[Model.ModelState]] carries:
  *   - per-peer clocks `currentModelTimes` (advanced in `runState` by `interArrivalDelay`),
  *   - per-peer L1 funding UTxOs `peerUtxosL1` and shared L2 UTxOs `utxosL2Active`,
  *   - per-peer `pendingDeposits: List[PendingDeposit]` (deposits not yet absorbed into
  *     `utxosL2Active`),
  *   - `modelFlags: Map[RequestId, ValidityFlag]` — the model's submission-order verdict
  *     for every user request,
  *   - `registeredDeposits: Map[RequestId, PendingDeposit]` — every deposit ever
  *     registered, retained even after absorption so that shutdown analysis has access to
  *     each deposit's `absorptionStartTime` (protocol maturity) and `expectedAbsorptionTime`
  *     (= `absorptionStartTime + Params.absorptionSlack`, a model-side conservatism).
  *
  * Deposits in stage 4 are always submitted to the shared mock L1 backend the moment the
  * command fires (decline/refund flows are tested in stage 1, not stage 4). Each peer's
  * `CardanoLiaison` polls the backend on its own cadence (see "Polling invariant" below).
  *
  * ==Properties checked at shutdown==
  *
  * `Stage4Suite.analyzeBlockBriefs` returns `propLiveness && propDepositTiming &&
  * propValidRatio`.
  *
  * '''propLiveness — no silent loss.''' Every `RequestId` in
  * [[Sut.Stage4Sut.submittedRequestIds]] must appear in some block brief, either as an
  * event (Valid or Invalid) or as a deposit (absorbed or refunded). Catches dropped
  * messages anywhere in the request pipeline. We have already hit one bug of exactly
  * this shape (`Mempool.extractRequestsWhile` was operating on the wrong receiver and
  * silently dropping every extracted request); without an end-to-end completeness check
  * that class of bug is invisible to the test until something downstream fails for the
  * wrong reason.
  *
  * '''propDepositTiming — no early absorption.''' For every deposit `r` that appears in
  * `brief.depositsAbsorbed`,
  * {{{
  *   brief.endTime >= r.absorptionStartTime
  * }}}
  * holds. The unpadded `absorptionStartTime` (= `txTiming.depositAbsorptionStartTime(validityEnd)`)
  * is the protocol's actual maturity time; a violation means the off-chain code claimed
  * to absorb a deposit before its on-chain validators would have permitted it. The
  * symmetric refund-window check is a TODO — see "Open follow-ups".
  *
  * '''propValidRatio — SUT not more permissive than model.''' Restricted to L2-tx
  * requests so denominators are comparable to `briefs.events`,
  * {{{
  *   sutValid / sutTotal  <=  modelValid / modelTotal
  * }}}
  * compared as exact rationals via cross-multiplication. The SUT may turn an L2 tx that
  * the model considered Valid into Invalid (because reordering inside a block can break
  * causal dependencies), but the SUT must never accept a tx that the model rejected on
  * its own input — that would mean the SUT is using a weaker validity rule than the
  * mutator the model uses.
  *
  * ==Why no model replay in SUT block order==
  *
  * The natural-looking next step would be to replay the model in the SUT's block order
  * to compare the per-request flag and catch any divergence. We considered it and
  * dropped it for two reasons.
  *
  * '''(1) Delays don't transfer.''' Each command carries an `interArrivalDelay`
  * representing the wall-clock gap between the previous and the current command at
  * '''submission time'''. `Model.runState` advances the peer's clock by that delay, then
  * uses the clock for `absorbDeposits` and for the `time` argument to
  * [[hydrozoa.multisig.ledger.eutxol2.HydrozoaTransactionMutator.transit]]. Once the SUT
  * reorders requests inside a block, the cumulative submission delay at the new position
  * no longer corresponds to anything real:
  *   - The SUT used the block's `startTime` as the mutator time, not whatever the
  *     submission delays would imply.
  *   - The SUT absorbs each deposit at its block's time, not at the model's
  *     `expectedAbsorptionTime` derived from per-peer clocks.
  *
  * A correct replay would have to ignore `interArrivalDelay` and per-peer clocks
  * entirely and feed block times into a slimmed model entry that takes
  * `(state, blockTime, ledgerEvent)`. That's a non-trivial restructuring of the model.
  *
  * '''(2) Even with that fix, the check would be tautological.''' The SUT's verdict
  * came from `HydrozoaTransactionMutator.transit(blockStartTime, utxos, tx)`. A faithful
  * replay would call the same deterministic mutator with the same block time, the same
  * UTxO snapshot (reconstructed by walking the SUT's own `depositsAbsorbed` and prior
  * events in block order), and the same tx bytes. By construction it would match. A
  * "pass" would tell us only that we wired the replay correctly.
  *
  * The signal that motivated replay — "did reordering flip outcomes?" — is captured
  * cheaply by `propValidRatio` plus the printed `model: m₁/n₁  SUT: m₂/n₂` ratios in
  * the analysis table.
  *
  * Cross-peer block-brief equality is also out of scope here: the protocol guarantees it
  * by consensus, and a per-block disagreement between peers would already be caught by
  * `JointLedger.panicOnMismatchWithExpectedBlock` during execution.
  *
  * ==Polling invariant==
  *
  * [[hydrozoa.config.head.multisig.timing.TxTiming.cardanoLiaisonPollingPeriodSafetyFactor]]
  * (= 5) and `TxTiming.Section.maxCardanoLiaisonPollingPeriod` enforce
  * `pollingPeriod * 5 <= depositMaturityDuration`. This is checked in
  * `NodeConfig`'s case-class body and respected by
  * [[hydrozoa.config.node.operation.multisig.generateNodeOperationMultisigConfig]].
  * Without it, a leader can absorb a deposit on its own pollResults while a follower
  * still classifies it `NotInPollResults` and tries to refund — leading to
  * `panicOnMismatchWithExpectedBlock` in the follower.
  *
  * ==Files==
  *
  *   - [[Commands]] — `DelayCommand`, `L2TxCommand`, `RegisterAndSubmitDepositCommand`
  *     plus their `CommandLabel` / `CommandProp` instances.
  *   - [[Model]] — `Params`, `PendingDeposit`, `ModelState` and the `ModelCommand`
  *     instances that update state on each command.
  *   - [[Generator]] — per-command `Gen` functions and `Stage4ScenarioGen`.
  *   - [[Sut]] — `BlockBriefObserver`, `Stage4PeerHandle`, `Stage4Sut`,
  *     `Stage4SutCommands`.
  *   - [[Suite]] — `Stage4Suite` (the `ModelBasedSuite` implementation),
  *     `Stage4Suite.genInitialState`, the three property functions, and the per-block
  *     printout.
  *   - [[Runner]] — `Stage4Properties` (test entry point), `Stage4Runner.renderTable` for
  *     the per-peer command table, and the `@main stage4PrintCommandSequence` helper.
  *
  * ==Open follow-ups==
  *
  *   - Refund-window check in `propDepositTiming`: assert that every refunded deposit
  *     either had its absorption window expire or was never observed on L1 by any
  *     peer's `CardanoLiaison`. The first sub-case needs `depositAbsorptionEndTime` on
  *     `PendingDeposit`; the second sub-case needs per-peer poll-result history that
  *     we don't currently track.
  *   - Stage 5: real network transport, container-per-peer.
  */
package object stage4 {}
