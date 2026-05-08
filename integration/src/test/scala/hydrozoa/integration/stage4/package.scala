package hydrozoa.integration

/** =Stage 4 integration test=
  *
  * ==Goal==
  *
  * Run the full multisig actor stack for `N` peers (`N >= 2`) inside a single JVM and one
  * `cats-actors` `ActorSystem`, and verify that the protocol behaves correctly under
  * concurrent user requests, deposits, and inevitable per-peer message reordering.
  *
  * Each peer's `PeerLiaison` actors are wired directly to their counterparts in-process via
  * `cats-actors` `ActorRef`s — there is no serialization and no network. Stage 5 (planned)
  * replaces the in-process wiring with real point-to-point peer transport,
  * container-per-peer.
  *
  * ==SUT architecture==
  *
  *   - For each peer: `BlockWeaver`, `CardanoLiaison`, `EventSequencer`, `JointLedger`,
  *     `ConsensusActor`, plus one `PeerLiaison` per (local, remote) pair, all wired
  *     in-process. There is no real network transport.
  *   - One shared [[hydrozoa.multisig.backend.cardano.CardanoBackendMock]] for all peers
  *     (shared L1 state).
  *   - Factory pattern: build all actors against a `Deferred[IO,
  *     MultisigRegimeManager.Connections]`, then complete each peer's deferred after all
  *     actors exist so the cross-peer `PeerLiaison` graph can be wired.
  *   - A [[Sut.BlockBriefObserver]] proxy actor wraps each peer's `ConsensusActor` to
  *     capture both leader-produced and follower-reproduced block briefs into per-peer
  *     `Ref[IO, Vector[BlockBrief.Intermediate]]`.
  *   - One side fiber per peer in `Stage4Sut.liaisonTickFibers` periodically delivers
  *     `CardanoLiaison.Timeout` to drive polling — see "Polling cadence" below.
  *
  * ==Cumulative model time==
  *
  * Stage 4 uses a '''single global model clock''' [[Model.ModelState.currentModelTime]]
  * that advances by `cmd.interArrivalDelay` (or `cmd.duration` for `DelayCommand`) on
  * every command's `runState`. The framework also calls `IO.sleep` with the same value
  * before submission, so the model clock and the SUT virtual clock stay in lockstep at
  * every command boundary.
  *
  * Validity intervals on user requests are computed in the generator as
  * `[currentModelTime + interArrivalDelay - 5s, currentModelTime + interArrivalDelay + 2min]`,
  * i.e. `[submissionTime - 5s, submissionTime + 2min]`, where `submissionTime` is the SUT
  * virtual clock at the moment the leader's `JointLedger` will receive this request.
  * Because cumulative model time matches the SUT virtual clock, the leader's
  * `blockCreationStartTime` falls comfortably inside the window.
  *
  * '''Why a global clock instead of per-peer.''' An earlier iteration kept per-peer
  * `currentModelTimes(peerNum)` and used the acting peer's clock to compute validity. With
  * `N` peers each ticking independently, the acting peer's clock evolved at roughly `1/N`
  * the rate of the SUT virtual clock; validity windows expired before the SUT processed
  * the request, manifesting as `BlockOutOfRequestValidityInterval` rejections in the
  * leader. The global clock fixes that without losing the "less-active vs more-active
  * peer" capability — which moves to the generator's superposition picker (see below).
  *
  * ==Three-phase test flow==
  *
  *   1. '''Generation''' — [[Generator.Stage4ScenarioGen]] produces a single flat sequence
  *      of commands. Each iteration calls `genSuperposedNextEvent` to sample
  *      `(peer, interArrivalDelay)` from the true Poisson superposition of `N` independent
  *      processes, then dispatches to the chosen peer's command builder
  *      ([[Generator.CommandGenerators.genL2TxCommand]] or
  *      [[Generator.CommandGenerators.genRegisterDepositCommand]]).
  *   2. '''Execution''' — `SutCommand.run` submits each request directly to the owning
  *      peer's `EventSequencer`. Validity is recorded per request in
  *      [[Model.ModelState.modelFlags]]; the SUT's verdicts come later from the block
  *      briefs.
  *   3. '''Shutdown / analysis''' — `Stage4Suite.shutdownSut` cancels the liaison tick
  *      fibers, drains the actor system via `waitForIdle` so any in-progress block seals
  *      and is captured by `BlockBriefObserver`, terminates the system, and combines the
  *      three properties into the test verdict.
  *
  * ==Poisson superposition for arrival generation==
  *
  * [[Generator.CommandGenerators.genSuperposedNextEvent]] samples the next event of the
  * superposition of `N` independent Poisson processes (one per peer, rate
  * `lambda_p = 1/mean_p`):
  *
  *   - `interArrivalDelay ~ Exp(sum of lambda_p)` — gap from the previous event in the
  *     merged stream
  *   - `peer` chosen with probability `lambda_p / sum(lambda_q)` via `Gen.frequency`
  *
  * The marginal stream of any peer `p` (project onto peer-`p` events only) is exactly
  * `Poisson(lambda_p)`. Configuring per-peer mean inter-arrival times therefore produces
  * the intended less-active vs more-active behavior.
  *
  * ==Model state==
  *
  * [[Model.ModelState]] carries:
  *   - `currentModelTime` — single global clock,
  *   - `peerLastRequestTimes` — per-peer activity bookkeeping,
  *   - per-peer L1 funding UTxOs `peerUtxosL1` and shared L2 UTxOs `utxosL2Active`,
  *   - per-peer `pendingDeposits: List[PendingDeposit]` (deposits not yet absorbed into
  *     `utxosL2Active`); the global `absorbDeposits` helper checks all peers' lists when
  *     `currentModelTime` crosses each deposit's `expectedAbsorptionTime`,
  *   - `modelFlags: Map[RequestId, ValidityFlag]` — the model's submission-order verdict
  *     for every user request,
  *   - `registeredDeposits: Map[RequestId, PendingDeposit]` — every deposit ever
  *     registered, retained even after absorption so that shutdown analysis has access to
  *     each deposit's `absorptionStartTime` (protocol maturity) and `expectedAbsorptionTime`
  *     (= `absorptionStartTime + Params.absorptionSlack`, a model-side conservatism).
  *
  * Deposits in stage 4 are always submitted to the shared mock L1 backend the moment the
  * command fires (decline/refund flows are tested in stage 1, not stage 4). Each peer's
  * `CardanoLiaison` polls the backend on its own cadence — see "Polling cadence" below.
  *
  * ==Properties checked at shutdown==
  *
  * `Stage4Suite.analyzeBlockBriefs` returns `propLiveness && propDepositTiming &&
  * propValidRatio`.
  *
  * '''propLiveness — no silent loss.''' Every `RequestId` in
  * [[Sut.Stage4Sut.submittedRequestIds]] must appear in some block brief, either as an
  * event (Valid or Invalid) or as a deposit (absorbed or refunded). Catches dropped
  * messages anywhere in the request pipeline. We have already hit one bug of exactly this
  * shape (`Mempool.extractRequestsWhile` was operating on the wrong receiver and silently
  * dropping every extracted request); without an end-to-end completeness check that class
  * of bug is invisible to the test until something downstream fails for the wrong reason.
  *
  * '''propDepositTiming — no early absorption.''' For every deposit `r` that appears in
  * `brief.depositsAbsorbed`,
  * {{{
  *   brief.endTime >= r.absorptionStartTime
  * }}}
  * holds. The unpadded `absorptionStartTime` (= `txTiming.depositAbsorptionStartTime(validityEnd)`)
  * is the protocol's actual maturity time; a violation means the off-chain code claimed
  * to absorb a deposit before its on-chain validators would have permitted it. Stage 4
  * only generates valid deposits, so there is no symmetric refund-window check.
  *
  * '''propValidRatio — SUT not more permissive than model.''' Restricted to L2-tx reqIds
  * (so denominators are comparable to `briefs.events`; deposit reqIds rejected via
  * `JointLedger.rejectEvent` end up in `events` too and are filtered out symmetrically on
  * both sides):
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
  * The natural-looking next step would be to replay the model in the SUT's block order to
  * compare per-request flags. We considered it and dropped it for two reasons.
  *
  *   1. The model is the test oracle; in MBT terms, replaying the model against itself
  *      with reconstructed inputs is tautological. The SUT's verdict came from
  *      `HydrozoaTransactionMutator.transit(blockStartTime, utxos, tx)`. A faithful replay
  *      with the same block time, the same UTxO snapshot, and the same tx bytes would
  *      match by construction.
  *   2. The signal we wanted from replay — "did reordering flip outcomes?" — is captured
  *      cheaply by `propValidRatio` plus the printed model/SUT valid/total ratios in the
  *      analysis table.
  *
  * Cross-peer block-brief equality is out of scope: the protocol guarantees it by
  * consensus, and a per-block disagreement between peers is already caught by
  * `JointLedger.panicOnMismatchWithExpectedBlock` during execution.
  *
  * ==Polling cadence==
  *
  * Hydrozoa's safety relies on every peer's `CardanoLiaison` polling L1 frequently enough
  * that all peers observe a deposit utxo before the protocol decides to absorb it.
  * [[hydrozoa.config.head.multisig.timing.TxTiming.cardanoLiaisonPollingPeriodSafetyFactor]]
  * (= 5) and `TxTiming.Section.maxCardanoLiaisonPollingPeriod` enforce
  * `pollingPeriod * 5 <= depositMaturityDuration`. This is checked in `NodeConfig`'s
  * case-class body and respected by
  * [[hydrozoa.config.node.operation.multisig.generateNodeOperationMultisigConfig]].
  *
  * '''Driving polling under TestControl.''' Production `CardanoLiaison` uses
  * `setReceiveTimeout` to schedule periodic `Timeout` self-messages; but cats-actors
  * `setReceiveTimeout` is unusable under TestControl because it (a) is gated on a
  * hardcoded 1-second-virtual ping loop in `ActorCell`, and (b) compares against
  * `System.currentTimeMillis()` (real wall-clock) rather than the `F`-effect clock — both
  * disconnected from TestControl's virtual clock. As a workaround, `Stage4Suite.startupSut`
  * starts one side fiber per peer that loops `IO.sleep(period) >> liaison ! Timeout`.
  * `IO.sleep` IS virtual-clock-aware, so the configured `cardanoLiaisonPollingPeriod` is
  * honored against the simulated timeline. The fibers are stored in
  * [[Sut.Stage4Sut.liaisonTickFibers]] and cancelled in `shutdownSut` before
  * `waitForIdle` and `terminate`.
  *
  * ==Files==
  *
  *   - [[Commands]] — `DelayCommand`, `L2TxCommand`, `RegisterAndSubmitDepositCommand`
  *     plus their `CommandLabel` / `CommandProp` instances.
  *   - [[Model]] — `Params`, `PendingDeposit`, `ModelState`, the global `absorbDeposits`,
  *     and the `ModelCommand` instances that update state on each command.
  *   - [[Generator]] — `genSuperposedNextEvent`, per-command `Gen` functions, and
  *     `Stage4ScenarioGen`.
  *   - [[Sut]] — `BlockBriefObserver`, `Stage4PeerHandle`, `Stage4Sut`,
  *     `Stage4SutCommands`.
  *   - [[Suite]] — `Stage4Suite` (the `ModelBasedSuite` implementation), the liaison tick
  *     fiber wiring, `genInitialState`, the three property functions, and the per-block
  *     printout.
  *   - [[Runner]] — `Stage4Properties` (test entry point), `Stage4Runner.renderTable` for
  *     the per-peer command table, and the `@main stage4PrintCommandSequence` helper.
  *
  * ==Open follow-ups==
  *
  *   - Side-channel rejection capture (TODO in `Stage4Suite`): collect
  *     `JointLedger.UserRequestError` rejections in a `Ref` so we can flag
  *     `BlockOutOfRequestValidityInterval` separately from legitimate reordering-induced
  *     `BadAllInputsUTxOException` rejections.
  *   - Cap on extreme inter-arrival samples to keep the test inside the happy-path regime
  *     (a long-tail `Exp(sum lambda)` sample can advance the SUT past
  *     `forcedMajorBlockWakeupTime`, into silence period or rule-based fallback, which
  *     stage 4 does not model).
  */
package object stage4 {}
