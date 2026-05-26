package hydrozoa.integration

/** =Stage 4 integration test=
  *
  * ==Goal==
  *
  * Run the full multisig actor stack for `N` peers (`N >= 2`) inside a single JVM and one
  * `cats-actors` `ActorSystem`, and verify that the protocol behaves correctly under concurrent
  * user requests, deposits, and inevitable per-peer message reordering.
  *
  * Each peer's `PeerLiaison` actors are wired directly to their counterparts in-process via
  * `cats-actors` `ActorRef`s — there is no serialization and no network. Stage 5 (planned) replaces
  * the in-process wiring with real point-to-point peer transport, container-per-peer.
  *
  * ==SUT architecture==
  *
  *   - For each peer: `BlockWeaver`, `CardanoLiaison`, `EventSequencer`, `JointLedger`,
  *     `FastConsensusActor`, plus one `PeerLiaison` per (local, remote) pair, all wired in-process.
  *     There is no real network transport.
  *   - One shared [[hydrozoa.multisig.backend.cardano.CardanoBackendMock]] for all peers (shared L1
  *     state).
  *   - Factory pattern: build all actors against a `Deferred[IO,
  *     MultisigRegimeManager.Connections]`, then complete each peer's deferred after all actors
  *     exist so the cross-peer `PeerLiaison` graph can be wired.
  *   - A [[Sut.BlockBriefObserver]] proxy actor wraps each peer's `FastConsensusActor` to capture
  *     both leader-produced and follower-reproduced block briefs into per-peer
  *     `Ref[IO, Vector[BlockBrief.Intermediate]]`.
  *   - One side fiber per peer in `Stage4Sut.liaisonTickFibers` periodically delivers
  *     `CardanoLiaison.Timeout` to drive polling — see "Polling cadence" below.
  *
  * ==Cumulative model time==
  *
  * Stage 4 uses a '''single global model clock''' [[Model.ModelState.currentModelTime]] that
  * advances by `cmd.interArrivalDelay` (or `cmd.duration` for `DelayCommand`) on every command's
  * `runState`. The framework also calls `IO.sleep` with the same value before submission, so the
  * model clock and the SUT virtual clock stay in lockstep at every command boundary.
  *
  * Validity intervals on user requests are computed in the generator as
  * `[currentModelTime + interArrivalDelay - 5s, currentModelTime + interArrivalDelay + 2min]`, i.e.
  * `[submissionTime - 5s, submissionTime + 2min]`, where `submissionTime` is the SUT virtual clock
  * at the moment the leader's `JointLedger` will receive this request. Because cumulative model
  * time matches the SUT virtual clock, the leader's `blockCreationStartTime` falls comfortably
  * inside the window.
  *
  * '''Why a global clock instead of per-peer.''' An earlier iteration kept per-peer
  * `currentModelTimes(peerNum)` and used the acting peer's clock to compute validity. With `N`
  * peers each ticking independently, the acting peer's clock evolved at roughly `1/N` the rate of
  * the SUT virtual clock; validity windows expired before the SUT processed the request,
  * manifesting as `BlockOutOfRequestValidityInterval` rejections in the leader. The global clock
  * fixes that without losing the "less-active vs more-active peer" capability — which moves to the
  * generator's superposition picker (see below).
  *
  * ==TestControl: preferred for Direct, but not required==
  *
  * Stage 4 supports both `useTestControl = true` and `useTestControl = false`. TestControl is the
  * recommended default for `Direct` [[Sut.TransportMode]] runs and is set as the default on
  * `Stage4Suite`; non-TestControl is a fully-wired alternative and the only option once `WebSocket`
  * lands (real sockets do not speak virtual time). Two reasons to prefer TestControl when you can:
  *
  * '''Speed.''' TestControl jumps the virtual clock past `IO.sleep` rather than waiting, which is
  * exactly why stage 1 can simulate years of head lifetime in a fraction of an hour. Stage 4 is far
  * less dramatic: the 20-peer scenario only covers ~40 minutes of simulated time in ~20 minutes of
  * wall time — roughly a 2× speedup, because the actor stack for 20 peers is heavy enough that wall
  * time spent in CPU starts catching up to wall time spent in `IO.sleep`. After stage 1's
  * spectacular speedups the disappointing 2× ratio tempts you to conclude "barely paying for
  * itself, just turn it off" — especially when chasing some unrelated TestControl-side bug. The 2×
  * still matters, but it is a convenience, not a correctness argument.
  *
  * '''Soundness under load.''' The design requires the model clock and SUT clock to coincide at
  * every command boundary: the model advances by `cmd.interArrivalDelay` in `runState`, the
  * framework does `IO.sleep(cmd.interArrivalDelay)` before submission, and validity windows are
  * computed from the model clock. Under TestControl the SUT clock '''only''' advances during
  * `IO.sleep` (and other timer-aware effects) — not during command processing — so the two stay
  * equal by construction.
  *
  * Without TestControl the model clock and the harness submission wall-clock still stay in lockstep
  * at every command boundary — the harness sleeps `interArrivalDelay` real milliseconds and the
  * model advances by the same `interArrivalDelay` in `runState`, so the value the model assigns to
  * a request's submission time matches the wall time when `EventSequencer` accepts the `?:`. The
  * subtler concern is one level deeper: the `?:` returns as soon as the EventSequencer enqueues the
  * request, '''not''' when the leader actually folds it into a block. Block production, cross-peer
  * consensus, peer round-trips, and L1 polling all happen asynchronously on real wall-clock time
  * when TestControl is off. The leader evaluates the event at its block's `blockCreationStartTime`,
  * and the validity check compares that timestamp against the window the model assigned at
  * submission — `[submission - 5s, submission + 2min]`.
  *
  * As long as the leader can drain its mailbox fast enough that the wall-clock gap between
  * submission and block evaluation stays under the 2-minute upper margin, the test works fine. The
  * failure mode is saturation: if commands arrive faster than the leader (hosting 20 peers' worth
  * of actor traffic in one JVM) can produce blocks, the mailbox grows, and the gap between
  * submission and evaluation grows without bound. Once it exceeds 2 minutes, the leader rejects
  * every subsequent event with `BlockOutOfRequestValidityInterval` — the same failure shape as
  * stage 1's pre-fix design, but the mechanism is queue saturation rather than constant per-command
  * drift.
  *
  * Comparison with the stage-1 case:
  *   - '''Stage 1''' had a real Yaci DevKit clock and a model with no virtual clock; the
  *     unaccounted real time was the '''test-harness setup''' (Yaci spin-up, init tx, etc.)
  *     elapsing before the model thought the head had started. One-shot drift at the boundary of
  *     the test. Fix: anchor `takeoffTime = now + 60s` in `genInitialState` and have `startupSut`
  *     sleep until that anchor, so all setup fits inside a pre-takeoff window the model never
  *     observes.
  *   - '''Stage 4 without TestControl''' has unaccounted real time accumulate '''only when the SUT
  *     cannot keep up with the request rate'''. No constant per-command drift; saturation triggers
  *     it. Mitigation when running TestControl-off: keep `meanInterArrivalTime` comfortably above
  *     the leader's per-block service time, and watch for `BlockOutOfRequestValidityInterval`
  *     rejections as the saturation signal. Future work (relaxed validity windows / padded
  *     inter-arrival floors) would buy more headroom for `WebSocket` mode.
  *
  * '''Startup-time handling for both modes.''' `Stage4Suite.startupSut` advances the SUT clock to
  * the head's start epoch '''before''' creating the actor system, but the way it does so depends on
  * `useTestControl`:
  *
  *   - '''TestControl on''' (default `Direct` mode): `IO.sleep(state.currentModelTime.toEpochMs)`
  *     jumps the virtual clock from 0 to the head's start epoch instantly. The start epoch is the
  *     absolute Unix-epoch milliseconds (~1.77e12 today, roughly 56 years), drawn from a
  *     deterministic 100-day-window distribution starting Jan 1 2026.
  *   - '''TestControl off''' (e.g. `WebSocket`): `genInitialState` computes
  *     `takeoffTime = Instant.now() + 60s` and pins the head's initial `BlockCreationEndTime` to
  *     it; `startupSut` sleeps wall-clock time until `takeoffTime`, or aborts with a
  *     `RuntimeException` if setup overran the 60s budget. Same shape as stage 1's `takeoffTime`
  *     anchor; the budget value matches stage 1 today and may need raising for the heavier 20-peer
  *     JVM warmup.
  *
  * `Model.ModelState.takeoffTime: Option[java.time.Instant]` carries the wall-clock anchor
  * (`Some(t)` for non-TestControl mode, `None` otherwise). Stage 4 deliberately allows the
  * `(Direct, useTestControl = false)` combination — unlike stage 1, where `Mock` is hardcoded to
  * TestControl. The flag is set manually on `Stage4Suite` for now; once the WebSocket branch lands,
  * it will be derived from `TransportMode`.
  *
  * ==Three-phase test flow==
  *
  *   1. '''Generation''' — [[Generator.Stage4ScenarioGen]] produces a single flat sequence of
  *      commands. Each iteration calls `genSuperposedNextEvent` to sample
  *      `(peer, interArrivalDelay)` from the true Poisson superposition of `N` independent
  *      processes, then dispatches to the chosen peer's command builder
  *      ([[Generator.CommandGenerators.genL2TxCommand]] or
  *      [[Generator.CommandGenerators.genRegisterDepositCommand]]).
  *   2. '''Execution''' — `SutCommand.run` submits each request directly to the owning peer's
  *      `EventSequencer`. Validity is recorded per request in [[Model.ModelState.modelFlags]]; the
  *      SUT's verdicts come later from the block briefs.
  *   3. '''Shutdown / analysis''' — `Stage4Suite.shutdownSut` cancels the liaison tick fibers,
  *      drains the actor system via `waitForIdle` so any in-progress block seals and is captured by
  *      `BlockBriefObserver`, terminates the system, and combines the three properties into the
  *      test verdict.
  *
  * ==Poisson superposition for arrival generation==
  *
  * [[Generator.CommandGenerators.genSuperposedNextEvent]] samples the next event of the
  * superposition of `N` independent Poisson processes (one per peer, rate `lambda_p = 1/mean_p`):
  *
  *   - `interArrivalDelay ~ Exp(sum of lambda_p)` — gap from the previous event in the merged
  *     stream
  *   - `peer` chosen with probability `lambda_p / sum(lambda_q)` via `Gen.frequency`
  *
  * The marginal stream of any peer `p` (project onto peer-`p` events only) is exactly
  * `Poisson(lambda_p)`. Configuring per-peer mean inter-arrival times therefore produces the
  * intended less-active vs more-active behavior.
  *
  * ==Model state==
  *
  * [[Model.ModelState]] carries:
  *   - `currentModelTime` — single global clock,
  *   - `takeoffTime: Option[java.time.Instant]` — wall-clock anchor for non-TestControl runs (see
  *     "Why TestControl is mandatory" above); `None` under TestControl,
  *   - per-peer L1 funding UTxOs `peerUtxosL1` and shared L2 UTxOs `utxosL2Active`,
  *   - per-peer `pendingDeposits: List[PendingDeposit]` (deposits not yet absorbed into
  *     `utxosL2Active`); the global `absorbDeposits` helper checks all peers' lists when
  *     `currentModelTime` crosses each deposit's `expectedAbsorptionTime`,
  *   - `modelFlags: Map[RequestId, ValidityFlag]` — the model's submission-order verdict for every
  *     user request,
  *   - `registeredDeposits: Map[RequestId, PendingDeposit]` — every deposit ever registered,
  *     retained even after absorption so that shutdown analysis has access to each deposit's
  *     `absorptionStartTime` (protocol maturity) and `expectedAbsorptionTime` (=
  *     `absorptionStartTime + Params.absorptionSlack`, a model-side conservatism).
  *
  * Deposits in stage 4 are always submitted to the shared mock L1 backend the moment the command
  * fires (decline/refund flows are tested in stage 1, not stage 4). Each peer's `CardanoLiaison`
  * polls the backend on its own cadence — see "Polling cadence" below.
  *
  * ==Properties checked at shutdown==
  *
  * `Stage4Suite.analyzeBlockBriefs` returns `propLiveness && propDepositTiming && propValidRatio`.
  *
  * '''propLiveness — no silent loss.''' Every `RequestId` in [[Sut.Stage4Sut.submittedRequestIds]]
  * must appear in some block brief, either as an event (Valid or Invalid) or as a deposit (absorbed
  * or refunded). Catches dropped messages anywhere in the request pipeline. We have already hit one
  * bug of exactly this shape (`Mempool.extractRequestsWhile` was operating on the wrong receiver
  * and silently dropping every extracted request); without an end-to-end completeness check that
  * class of bug is invisible to the test until something downstream fails for the wrong reason.
  *
  * '''propDepositTiming — no early absorption.''' For every deposit `r` that appears in
  * `brief.depositsAbsorbed`,
  * {{{
  *   brief.endTime >= r.absorptionStartTime
  * }}}
  * holds. The unpadded `absorptionStartTime` (= `txTiming.depositAbsorptionStartTime(validityEnd)`)
  * is the protocol's actual maturity time; a violation means the off-chain code claimed to absorb a
  * deposit before its on-chain validators would have permitted it. Stage 4 only generates valid
  * deposits, so there is no symmetric refund-window check.
  *
  * '''propValidRatio — SUT not more permissive than model.''' Restricted to L2-tx reqIds (so
  * denominators are comparable to `briefs.events`; deposit reqIds rejected via
  * `JointLedger.rejectEvent` end up in `events` too and are filtered out symmetrically on both
  * sides):
  * {{{
  *   sutValid / sutTotal  <=  modelValid / modelTotal
  * }}}
  * compared as exact rationals via cross-multiplication. The SUT may turn an L2 tx that the model
  * considered Valid into Invalid (because reordering inside a block can break causal dependencies),
  * but the SUT must never accept a tx that the model rejected on its own input — that would mean
  * the SUT is using a weaker validity rule than the mutator the model uses.
  *
  * ==Why no model replay in SUT block order==
  *
  * The natural-looking next step would be to replay the model in the SUT's block order to compare
  * per-request flags. We considered it and dropped it for two reasons.
  *
  *   1. The model is the test oracle; in MBT terms, replaying the model against itself with
  *      reconstructed inputs is tautological. The SUT's verdict came from
  *      `HydrozoaTransactionMutator.transit(blockStartTime, utxos, tx)`. A faithful replay with the
  *      same block time, the same UTxO snapshot, and the same tx bytes would match by construction.
  *   2. The signal we wanted from replay — "did reordering flip outcomes?" — is captured cheaply by
  *      `propValidRatio` plus the printed model/SUT valid/total ratios in the analysis table.
  *
  * Cross-peer block-brief equality is out of scope: the protocol guarantees it by consensus, and a
  * per-block disagreement between peers is already caught by
  * `JointLedger.panicOnMismatchWithExpectedBlock` during execution.
  *
  * ==Polling cadence==
  *
  * Hydrozoa's safety relies on every peer's `CardanoLiaison` polling L1 frequently enough that all
  * peers observe a deposit utxo before the protocol decides to absorb it.
  * [[hydrozoa.config.head.multisig.timing.TxTiming.cardanoLiaisonPollingPeriodSafetyFactor]] (= 5)
  * and `TxTiming.Section.maxCardanoLiaisonPollingPeriod` enforce
  * `pollingPeriod * 5 <= depositMaturityDuration`. This is checked in `NodeConfig`'s case-class
  * body and respected by
  * [[hydrozoa.config.node.operation.multisig.generateNodeOperationMultisigConfig]].
  *
  * '''Stage 4 currently halves the upper bound.''' `genInitialState` overrides the default
  * `generateNodeOperationMultisigConfig` to sample `cardanoLiaisonPollingPeriod` uniformly from
  * `[1ms, maxCardanoLiaisonPollingPeriod / 2]` rather than the full `[1ms, max]` range. Reason:
  * with 20 peers polling on independent cadences, longer polling periods can produce mismatched
  * block briefs at major-block consensus when peer A has observed a new deposit but peer B has not;
  * halving the upper bound keeps inter-peer L1-observation skew tighter. The
  * `pollingPeriod * 5 <= depositMaturityDuration` invariant still holds with extra headroom
  * (effectively `pollingPeriod * 10 <= depositMaturityDuration`). Treat as a debugging knob — if
  * brief mismatches go away with the halving in place, that confirms the polling-skew hypothesis
  * and the bound can be tuned more deliberately.
  *
  * '''Driving polling under TestControl.''' Production `CardanoLiaison` uses `setReceiveTimeout` to
  * schedule periodic `Timeout` self-messages; but cats-actors `setReceiveTimeout` is unusable under
  * TestControl because it (a) is gated on a hardcoded 1-second-virtual ping loop in `ActorCell`,
  * and (b) compares against `System.currentTimeMillis()` (real wall-clock) rather than the
  * `F`-effect clock — both disconnected from TestControl's virtual clock. As a workaround,
  * `Stage4Suite.startupSut` starts one side fiber per peer that loops
  * `IO.sleep(period) >> liaison ! Timeout`. `IO.sleep` IS virtual-clock-aware, so the configured
  * `cardanoLiaisonPollingPeriod` is honored against the simulated timeline. The fibers are stored
  * in [[Sut.Stage4Sut.liaisonTickFibers]] and cancelled in `shutdownSut` before `waitForIdle` and
  * `terminate`.
  *
  * ==Files==
  *
  *   - [[Commands]] — `DelayCommand`, `L2TxCommand`, `RegisterAndSubmitDepositCommand` plus their
  *     `CommandLabel` / `CommandProp` instances.
  *   - [[Model]] — `Params`, `PendingDeposit`, `ModelState`, the global `absorbDeposits`, and the
  *     `ModelCommand` instances that update state on each command.
  *   - [[Generator]] — `genSuperposedNextEvent`, per-command `Gen` functions, and
  *     `Stage4ScenarioGen`.
  *   - [[Sut]] — `BlockBriefObserver`, `Stage4PeerHandle`, `Stage4Sut`, `Stage4SutCommands`.
  *   - [[Suite]] — `Stage4Suite` (the `ModelBasedSuite` implementation), the liaison tick fiber
  *     wiring, `genInitialState`, the three property functions, and the per-block printout.
  *   - [[Runner]] — `Stage4Properties` (test entry point), `Stage4Runner.renderTable` for the
  *     per-peer command table, and the `@main stage4PrintCommandSequence` helper.
  *
  * ==Open follow-ups==
  *
  *   - Side-channel rejection capture (TODO in `Stage4Suite`): collect
  *     `JointLedger.UserRequestError` rejections in a `Ref` so we can flag
  *     `BlockOutOfRequestValidityInterval` separately from legitimate reordering-induced
  *     `BadAllInputsUTxOException` rejections.
  *   - Cap on extreme inter-arrival samples to keep the test inside the happy-path regime (a
  *     long-tail `Exp(sum lambda)` sample can advance the SUT past `forcedMajorBlockWakeupTime`,
  *     into silence period or rule-based fallback, which stage 4 does not model).
  */
package object stage4 {}
