# Integration Test Stages

This document explains the two integration test stages that live under the `integration/` subproject. The goal is to give a new contributor enough orientation to know **which stage tests what**, **where to add a new test**, and **what every property is checking**. It assumes familiarity with the basics of the protocol (fast/slow consensus split, head peers, blocks vs stacks). For deeper architectural background, see `docs/spec/fast-consensus.md`, `docs/spec/slow-consensus.md`, and `docs/spec/testcontrol-driver.md`.

Both stages are built on the same `ModelBasedSuite` framework (`org.scalacheck.commands.ModelBasedSuite`), which runs ScalaCheck-generated command sequences against:

- a **model** — pure-functional state machine that predicts what *should* happen
- a **SUT** (System Under Test) — the real protocol actors wired up against a Cardano L1 backend

After each generated scenario, the model and the SUT are reconciled by a set of **properties** (ScalaCheck `Prop`s). A failure means the SUT diverged from what the model predicted.

The two stages exist because they exercise **different slices** of the protocol with different cost/coverage trade-offs.

## Stage Overview

| Aspect                  | Stage 1                                                  | Stage 4                                                       |
|-------------------------|----------------------------------------------------------|---------------------------------------------------------------|
| Peers                   | Single (`Alice`)                                         | Multi-peer (default 2; runners up to 20)                      |
| Driver                  | Command-driven (explicit `StartBlock` / `CompleteBlock`) | Timer-driven (`BlockWeaver` fires blocks autonomously)        |
| Fast cycle              | Real `JointLedger` + `FastConsensusActor` via `AgentActor`   | Real `JointLedger` + `FastConsensusActor` + `BlockWeaver` per peer |
| Slow cycle              | **Stubbed** (`StackComposerStub`, no `SlowConsensusActor`) | Real `StackComposer` + `SlowConsensusActor` per peer          |
| L1 backend              | Mock / Yaci DevKit / Blockfrost (public Preview)         | Mock                                                          |
| Clock                   | Virtual (`TestControl`) for Mock; real for Yaci/Blockfrost | Virtual (`TestControl`) in Direct transport; real in WebSocket |
| Inter-peer transport    | n/a (single peer)                                        | In-process actor refs **or** real WebSocket                   |
| Primary value           | "Does the protocol actually work against real Cardano?"   | "Does multi-peer consensus converge under stress?"            |

Both stages live in `integration/src/test/scala/hydrozoa/integration/<stage>/`. Each has the same five files:

- `Suite.scala` — the test class with `genInitialState`, `startupSut`, `shutdownSut`, and the properties.
- `Sut.scala` — the SUT data type plus the `SutCommand` typeclass instances that execute commands against it.
- `Model.scala` — the pure model state machine.
- `Commands.scala` — the command ADT (`L2TxCommand`, `DelayCommand`, etc.).
- `Generator.scala` — ScalaCheck generators for initial state and command sequences.
- `Runner.scala` — `@main` entry points for running scenarios manually.

---

## Stage 1

### Purpose

Stage 1 is the **simplest** stage and the **only stage that talks to a real Cardano backend** (Yaci DevKit or Blockfrost Preview). Its job is to prove that the protocol's fast cycle — block production, soft confirmation, and L1 effect submission via `CardanoLiaison` — actually works end-to-end against a Cardano node, not just against a mock.

Because real L1 backends are slow and the test exists primarily to catch L1-side breakage (signing, fee calculation, witness assembly, validity intervals, treasury rotation, etc.), stage 1 sacrifices breadth for fidelity: only one peer, no slow consensus, deterministic command-driven progression.

### SUT Architecture

Stage 1 runs **three real actors** for the single peer `Alice` and stubs everything else:

- **`JointLedger`** — real, produces `BlockBrief.Intermediate` synchronously when a block is completed.
- **`FastConsensusActor`** — real, collects soft-acks (single-peer ⇒ self-saturates immediately).
- **`CardanoLiaison`** — real, polls the L1 backend on each tick and submits any due L1 effects (init / fallback) discovered by the protocol.

These three are wrapped by an **`AgentActor`** (`Sut.scala:80`) which handles the synchronous `StartBlock` / `CompleteBlock` commands. The agent is what makes stage 1 command-driven: commands send messages to the agent, which orchestrates the three real actors in the right order.

Stubbed:

- **`BlockWeaverMock`** (`Suite.scala:60`) — does **not** auto-fire blocks; just tracks leader role for tracing purposes. Blocks happen only when the test scenario sends a `CompleteBlock` command.
- **`RequestSequencerStub`** — minimal request bookkeeping.
- **`StackComposerStub`** — accepts `BlockResult`s and ignores them. **No `SlowConsensusActor` is started.** Stage 1 has zero slow-cycle activity and asserts nothing about it.

### Commands

Stage 1's command set is intentionally explicit:

| Command                  | Effect                                                                                          |
|--------------------------|--------------------------------------------------------------------------------------------------|
| `DelayCommand`           | Advances simulated time by some duration; under TestControl this is virtual.                    |
| `StartBlockCommand`      | Tells `AgentActor` to open a new block (sets block creation start time).                        |
| `L2TxCommand`            | Submits an L2 tx to `RequestSequencer` for inclusion in the open block.                            |
| `RegisterDepositCommand` | Registers a deposit with `RequestSequencer` (model-side; SUT records the registration only).      |
| `SubmitDepositsCommand`  | Pushes the deposit's signed L1 tx directly to the backend, so `CardanoLiaison` observes it.      |
| `CompleteBlockCommand`   | Closes the block (block creation end time) and waits for the synchronous `BlockBrief.Next` reply. |

Scenarios are short sequences of these commands generated by `Generator.scala`. Because the test is single-peer and command-driven, every step is deterministic.

### L1 Backends

Stage 1 supports three backend configurations via the `SuiteCardano` enum (`Suite.scala:80`):

- **`Mock`** — `CardanoBackendMock`. Runs entirely under `TestControl` with virtual time. Default for CI; the `Mock` runner is included in `integration/test`.
- **`Yaci`** — `CardanoBackendBlockfrost` pointed at a local Yaci DevKit instance. Real wall-clock time. **Excluded** from the default `integration/test` task (requires a running Yaci container); selected via the Yaci runner.
- **`Blockfrost` (public Preview)** — `CardanoBackendBlockfrost` against `cardano-preview.blockfrost.io`. Real wall-clock time. Included in `integration/test` because public Preview is always available; needs a Blockfrost API key in env.

The backend is wired into both `CardanoLiaison` (which submits txs through it) and SUT commands (`Sut.scala:192,236` — query head UTxOs, submit deposit txs).

### Properties

The dominant check is the **per-`CompleteBlock` block-brief equality postcondition** (`Commands.scala:141`):

```scala
implicit given CommandProp[CompleteBlockCommand, BlockBrief, Model.State] with
    override def postCondition(
        cmd: CompleteBlockCommand,
        expectedResult: BlockBrief,
        state: Model.State,
        result: BlockBrief,
    ): Prop =
        (expectedResult == result) :|
            s"block briefs should be identical: …"
```

Every `CompleteBlockCommand` returns a synchronous `BlockBrief.Next` from `AgentActor`, and the model independently predicts the same brief from its own state. The postcondition fails the property if the two diverge on **any** field — events, validity flags, absorbed/refunded deposits, block version, end time, header — pinning the fast cycle's per-block output to the model on every block boundary, against whichever L1 backend is active. This is the contract stage 1 actually defends.

`shutdownSut` adds:

- **Actor-error surfacing** — any exception raised inside an actor (e.g. a fee miscalculation rejected by Yaci, a witness assembly bug) is reported and fails the property.

Effect-presence semantics — the assertion that the right txs landed on L1 — live in stage 4 (`EffectsLanded.propEffectsLanded`). To verify "the head's settlement tx landed on Yaci", that goes in a future stage-4 variant with a Yaci backend, not in stage 1.

### How to Run

```bash
# Default run (Mock + Public Preview included)
sbt "integration / test"

# Specific suite
sbt "integration / testOnly *Stage1Suite*"

# Yaci-backed (requires a Yaci DevKit container running locally)
sbt "integration / Test / runMain hydrozoa.integration.stage1.YaciRunner"
```

The `Mock` runs in seconds, `Yaci`/`Blockfrost` take minutes per scenario depending on network latency.

### Stage 1 Gotchas

- **`TestControl` timing anchor.** For the Mock backend, `genInitialState` produces an `Instant` deep in the future (Jan 2026 + random offset) and `startupSut` jumps the virtual clock to that instant before starting any actor. This is required because `TestControl.advance` only advances when no actor fibers are running — once the system is up, the per-actor 1-second ping loop competes for ticks and a multi-decade `IO.sleep` would never complete. See `docs/spec/testcontrol-driver.md`.
- **`takeoffTime` for Yaci/Blockfrost.** When the model clock must coincide with the wall clock at command 1, the model anchors `currentModelTime` at `Instant.now() + 60s` and `startupSut` sleeps until that anchor. 60 s is enough for the head's actor wiring; if startup overruns the test fails loudly rather than starting with already-violated timing.
- **`waitForIdle(5.minutes)` in `shutdownSut`.** Virtual under `TestControl`, wall-clock for Yaci/Blockfrost. The 5-minute cap is the upper bound on how long shutdown is allowed to take in real time before the test aborts.

---

## Stage 4

### Purpose

Stage 4 is the **multi-peer, full-consensus** stage. Both the fast and slow cycles run for real on every peer, in one actor system. The L1 backend is mock (instant, deterministic), but the protocol logic is fully exercised: leader election, soft-ack saturation, hard-ack saturation, two-round stack confirmation, deposit absorption, etc.

Stage 4 is timer-driven, not command-driven. A real `BlockWeaver` per peer fires blocks autonomously based on wakeup timers; commands only inject **user-level activity** (L2 txs, deposits) and **pass time**. This makes stage 4 a stress test for the consensus machinery under concurrent load, not a script of explicit protocol steps.

### SUT Architecture

Per peer, stage 4 starts **seven real actors** wired together by `HeadMultisigRegimeManager.Connections`:

- `BlockWeaver` — leader role + autonomous block firing via dead-man wakeups and deposit-decision wakeups.
- `JointLedger` — block construction, leader/follower symmetric.
- `FastConsensusActor` — soft-ack collector (fast cycle).
- `StackComposer` — pairs `BlockResult`s with soft-confirmations into closed stacks.
- `SlowConsensusActor` — hard-ack collector (slow cycle).
- `RequestSequencer` — request ingress.
- `CardanoLiaison` — L1 submission and polling.

Intermediate state is captured via `ContraTracer`. Specifically:

- A vector of `BlockBrief.Intermediate` is captured via tracing from the FastConsensusActor
- A vector of `Stack.HardConfirmed` is captured via tracing from the SlowConsensusActor  

### Multi-Peer in One Actor System

Every peer's actor stack lives in **the same `ActorSystem[IO]`**. This is the key simplification: there is no network in `TransportMode.Direct`, just actor refs pointing at each other. The shared `CardanoBackendMock` is also the same instance for every peer — they observe a single L1 state.

The leader role is deterministic (round-robin by block number, then by stack number for the slow cycle). Each peer's `PeerLiaison` actor manages the cursor protocol with each remote peer (see `PeerLiaison.scala` for the per-lane reference table).

### Commands and Generator

Stage 4 has only three commands:

| Command | Effect |
|---|---|
| `DelayCommand` | Pass time. Drives the `BlockWeaver` timers and the model clock. |
| `L2TxCommand(peerNum, request)` | Submit an L2 tx through that peer's `RequestSequencer`. |
| `RegisterAndSubmitDepositCommand` | Register the deposit + submit its signed tx straight to the shared mock L1 (so `CardanoLiaison` sees it). |

The generator uses **cumulative model time + Poisson superposition**:

- Each peer has an independent Poisson process of L2-tx arrivals (mean inter-arrival per peer is configurable).
- The combined process is itself Poisson; the generator produces the superposed arrival stream and labels each arrival with its originating peer.
- `DelayCommand`s are interleaved between arrivals so model time advances correctly.

This generator design gives realistic concurrent load without forcing the test author to script peer interleavings by hand.

#### Command responses are not used in assertions

A property-based-testing convention has each command's **synchronous response** participate in a per-command postcondition (this is exactly what stage 1's `CompleteBlock` block-brief check does). Stage 4 deliberately does **not** follow that convention: `L2TxCommand` and `RegisterAndSubmitDepositCommand` both return `ValidityFlag.Valid` unconditionally (`Sut.scala:178-200`), and `DelayCommand` returns `Unit`. The model predicts only the *eventual* outcome, not the immediate command response.

This is because stage 4 is timer-driven and concurrent: the moment of L2-tx submission is uncoupled from the moment of block inclusion (which depends on `BlockWeaver`'s autonomous timers, the leader's schedule, and the peer's request reordering). There is no per-command "did this request land correctly?" that the model can predict synchronously. All ground-truth validation happens **after** the scenario runs, via the shutdown properties (`propLiveness`, `propDepositTiming`, `propValidRatio`, `propStackCoverage`, `propEffectsLanded`), which compare the model's predictions against the recorded sequence of `BlockBrief`s and `Stack.HardConfirmed`s.

### Transport Modes

```scala
enum TransportMode:
    case Direct
    case WebSocket(basePort: Int = 31000)
```

- **`Direct`** (default) — every peer's `PeerLiaison` gets the actual remote handle from the other peer's actor stack. In-process, no network. **Compatible with `TestControl`** → virtual clock.
- **`WebSocket`** — every peer runs a `PeerWsTransport` bound to a distinct localhost port. Cross-peer messages travel through real WebSocket connections. **Forces real wall-clock** because real sockets don't speak virtual time.

Direct is faster and deterministic; WebSocket is the integration check for the on-wire protocol (codecs, framing, batch cursors).

### TestControl Branch

When `useTestControl == true` (Direct mode), `startupSut` advances the virtual clock to the head's start epoch **before** starting any actor:

```scala
_ <- IO.whenA(useTestControl)(IO.sleep(...))
```

Once actors are up, the per-actor 1-second ping loop competes for ticks and `tc.advance` becomes useless for big jumps. The pre-startup `IO.sleep` is the only chance to jump tens of years of simulated time. This pattern is shared with stage 1's Mock branch.

For wall-clock runs (WebSocket), `genInitialState` instead anchors the model clock at `Instant.now() + 60s` and `startupSut` sleeps until that anchor — same pattern as stage 1's Yaci/Blockfrost branch.

### Side-Fiber Polling

`CardanoLiaison` is designed to use `setReceiveTimeout` to drive its periodic poll. cats-actors' `setReceiveTimeout` is **broken under `TestControl`** (it uses `System.currentTimeMillis()` and a hardcoded 1-second ping). Stage 4 works around this by spawning one **side-fiber per peer** that sleeps for the configured polling period (virtual under TestControl, wall-clock otherwise) and pokes the liaison with a `CardanoLiaison.Timeout` message:

```scala
liaisonTickFibers <- peers.traverse { peerNum =>
    val period = nodeConfig.cardanoLiaisonPollingPeriod
    val liaison = peerStackMap(peerNum).cardanoLiaison
    (IO.sleep(period) >> (liaison ! CardanoLiaison.Timeout)).foreverM.start
}
```

These fibers are stored on `Stage4Sut.liaisonTickFibers` and cancelled in `shutdownSut` **after** `waitForIdle()` (so they keep the L1-polling-driven seal path alive while waitForIdle settles the system).

### Properties

`analyzeBlockBriefs` (`Suite.scala:521`) runs these properties at shutdown, AND-combined:

1. **`propLiveness`** — every submitted request id eventually appears in some block (as an event, an absorbed deposit, or a refunded deposit). Catches silent message loss.
2. **`propDepositTiming`** — every absorbed deposit was mature (`brief.endTime >= deposit.absorptionStartTime`). Catches premature absorption.
3. **`propValidRatio`** — the SUT's valid/total ratio doesn't exceed the model's. Catches the SUT being more permissive than the spec.
4. **`propStackCoverage`** — every block observed by the canonical peer is contained within some hard-confirmed stack's block range, and at least one stack was hard-confirmed. Catches a stalled slow cycle.
5. **`propEffectsLanded`** — for each Major/Final partition in each hard-confirmed stack: either the happy path (settlement/finalization + all rollouts) **or** the competing fallback landed on the mock L1 backend. Per-block disjunction; no model-time predicate needed (see `EffectsLanded.scala`).

### Effects-Landed Stats Table

`propEffectsLanded` emits a tracer-logged table after polling, matching the visual style of `traceBlockTable` / `traceStackTable`:

```
+--------------------------------------------------------------------------+
| Effects landed                                                           |
+--------------------------------------------------------------------------+
| stack#0 init: Happy (happy=1/1, fb=0/1)                                  |
| stack#1,p0 major: Happy (happy=3/3, fb=0/1)                              |
| stack#2,p0 final: Happy (happy=2/2, fb=n/a)                              |
+--------------------------------------------------------------------------+
Blocks: 3  Happy=3  Fallback=0  Pending=0
Happy txs landed: 6/6  Fallback txs landed: 0/2
```

`fb=n/a` means the partition has no fallback partner (Final blocks). `Pending` means neither happy nor fallback landed within the configured `attempts × sleep` budget — for the mock backend, `attempts=1, sleep=0.seconds` is enough; bump these for a future Yaci/testnet swap.

### How to Run

```bash
# Default (Direct + TestControl)
sbt "integration / testOnly *Stage4Suite*"

# WebSocket transport (real clock)
sbt "integration / Test / runMain hydrozoa.integration.stage4.WsRunner"
```

The default 2-peer run takes seconds; 10-peer WS runs at `commits=500` take a few minutes.

### Stage 4 Gotchas

- **Slow-cycle tail drain.** `waitForIdle()` returns when mailboxes are empty + child set is stable + deadLetters are drained, but it does **not** wait for scheduled future sleeps. The `PeerLiaison.startResendTimer` fiber sleeps between ticks; under WS-real-clock the last stack's hard-acks may not propagate before `terminate()`. `shutdownSut` sleeps `2 × peerLiaisonResendInterval` between `waitForIdle()` and `terminate()` to guarantee one full resend cycle.
- **Settling is `IO.cede`, not `IO.sleep`.** Under TestControl, `IO.sleep` doesn't make ticks happen — only `tc.tickOne` does. Settling between commands uses `IO.cede.replicateA_(N * nPeers)` to drain immediately-eligible fibers. See `docs/spec/testcontrol-driver.md`.
- **Polling vs maturity invariant.** `pollingPeriod * 5 ≤ depositMaturityDuration` — checked in `NodeConfig`. Otherwise `JointLedger` can race a deposit observation and panic.
- **No fallback-timing test.** Stage 4 has no `AfterCompetingFallbackStartTime` analogue and no commands that advance time past happy-path expiration without producing more blocks. `propEffectsLanded` accepts whichever path the protocol takes; the fallback branch is structurally testable but not currently exercised.

---

## E2E: Docker suites on the shipped topology (CI-excluded)

`integration/src/test/scala/hydrozoa/integration/e2e/` is a black-box level outside the `ModelBasedSuite` framework. `DockerHeadSuite` owns the bring-up — everything up to "every head peer reports `/ready`" — and each concrete suite supplies the scenario that runs against the formed head: `DockerSmokeTest` (L2 propagation) and `DockerRecoveryTest` (crash and recovery). Bringing a head up costs minutes, which is why the two share it.

### DockerSmokeTest — L2 propagation

It stands up the `docker-compose.yml` that `hydrozoa scaffold` writes — **2 head + 4 coil peers** — on a local **Yaci** devnet, forms a head through the packaged image and the real WS mesh + Ember HTTP API, submits an L2 transaction to head-0 over HTTP, and asserts it reaches both head peers' L2 ledgers. It's the one test that covers the documented deployment path end to end (`docker compose`, the image, real sockets, the custom-network config flow); the stages only reach `HydrozoaRoutes` in-process.

- **Shipped topology, not a lookalike.** It runs `keygen-fleet 2 4 2` against the scaffolded `docker-compose.yml` + `docker-compose.yaci.yml` overlay — the pair docs/user-guide/DEPLOYMENT.md hands an operator — so a failure is a failure of the documented path. The peer count lives in two places (the `keygen-fleet` args and the compose services); keep them in sync.
- **Only head peers are asserted on.** Coil peers run no `HydrozoaServer`, but they are load-bearing — the head can't initialize without `coilQuorum` of them signing — so a broken coil surfaces as a `/ready` timeout, not a silent pass.
- **Bring-up goes through `scripts/yaci-devnet.sh`** (`up`/`network`/`topup`, the script docs/user-guide/DEPLOYMENT.md documents); the rest shells out to the staged `hydrozoa` launcher and `docker compose`, with `SubmissionClient`/`EutxoL2QueryClient` for submit + polling. Container logs are dumped on failure.
- **Devnet requirements** (pinned in `docker-compose.yaci.yml`): protocol major version ≥ 11 and `companion` node mode — the validators are ill-formed below PV11, and the default mode can't install the PV11 cost models. Missing either fails in `deploy-scripts-and-g2-setup`.
- **CI-excluded** (minutes-long; needs Docker + Yaci + the built image): its FQN is in `Tests.Exclude(...)` in `build.sbt`, like `Stage1PropertiesYaci`. A stray `testOnly *` that reaches it without Docker or the launcher **cancels** (stays green), not fails.

```bash
just integration-e2e-docker   # builds the image, stages the launcher, runs the suite
```

### DockerRecoveryTest — a head peer crashes and comes back

Same deployment, run through the loss of a head peer. After a baseline L2 transaction confirms, the test SIGKILLs head-1 (`docker compose kill` — no SIGTERM, no flush, and the daemon treats it as a manual stop so `restart: on-failure` doesn't resurrect it), submits another transaction while the peer is gone, then `docker compose start`s the same container on the same named volume and asserts the head comes back whole: head-1 reaches `/ready` again, its recovered L2 view still carries the pre-crash transaction, the transaction submitted during the outage confirms on every head peer, and a fresh one confirms after that.

- **head-1 is the peer that matters.** The head multisig is *every* head peer plus a coil quorum, so losing one of two head peers is the outage that stops consensus; a coil peer's loss is absorbed by the 2-of-4 quorum. head-1 also publishes the HTTP API, so its recovered state is read directly rather than inferred.
- **Its own compose project** (`hydrozoa-e2e-recovery`), so the peers' named volumes are namespaced away from a smoke run's — the run must start from empty stores, since what a peer reads back from its store is the point.
- **It does not assert that head-0's L2 view stays empty during the outage.** A peer's ledger commits a block when the block is *produced*, and only the next block waits on soft-confirmation ([`fast-consensus.md`](fast-consensus.md)), so a surviving leader may legitimately have applied one more block before stalling. Convergence across peers is the property the suite checks instead.
- It is the black-box counterpart to the crash-restart coverage [`persistence-and-crash-recovery.md`](persistence-and-crash-recovery.md) §9 lists as not yet built: a whole-node reboot end to end, rather than a single actor's recovery contract.

```bash
just integration-e2e-docker-recovery
```

---

## Choosing Where to Add a Test

| Goal                                                                          | Stage     |
|-------------------------------------------------------------------------------|-----------|
| Verify a tx body / witness format against real Cardano                         | Stage 1 (Yaci/Blockfrost) |
| Verify the head bootstraps and `CardanoLiaison` submits init/fallback         | Stage 1   |Properties
| Exercise multi-peer consensus / leader handoff                                 | Stage 4   |
| Exercise the slow cycle (stacks, hard-acks, two-round confirmation)            | Stage 4   |
| Stress the system with concurrent L2 traffic                                   | Stage 4   |
| Cross-peer codec / framing / batch-cursor regression                            | Stage 4 (WebSocket mode) |
| L2 ledger correctness under reordering                                         | Stage 4   |
| L1-side fee / validity-interval / treasury-rotation regression                  | Stage 1 (Yaci/Blockfrost) |
| Black-box propagation across the packaged image + `docker compose` + HTTP API   | E2E (Docker, CI-excluded) |
| Whole-node crash + restart against a real store and a real mesh                 | E2E (Docker recovery, CI-excluded) |

A scenario that needs **both** real L1 and slow-cycle multi-peer consensus does not yet have a home — that's the "stage 4 against Yaci" path. The harness already supports the swap (`CardanoBackend[IO]` is the only L1 dependency); only `propEffectsLanded`'s `(attempts, sleep)` budget needs widening.

## Common Pitfalls

- **Stage 1 cannot test slow-cycle effects.** It stubs the slow side. Don't try to assert `Stack.HardConfirmed` produced anything in stage 1; there's no producer.
- **Stage 4 cannot test L1 backend issues.** The mock backend never rejects, never delays, never enforces protocol parameters precisely. Real-world fee or witness bugs caught only by stage 1.
- **All harness tables go through the tracer.** `traceBlockTable`, `traceStackTable`, and `EffectsLanded.traceEffectsTable` each render the table as a single multi-line string and emit it via `Tracer.info`, so the output lands in the test log via logback (not just stdout). When adding a new table, follow the same `render…` + `trace…` pair — never raw `println`.
- **Mock L1 is shared across peers in stage 4.** Every peer sees the same on-chain state. That's correct for a single head but doesn't model partitions or per-peer L1 views.
- **The cardanoBackend handle on `Stage1Sut`.** Used directly by SUT commands (query head UTxOs, submit deposit txs) and indirectly by `CardanoLiaison.runEffects` (poll → `PollResults` → `JointLedger`). It is load-bearing for `JointLedger` to observe L1 state regardless of whether the suite asserts effect presence.

## Related Code Map

- `integration/src/test/scala/hydrozoa/integration/`
  - `stage1/Suite.scala` — stage 1 test class.
  - `stage1/Sut.scala` — `Stage1Sut` + `AgentActor` + command typeclass instances.
  - `stage4/Suite.scala` — stage 4 test class, properties (`propLiveness`, `propDepositTiming`, `propValidRatio`, `propStackCoverage`).
  - `stage4/Sut.scala` — `Stage4Sut` + ContraTracer capture refs + command typeclass instances.
  - `stage4/EffectsLanded.scala` — per-block happy-or-fallback assertion + stats table.
- `org/scalacheck/commands/ModelBasedSuite.scala` — shared framework.
- `docs/spec/testcontrol-driver.md` — how virtual time works in the SUT.
- `docs/spec/fast-consensus.md`, `docs/spec/slow-consensus.md` — the two protocol cycles tested here.
