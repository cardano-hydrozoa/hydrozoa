# Stage1 slow-consensus wiring — decision brief

**Status:** analysis only, no decision taken. Companion to `.scratch/slow-consensus-plan.md`
(milestone **M11**). Stage4 half of M11 (StackObserver + `propStackCoverage`) is implemented;
this brief covers the deferred stage1 half.

Date: 2026-05-18.

---

## 1. What stage1 is

- **Single-peer** (`Alice`; `import test.TestPeerName.Alice`, `GenWithTestPeers`).
- **Command-driven**, not timer-driven: `StartBlock` / `CompleteBlock` SUT commands drive a
  **real `JointLedger`** + **real `ConsensusActor`** through `AgentActor`
  (`integration/.../stage1/Sut.scala:80`).
- Stubs: `BlockWeaverMock` (no real weaving), `EventSequencerStub`, `StackComposerStub`
  (`integration/.../stage1/Suite.scala:520,531`). No `SlowConsensusActor`. `peerLiaisons = List()`.
- Runs against **real Yaci L1**.
- **Purpose:** prove the protocol's L1 effect txs (init / settlement / rollout / finalization /
  deinit / fallback) actually land on a real Cardano node.

This purpose is **currently dead**: `Suite.scala:695` hardcodes
`effects <- IO.pure(List.empty[BlockEffects.Unsigned])`, so the effect-presence assertion is
vacuously satisfied.

## 2. The real problem (not "fill the list")

`mkExpectedEffects` (`Suite.scala:748`) is built on **fast-side `BlockEffects.Unsigned`** —
per-block `Major → settlementTx + rolloutTxs`, `Final → finalizationTx + rolloutTxs`. The
fast/slow split removed effect production from the fast cycle; that type is never filled now.

The slow cycle produces **`StackEffects.HardConfirmed` per stack** (after the
Unsigned→HardConfirmed split). Restoring stage1 means **rebuilding the assertion around
`StackEffects.HardConfirmed`**, sourced from observed `Stack.HardConfirmed` events — not
repopulating a `List[BlockEffects.Unsigned]`.

## 3. Wiring delta (tractable — single-peer is the key)

`allPeers = {Alice}` ⇒ `SlowConsensusActor` **self-saturates on its own hard-acks** (no remote
`PeerLiaison`s, no quorum wait). So the change is ~4–5 edges, not a harness teardown:

1. `StackComposerStub` → **real `StackComposer`** — pairs real `BlockResult` (from JointLedger)
   with real `Block.SoftConfirmed` (from ConsensusActor); single-peer soft-confirms on own ack.
2. Add **real `SlowConsensusActor`** — self-saturates → emits `Stack.HardConfirmed` to
   `CardanoLiaison` (which already submits to Yaci, M9-full) + `PreviousStackHardConfirmation`
   to StackComposer.
3. Rewire `JointLedger.Connections.stackComposer` and `ConsensusActor.Connections.stackComposer`
   from the stub to the real StackComposer; give StackComposer + SlowConsensusActor their
   Connections.
4. Add a stage1 **`StackObserver`** wrapping `CardanoLiaison` (mirror of the stage4 one in
   `stage4/Sut.scala`) to capture `Stack.HardConfirmed` for the assertion.
5. Rewrite `mkExpectedEffects` to walk `StackEffects.HardConfirmed` (init / settlement /
   fallback / rollout / finalization tx ids) instead of `BlockEffects.Unsigned`; feed the
   existing Yaci `isTxKnown` poll loop (`Suite.scala:712`).

## 4. The actual design fork — assertion strength

| Option | What it asserts | Catches | Misses | Cost |
|---|---|---|---|---|
| **Weak (self-consistent)** | every effect tx in observed `Stack.HardConfirmed` is `isTxKnown` on Yaci | built-but-not-submitted; node-rejected tx | *slow side never produced an expected settlement* (nothing to compare against) | low — observer + walk + poll |
| **Strong (model-predicted)** | model predicts expected backbone by role (init + per-Major settlement(+rollouts) + final + fallback); assert those specific txs landed | also catches **missing** effects | — | higher — model must expose **stack-level** expectations (today it predicts fast-side per-block effects) |

The old `mkExpectedEffects` was effectively *strong* (it took the model's per-block effects).
Re-attaining strong requires teaching the stage1 model to predict stack-level effect presence.

## 5. Risks

1. **Yaci real-clock latency.** Slow pipeline adds stack-close → hard-ack → submit on top of
   fast confirmation. `waitForIdle(5.minutes)` + the 10×1s `isTxKnown` poll
   (`Suite.scala:712`) likely need widening. Stage1 is a **currently-green** suite — regression
   risk is real.
2. **Scenarios must close stacks.** Need enough soft-confirmed blocks for a stack to close
   (a Major ⇒ 2-phase stack; minor-only ⇒ sole). If the generator rarely closes a stack the
   assertion goes vacuous-by-accident — must guard against a green-but-empty result.
3. The earlier "spec-only now" decision was made under the assumption this was a large harness
   rewrite. Single-peer self-saturation makes it ~5 edges + an assertion rewrite — worth
   revisiting whether to implement now.

## 6. Open decisions (to settle later)

- **(a) Assertion strength:** weak (verify produced effects land) vs strong (also catch a
  *missing* expected effect, needs model work).
- **(b) Scope/timing:** still spec-only, or implement now given it's tractable?
- **(c) Timing tolerance:** how to widen `waitForIdle` / poll for the added slow-pipeline
  latency without making the suite flaky on Yaci.

## 7. Anchors

- `integration/src/test/scala/hydrozoa/integration/stage1/Suite.scala`
  — `:520` EventSequencerStub, `:531` StackComposerStub, `:535` JointLedger.Connections,
  `:555` ConsensusActor.Connections, `:695` vacuous effects, `:712` poll loop,
  `:748` `mkExpectedEffects`, `:735` `TxLabel`.
- `integration/src/test/scala/hydrozoa/integration/stage1/Sut.scala:80` — `AgentActor`.
- Stage4 reference implementation: `stage4/Sut.scala` `StackObserver`,
  `stage4/Suite.scala` `propStackCoverage` / `analyzeBlockBriefs`.
