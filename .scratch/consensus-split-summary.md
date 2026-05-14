# Consensus Split — Review Summary

Branch: `feature/consensus-split`. Three commits May 12, plus a scalafmt cleanup.

---

## `f8e94e40` Feat: scaffolding for fast/slow consensus split

Additive only — wiring untouched. Sets up types the fast path needs.

- `BlockHeader.HeaderSignature` — top-level alias of `Minor.HeaderSignature`. Single sig type covers Minor/Major/Final on the fast side.
- `BlockHeader.Section.signingBytes` — canonical bytes peers sign for soft-ack. Reuses `Minor.Onchain` PlutusData layout via shared `Section` fields; Final passes empty KZG commitment.
- `SoftAck` — new ack type. Carries `{ackId, blockNum, headerSig, finalizationRequested}`. `ackNum := blockNum` so the PeerLiaison batch sequence stays gap-free.
- `SlowConsensusActor.scala` + `StackActor.scala` — empty stubs marking parking spots for code about to be ripped out (Major1/2 + Final1/2 cells, effect tx-sig aggregation, `mkBlockEffectsIntermediate`, settlement/fallback/rollout/refund assembly).

---

## `8c984ac8` Feat: Block.SoftConfirmed family for fast-consensus output

- `Block.SoftConfirmed.{Minor,Major,Final}` + `Next` / `Intermediate` / `NonFinal` aliases. Brief + per-peer header sigs + OR'd `finalizationRequested`.
- Deliberately NOT a `Block`. Block = brief + L1 effects; SoftConfirmed has only the brief. Keeps `Block.Section`'s "must have effects" invariant intact for existing `Unsigned` / `MultiSigned` (which become slow-side later).
- `headerNonFinal` extension downcasts brief header for non-final-only fields (BlockWeaver wakeup timing).

---

## `2c62abef` Feat: switch fast path to brief + soft-ack pipeline

The big one. **+378 / -1659**; ConsensusActor lost ~1300 lines.

### JointLedger

- Deleted `mkBlockEffectsIntermediate` + Final-branch effect construction. No more settlement/fallback/rollout/refund/finalization tx building on the fast side.
- `completeBlockRegular` → emits `BlockBrief.Intermediate`, runs only L1 deposit-map drain. No settlement-tx-seq build → treasury utxo stops rotating on the fast side (slow's job).
- `completeBlockFinal` → emits `BlockBrief.Final` directly. No `L1LedgerM.finalizeLedger`.
- `handleBlock` takes brief, signs once via `HeadPeerWallet.mkSoftAck`, ships brief + own SoftAck to consensus + brief to liaisons when leading.
- `proxyConfirmation` takes `Block.SoftConfirmed.Next` not `MultiSigned.Next`. Refund CBORs unavailable on fast-only path → L2 ledger gets an empty refund list until slow is up.

### ConsensusActor — rewritten from scratch

- One `ConsensusCell` per in-flight blockNum. Replaces the five round-specific cells (Minor / Major1 / Major2 / Final1 / Final2).
- Saturated when brief present + every peer has a SoftAck. Verifies each Ed25519 sig vs the brief's `signingBytes`. Emits `Block.SoftConfirmed.Next`.
- Postponed-ack semantics retained: own ack for N+1 received pre-saturation of N is stashed on cell N, announced when N completes.
- Output fans out to BlockWeaver, peer liaisons, and `JointLedger.proxyConfirmation`. CardanoLiaison stays wired but is never sent a confirmation on this branch.
- Old cell hierarchy + round-1 / round-2 own-ack scheduling dropped here — will be re-introduced in `SlowConsensusActor` when slow is wired.

### PeerLiaison + transport

- Outbox queue + verification speak `SoftAck` not `AckBlock`. Wire codec collapses to `Codec[SoftAck]`; per-variant `kind` ADT codec is gone.
- `BlockConfirmed` alias → `BlockStatus.SoftConfirmed`. Brief gets `asSoftConfirmed` mirror of `asMultiSigned`.

### BlockWeaver

- Receives `Block.SoftConfirmed` instead of `MultiSigned` everywhere (Request type, follower / leader handlers, wakeup-fiber input, unexpected-request guards).
- `headerNonFinal` extension on `SoftConfirmed.NonFinal` replaces MultiSigned version for forced-major / deposit-decision wakeup times.

### Stage1 SUT

- `CompleteBlock` sync return `Unsigned.Next` → `BlockBrief.Next`.
- `effectsAcc` removed (no effects flow on fast-only path).
- Effect-presence assertion → empty-list pass-through (vacuously satisfied) until slow consensus is wired.

### Stage4 SUT

- `BlockBriefObserver` intercepts `BlockBrief.{Minor,Major}` on the consensus actor's request channel instead of the old `Block.Unsigned.{Minor,Major}` envelopes.

---

## `0a4c8b24` Chore: scalafmt + flip stage4 runner to Direct / 1-test

- Format-only across 9 files: ConsensusActor, SlowConsensusActor, StackActor, SoftAck, HeadPeerWallet, Block, BlockHeader, BlockStatus, JointLedger.
- Runner.scala flipped active filter back to "Two-peers head works" Direct + `withMinSuccessfulTests(1)` for tight inner-loop iteration.

---

## Verified

Stage4 Direct under TestControl green: 5 test cases, 5,376 soft-confirmed blocks, 0 panics (per `integration/integration-tests.log` 2026-05-12 run).

---

## Three invariants the rework respects

1. **BlockWeaver = sole leadership owner.** (Will need two leader roles when slow is wired: fast per-block, slow per-stack — independent round-robin schedules.)
2. **JointLedger = leader/follower symmetric.** Produces `BlockBrief` synchronously on the fast path; will cache request outcomes for slow-side effect derivation. Effect-building moved OFF the fast path.
3. **ConsensusActor = pure ack collector.** Now soft-ack only. Hard-ack collector will live in `SlowConsensusActor` (parked).

---

## Parked, not done (for the full spec)

- Block stacks abstraction.
- Coil peer topology + actor + quorum logic.
- Independent `isSlowLeader(stackNum)` schedule.
- Necessary-effects selection (partition-by-major-version; drop superseded standalone evac-commitments).
- `GetMsgBatch` / `NewMsgBatch` extension with `coilHardAcks` map.
- `JointLedger` outcome cache surface for deferred effect derivation.
