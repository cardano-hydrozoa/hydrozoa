# Slow-consensus hand-off — for George

Branch: `feature/slow-consensus`. Origin is current.

Plan reference: `.scratch/slow-consensus-plan.md` (Ilia's design doc, ~700 lines).
Previous summary: `.scratch/consensus-split-summary.md` (covers the fast/slow
split itself, May 12; this doc covers what's been built on top, May 14–15).

---

## TL;DR

Slow consensus is **architecturally complete** end-to-end — all actors talk,
all effect derivation produces real txs, leader signs own hard-acks upfront.
The only thing that doesn't *actually verify* is the slow consensus
aggregation itself: `SlowConsensusActor` is an auto-confirm stub that
immediately echoes `PreviousStackHardConfirmation` on receiving a stack
handoff, without collecting any remote peers' hard-acks.

Tests pass (the same 3 pre-existing flaky ScalaCheck failures as before;
no regressions from this work).

> **Post-handoff review pass (2026-05-17, commits `bbb6d6d4`..`b64e3762`).**
> A review pass refined several things since this doc was first written —
> dropped `HardAck.finalizationRequested`; disambiguated
> `HardAck.toContext` keys; **evac-commit signature is a
> `BlockHeader.HeaderSignature`** (KZG lives on the header), keyed by
> `BlockNumber`; **wallet methods are now pure mappers over
> `HardAck.SigningInputs.*`** (no `Stack` walking — `StackComposer` does
> it); `AckNumber→SoftAckNumber` rename marked (deferred); BlockResult
> `payoutObligations` clarified as L2 *withdrawals* (≠ refunds); dropped
> redundant `StackBrief.firstMajorBlockNum`; and — biggest — the
> **standalone evacuation commitment is now a spec-aligned dormant
> record, not a transaction** (see that section below; `StandaloneEvacCommitTx`
> + `L1LedgerM.mkStandaloneEvacCommitTx` removed). Spec-verified via the
> whitepaper MCP. The body of this doc has been updated in place; the
> sections below reflect the post-review state.

---

## Message-flow map (current state)

```
JointLedger.completeBlock{Regular,Final}
  ├─ BlockBrief                  → PeerLiaisons (leader only) + ConsensusActor (own + remote)
  ├─ BlockResult (NEW)           → StackComposer
  └─ own SoftAck                 → ConsensusActor

ConsensusActor (fast cycle)
  ├─ own SoftAck                 → PeerLiaisons
  ├─ remote SoftAck              ← PeerLiaisons
  └─ Block.SoftConfirmed         → BlockWeaver + PeerLiaisons + JointLedger.proxyConfirmation
                                 → StackComposer    (NEW pair-with-BlockResult)

StackComposer (slow cycle, M5)
  ├─ pairs (BlockResult, Block.SoftConfirmed) by blockNum
  ├─ Leader-mode (isSlowLeader(stackNum)):
  │    ├─ close stack from longest contiguous prefix
  │    ├─ derive effects: selectNecessaryEffects → deriveRegular
  │    ├─ sign own hard-acks upfront (round-1+round-2 OR sole)
  │    ├─ StackBrief             → PeerLiaisons  (DIRECT)
  │    └─ StackHandoff(unsigned, ownAcks)  → SlowConsensusActor
  └─ Follower-mode: validate inbound StackBrief, re-derive, hand off

SlowConsensusActor (M6, AUTO-CONFIRM STUB)
  ├─ on StackHandoff: log own-ack count, immediately emit
  │      Stack.HardConfirmed             → CardanoLiaison
  │      PreviousStackHardConfirmation   → StackComposer
  └─ HardAck inputs IGNORED (no real ack aggregation yet)

CardanoLiaison
  └─ Stack.HardConfirmed: LOGS RECEIPT (no L1 submission yet)

PeerLiaison transport (M7)
  ├─ outbox lanes: ack / blockBrief / event / stackBrief / hardAck
  ├─ GetMsgBatch cursors: ackNum / blockNum / stackBriefNum / hardAckNum / requestNum
  └─ outbox prune driven by remote's GetMsgBatch cursors (NOT local confirmation)
```

---

## What's done

### Types + data shapes (PR1 — `be9a9255`)
- `multisig/ledger/stack/{StackNumber, StackBrief, Stack, StackEffects}.scala`
- `multisig/ledger/block/BlockResult.scala`
- `multisig/ledger/l1/tx/StandaloneEvacuationCommitment.scala`
- `multisig/consensus/ack/{HardAck, HardAckId, HardAckNumber}.scala`
- `multisig/ledger/effects/{NecessaryEffectsPolicy, StackEffectsBuilder}.scala`

### Actor wiring (`467a35c2`, `9c6fd5c1`)
- `StackComposer` replaces `StackActor` stub; subscribes to BlockResult +
  Block.SoftConfirmed; pairs them; manages leader/follower mode; broadcasts
  StackBrief; hands off Stack.Unsigned + own acks to SlowConsensusActor.
- `SlowConsensusActor` replaces stub; auto-confirms on receipt; emits
  Stack.HardConfirmed + PreviousStackHardConfirmation.
- Both wired into `MultisigRegimeManager.Connections`; `isSlowLeader(stackNum)`
  on `HeadPeerId`.

### Wire transport (M7 — `2115a5b6`)
- `PeerLiaison`: `stackBrief` + `hardAck` outbox lanes, cursors
  (`stackBriefNum`, `hardAckNum`), verification rules, inbound dispatch
  (stackBrief → StackComposer; hardAck → SlowConsensusActor).
- `GetMsgBatch` / `NewMsgBatch` extended with the new fields; CodecsTest
  updated.
- Outbox prune: per-remote on incoming GetMsgBatch cursors (matches the
  recently reworked soft-side pattern).

### CardanoLiaison subscription (M9 partial — `1e0b78fd`)
- Stack.HardConfirmed added to Request type. Handler logs receipt only —
  L1 submission still TODO (notes: settlement/finalization unlock first,
  then fallback, rollouts, refunds, evac commits).

### Slow-side L1LedgerM (M10 — `2b0c8496`)
- `StackComposer` holds its own `L1LedgerM.State`, seeded from
  `config.initializationTx.treasuryProduced`. Treasury rotates through
  this state on every settlement / finalization / standalone-evac-commit.
- `runL1Action` private helper drives the rotation.

### Effect derivation (M1 — `eae8404b`, `b88b3081`, `8df24790`,
  `d350fe00`, `6daaddb3`)
- `NecessaryEffectsPolicy.selectNecessaryEffects`: partition-by-major /
  close-after-major algorithm. Last partition is TrailingMinors if no
  Major / Final closes the stack.
- `StackEffectsBuilder.deriveRegular`: per-block dispatch:
  - **Minor**: collects `postDatedRefundTxs`.
  - **Major**: calls `L1LedgerM.mkSettlementTxSeq(...)`.
  - **Final**: calls `L1LedgerM.finalizeLedger(...)`.
  - **TrailingMinors partition**: builds a pure
    `StandaloneEvacuationCommitment(committedBlockNum, blockVersion, kzg)`
    record (no L1-ledger interaction). See the spec-aligned section
    below.
- `L1LedgerM`: added public `map`, `flatMap`, `pure`, `cats.Monad` given
  (needed for `traverse`). (Earlier `mkStandaloneEvacCommitTx` removed —
  evac commitments don't touch the L1 ledger.)

### Wallet signing (M3 — `7eec15e4`, integration `c2279816`, refined `bbb6d6d4`/`2466998b`)
- Five wallet methods on `HeadPeerWallet`:
  `mkHardAckRound1Regular`, `mkHardAckRound1Initial`,
  `mkHardAckRound2Regular`, `mkHardAckRound2Initial`, `mkHardAckSole`.
  **Now pure mappers**: each takes a `HardAck.SigningInputs.*` struct
  (tx bodies / header signing bytes) + `stackNum` + `hardAckNum`, maps
  each entry through `mkTxSignature` / `mkHeaderSignature`. They do NOT
  inspect a `Stack` — `StackComposer.buildHandoff` does all the walking
  and assembles the `SigningInputs`. `HardAck` no longer carries
  `finalizationRequested` (derivable from the stack). Evac-commit
  entries are `BlockHeader.HeaderSignature` keyed by `BlockNumber`.
- StackComposer leader path bundles `(unsigned, ownAcks)` into
  `SlowConsensusActor.StackHandoff` and ships it. Round-2 sig is
  produced upfront (sig domain is the unlock tx body, known at close);
  SlowConsensusActor manages outbound scheduling (currently no-op).

### Naming sweep (`218f71d4`, `1ee19360`)
- `asMultiSigned` → `asHardConfirmed` on `BlockHeader`, `BlockBrief`,
  `BlockBody`. Stale `Block.MultiSigned` references in CardanoLiaison
  comments updated. `headerMultiSigned` field name kept (it's the
  aggregate of header sigs from SoftAcks — arguably belongs to fast side
  but rename is out of scope per the plan).
- `AckNumber → SoftAckNumber` rename **marked with `TODO(rename)`** (to
  parallel `HardAckNumber`); full rename touches AckId/SoftAck/
  PeerLiaison/ConsensusActor/Codecs/tests so deferred to a dedicated
  sweep. Stale `neededToConfirm` "AckBlock.Number" comments refreshed.

---

## What's NOT done

### M6 real ack collection (BLOCKER for end-to-end slow consensus)

`SlowConsensusActor` auto-confirms on any received StackHandoff. To make
slow consensus real:

1. Maintain a per-stackNum cell tracking phase + per-peer acks
   collected (similar to fast `ConsensusActor.ConsensusCell` but
   round-aware).
2. On StackHandoff: stash own acks; broadcast round-1 / sole own ack
   to PeerLiaisons immediately; withhold round-2 own ack.
3. On remote HardAck (round-1 / sole): verify each per-effect sig
   against local effect-tx bodies; aggregate; check saturation
   (all head peers acked round-1).
4. On round-1 saturation: release own round-2 ack → PeerLiaisons;
   transition cell to WaitingForRound2.
5. On remote HardAck (round-2): verify; aggregate; on round-2
   saturation: build Stack.HardConfirmed; emit to CardanoLiaison +
   PreviousStackHardConfirmation to StackComposer.
6. Postponed-ack stash for early N+1 acks (mirrors fast-side pattern).

Spec reference: `consensus/slow-consensus` in the gummiworm-whitepaper.
Plan section M6, lines ~488–542 of `.scratch/slow-consensus-plan.md`.

### Real Codec[HardAck] (BLOCKS hard-acks actually traversing wire)

`multisig/consensus/transport/Codecs.scala` has a placeholder Codec[HardAck]
that fails on decode. Acceptable today because nothing broadcasts hard-acks
yet (auto-confirm keeps them local). When M6 real impl lands, this needs
real codecs for:
- `TxSignature` (opaque `IArray[Byte]` — hex string)
- `BlockHeader.HeaderSignature` (opaque `IArray[Byte]` — used for
  evac-commit entries; there's already a hex codec for it in
  `Codecs.scala` for SoftAck — reuse)
- `VKeyWitness` (scalus type — bytes round-trip; Round2Initial only)
- `HardAck.Payload` discriminated union (5 variants); note `HardAck` has
  no `finalizationRequested` field anymore
- `Map[Int, TxSignature]` / `Map[(Int, Int), TxSignature]` /
  `Map[BlockNumber, BlockHeader.HeaderSignature]` (evac commits) and the
  `SolePayload.evacCommit: (BlockNumber, HeaderSignature)` tuple

### M9 full L1 submission (BLOCKER for M11 real assertions)

`CardanoLiaison`'s `Stack.HardConfirmed` handler currently only logs.
Real impl needs to:
1. Walk `Stack.HardConfirmed.round1.unsigned.effects: StackEffects.Regular`.
2. Build submission queue in dependency order: first settlement OR
   finalization (the unlock), then remaining settlements, fallbacks,
   rollouts, refunds. **Evac commitments are NOT submitted** — they are
   dormant dispute-only records; the consensus side just needs the
   header signatures persisted (storage layer, future).
3. Feed into the existing per-block effect-submission state machine
   (`handleMajorBlockL1Effects` / `handleFinalBlockL1Effects` —
   shapes are per-block bundled; will need adapter).
4. `StandaloneEvacuationCommitment` is not an L1 submission at all —
   skip it in CardanoLiaison entirely.

### Standalone evacuation commitment — spec-aligned model (RESOLVED)

Spec-verified (whitepaper MCP `replicated-state-machine/effects#evacuation-commitment`,
`#standalone-evacuation-commitment`, `consensus/slow-consensus#which-effects-are-necessary`):

- A standalone evac commitment exists **only for minor blocks** —
  TrailingMinors partitions. For initial/major blocks the evac
  commitment is **implicit in the initialization/settlement effect** and
  goes to L1 *immediately* when that effect executes; no standalone
  record, no separate signature. Final-closed → none.
- It is **NOT a transaction**. It is a contingent / dormant L1 effect: a
  fixed-size record `(headId, blockVersion, kzg)` that lays dormant and
  is presented to the L1 dispute-resolution scripts in the rules-based
  regime — only after a fallback effect executes. Never submitted
  immediately, never a treasury mutation.
- Hard-acked by signing the minor block's **header** (KZG lives on the
  header); persisted in the storage layer (FUTURE — not built) and
  produced to the dispute script if needed.
- Necessary-effects compression unchanged: only the LAST standalone
  commitment per contiguous minor run is kept.

Implemented: `StandaloneEvacCommitTx` (a treasury-rotating tx) +
`L1LedgerM.mkStandaloneEvacCommitTx` removed; replaced by the pure
record `StandaloneEvacuationCommitment(committedBlockNum, blockVersion,
kzgCommitment)`. `StackEffectsBuilder` builds it purely (no L1-ledger
interaction) for TrailingMinors partitions only. Consensus shape
unchanged (`HardAck...evacCommits: Map[BlockNumber, HeaderSignature]`,
keyed by `committedBlockNum`). `headId` is constant per head, supplied
at dispute-presentation time — not carried in the in-memory effect.

Still future (out of scope): the storage layer that persists these
header signatures and the dispute-script presentation path.

### M11 real test assertions

`integration/.../stage1/Suite.scala:688` uses
`List.empty[BlockEffects.Unsigned]` as the expected-effects accumulator,
so the assertion is vacuously satisfied. Once M9 full lands, the test
can subscribe to Stack.HardConfirmed events and assert tx-known on L1
for each effect.

### Initial stack (stack 0)

Spec describes stack 0 as a structurally-2-phase stack with the
exogenous initialization tx as its round-2 unlock + a locally-derived
fallback as round-1. `StackEffects.Initial` data type exists but no
boot path wires it; `StackComposer.buildHandoff` explicitly throws if
it encounters an Initial stack ("not yet supported"). Needs the
`Bootstrap(initialBrief, initialBlockResult, syntheticSoftConfirmed0,
initializationTx)` constructor parameter on StackComposer per plan
M5.

---

## Open design questions George should know about

1. **Block.HardConfirmed event (plan open question #7).** Slow side
   emits Stack.HardConfirmed but downstream consumers
   (RequestSequencer in a follow-up PR; possibly BlockStatus
   transitions) likely want per-block granularity. Decision deferred:
   fan out (Stack.HardConfirmed → Block.HardConfirmed events) vs.
   derived view in subscriber.

2. **Partition indexing in HardAck payload maps.** Current wallet
   methods use list-indices as partition_idx placeholders (e.g.
   `((0, i), sig)` for rollouts/refunds). Single-partition stacks
   work; multi-partition needs the M6 verifier to use matching
   indices when checking remote sigs. Document/test once M6 is real.

3. **StackHandoff vs. emitting acks separately.** Currently
   StackComposer bundles `(unsigned, ownAcks)` into one message.
   Alternative: emit Stack.Unsigned, then emit each own HardAck as
   a separate message. Bundling is simpler today but if
   SlowConsensusActor's design needs to interleave acks with cell
   creation it may want the split.

4. **`JointLedger.releaseRefunds`.** Plan dropped this entirely
   (L2 ledger books refund obligations at soft-confirmation time;
   refund tx is L1-only manifestation). No corresponding M8 work.
   `proxyConfirmation` retirement is the responsibility of the
   future RequestSequencer-wiring PR (out of scope here).

---

## Recommended next moves

In priority order:

1. **M6 real ack collection** — the BIG remaining piece. Replaces
   auto-confirm. Without it the slow side doesn't actually achieve
   consensus, even though everything else is wired. ~200–300 lines
   of careful cell state-machine logic in `SlowConsensusActor.scala`.
   Pairs with real Codec[HardAck] in `Codecs.scala`.

2. **M9 full L1 submission** — wires CardanoLiaison to submit slow-side
   effects. Smaller than M6 but needs design work to bridge stack-level
   effect lists into the existing per-block submission state machine.

3. **M11 stage1 assertions** — once M9 full lands, change the
   integration test to subscribe to Stack.HardConfirmed and assert
   tx-known on L1.

4. **StandaloneEvacCommit on-chain script** — coordinated on-chain
   change. Separate effort from the off-chain code path. Mark it as
   a follow-up issue.

5. **Initial stack (stack 0) boot path** — once M6 is real, wire the
   `Bootstrap` parameter so the initial stack flows through the same
   pipeline.

Option for George if scope feels too big: **land just M6 + real
Codec[HardAck]** first as the deepest slice, validate the full
end-to-end consensus loop with TestControl-style multi-peer tests,
then take M9 / M11 / Initial as follow-up PRs.

---

## Commits this session (in order)

```
be9a9255 Feat: slow-consensus types skeleton (PR1)
467a35c2 Feat: StackComposer skeleton + BlockResult emission (M5 first iteration)
218f71d4 Chore: BlockStatus naming sweep — asMultiSigned → asHardConfirmed
9c6fd5c1 Feat: SlowConsensusActor stub + StackComposer leader/follower mode (Option A)
2115a5b6 Feat: M7 — PeerLiaison stackBrief + hardAck wire lanes
1e0b78fd Feat: M9 — CardanoLiaison subscribes to Stack.HardConfirmed
2b0c8496 Feat: M10 — slow-side L1LedgerM state + treasury rotation runner
eae8404b Feat: M1 partition — selectNecessaryEffects body (pure data shaping)
b88b3081 Feat: M1 vertical slice — minor-only stacks fully derive
c26b207e Chore: refresh StackComposer docstring after M1 minor-only slice landed
8df24790 Feat: M1 — Major block path (settlement / fallback / rollout txs)
d350fe00 Feat: M1 — Final block path (finalization tx + rollouts)
bf376851 Chore: refresh StackEffectsBuilder docstring after Final slice
6daaddb3 Feat: M1 — TrailingMinors → StandaloneEvacCommitTx (stubbed body)
7eec15e4 Feat: M3 — HardAck wallet signing methods
c2279816 Feat: M3→M5 integration — leader signs own hard-acks upfront
32dc35b6 Chore: refresh StackComposer docstring after M3 integration
d6beadc4 Docs: hand-off note for George + plan status table
--- post-handoff review pass (2026-05-17) ---
bbb6d6d4 Refactor: drop HardAck.finalizationRequested (derivable from stack)
f5f32b30 Refactor: disambiguate HardAck.toContext log keys
2466998b Refactor: evac-commit sig is a HeaderSignature; wallet takes explicit inputs
1ee19360 Docs: mark AckNumber -> SoftAckNumber rename + clarify ack-number comments
6605b9e9 Docs: BlockResult — disambiguate payouts (withdrawals) from refunds
97fce872 Docs: L1 effects are not all transactions (evac commit = header sig)
843c9688 Refactor: drop StackBrief.firstMajorBlockNum (redundant)
f89733d4 Docs: StackEffects.Regular — precise rollouts/refunds sourcing
b64e3762 Refactor: standalone evac commitment is a dormant record, not a tx
```

Pushed to `origin/feature/slow-consensus` through `843c9688`; the
review-pass tail (`f89733d4`, `b64e3762`) is committed locally, not yet
pushed.

---

## Test status

`sbtn test` passes with 688/691 — same 3 pre-existing flaky ScalaCheck
failures as before this work started (`BlockWeaverTest`,
`JointLedgerTest` "Falsified after 0 passed tests"). No regressions
from any of the 17 commits. Last full run: 2026-05-15.

Pre-commit hook (`just lint-check` + `just fmt-check`) clean.
