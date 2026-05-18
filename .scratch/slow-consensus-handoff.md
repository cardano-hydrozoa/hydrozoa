# Slow-consensus hand-off — for George

Branch: `feature/slow-consensus`. Origin is current.

Plan reference: `.scratch/slow-consensus-plan.md` (Ilia's design doc, ~700 lines).
Previous summary: `.scratch/consensus-split-summary.md` (covers the fast/slow
split itself, May 12; this doc covers what's been built on top, May 14–15).

---

## TL;DR

Slow consensus is **functionally complete** end-to-end — all actors talk,
effect derivation produces real txs, leader signs own hard-acks upfront,
**`SlowConsensusActor` now does real per-stack ack aggregation +
per-effect signature verification (no more auto-confirm)**, and
hard-confirmed stacks submit their L1 effects via `CardanoLiaison`
(M9-full). Real `Codec[HardAck]` is in place so hard-acks traverse the
wire.

Remaining: M11 real stage1/stage4 effect-presence assertions (now
unblocked), the initial-stack `Bootstrap` boot path, and the future
evac-commit storage/dispute layer.

> **Review checkpoint:** Ilia asked to re-review after M9+M6, before the
> M11/test phase. Commits `3ff4a47b`..`23cecfe4` (this session).

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
  └─ Follower-mode: classify inbound StackBrief 3 ways —
       structural divergence → rule-based fallback (TODO);
       not-yet-covered → wait silently (re-fires on next event);
       covered → build from EXACTLY the brief's range, re-derive, hand off

SlowConsensusActor (M6, REAL)
  ├─ on StackHandoff: derive HardAckSigningPlan; create cell
  │    (WaitingRound1→Round2 | WaitingSole); verify+insert own ack;
  │    broadcast own round-1/sole → PeerLiaisons; replay orphans
  ├─ on remote HardAck: verify every per-effect sig + evac header sig
  │    vs the plan (keyset-exact); aggregate per peer
  ├─ round-1 saturation: release withheld own round-2 → PeerLiaisons
  └─ on saturation (all head peers): emit
         Stack.HardConfirmed             → CardanoLiaison
         PreviousStackHardConfirmation   → StackComposer
     (bad sig / keyset ⇒ raise: divergence → rule-based fallback)

CardanoLiaison (M9-full)
  └─ Stack.HardConfirmed: handleStackL1Effects → runEffects
       (StackEffects.Regular → effectInputs/happyPathEffects/fallbackEffects;
        evac commits + refunds NOT submitted)

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
- `SlowConsensusActor` (M6, real — see dedicated section below) does
  per-stack ack aggregation + verification; emits Stack.HardConfirmed +
  PreviousStackHardConfirmation on saturation.
- Both wired into `MultisigRegimeManager.Connections`; `isSlowLeader(stackNum)`
  on `HeadPeerId`.

### SlowConsensusActor — real ack collection (M6 — `4a2470aa`, `463bea52`, `23cecfe4`)
- `HardAckSigningPlan` (`consensus/ack/`) extracted from
  `StackComposer.buildHandoff`: pure `from(Stack.Unsigned)` →
  `TwoPhase(Round1Regular, Round2Regular)` | `Sole`. **Single source of
  truth** — the composer signs against it; the SlowConsensusActor
  reconstructs the byte-identical keyed message set to verify remote
  sigs. (StackComposer no longer keys effects itself.)
- Per-stackNum `Cell`: `WaitingRound1 → WaitingRound2` (2-phase) /
  `WaitingSole` (minor-only). Saturation = a verified ack from every
  head peer (own included).
- Own round-1/sole broadcast immediately; own round-2 withheld in the
  cell, released on local round-1 confirmation (fast-side "scheduled own
  ack" analogue). Early remote round-2 verified-and-stashed, folded in
  on transition. Per-stackNum orphan buffer for acks arriving before the
  local handoff; replayed (and verified) on cell creation.
- Verification: each `TxSignature` via `verifyEd25519Signature` against
  the locally-derived tx body; evac-commit header sig against the minor
  header bytes; **keyset required to match exactly**. Bad sig / keyset
  ⇒ raise (deterministic-derivation divergence ⇒ halt → rule-based
  fallback per plan). Stricter than the fast side (which discards the
  verify boolean).
- Real `Codec[HardAck]` (`463bea52`): discriminated by `kind`;
  round1Regular / round2Regular / round1Initial / sole full round-trip;
  `round2Initial` an explicit failure (VKeyWitness list; initial-stack
  boot path unwired). Opaque/tuple map keys → `[[k,v],…]` arrays. +5
  CodecsTest round-trips.

### Wire transport (M7 — `2115a5b6`)
- `PeerLiaison`: `stackBrief` + `hardAck` outbox lanes, cursors
  (`stackBriefNum`, `hardAckNum`), verification rules, inbound dispatch
  (stackBrief → StackComposer; hardAck → SlowConsensusActor).
- `GetMsgBatch` / `NewMsgBatch` extended with the new fields; CodecsTest
  updated.
- Outbox prune: per-remote on incoming GetMsgBatch cursors (matches the
  recently reworked soft-side pattern).

### CardanoLiaison L1 submission (M9-full — `1e0b78fd`, `064440c3`, `3ff4a47b`)
- `Request` is `PreStart | Timeout | Stack.HardConfirmed` (vestigial
  pre-split `BlockConfirmed.{Major,Final}` inbound dropped).
- `handleStackL1Effects(StackEffects.Regular)` feeds the stack's effects
  into the pre-split submission state machine
  (`mkHappyPathEffectInputsAndEffects` → effectInputs/happyPathEffects;
  fallbacks keyed by settlement `majorVersionProduced`; targetState =
  Finalized if a Final block, else Active(last settlement treasury), else
  unchanged for minor-only) then `runEffects` immediately — mirrors the
  pre-split `handle*BlockL1Effects >> runEffects`.
- `rolloutsFor` regroups the flat `StackEffects.Regular.rollouts` per
  backbone by walking the rollout-utxo chain (order-independent).
- NOT submitted (parity with pre-split + spec): evac commitments
  (dormant) and post-dated refunds (no pre-split path; fund14).
- `StackEffects.Initial` branch is defensive-only (init/fallback already
  seeded from head config) until the Bootstrap boot path is wired.
- Legacy `handleMajorBlockL1Effects` / `handleFinalBlockL1Effects` kept
  `@unused` purely as reference / for the commented `CardanoLiaisonTest`;
  nothing routes to them post-split (the stack path supersedes them).

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

### M6 real ack collection — DONE (`4a2470aa` → `463bea52` → `23cecfe4`)

`SlowConsensusActor` is a real per-stack aggregation cell SM (no more
auto-confirm) — see "What's done › SlowConsensusActor (M6)" below and the
actor's class doc. Shared `HardAckSigningPlan` (extracted from
StackComposer) is the single source of truth both the signer and the
verifier use.

### Real Codec[HardAck] — DONE (`463bea52`)

Real discriminated `Codec[HardAck]` in `Codecs.scala`: round1Regular /
round2Regular / round1Initial / sole round-trip fully; `round2Initial`
(needs a `VKeyWitness` list) is an explicit encode/decode failure — the
initial-stack boot path is unwired so it never reaches transport. New
hex `Codec[TxSignature]`; opaque/tuple map keys serialize as
`[[k,v],…]` arrays (`entriesCodec`). CodecsTest +5 round-trips.

### M9 full L1 submission — DONE (`3ff4a47b`)

`handleStackL1Effects` translates `StackEffects.Regular` into the
existing submission state machine; see "What's done › CardanoLiaison L1
submission" above. Dependency order is enforced by the existing
`EffectId (major, 0)=backbone, (major,1..n)=rollouts` /
`fallbackEffects.get(major.decrement)` machinery, so no separate
submission-queue builder was needed. Evac commitments + refunds remain
unsubmitted by design (dormant / fund14). Remaining M9-adjacent gap is
only the storage layer for evac-commit header sigs (below).

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

2. **Partition indexing in HardAck payload maps.** Keys are now the
   typed opaque wrappers `PartitionIndex` / `WithinPartitionIndex`
   (`multisig/ledger/effects/PartitionIndex.scala`), not bare `Int`.
   `StackComposer.buildHandoff` still assigns them positionally
   (`PartitionIndex.zero` + list index) — fine for single-partition
   stacks; multi-partition needs the M6 verifier to use matching
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

1. **M11 stage1/stage4 assertions** — M6 + M9-full have landed and
   unblock this. Change the integration tests to subscribe to
   Stack.HardConfirmed and assert tx-known on L1 + exercise both
   1-phase (minor-only sole) and 2-phase paths, round-2-after-round-1,
   multi-partition necessary-effects. (Currently vacuous —
   `List.empty[BlockEffects.Unsigned]` accumulator.)

2. **Initial stack (stack 0) boot path** — wire the `Bootstrap`
   parameter so the initial stack flows through the same pipeline
   (StackComposer + HardAckSigningPlan both throw on Initial today; the
   Round2Initial wire codec is also deliberately unsupported until this).

3. **Storage layer for evac-commitment header signatures** — the slow
   side derives the `StandaloneEvacuationCommitment` records + their
   hard-ack header signatures, but nothing persists them or presents
   them to the L1 dispute scripts. (No on-chain script change is
   needed — an evac commitment is a dormant dispute-only record, not a
   tx.) Future / separate effort.

4. **Follower robustness (explicit `Awaiting*` state).** The follower
   now correctly distinguishes "not yet covered → wait silently" from
   "structural divergence → fallback". This is correct for liveness via
   the existing event-driven retry, but there is no timeout: a genuinely
   stuck follower waits forever. An explicit `Awaiting*` state (à la
   BlockWeaver's `AwaitingConfirmation`) would let it report *what*
   it's blocked on and escalate to the rule-based fallback on timeout.
   Pairs naturally with M6 + the divergence-fallback wiring.

M6 + real Codec[HardAck] + M9-full have landed — the deepest slice
(end-to-end real consensus) is in. Natural follow-up split: M11
(test restoration) next, then Initial-stack Bootstrap + the
evac-commit storage/dispute layer as separate efforts.

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
7d72fed2 Docs: refresh plan status table + handoff note for post-review state
1b58cfa5 Refactor: typed PartitionIndex / WithinPartitionIndex for HardAck maps
064440c3 Refactor: narrow CardanoLiaison.Request to the post-split inbound
--- M9-full + M6 (2026-05-17, session resumed) ---
639f5034 Refactor: move StandaloneEvacuationCommitment out of l1/tx → stack
3ff4a47b Feat: M9-full — CardanoLiaison submits hard-confirmed stack's L1 effects
ab2a3bf2 Docs: M9-full done — refresh plan status table + handoff note
4a2470aa Refactor: extract HardAckSigningPlan (shared signing-input derivation)
463bea52 Feat: M6 — real Codec[HardAck] (Regular / Sole / Round1Initial)
23cecfe4 Feat: M6 — SlowConsensusActor real ack collection (replaces auto-confirm)
```

Pushed to `origin/feature/slow-consensus` through `639f5034`; the tail
(`3ff4a47b`..`23cecfe4` + this doc refresh) is committed locally, not
yet pushed (awaiting the post-M9+M6 review).

---

## Test status

`sbtn test`: 693 passed / 3 failed / 3 ignored — the 3 failures are the
same pre-existing flaky ScalaCheck props as before this work started
(`BlockWeaverTest`, `JointLedgerTest` "Falsified after 0 passed tests").
ScalaTest side 613/613 incl. `CodecsTest` (+5 HardAck round-trips). No
regressions from M9-full or M6. Last full run: 2026-05-17 (post-M6,
~40 min).

Pre-commit hook (`just lint-check` + `just fmt-check`) clean.
