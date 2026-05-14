# Plan: Slow Consensus — Head Peers Only

## Goal

Reintroduce the removed effect-production code as a proper slow cycle, aligned with the whitepaper's slow-consensus article. Head peers only in Phase A — no coil quorum yet, but the two-round hard-ack flow ("unlock" pattern) is built in from day one for stacks containing settlement or finalization, including the **initial stack** (a two-phase, one-block stack with the initialization tx as its unlock). Slow cycle mirrors the fast cycle's leader-broadcast / peers-ack shape, with consumer-side gating on soft-confirmation.

## Terminology

To avoid confusion: **ack** vs **confirmation** are not synonymous.

- **Ack** (soft / hard) = a single peer's signature. Per-peer event. Produced by signers, transported by PeerLiaisons, collected by ConsensusActor (fast) / SlowConsensusActor (slow).
- **Confirmation** (soft / hard) = the aggregated, saturated event. Produced by ConsensusActor (soft) or SlowConsensusActor (hard) once all required acks are collected. Consumed by downstream actors (BlockWeaver, StackActor, CardanoLiaison, JointLedger.releaseRefunds, PeerLiaisons-for-outbox-prune).

**Routing rule.** Briefs (BlockBrief, StackBrief) go from producer **directly to PeerLiaisons**; the consensus actor is not on the brief path. Acks go through the consensus actor (own acks routed through it for outbound scheduling; remote acks delivered through it for aggregation).

Actor flow:
- BlockWeaver ← soft **confirmations** ← ConsensusActor.
- JointLedger → BlockBrief (if leader) → PeerLiaisons (direct broadcast); JointLedger → own soft **ack** → ConsensusActor.
- ConsensusActor ← soft **acks** ← (JointLedger own + PeerLiaisons remote); ConsensusActor → own soft **ack** → PeerLiaisons (broadcast); ConsensusActor → soft **confirmations** → (BlockWeaver + StackActor).
- StackActor ← soft **confirmations** ← ConsensusActor; StackActor ← incoming `StackBrief` ← PeerLiaisons (follower-side, direct); StackActor → `StackBrief` (if leader) → PeerLiaisons (direct broadcast); StackActor → own hard **acks** (all rounds) → SlowConsensusActor.
- SlowConsensusActor ← hard **acks** ← (StackActor own + PeerLiaisons remote); SlowConsensusActor → own hard **acks** (round-1 / sole immediate, round-2 withheld until local round-1 confirmation) → PeerLiaisons; SlowConsensusActor → hard **confirmations** → (CardanoLiaison + JointLedger.releaseRefunds + PeerLiaisons + StackActor's `PreviousStackHardConfirmation`).

**Implications:**
- All hard-ack signing lives in StackActor (which holds the wallet for slow-side signing). Both round-1 and round-2 acks are signed upfront at stack close — round-2 signs over the unlock tx body, which is known at close time, so no dependence on round-1 confirmation for signing.
- SlowConsensusActor is a pure ack aggregator + scheduled-broadcast controller + confirmation emitter — no wallet, no signing. Withholds own round-2 ack until local round-1 confirmation observed, then releases it to PeerLiaisons for broadcast. Mirrors the pre-split ConsensusActor's "scheduled own ack" pattern (formerly used for major-block round-2 acks).

**Codebase naming.** Current codebase: `ConsensusActor` is the fast-side soft-ack collector; `SlowConsensusActor` is the slow-side stub that this work fills in. This document's `SlowConsensusActor` references already match the codebase class name. Whether to rename `ConsensusActor` → `FastConsensusActor` for symmetry is a separate decision, not blocking.

## Design principle: fast/slow isomorphism, gated by soft-confirmation

- **Fast consensus** = soft-ack over `BlockHeader.signingBytes`. Fast leader picks the brief, **broadcasts the brief directly to PeerLiaisons, and signs + hands her own soft-ack to her local ConsensusActor (which broadcasts the ack to PeerLiaisons and aggregates remote acks).** Other peers receive the brief from PeerLiaisons, sign their own soft-ack, and route it through their local ConsensusActor. **Soft-confirmation requires soft-acks from ALL head peers (leader's own included) — not a quorum.** A single missing head-peer ack stalls the brief. Weaker guarantee than slow (no L1 commitment), but lower latency. Useful standalone for clients that don't need L1 commitment.
- **Slow consensus** = hard-ack over per-effect signatures of a closed stack. Slow leader picks the stack composition (cuts), **broadcasts the `StackBrief` directly to PeerLiaisons, and signs + hands her own hard-acks (all rounds upfront) to her local SlowConsensusActor (which broadcasts round-1 / sole acks immediately, withholds round-2 until local round-1 confirmation, and aggregates remote acks).** Other peers receive the brief from PeerLiaisons, validate composition + re-derive effects locally, sign their own hard-acks, and route them through their local SlowConsensusActor. **Hard-confirmation requires hard-acks from ALL head peers (leader's own included) in Phase A; when coil peers join in a later PR, it will additionally require a quorum of coil peers.** Stronger guarantee, higher latency, batched.
- **Symmetry.** Both cycles follow the same pattern: leader fixes the order, peers ack (and the consensus actor aggregates acks into a confirmation). Distributed-system safety needs the leader's broadcast to synchronize the closure decision — peers cannot be assumed to converge independently.
- **Producer-side decoupling.** JointLedger emits `BlockResult` immediately on local block completion, independent of the soft-ack round. The fast cycle proceeds in parallel.
- **Consumer-side gating.** StackActor pairs `BlockResult.N` (data, from JointLedger) with `Block.SoftConfirmed.N` (proof, from ConsensusActor) before treating a block as stackable. A stack cannot close until every constituent block is paired.
- **Leader broadcast.** The slow leader, on receiving the previous-stack hard-confirmation signal (and having at least one new soft-confirmed block to include), builds the stack locally, signs her hard acks, **broadcasts `StackBrief` directly to PeerLiaisons** (composition only, never effects), and hands her hard acks to her local SlowConsensusActor. Followers receive the `StackBrief` from PeerLiaisons, re-derive effects locally, and sign the same `StackBrief`.
- **Leader's soft-confirmation gate — longest contiguous prefix.** The slow leader cannot arbitrarily skip blocks. She picks the **longest contiguous prefix** of newly-soft-confirmed blocks starting at `lastClosedBlockNum + 1`. If blocks 5, 6, 8 are soft-confirmed but 7 is not yet, only [5, 6] is stackable — block 8 must wait until 7 is also soft-confirmed. Stack composition is therefore deterministic given the soft-confirmation prefix length at trigger time.
- **Stack-closure trigger.** Wrap-up of stack N+1 is signaled by hard-confirmation of stack N — mirroring how BlockWeaver wraps block N+1 on the signal of multi-signed block N. Single-flight serialization: only one stack is in slow consensus at a time. The initial block stack (stack 0, containing just the initial block) bootstraps the trigger chain, same way BlockWeaver bootstraps from initial-block confirmation. Phase A also has the special case of "first stack can close after the very first non-initial block."
- Slow side may lag arbitrarily behind fast side. That's the point — amortize L1 cost and (later) coil round-trip.

`JointLedger.proxyConfirmation` will be retired. It was a March-era kludge using the L2 ledger as a routing point for confirmation results; new design writes confirmation results to their proper destinations directly (slow consensus → L1 submission + `releaseRefunds` to L2 ledger).

## Scope

**In:**
- `BlockResult` emitted on JointLedger block completion (independent of `proxyConfirmation`).
- `StackActor` consumes `BlockResult` (from JointLedger) AND `Block.SoftConfirmed` (from ConsensusActor); pairs by blockNum.
- **Stack closure per spec:** leader, on previous-stack hard-confirmation, announces a stack containing all blocks soft-confirmed since the previous close. Stack content (minor-only, single-major, multi-major, final-terminated) emerges from whatever was newly soft-confirmed at trigger time. No "close on Major/Final" simplification.
- Necessary-effects selection per spec: partition stack by major version; in each partition, include all non-standalone-evac-commitment effects + keep only the last standalone evac commitment. Multi-major case fully supported.
- **Hard-ack protocol** with cell phase count determined by stack type (per spec):
  - Stacks containing settlement / finalization: 2 phases.
    - Round 1: per-effect sigs for everything EXCEPT the first settlement/finalization (subsequent settlements in multi-major stacks are signed in round 1).
    - Round 2: per-effect sig for the first settlement/finalization (the unlock).
  - Minor-only stacks: 1 phase, sole ack = sig over the last evac commitment.
- `HardAck` carries multiple per-effect sigs (not a single sig over an effects digest).
- Slow leader announces `StackBrief` (only after previous stack is hard-confirmed, per spec).
- `SlowConsensusActor` cell phase count depends on stack type (per spec): 2 phases for stacks containing settlement / finalization, 1 phase for minor-only stacks. Both paths are exercised in Phase A (minor-only stacks arise naturally when no major has been soft-confirmed since the previous close).
- PeerLiaison: `stackBrief` + `hardAck` lanes (only the brief is wire-broadcast; effects never on the wire).
- `CardanoLiaison` receives `Stack.HardConfirmed` → submits L1 effect txs in dependency order.
- L2-release path replacing `proxyConfirmation`: `SlowConsensusActor` → `JointLedger.releaseRefunds`.
- `L1LedgerM` treasury rotation + `finalizeLedger` re-enabled.
- Stage1 / Stage4 effect-presence assertions restored.
- **Multi-block stacks from day one** (Major / Final-boundary policy).
- **Initial block handling** — removes pre-built effects from the config's initial block; stack 0 runs through slow consensus on a specialized `StackEffects.Initial` path (init tx from head config + locally-derived fallback tx). Head peer 0 leads stack 0 naturally via round-robin.

- **Independent slow-leader round-robin per stack** — leader of stack N = round-robin(stackNum). Reuse the same round-robin code that the fast cycle uses (peer-index modulo nPeers, or whatever the existing implementation is); just key on `stackNum` instead of `blockNum`.
- **`BlockStatus` naming sweep** — `asMultiSigned` → `asHardConfirmed` (and any related callsites). Aligns the type name with the fast/slow split terminology.

**Out (later PRs):**
- Coil peers + coil-quorum sigs at both rounds + coil-side stack-brief verification.
- Full retirement of `proxyConfirmation` (we add the new L2-release path; old proxy may stay around briefly until callers migrate).

## Stack closure policy (per spec)

Slow leader announces a new stack when **both** conditions hold:
1. The previous stack has been hard-confirmed — signaled by `SlowConsensusActor` → `StackActor`. (Single-flight serialization: only one stack in slow consensus at a time.)
2. At least one new block (beyond the previous stack's last block) is soft-confirmed.

The new stack contains **all** blocks soft-confirmed since the previous close. Stack content emerges from accumulation, not from a "close on Major/Final" rule:
- Single soft-confirmed minor → 1-block minor-only stack (1 round).
- N minors accumulated → N-block minor-only stack (1 round, sole ack = sig over last evac commitment).
- Mix of minors + 1 Major → 2-round stack.
- Mix with multiple Majors → 2-round multi-partition stack (necessary-effects selection across partitions; round 2 unlock is only the FIRST settlement; subsequent settlements signed in round 1).
- Final block at the end → 2-round stack (round-2 unlock is finalization).

Mirrors BlockWeaver's wrap-up rule (wait for confirmation of the previous unit, then act on whatever's accumulated since).

**Bootstrap.** Stack 0 (the initial block stack) is NOT pre-confirmed — it runs through slow consensus on its own. See "Initial block handling (boot path)" below for the full flow.

At boot: `lastClosedStackNum = None`, `previousStackHardConfirmed = true` (synthetic — there is no real previous stack to wait on, so stack 0 may close immediately). Stack 0's hard-confirmation provides the `PreviousStackHardConfirmed` signal that unblocks stack 1; from then on, stack N+1 is gated on hard-confirmation of stack N normally.

**Slow leader for stack N** in Phase A: independent round-robin keyed on `stackNum`. Reuses the same round-robin function the fast cycle uses (`isLeader(peerIdx, n) := n % nPeers == peerIdx` or equivalent), just keyed on `stackNum` rather than `blockNum`. Slow and fast leadership are therefore independent — different stack and block sequences advance at different paces, so the same peer index modulo nPeers maps to different roles per cycle.

Implications:
- Stack content type drives cell phase count: 1 phase for minor-only stacks, 2 phases otherwise.
- Necessary-effects selection is non-trivial in the multi-major case — implementation must handle the partition algorithm general-form, not just the single-partition shortcut.
- Evac-commitment compression: per partition, sign only the last evac commitment (drop superseded ones). For minor-only stacks, that means one evac-commitment sig regardless of stack length.

## Necessary-effects selection (per spec)

Algorithm (applies generally, including future multi-major stacks):
1. Partition the stack's blocks by major version.
2. In each partition:
   - Include all effects that are NOT standalone evacuation commitments.
   - Exclude all standalone evac commitments except the last one (if any).

Multi-partition example (per spec): stack of 100 minor + Major.A + 20 minor + Major.B + 50 minor:
- Partition 1: 100 minor + Major.A → last evac commit + Major.A's settlement / fallback / rollouts / refunds + 100 minor blocks' refunds.
- Partition 2: 20 minor + Major.B → last evac commit + Major.B's settlement / fallback / rollouts / refunds + 20 minor blocks' refunds.
- Partition 3 (trailing minors): last evac commit + 50 minor blocks' refunds.

**Minor block refunds.** Confirmed: per spec, refund effects from minor blocks are NOT compressed away — only their evac commitments are. Each minor block's refund txs (if any) get their own sigs in round 1. This means the necessary-effects set is potentially large in stacks with many minors (one refund-tx sig per minor that absorbed a deposit), even though the evac-commitment-sig count compresses to one per partition.

## Initial block handling (boot path)

Stack 0 is **structurally a 2-phase slow-consensus stack** (per spec: "Initial block consensus is a regular instance of the slow consensus cycle"), but with a few boot-specific twists. The current code's pre-built effects on the initial block are removed; only the initialization tx body lives in head config.

**Effects content (specialized type `StackEffects.Initial`):**
- **Initialization tx** — exogenous, lives in `headConfig.initTx`. May carry pre-existing operator-supplied witnesses (e.g. signatures for non-consensus inputs the operator funded). Not built by JointLedger or StackActor; just embedded.
- **Fallback tx** — derived locally by each peer from `initTx + headParams + treasury/regime utxos`. Deterministic across peers → matching sigs in round 1.

**Round mapping (same 2-phase structure as a major stack):**
- Round 1: sig over the fallback tx body.
- Round 2: sig over the initialization tx body + each head peer's individual key witnesses for utxos spent from their individual addresses (operator-supplied funding). Coil peers (future) only contribute the multisig signature in round 2; only head peers contribute individual witnesses.

**Leader:** head peer 0, naturally from `roundRobin(0, nPeers) = 0`. No special leadership case.

**Fast-consensus round 0?** **No.** Per spec, only slow consensus runs for the initial block. The brief itself is fully determined by head config — every peer derives the same `BlockHeader.Initial` from the same config, so there's no soft-ack round to run. StackActor receives a synthetic `Block.SoftConfirmed.0` at boot.

**Bootstrap injection:** StackActor takes a `Bootstrap` constructor parameter:
```scala
case class Bootstrap(
    initialBrief: BlockHeader.Initial,        // derived from head config
    initialBlockResult: BlockResult,          // empty body → empty evacuation map diff, no payout obligations
    syntheticSoftConfirmed0: Block.SoftConfirmed.Initial,  // bypass fast consensus for stack 0
    initializationTx: InitializationTx,       // pulled from headConfig.initTx
)
```
Processed during `PreStart` — the actor injects these into its `pending` / `ready` queues and immediately triggers stack-0 closure (since `previousStackHardConfirmed = true` synthetically).

**StackEffects derivation for stack 0:**
- Bypass `selectNecessaryEffects` / general derivation logic.
- Construct `StackEffects.Initial(initTx, fallbackTx)` directly, where `fallbackTx = deriveFallbackTx(initTx, headParams)` (new pure function from M1).

**M1 addition:** `deriveFallbackTx(initTx: InitializationTx, headParams: HeadParams): FallbackTx`. Reads treasury/regime output indexes from initTx metadata, builds the fallback tx, sets validity start time per timing rules.

**Code migration:** `InitialBlock.scala` no longer carries pre-derived effects. The config's initial block is just the brief + the initialization tx body. All prior call-sites that read effects from the config-resident initial block need updating to pull effects from the slow-consensus-confirmed stack 0 instead.

**Hard-confirmation of stack 0:**
- L1 submission: init tx (with full multisig + per-head-peer individual witnesses + any operator pre-existing witnesses) followed by no further tx (fallback is not submitted until/unless dead-man's switch fires).
- StackActor receives `PreviousStackHardConfirmed(stackNum=0)` → unblocks stack 1.
- Head begins normal fast consensus for block 1.

## Hard-ack rounds (per spec)

**Setup (not a round).** Slow leader broadcasts `StackBrief` (the composition cut: stackNum, firstBlockNum, lastBlockNum, optionally firstMajorBlockNum) directly to PeerLiaisons. Followers receive it on their PeerLiaison and route it to their StackActor for validation.

**Round 1 — first hard ack (all-but-unlock).**
- Each peer signs every necessary effect EXCEPT the first settlement/finalization at stack close (StackActor produces all own acks upfront — see Terminology section). Per-effect sigs.
- Content per partition:
  - Sig over the partition's last evac commit (if non-empty).
  - Sigs over each refund tx in the partition (including refunds from minor blocks).
  - For partitions ending in a Major: sigs over fallback + each rollout + the Major's refunds + the Major's settlement (UNLESS it's the first settlement / finalization — that one's withheld for round 2).
  - For the partition ending in a Final: sigs over each rollout (and Final's refunds if any).
- The first settlement OR finalization tx in the stack is the only one withheld for round 2. In a multi-major stack, second and subsequent settlements are signed in round 1.
- Round-1 ack travels: StackActor → SlowConsensusActor → PeerLiaisons → remote peers' SlowConsensusActor.
- Round-1 confirmation reached locally when all head peers' round-1 acks have been received and every per-effect sig verifies against the locally-derived effect bodies.

**Round 2 — second hard ack (the unlock).**
- Each peer signs the first settlement (Major) or finalization (Final) at stack close (upfront, alongside round-1 ack — round-2 sig domain is just the unlock tx body, fully known then).
- The own round-2 ack is held by the local SlowConsensusActor until that actor observes round-1 confirmation locally; only then is the ack released to PeerLiaisons for broadcast. Mirrors the pre-split ConsensusActor's "scheduled own ack" pattern.
- Round-2 confirmation reached when all head peers' round-2 acks have been received and verified.

**L1 submission.** After round-2 confirmation. The full effect-tx set is now multisigned and submittable on L1 in dependency order (settlement / finalization first, then dependent rollouts / fallback / refunds).

**Why withhold settlement.** Settlement is the L1 entry point — all subsequent dependent txs spend its outputs. If a peer withholds round-2 sig, the other effects are signed but unactionable on L1 (no settlement to spend from). If round 1 stalls, no commitment was made at all. Atomicity preserved either way.

**Minor-only-stack path:**
- Sole ack: sig over last evac commitment from the stack's single partition + per-tx sigs over each minor block's refund txs.
- One round; on sole-round confirmation the stack is hard-confirmed. L1 submission of refund txs (the evac commitment itself is not a standalone L1 tx — it's a value carried into the next major's settlement / used in dispute resolution).

## Stack data types (sketch)

```scala
// Per-block local data — pushed by JointLedger on local block completion.
// Contains everything needed to derive this block's contribution to L1 effects:
//   - evacuationMapDiff: drives the block's standalone evac commitment (KZG commitment)
//     and feeds into the next major's settlement.
//   - payoutObligations: drive the block's refund txs (for minors absorbing deposits)
//     and the block's rollout txs (for any user requests producing immediate L1 payouts).
// Together, BlockResults from all blocks in a stack are sufficient to derive every
// necessary L1 effect for that stack — no JointLedger state lookup at slow-side
// derivation time.
case class BlockResult(
    brief: BlockBrief,                     // Minor | Major | Final
    evacuationMapDiff: EvacuationMapDiff,  // adds/removes vs previous block
    payoutObligations: List[PayoutObligation],  // refund + rollout obligations
)

// Slow leader's stack-composition announcement. Wire payload.
case class StackBrief(
    stackNum: StackNumber,
    firstBlockNum: BlockNumber,
    lastBlockNum: BlockNumber,                  // = closing Major / Final block
    firstMajorBlockNum: Option[BlockNumber],    // optional per spec
)

// Necessary effects after partition-by-major and evac-commitment-compression.
// Built locally by each peer; never wire-broadcast (effects can be large).
// Specialized variant for the initial stack — the only stack whose unlock tx
// is exogenous (provided in head config rather than derived).
sealed trait StackEffects
object StackEffects:
    // Stack 1+. Regular slow-consensus flow.
    case class Regular(
        settlements:   List[SettlementTx],     // 0..N (one per major-partition); FIRST is the unlock
        fallbacks:     List[FallbackTx],       // 0..N (one per major)
        rollouts:      List[RolloutTx],
        refunds:       List[RefundTx],
        evacCommits:   List[StandaloneEvacCommitTx],  // ≤ one per partition (the last)
        finalization:  Option[FinalizationTx], // if stack contains Final block; the unlock when no settlement precedes it
    ) extends StackEffects

    // Stack 0 only. Initialization tx is exogenous (head config).
    // Round 1 signs fallback. Round 2 signs initTx + optional individual witnesses.
    case class Initial(
        initializationTx: InitializationTx,    // from head config (may carry pre-existing operator witnesses)
        fallbackTx:       FallbackTx,          // derived locally from initTx + head params
    ) extends StackEffects

// Round-1 ack: sigs for everything except the first settlement/finalization.
case class HardAckRound1(
    stackNum: StackNumber,
    settlement: Map[Int /* partition index */, EffectSig], // skip partition 0 if it contains the first settlement
    fallback: Map[Int, EffectSig],
    rollouts: Map[(Int, Int) /* partition, index */, EffectSig],
    refunds: Map[(Int, Int), EffectSig],
    evacCommits: Map[Int, EffectSig],     // last evac commit per partition with minors
    finalization: Map[Int, EffectSig],    // skip partition 0 if it contains the first finalization
)
// For Phase A (one partition with the major/final): the maps simplify dramatically —
// either `settlement` or `finalization` is missing from round 1 entirely.

case class HardAckRound2(
    stackNum: StackNumber,
    firstSettlementOrFinalizationSig: EffectSig,
)

case class HardAck(
    ackId: AckId,
    stackNum: StackNumber,
    payload: HardAckRound1 | HardAckRound2,    // round discriminated by payload type
    finalizationRequested: Bool,                // mirrors SoftAck.finalizationRequested (per-peer flag, NOT a confirmation)
)

sealed trait Stack
object Stack:
    case class Unsigned(
        brief: StackBrief,
        results: NonEmptyList[BlockResult],
        softConfirmations: NonEmptyList[Block.SoftConfirmed],
        effects: StackEffects,
    )
    case class Round1Confirmed(
        unsigned: Unsigned,
        round1Acks: NonEmptyList[HardAckRound1],
    )
    case class HardConfirmed(
        round1: Round1Confirmed,
        round2Acks: List[HardAckRound2],   // empty for minor-only stacks (no second round)
    )
```

`HardAckRound1`'s shape is intentionally over-general for future multi-major support. Phase A code paths will have a single partition and skip half the fields.

## Data flow

```
JointLedger.completeBlockRegular / completeBlockFinal
  ├─ produce BlockBrief + own soft ack               → ConsensusActor (fast: aggregates soft acks)
  └─ emit BlockResult.N (NEW)                        → StackActor

ConsensusActor (fast cycle)
  └─ on soft-confirmation: Block.SoftConfirmed.N     → JointLedger.proxyConfirmation (legacy, retired later)
                                                      → StackActor (gate signal)

StackActor (leader role — peer where isSlowLeader(stackNum) holds)
  ├─ pair (BlockResult.N, Block.SoftConfirmed.N) by blockNum
  └─ when previous-stack hard-confirmation received AND ready queue's longest contiguous prefix non-empty:
       ├─ close stack: drain ready prefix (any combination of minors / majors / final)
       ├─ build StackBrief
       ├─ derive necessary effects (or use StackEffects.Initial for stack 0)
       ├─ build Stack.Unsigned
       ├─ sign own round-1 + round-2 hard acks upfront (or sole hard ack for minor-only)
       └─ StackBrief + Stack.Unsigned + all own hard acks  → SlowConsensusActor
                                                            (which broadcasts StackBrief, schedules ack broadcast)

StackActor (follower role)
  ├─ pair streams
  ├─ wait for SlowConsensusActor to forward leader's StackBrief
  └─ when leader's StackBrief arrives AND all constituent blockNums locally paired:
       ├─ validate composition; re-derive necessary effects locally
       ├─ sign own round-1 + round-2 hard acks upfront (or sole)
       └─ Stack.Unsigned + all own hard acks         → SlowConsensusActor

SlowConsensusActor (per-stack cell, pure ack aggregator + scheduled broadcaster; sole slow-side actor talking to PeerLiaisons)
  ├─ on (StackBrief if leader, Stack.Unsigned, all own acks) from StackActor:
  │    ├─ create cell
  │    ├─ deposit own round-1 / sole ack into cell
  │    ├─ if leader: broadcast StackBrief           → PeerLiaisons (immediately)
  │    ├─ broadcast own round-1 / sole ack          → PeerLiaisons (immediately)
  │    └─ stash own round-2 ack in cell (withheld until local round-1 confirmation)
  ├─ on incoming StackBrief from PeerLiaison (follower-side): forward → StackActor (so follower can validate + sign)
  ├─ Phase 1: collect round-1 hard acks from PeerLiaisons (other peers); verify per-effect sigs
  ├─ on local round-1 confirmation:
  │    └─ release stashed own round-2 ack            → PeerLiaisons (broadcast to other peers)
  ├─ Phase 2: collect round-2 hard acks from PeerLiaisons; verify per-variant payload
  └─ on local round-2 confirmation:
       ├─ Stack.HardConfirmed (with both ack sets)   → CardanoLiaison (L1 submit, dependency order)
       ├─                                            → JointLedger.releaseRefunds (L2 update; replaces proxyConfirmation)
       ├─                                            → PeerLiaisons (purge hard-ack outbox + stack-brief outbox)
       └─                                            → StackActor (PreviousStackHardConfirmation signal)
```

Note `Stack.Unsigned` lives only inside each peer's StackActor — it's not wire-broadcast. Only `StackBrief` + `HardAck`s travel.

---

## Milestone breakdown

### M1: Effect derivation as pure functions (additive, no wiring)

Move the deleted effect-production code back as **pure functions**, restructured for the necessary-effects selection algorithm:

```scala
// new module, e.g. multisig/ledger/effects/StackEffects.scala
def selectNecessaryEffects(
    results: NonEmptyList[BlockResult],
): List[Partition]              // partition by major version

def deriveStackEffects(
    partitions: List[Partition],
): StackEffects                  // per-partition: drop superseded evac commits; build txs
```

`StackEffects` outputs match pre-split types (`SettlementTx`, `FallbackTx`, `RolloutTx`, `RefundTx`, `StandaloneEvacCommitTx`, `FinalizationTx`).

No callers yet. Just shape + compile.

**Files:** new `StackEffects.scala`, `NecessaryEffectsPolicy.scala`; recover deleted `mkBlockEffectsIntermediate` body from git as basis.

---

### M2: `BlockResult` type + JointLedger outbound emission

```scala
case class BlockResult(
    brief: BlockBrief,
    evacuationMapDiff: EvacuationMapDiff,       // L2 state delta for KZG commit derivation
    payoutObligations: List[PayoutObligation],  // refund + rollout obligations to be realized on L1
)
```

`BlockResult` carries exactly what the slow side needs to derive every L1 effect for a stack — no JointLedger state-lookup at slow-side derivation time. Composing `BlockResult`s across a stack's blocks is enough to:
- compute the per-partition last evac commitment (KZG hash over cumulative evac-map diffs in the partition)
- build each block's refund txs from payout obligations
- build each major's settlement tx from accumulated cumulative-evac-map + treasury rotation (treasury chain is tracked separately across stacks)
- build each major's fallback tx from settlement output + head params
- build each block's rollout txs from payout obligations

JointLedger gains an outbound to StackActor. Emits `BlockResult.N` at the end of `completeBlockRegular` / `completeBlockFinal`. Independent of `proxyConfirmation` — fires on local block completion, not on soft-confirmation.

**Files:** `BlockResult.scala`, `EvacuationMapDiff.scala`, `PayoutObligation.scala`, `JointLedger.scala`, `MultisigRegimeManager.scala` (actor wiring).

---

### M3: `HardAck` family + signing

```scala
enum HardAckRound: case Round1, Round2, Sole

// Round 1 payload varies by stack-effects variant.
sealed trait HardAckRound1Payload
case class Round1Regular(perEffectSigs: PerEffectSigMap) extends HardAckRound1Payload
case class Round1Initial(fallbackSig: EffectSig)         extends HardAckRound1Payload  // stack 0

sealed trait HardAckRound2Payload
case class Round2Regular(firstUnlockSig: EffectSig)      extends HardAckRound2Payload  // first settlement or finalization
case class Round2Initial(                                                              // stack 0
    initTxSig: EffectSig,
    individualWitnesses: List[KeyWitness],   // for utxos spent from this peer's individual address
) extends HardAckRound2Payload

// 1-phase path: minor-only stack
case class HardAckSolePayload(perEffectSigs: PerEffectSigMap)

case class HardAck(
    ackId: AckId,
    stackNum: StackNumber,
    payload: HardAckRound1Payload | HardAckRound2Payload | HardAckSolePayload,
    finalizationRequested: Bool,    // mirrors SoftAck.finalizationRequested — per-peer request flag, not a confirmation
)
```

Wallet API (called exclusively by StackActor — SlowConsensusActor is wallet-free):
- `HeadPeerWallet.mkHardAckRound1Regular(stack: Stack.Unsigned): HardAck` — signs each non-unlock effect tx body individually.
- `HeadPeerWallet.mkHardAckRound1Initial(fallbackTx): HardAck` — signs the fallback tx body for stack 0.
- `HeadPeerWallet.mkHardAckRound2Regular(stack: Stack.Round1Confirmed): HardAck` — signs the first settlement / finalization tx body.
- `HeadPeerWallet.mkHardAckRound2Initial(stack: Stack.Round1Confirmed): HardAck` — signs init tx body + adds individual key witnesses for spent utxos under this peer's individual address.
- `HeadPeerWallet.mkHardAckSole(stack: Stack.Unsigned): HardAck` — signs each effect in a minor-only stack.

Codec + CodecsTest. Wire ordering: round-1 acks precede the same peer's round-2 ack for a given stack.

**Files:** new `consensus/ack/HardAck.scala`; `HeadPeerWallet.scala`; `Codecs.scala` + `CodecsTest.scala`.

---

### M4: `Stack.Unsigned` / `Stack.Round1Confirmed` / `Stack.HardConfirmed` family

See "Stack data types" sketch above. `StackBrief` follows the spec shape `(stackNum, firstBlockNum, lastBlockNum, firstMajorBlockNum?)`.

**Files:** new `multisig/ledger/stack/Stack.scala`, `StackBrief.scala`.

---

### M5: `StackActor` (replaces stub) — leader / follower split

Inputs:
- `BlockResult.N` stream from JointLedger (gap-free).
- `Block.SoftConfirmed.N` stream from ConsensusActor (gap-free).
- `IncomingStackBrief` from PeerLiaison (follower path).
- `PreviousStackHardConfirmed(stackNum)` from SlowConsensusActor — gate for the next stack closure per spec.

Constructor parameter (boot-only):
- `Bootstrap(initialBrief, initialBlockResult, syntheticSoftConfirmed0, initializationTx)` — see "Initial block handling (boot path)". Processed during PreStart: injects synthetic stack-0 inputs and immediately triggers stack 0 closure (with `previousStackHardConfirmed = true` synthetically). After PreStart, StackActor proceeds normally.

**Leadership:** `isSlowLeader(stackNum) := roundRobin(stackNum, nPeers)`. Reuses the same round-robin function the fast cycle uses, keyed on `stackNum`. Slow and fast leaderships advance independently.

State:
```scala
case class StackActorState(
    pending: Map[BlockNumber, PendingBlock],
    ready: Queue[StackInput],
    inboundLeaderBrief: Map[StackNumber, StackBrief],
    lastClosedStackNum: StackNumber,           // initialized to 0 (initial block stack)
    previousStackHardConfirmed: Boolean,       // initialized to true (initial stack at boot)
)
```

Pairing (both roles): on `BlockResult.N` or `Block.SoftConfirmed.N`, update `pending[N]`; if both halves present, move to `ready` queue.

Leader path (only when `isSlowLeader(lastClosedStackNum + 1)` holds for this peer):
1. After every pairing update OR `PreviousStackHardConfirmation`, check eligibility:
   - `previousStackHardConfirmed == true`
   - `ready` queue's longest contiguous-from-`lastClosedBlockNum + 1` prefix is non-empty (at least one new soft-confirmed block; gap-free per the soft-confirmation gate)
2. If eligible: close a stack containing **all** of that contiguous prefix from `ready` (any combination of minors / majors / final).
3. Close-stack:
   - drain the contiguous prefix from `ready`
   - build `StackBrief` from drained blocks
   - if `stackNum == 0` (boot): build `StackEffects.Initial(initTx, deriveFallbackTx(initTx, headParams))` from bootstrap data; skip the necessary-effects partition algorithm
   - else: run necessary-effects selection (M1) → `StackEffects.Regular(...)`
   - build `Stack.Unsigned`
   - cell-phase decision based on stack-effects variant:
     - `StackEffects.Initial` → 2-phase (round 1 = fallback ack; round 2 = init tx ack + individual witnesses)
     - `StackEffects.Regular` with settlement / finalization → 2-phase
     - `StackEffects.Regular` minor-only → 1-phase
   - sign all own hard acks upfront via the appropriate wallet methods:
     - 2-phase: own round-1 ack + own round-2 ack
     - 1-phase: own sole ack
   - send `StackBrief + Stack.Unsigned + own hard acks (all rounds)` → SlowConsensusActor (which broadcasts StackBrief + round-1/sole ack immediately, withholds round-2 until local round-1 confirmation)
   - `previousStackHardConfirmed = false` until the next `PreviousStackHardConfirmation` event

StackActor signs round-2 upfront because the round-2 sig domain is just the unlock tx body, which is fully known at stack close. No dependence on round-1 ack set.

Follower path:
1. On `IncomingStackBrief` from PeerLiaison: insert into `inboundLeaderBrief`, then try-process.
2. On any state change: look for `inboundLeaderBrief[lastClosed+1]` with all constituent blocks paired.
3. On match: validate composition, re-derive effects, check against leader's brief, sign all own hard acks upfront (round-1 + round-2 OR sole), send `Stack.Unsigned + all own hard acks` → SlowConsensusActor (which schedules outbound broadcast).
4. On mismatch: log + stall (open question).

**Files:** `StackActor.scala` (replace stub).

---

### M6: `SlowConsensusActor` (replaces stub) — pure ack aggregator + scheduled broadcaster

> Codebase naming: keeps the existing `SlowConsensusActor` class name. Fast-side `ConsensusActor` may be renamed to `FastConsensusActor` for symmetry, but that's a separate decision.

**No wallet, no signing.** All hard-ack signing lives in StackActor. SlowConsensusActor's responsibilities:
1. Aggregate hard acks (from StackActor + PeerLiaisons), verify against locally-derived effect bodies.
2. Schedule outbound broadcast of own acks: round-1 / sole released immediately on receipt from StackActor; round-2 withheld until local round-1 confirmation (mirrors pre-split ConsensusActor's "scheduled own ack" pattern for major rounds).
3. Emit hard confirmations on saturation.

```scala
sealed trait SlowConsensusCell
// 2-phase path: stack contains settlement/finalization OR initial stack
case class WaitingForRound1(
    unsigned: Stack.Unsigned,
    round1Acks: Map[HeadPeerId, HardAckRound1Payload],
    ownRound2AckHeld: HardAckRound2Payload,   // held until local round-1 confirmation
)
case class WaitingForRound2(
    round1: Stack.Round1Confirmed,
    round2Acks: Map[HeadPeerId, HardAckRound2Payload],
)
// 1-phase path: minor-only stack
case class WaitingForSole(
    unsigned: Stack.Unsigned,
    soleAcks: Map[HeadPeerId, HardAckSolePayload],
)
```

Transitions (2-phase path; works for both `StackEffects.Regular` w/ settlement/finalization and `StackEffects.Initial`):
- On `(StackBrief if leader, Stack.Unsigned, ownRound1, ownRound2)` from StackActor:
  - create cell `WaitingForRound1(unsigned, {self → ownRound1}, ownRound2AckHeld = ownRound2)`
  - if leader: broadcast `StackBrief` → PeerLiaisons (immediately)
  - broadcast `ownRound1` → PeerLiaisons (immediately)
- On incoming round-1 hard ack from PeerLiaison: insert; verify per-effect sigs. If round-1 confirmation reached:
  - release `ownRound2AckHeld` → PeerLiaisons (broadcast)
  - transition to `WaitingForRound2({self → ownRound2AckHeld})`
- On incoming round-2 hard ack from PeerLiaison: insert; verify per-variant payload. If round-2 confirmation reached:
  - build `Stack.HardConfirmed`
  - emit hard confirmation:
    - `Stack.HardConfirmed` → CardanoLiaison (L1 submit, dependency order)
    - `Stack.HardConfirmed` → JointLedger.releaseRefunds (L2 update)
    - `Stack.HardConfirmed` → PeerLiaisons (purge hard-ack + stack-brief outbox)
    - `PreviousStackHardConfirmation(stackNum)` → StackActor (unblocks next stack)
  - drop cell

Transitions (1-phase path):
- On `(Stack.Unsigned, ownSole)` from StackActor:
  - create cell `WaitingForSole(unsigned, {self → ownSole})`
  - broadcast `ownSole` → PeerLiaisons (immediately)
- On incoming sole hard ack: insert; verify last-evac-commit sig + minor-refund sigs. If sole-round confirmation reached:
  - build `Stack.HardConfirmed` (no round-2 acks)
  - same emission set as 2-phase confirmation, except CardanoLiaison submission only if any refund txs

Cell-phase choice is determined by `Stack.Unsigned`'s effects variant at hand-off time from StackActor.

Postponed-ack semantics: stash early acks for stackNum N+1 on cell N if cell N has not reached confirmation yet (same as fast-side). Applies to both rounds and to the held own round-2 ack — if a remote peer's round-2 ack arrives before local round-1 confirmation, stash it for transition into `WaitingForRound2`'s `round2Acks` map.

**Files:** `SlowConsensusActor.scala` (replace stub).

---

### M7: PeerLiaison extension

Add `stackBrief` and `hardAck` lanes, **independent** of the existing fast-side lanes:

```scala
case class GetMsgBatch(
    batchNum, ackNum, blockNum, requestNum,
    hardAckNum: HardAckNumber,        // NEW
    stackBriefNum: StackNumber,       // NEW — leader's stack proposals cursor
)
case class NewMsgBatch(
    ackNum, blockBrief, requests,
    hardAck: Option[HardAck],         // NEW — round1 or round2 (payload variant disambiguates)
    stackBrief: Option[StackBrief],   // NEW — leader's stack-closure proposal
)
```

Round ordering on wire: for each stack, the same peer's round-1 ack always precedes its round-2 ack. `hardAckNum` is monotonic per peer over (stackNum, round) pairs. Followers' `stackBriefs` queue stays empty for stacks they don't lead.

Outbox extended: per-remote `hardAcks` queue + `stackBriefs` queue, pruned on remote's advanced cursors.

**Files:** `PeerLiaison.scala`, `Codecs.scala`.

---

### M8: L2-release path replacing `proxyConfirmation`

New method on JointLedger: `releaseRefunds(stack: Stack.HardConfirmed): IO[Unit]`. Extracts refund tx CBORs (across all partitions of the stack) and forwards them to the L2 ledger. Same destination as old `proxyConfirmation`, different source.

`proxyConfirmation` itself stays in place for now (fast-side ConsensusActor calls it with empty refund list). Retirement is a separate cleanup PR after slow side is live.

**Files:** `JointLedger.scala`.

---

### M9: CardanoLiaison wiring

Subscribe to `Stack.HardConfirmed`. Submit effect txs to L1 in dependency order: settlement / finalization first (the unlock), then fallback, rollouts, refunds, evac commitments.

**Files:** `CardanoLiaison.scala`, actor-graph wiring in `MultisigRegimeManager.scala`.

---

### M10: L1LedgerM rotation

Re-enable treasury utxo rotation on settlement (Major stacks). On Final stack hard-confirmation, call `finalizeLedger`.

**Files:** `L1LedgerM.scala`, `JointLedger.scala`.

---

### M11: Test restoration

- Stage1: effect-presence assertion restored — pulls effects from `Stack.HardConfirmed` events, not from the vacuous fast-side pass-through.
- Stage4: add `StackObserver` parallel to `BlockBriefObserver`, watching `SlowConsensusActor`'s request channel. Verify: stack count, both 1-phase (minor-only) and 2-phase (settlement/finalization-containing) paths are exercised, round-2 always follows round-1 confirmation in 2-phase cells, multi-partition necessary-effects selection produces correct sig sets.

**Files:** `integration/.../stage1/Sut.scala`, `integration/.../stage4/Sut.scala`.

---

## Test plan

- Stage4 Direct under TestControl: existing soft-confirmation continues; **stacks are produced from accumulated soft-confirmed blocks per spec policy**. Both 1-phase (minor-only) and 2-phase (settlement / finalization-containing) cells are exercised. Settlement utxo rotates. Stage1 effect assertions pass.
- WS variant: same. Real L1 submissions resume via CardanoLiaison.
- Property: slow side lags fast side by ≥ 0 blocks, always catches up before stage4 shutdown idle.
- Property: round-2 ack count == round-1 ack count per 2-phase stack; sole-ack count == n-peers per 1-phase stack.

---

## Open questions

1. **Leader broadcast payload: `StackBrief` only.** Decided — effects can be large (settlement + fallback + rollouts + refunds + evac commits = many tx CBORs). Effects never wired. Followers always re-derive locally.

2. **Mempool eviction.** StackActor drops `pending` / `ready` entries once their stack is handed off to SlowConsensusActor. Simple, no back-edge needed.

3. **~~Effect-derivation divergence between peers~~** — not an open concern. Deterministic effect derivation is a core Hydrozoa property: given the same `BlockResult`s + `StackBrief`, every head peer must derive byte-identical effects. Divergence is a critical bug that breaks consensus by design — there's no "graceful" handling for head peers; slow consensus halts, fallback timer fires, and the system falls back into the rule-based regime (dispute-resolution / dead-man's switch). (Coil peers are a different story for a future PR.) Standard observability hygiene applies — log mismatched sig verifications with peer ID + tx body — but this is normal debugging infrastructure, not a special design concern.

4. **`proxyConfirmation` retirement timeline.** Phase A adds `releaseRefunds`. ConsensusActor still calls `proxyConfirmation` with empty refund list. Cut after slow side live — follow-up cleanup, not blocking.

5. **~~Where round-2 signing lives~~** — resolved: StackActor signs all hard acks (round-1 + round-2 OR sole) upfront at stack close, and hands them all to SlowConsensusActor. SlowConsensusActor is a pure ack aggregator with no wallet — it manages outbound broadcast scheduling: round-1 / sole released immediately to PeerLiaisons, round-2 withheld until local round-1 confirmation, then released. Mirrors the pre-split ConsensusActor's "scheduled own ack" pattern. Symmetric with fast consensus: JointLedger signs all soft acks; ConsensusActor handles aggregation and (now) outbound scheduling.

6. **`firstSettlementOrFinalizationSig` field naming.** Multi-major stacks contain multiple settlements; per spec only the FIRST is withheld for round 2. Round-2 carries one sig. Rename to `firstUnlockSig` for clarity.

---

## Ordering suggestion

- **PR1** = M1 + M2 + M3 + M4. Types + JointLedger outbound + pure effect derivation. Additive only, no wiring. Stage4 still green via fast-only path.
- **PR2** = M5 + M6 + M7 + M8 + M9 + M10 + M11. Wiring + tests. Cuts over to the new path.

If PR2 too big, split M5 + M6 (actors) from M7–M9 (transport + L1 submission). But intermediate state has stacks confirmed internally yet never reaching L1 — prefer one larger PR.
