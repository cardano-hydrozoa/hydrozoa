# Plan: Slow Consensus — Head Peers Only

## Goal

Reintroduce the removed effect-production code as a proper slow cycle, aligned with the whitepaper's slow-consensus article. Head peers only in Phase A — no coil quorum yet, but the two-round hard-ack flow ("unlock" pattern) is built in from day one for stacks containing settlement or finalization, including the **initial stack** (a two-phase, one-block stack with the initialization tx as its unlock). Slow cycle mirrors the fast cycle's leader-broadcast / peers-confirm shape, with consumer-side gating on soft-confirmation.

## Design principle: fast/slow isomorphism, gated by soft-confirmation

- **Fast consensus** = soft-ack over `BlockHeader.signingBytes`. Fast leader picks the brief, **also signs her own soft-ack, broadcasts both brief and her own ack to peers via PeerLiaison, and feeds her own ack into her local ConsensusActor**. Other peers verify the brief and broadcast their own soft-acks. **Soft-confirmation requires soft-acks from ALL head peers (leader's own included) — not a quorum.** A single missing head-peer ack stalls the brief. Weaker guarantee than slow (no L1 commitment), but lower latency. Useful standalone for clients that don't need L1 commitment.
- **Slow consensus** = hard-ack over per-effect signatures of a closed stack. Slow leader picks the stack composition (cuts), **also signs her own round-1 hard-ack, broadcasts the StackBrief + her own ack to peers via PeerLiaison, and feeds her own ack into her local SlowConsensusActor**. Other peers verify the brief, locally derive effects, broadcast their own hard-acks. **Hard-confirmation requires hard-acks from ALL head peers (leader's own included) in Phase A; when coil peers join in a later PR, it will additionally require a quorum of coil peers.** Stronger guarantee, higher latency, batched.
- **Symmetry.** Both cycles follow the same pattern: leader fixes the order, peers confirm. Distributed-system safety needs the leader's broadcast to synchronize the closure decision — peers cannot be assumed to converge independently.
- **Producer-side decoupling.** JointLedger emits `BlockResult` immediately on local block completion, independent of the soft-ack round. The fast cycle proceeds in parallel.
- **Consumer-side gating.** StackActor pairs `BlockResult.N` (data, from JointLedger) with `Block.SoftConfirmed.N` (proof, from ConsensusActor) before treating a block as stackable. A stack cannot close until every constituent block is paired.
- **Leader broadcast.** The slow leader, on receiving the previous-stack hard-confirmation signal (and having a soft-confirmed Major / Final block to close on), builds the stack locally, signs round-1 hard ack, broadcasts `StackBrief` (composition only, never effects). Followers re-derive effects locally and sign the same `StackBrief`.
- **Leader's soft-ack gate.** The slow leader does not include a block in a stack until she has observed its `Block.SoftConfirmed` (full soft-ack collection by fast consensus). No soft-confirmation → no stack inclusion.
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

**Setup (not a round).** Slow leader announces `StackBrief` (just the composition cut: stackNum, firstBlockNum, lastBlockNum, optionally firstMajorBlockNum). Broadcast to all peers via PeerLiaison.

**Round 1 — first hard ack (all-but-unlock).**
- Each peer, on observing the leader's `StackBrief` AND having all constituent blocks paired locally, signs every necessary effect EXCEPT the first settlement/finalization. Per-effect sigs.
- Content per partition:
  - Sig over the partition's last evac commit (if non-empty).
  - Sigs over each refund tx in the partition (including refunds from minor blocks).
  - For partitions ending in a Major: sigs over fallback + each rollout + the Major's refunds + the Major's settlement (UNLESS it's the first settlement / finalization — that one's withheld).
  - For the partition ending in a Final: sigs over each rollout (and Final's refunds if any).
- The first settlement OR finalization tx in the stack is the only one withheld for round 2. In a multi-major stack, second and subsequent settlements are signed in round 1.
- Each peer broadcasts its round-1 hard ack to every other peer.
- Round 1 saturates locally when all head peers' round-1 acks are received and every per-effect sig verifies against the locally-derived effect bodies.

**Round 2 — second hard ack (the unlock).**
- Triggered the moment a peer locally observes round-1 saturation.
- Each peer signs the first settlement (Major) or finalization (Final) of the stack.
- Broadcast to every other peer.
- Round 2 saturates when all head peers' round-2 acks are received.

**L1 submission.** After round-2 saturation. The full effect-tx set is now multisigned and submittable on L1 in dependency order (settlement / finalization first, then dependent rollouts / fallback / refunds).

**Why withhold settlement.** Settlement is the L1 entry point — all subsequent dependent txs spend its outputs. If a peer withholds round-2 sig, the other effects are signed but unactionable on L1 (no settlement to spend from). If round 1 stalls, no commitment was made at all. Atomicity preserved either way.

**Minor-only-stack path:**
- Sole ack: sig over last evac commitment from the stack's single partition + per-tx sigs over each minor block's refund txs.
- One round; on saturation the stack is hard-confirmed. L1 submission of refund txs (the evac commitment itself is not a standalone L1 tx — it's a value carried into the next major's settlement / used in dispute resolution).

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
    finalizationConfirmed: Bool,                // mirrors SoftAck pattern; may also be implicit in payload variant
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
        round2Acks: NonEmptyList[HardAckRound2],   // empty list if minor-only stack (no second round)
    )
```

`HardAckRound1`'s shape is intentionally over-general for future multi-major support. Phase A code paths will have a single partition and skip half the fields.

## Data flow

```
JointLedger.completeBlockRegular / completeBlockFinal
  ├─ produce BlockBrief                              → ConsensusActor (fast: collect SoftAcks)
  └─ emit BlockResult.N (NEW)                        → StackActor

ConsensusActor (fast cycle)
  └─ on SoftAck saturation: Block.SoftConfirmed.N    → JointLedger.proxyConfirmation (legacy, retired later)
                                                      → StackActor (gate signal)

StackActor (leader role — peer where isSlowLeader(closingBlockNum) holds)
  ├─ pair (BlockResult.N, Block.SoftConfirmed.N) by blockNum
  └─ when both present AND brief is Major/Final AND previous stack hard-confirmed AND no gap below:
       ├─ close stack: all paired blocks since last close, ending at this Major/Final
       ├─ build StackBrief (stackNum, firstBlockNum, lastBlockNum, firstMajorBlockNum)
       ├─ derive necessary effects (partition by major version; drop superseded evac commits)
       ├─ build StackEffects (settlement / finalization withheld for round 2)
       ├─ sign own HardAckRound1 (per-effect sigs, all-but-unlock)
       ├─ broadcast StackBrief                       → PeerLiaisons (leader's closure decision)
       └─ forward Stack.Unsigned + own round-1 ack   → SlowConsensusActor

StackActor (follower role)
  ├─ pair streams
  ├─ wait for leader's StackBrief
  └─ when leader's StackBrief arrives AND all constituent blockNums locally paired:
       ├─ validate composition: blockNums in spec-required form; ending block is Major/Final
       ├─ re-derive necessary effects locally
       ├─ sign own HardAckRound1, hand off (Stack.Unsigned + ack) → SlowConsensusActor

SlowConsensusActor (per-stack cell, two phases, leader-driven)
  ├─ Phase 1: collect round-1 HardAckRound1 from all head peers; verify per-effect sigs
  ├─ on round-1 saturate:
  │    ├─ sign own HardAckRound2 (sig over first settlement/finalization)
  │    └─ broadcast own round-2 ack via PeerLiaisons
  ├─ Phase 2: collect round-2 HardAckRound2 from all head peers; verify settlement/finalization sig
  └─ on round-2 saturate:
       ├─ Stack.HardConfirmed (with both ack sets)   → CardanoLiaison (L1 submit, dependency order)
       ├─                                            → JointLedger.releaseRefunds (L2 update; replaces proxyConfirmation)
       └─                                            → PeerLiaisons (purge hard-ack outbox)
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
    finalizationConfirmed: Bool,
)
```

Wallet API:
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
1. After every pairing update OR `PreviousStackHardConfirmed`, check eligibility:
   - `previousStackHardConfirmed == true`
   - `ready` queue is non-empty (at least one new soft-confirmed block since last close)
2. If eligible: close a stack containing **all** of `ready` (drained — every soft-confirmed-and-paired block since the previous close, regardless of type).
3. Close-stack:
   - drain `ready` entirely
   - build `StackBrief` from drained blocks
   - if `stackNum == 0` (boot): build `StackEffects.Initial(initTx, deriveFallbackTx(initTx, headParams))` from bootstrap data; skip the necessary-effects partition algorithm
   - else: run necessary-effects selection (M1) → `StackEffects.Regular(...)`
   - build `Stack.Unsigned`
   - cell-phase decision based on stack-effects variant:
     - `StackEffects.Initial` → 2-phase (round 1 = fallback sig; round 2 = init tx sig + individual witnesses)
     - `StackEffects.Regular` with settlement / finalization → 2-phase
     - `StackEffects.Regular` minor-only → 1-phase
   - sign own round-1 hard ack (or sole hard ack for minor-only) via the appropriate wallet method
   - broadcast `StackBrief` → PeerLiaisons
   - hand off `(unsigned, own ack)` → SlowConsensusActor
   - `previousStackHardConfirmed = false` until the next `PreviousStackHardConfirmed` event

Follower path:
1. On `IncomingStackBrief` from PeerLiaison: insert into `inboundLeaderBrief`, then try-process.
2. On any state change: look for `inboundLeaderBrief[lastClosed+1]` with all constituent blocks paired.
3. On match: validate composition, re-derive effects, check against leader's brief, sign own `HardAckRound1`, hand off to SlowConsensusActor.
4. On mismatch: log + stall (open question).

**Files:** `StackActor.scala` (replace stub).

---

### M6: `SlowConsensusActor` (replaces stub) — variable-phase per cell

```scala
sealed trait SlowConsensusCell
// 2-phase path: stack contains settlement/finalization
case class WaitingForRound1(
    unsigned: Stack.Unsigned,
    round1Acks: Map[HeadPeerId, HardAckRound1],
)
case class WaitingForRound2(
    round1: Stack.Round1Confirmed,
    round2Acks: Map[HeadPeerId, HardAckRound2],
)
// 1-phase path: minor-only stack
case class WaitingForSoleAck(
    unsigned: Stack.Unsigned,
    soleAcks: Map[HeadPeerId, HardAckSole],
)
```

Transitions (2-phase path; works for both `StackEffects.Regular` w/ settlement/finalization and `StackEffects.Initial`):
- On `Stack.Unsigned` + own round-1 ack from StackActor → `WaitingForRound1`.
- On incoming round-1 `HardAck`: insert; verify per-effect sigs against locally-derived effect tx bodies. If complete:
  - sign own round-2 ack via the appropriate wallet method:
    - `StackEffects.Regular` → `mkHardAckRound2Regular` (sig over first settlement / finalization)
    - `StackEffects.Initial` → `mkHardAckRound2Initial` (sig over init tx + individual witnesses)
  - transition to `WaitingForRound2`
  - broadcast own round-2 ack via PeerLiaisons
- On incoming round-2 `HardAck`: insert; verify per-variant payload (settlement/finalization sig, or init tx sig + witnesses). If complete:
  - build `Stack.HardConfirmed`
  - emit to CardanoLiaison (L1 submit), JointLedger.releaseRefunds, PeerLiaisons (purge outbox), StackActor (signal `PreviousStackHardConfirmed`)
  - drop cell

Transitions (1-phase path):
- On `Stack.Unsigned` (minor-only) + own sole ack from StackActor → `WaitingForSoleAck`.
- On incoming sole `HardAck`: insert; verify last-evac-commit sig + minor-refund sigs. If complete:
  - build `Stack.HardConfirmed` (no round-2 acks)
  - emit to CardanoLiaison (L1 submit only if any refund txs), JointLedger.releaseRefunds, PeerLiaisons (purge outbox), StackActor (signal `PreviousStackHardConfirmed`)
  - drop cell

Cell-phase choice is determined by `Stack.Unsigned`'s effects content at hand-off time from StackActor.

Postponed-ack semantics: stash early acks for stackNum N+1 on cell N if N not saturated yet (same as soft-side); applies to both rounds (or sole round).

Wallet access: SlowConsensusActor receives `HeadPeerWallet` reference.

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
- Stage4: add `StackObserver` parallel to `BlockBriefObserver`, watching `SlowConsensusActor`'s request channel. Verify: stack count, both 1-phase (minor-only) and 2-phase (settlement/finalization-containing) paths are exercised, round-2 always follows round-1 saturation in 2-phase cells, multi-partition necessary-effects selection produces correct sig sets.

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

3. **Effect-derivation divergence between peers.** Per-effect sig mismatch → cell never saturates → slow side stalls (fast side keeps moving). Need observability: log mismatched sigs with peer ID + which tx body diverged. Add before this lands.

4. **`proxyConfirmation` retirement timeline.** Phase A adds `releaseRefunds`. ConsensusActor still calls `proxyConfirmation` with empty refund list. Cut after slow side live — follow-up cleanup, not blocking.

5. **Where round-2 signing lives.** Plan: SlowConsensusActor (triggered on local round-1 saturation, needs wallet). Alternative: StackActor (already holds wallet). SlowConsensusActor wins because round 2 is consensus-state-driven, not stack-build-driven.

6. **`firstSettlementOrFinalizationSig` field naming.** Multi-major stacks contain multiple settlements; per spec only the FIRST is withheld for round 2. Round-2 carries one sig. Rename to `firstUnlockSig` for clarity.

---

## Ordering suggestion

- **PR1** = M1 + M2 + M3 + M4. Types + JointLedger outbound + pure effect derivation. Additive only, no wiring. Stage4 still green via fast-only path.
- **PR2** = M5 + M6 + M7 + M8 + M9 + M10 + M11. Wiring + tests. Cuts over to the new path.

If PR2 too big, split M5 + M6 (actors) from M7–M9 (transport + L1 submission). But intermediate state has stacks confirmed internally yet never reaching L1 — prefer one larger PR.
