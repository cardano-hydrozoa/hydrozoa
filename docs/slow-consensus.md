# Slow Consensus

## Overview

The **slow cycle** turns a contiguous run of soft-confirmed blocks into a
**multisigned, L1-submittable set of effect transactions**. It mirrors the fast cycle
(leader proposes; followers ack; aggregator confirms) but operates on **stacks** of blocks
instead of individual blocks, and produces signatures over **per-effect transaction
bodies** rather than over a single header. Slow consensus is what gives Hydrozoa its L1
commitment guarantee.

Hard-confirmation requires hard-acks from **every** head peer (leader's own included). A
single missing ack stalls the stack. The cycle may lag arbitrarily behind the fast cycle —
that is the point: amortize L1 cost (and, in a future PR, coil-quorum round-trips).

See `docs/fast-consensus.md` for the fast cycle that gates this one.

## Terminology

- **ack** (hard) — one peer's set of Ed25519 signatures over the bodies of a stack's
  effect transactions (plus, where applicable, over a standalone evacuation commitment's
  minor header). Per-peer event, transported by `PeerLiaison`, collected by
  `SlowConsensusActor`.
- **confirmation** (hard) — the saturated set of acks aggregated into actual
  `VKeyWitness`es attached to each effect tx body. The result is `Stack.HardConfirmed`,
  consumed by `CardanoLiaison` (L1 submission) and by `StackComposer`
  (next-stack release).

`softAck` ≠ `confirmation`; same distinction as the fast side.

## Stack lifecycle

```
JointLedger  ── BlockResult.N            ─▶  StackComposer
ConsensusActor ── Block.SoftConfirmed.N  ─▶  StackComposer
                                              │ pair by blockNum; close on the longest
                                              │ contiguous prefix once previous stack is
                                              │ hard-confirmed.
                                              │
                                              ├─▶ StackBrief  ─direct─▶  PeerLiaisons
                                              │
                                              └─▶ StackHandoff(Stack.Unsigned, own acks)
                                                  │
                                                  ▼
                                          SlowConsensusActor
                                              │ verify + aggregate per-effect sigs;
                                              │ schedule own round-2 release.
                                              │
                                              ├─▶ own round-1/sole HardAck ─▶ PeerLiaisons
                                              ├─▶ own round-2 HardAck (released after
                                              │    local round-1 confirmation) ─▶ PeerLiaisons
                                              │
                                              ├─▶ Stack.HardConfirmed       ─▶ CardanoLiaison
                                              └─▶ PreviousStackHardConfirmation
                                                                            ─▶ StackComposer
```

A stack's life:

1. **Inputs accumulate.** Each peer's `JointLedger` emits `BlockResult.N` on local block
   completion; `FastConsensusActor` emits `Block.SoftConfirmed.N` when all peers acked. The
   slow leader's `StackComposer` pairs them by `blockNum`.
2. **Leader closes.** When (a) the previous stack is hard-confirmed and (b) the ready
   queue has a non-empty longest-contiguous-from-`lastClosedBlockNum+1` prefix, the
   slow-leader for `nextStackNum` (`isSlowLeader(stackNum)`) drains the prefix, builds the
   `StackBrief`, runs the partition + effect-derivation pipeline, signs its own hard-acks
   upfront, broadcasts the brief direct to `PeerLiaisons`, and hands `Stack.Unsigned` +
   own acks to `SlowConsensusActor` via `StackHandoff`.
3. **Followers re-derive.** Followers receive the brief over the wire, run the same
   partition + effect-derivation locally (deterministic), sign their own hard-acks, and
   hand them off to their `SlowConsensusActor`. The brief carries composition only;
   **effects never travel on the wire**.
4. **Aggregation.** Each peer's `SlowConsensusActor` verifies incoming remote acks against
   the locally-derived effect bodies, accumulates per peer, and (for 2-phase stacks)
   releases its own round-2 ack only once local round-1 confirmation lands.
5. **Hard-confirm.** Once round-2 (or sole) saturates, `SlowConsensusActor` aggregates the
   verified per-effect signatures into `VKeyWitness`es, attaches them onto the effect tx
   bodies, emits `Stack.HardConfirmed` to `CardanoLiaison` (which submits to L1 in
   dependency order), and emits `PreviousStackHardConfirmation` back to `StackComposer`
   (which unblocks the next stack).

## Actors

### `StackComposer` (`multisig/consensus/StackComposer.scala`)

One per peer. Per-block-result + per-soft-confirmed pairing, stack-close decision (leader)
or stack-close validation (follower), partition + effect derivation, own-ack signing,
brief broadcast.

State (`State.empty` is the boot value):
- `pending: Map[BlockNumber, PendingBlock]` — `(BlockResult, Block.SoftConfirmed)`
  half-pairs awaiting their counterpart.
- `ready: Map[BlockNumber, ReadyBlock]` — paired, stack-eligible.
- `inboundLeaderBrief: Map[StackNumber, StackBrief]` — follower-side incoming briefs.
- `lastClosedStackNum`, `lastClosedBlockNum` — cursors.
- `previousStackHardConfirmed: Boolean` — single-flight gate; bootstrapped to `true` so
  stack-0 logic can fire (currently stack-0 injection is deferred — see "Bootstrap").
- `ownHardAckNum: HardAckNumber` — next per-peer hard-ack number to assign (0-based; see
  the cursor protocol table at the top of `PeerLiaison.scala`).

Mode is implicit: every paired-state change calls `tryProgress`, which decides leader vs
follower via `config.ownHeadPeerId.isSlowLeader(nextStackNum)`. Both paths share the
pairing logic; only the close trigger differs (leader composes from `ready`; follower
matches an incoming brief against the same queue).

#### Leader's close-stack flow

When `previousStackHardConfirmed && readyPrefix.nonEmpty`:

1. Drain the longest contiguous prefix of `ready` starting at `lastClosedBlockNum + 1`
   (gap-free).
2. Build `StackBrief(stackNum, firstBlockNum, lastBlockNum)`.
3. `StackPartition.partition(results)` → `NonEmptyList[StackPartition]` (head-based; see
   "Partition model" below).
4. `StackEffectsBuilder.deriveRegular(partitions)` → `StackEffects.Unsigned.Regular`.
5. `buildHandoff` signs all own hard-acks upfront: for 2-phase, both round-1 and round-2;
   for 1-phase (all-Minor sole), just the sole ack. `nextOwnHardAckNum` is the per-peer
   counter; the unlock partition is chosen by `PartitionEffects.unlock`.
6. Broadcasts `StackBrief` direct to all `PeerLiaisons`.
7. Hands `StackHandoff(unsigned, acks)` to `SlowConsensusActor`.
8. `previousStackHardConfirmed := false` until the next `PreviousStackHardConfirmation`
   event fires.

#### Follower's close-stack flow

On an incoming `StackBrief` (received over the wire, routed by `PeerLiaison` →
`StackComposer`), classify against the locally-paired view:

| outcome | condition | action |
|---|---|---|
| structural divergence | `brief.firstBlockNum != lastClosedBlockNum + 1` or `last < first` | log + stall (TODO: rule-based fallback) |
| not-yet-covered | brief spans paired + still-pending blocks | wait silently; `tryProgress` re-fires on the next event |
| covered | every block in `[first, last]` is paired | build from **exactly** the brief's range, derive locally, sign, hand off |

Followers do not partition independently — they consume the leader's `firstBlockNum..lastBlockNum`
and derive deterministically, so their effects byte-match the leader's. Any divergence is
a critical consensus break by design and must trigger rule-based fallback.

### `SlowConsensusActor` (`multisig/consensus/SlowConsensusActor.scala`)

Pure ack aggregator + scheduled-broadcast controller + confirmation emitter. **No wallet,
no signing** — all signing is done upstream by `StackComposer` (which holds the wallet
for slow-side signing).

Per-stack cell state machine:

```scala
sealed trait Cell
case class WaitingRound1(unsigned: Stack.Unsigned, round1: Map[Peer, Round1Payload],
                         ownRound2Held: HardAck)                  // 2-phase
case class WaitingRound2(unsigned: Stack.Unsigned, round2: Map[Peer, Round2Payload])
case class WaitingSole  (unsigned: Stack.Unsigned, sole:   Map[Peer, SolePayload])     // 1-phase
```

Responsibilities:

1. **On `StackHandoff` from `StackComposer`** — create the cell, verify and insert own
   ack(s), broadcast own round-1/sole immediately to `PeerLiaisons`, hold own round-2.
2. **On remote `HardAck`** — verify every per-effect signature and (for SECs) the minor
   header signature against the locally-derived effect bodies. Bad signature ⇒ raise (a
   verification mismatch is a deterministic divergence, not a transient issue).
3. **On round-1 saturation** — release the held own round-2 to `PeerLiaisons` and
   transition `WaitingRound1 → WaitingRound2`.
4. **On round-2 (or sole) saturation** — aggregate verified `TxSignature`s into
   `VKeyWitness`es keyed by effect `tx.id`, attach them via `Tx.addSignatures` onto each
   effect's tx wrapper, hoist the SEC to its `MultiSigned` variant, and emit:
   - `Stack.HardConfirmed` → `CardanoLiaison`
   - `PreviousStackHardConfirmation(stackNum)` → `StackComposer`

Early-round-2 acks arriving before local round-1 confirmation are verified-and-stashed.
Per-stackNum orphan acks arriving before the cell exists are buffered and replayed.

### `CardanoLiaison` (`multisig/consensus/CardanoLiaison.scala`)

Consumes `Stack.HardConfirmed`; submits the effect txs to L1 in dependency order via the
pre-split submission state machine: settlement / finalization first (the unlock), then
fallback, then rollouts. Refund txs are post-dated and queued for later submission.
Standalone evacuation commitments are dormant — never submitted as L1 transactions; they
exist only for the rule-based dispute regime.

### `PeerLiaison` slow lanes (`multisig/consensus/PeerLiaison.scala`)

Two slow-side lanes on top of the fast-side ones (see `docs/fast-consensus.md`):

| lane | family | producer |
|---|---|---|
| `stackBrief` | last-seen, sparse round-robin | only `isSlowLeader(stackNum)` |
| `hardAck` | next-expected, contiguous per peer | every peer (every stack) |

Both share `PeerLiaison`'s batch-cursor machinery; the per-lane invariants and cursor
initial values live in the comment table just above `final case class GetMsgBatch` in
`PeerLiaison.scala`. `stackBrief` mirrors the block lane (sparse — only the slow-leader
of a stack broadcasts the brief); `hardAck` is 0-based contiguous (initial stack, once
injected, takes hardAck 0 = round-1, 1 = round-2).

> **TODO (merge `feature/migrate-kzg`):** that branch unified ALL lanes to **next-expected**,
> so `stackBrief` becomes "sparse but next-expected" (cursor precomputed to the next slow-leader
> stack via `nextSlowLeaderStack`) rather than last-seen. Update the `stackBrief` row above when
> that change lands here (mirrors the `blockBrief` TODO in `fast-consensus.md`).

## Data types

`multisig/ledger/stack/`:

- **`BlockResult`** — per-block input from `JointLedger`: brief + `EvacuationMapDiff` +
  `payoutObligations` + `postDatedRefundTxs` + `absorbedDeposits` + `competingFallbackTxTime`.
  Construction-time input to partition + effect derivation; not retained on the closed
  stack.
- **`StackBrief`** — wire-broadcast composition record: `(stackNum, firstBlockNum, lastBlockNum)`.
  The leader sends this; followers reproduce effects from their local block streams.
- **`StackPartition`** — `(blocks: NonEmptyList[BlockResult], majorVersion: BlockVersion.Major, kind: Kind)`
  where `Kind = Initial | Major | Final | Minor`. See "Partition model" below.
- **`PartitionEffects[+S]`** — the partition-indexed effect ADT (see "Stack effects").
- **`StackEffects.Unsigned`** / **`StackEffects.HardConfirmed`** — the stack's effects in
  unsigned vs multisigned form. Each is `Initial | Regular`; `Regular` carries
  `NonEmptyList[PartitionEffects[…]]`.
- **`Stack.Unsigned(brief, effects)`** — composed locally; lives only inside each peer's
  `StackComposer` and `SlowConsensusActor`. Not wire-broadcast.
- **`Stack.HardConfirmed(unsigned, effects: StackEffects.HardConfirmed)`** — fully
  multisigned, L1-submittable. Emitted by `SlowConsensusActor`, consumed by
  `CardanoLiaison`.
- **`StandaloneEvacuationCommitment`** — dormant dispute-only KZG-commitment record. Not
  an L1 transaction; carries its own minor header serialization so signing/verification do
  not need `BlockResult` post-construction.

`multisig/consensus/ack/`:

- **`HardAckNumber`** — opaque `Int` per-peer cursor for hard-ack delivery; 0-based.
- **`HardAckId = (HeadPeerNumber, HardAckNumber)`**.
- **`HardAck(ackId, stackNum, payload)`** — wire payload. Round is implicit in
  `payload`'s variant:
  - `Round1Payload.Regular(partitions: NonEmptyList[PartitionSig])` — per-partition
    signature bundle; the unlock partition's settlement/finalization slot is `None`.
  - `Round1Payload.Initial(fallbackSig)` — stack 0's round 1.
  - `Round2Payload.Regular(firstUnlockSig)` — the single unlock signature.
  - `Round2Payload.Initial(initTxSig, individualWitnesses)` — stack 0's round 2.
  - `SolePayload(sec, refunds)` — all-Minor stack, one-phase path.

## Partition model

Stacks are partitioned **head-based**: the block that *opens* a partition determines its
kind, and a Major partition owns the same-major-version minors that **follow** it.

| Kind | Shape | Effects derived |
|---|---|---|
| **Initial** | stack 0 alone (single partition) | `StackEffects.Initial(initTx, fallbackTx)` (separate variant, not in the partition list) |
| **Major** | `Major + (trailing same-major-version minors)*` | settlement + fallback + rollouts + trailing minors' refunds + **SEC for the partition's last minor — optional, iff ≥ 1 trailing minor** |
| **Final** | `Final` alone | finalization + rollouts |
| **Minor** | leading minor run; stops at next Major/Final or stack end | **SEC for the latest minor — mandatory** + those minors' refunds |

Per-stack partition sequence for a `Regular` stack: `[Minor?] [Major]* [Final?]`. A
Minor-headed partition can only lead the stack (every other minor run is absorbed as a
Major's tail). An all-Minor stack is exactly one Minor partition = the 1-phase / sole-ack
case.

**Why SEC even when a settlement / finalization follows.** Settlement and finalization L1
execution is not guaranteed — the head can stall before the unlock lands. Trailing minors
advance the KZG beyond the settlement snapshot; if it never lands, voting in the
rule-based regime needs the latest minor's KZG. So the SEC is per *minor-containing*
partition, not ≤ 1 per stack.

## Stack effects

`PartitionEffects[+S]` is the partition-indexed effect ADT (`S` is
`StandaloneEvacuationCommitment` on `Unsigned`, `…MultiSigned` on `HardConfirmed`):

```scala
sealed trait PartitionEffects[+S]
case class Major[+S](
    settlement: SettlementTx,
    fallback: FallbackTx,
    rollouts: List[RolloutTx],
    refunds: List[RefundTx],
    sec: Option[S]                                  // last trailing minor, iff any
) extends PartitionEffects[S]
case class Final(finalization: FinalizationTx, rollouts: List[RolloutTx])
    extends PartitionEffects[Nothing]
case class Minor[+S](sec: S, refunds: List[RefundTx]) extends PartitionEffects[S]
```

`StackEffects.{Unsigned,HardConfirmed}.Regular(partitions: NonEmptyList[PartitionEffects[…]])`
is the spine. Single shared `PartitionEffects.unlock(partitions): Option[Unlock]`
selects the unlock target — first `Major`'s settlement, else `Final`'s finalization, else
`None` (all-Minor ⇒ sole). The signer (`StackComposer.buildHandoff`) calls it to split own
acks into round-1/round-2; the verifier (`SlowConsensusActor`) calls the same function to
know which slot round-2 fills. One function, not two mirroring types.

## Hard-ack rounds

### Two-phase (any stack with settlement or finalization, plus the Initial stack)

- **Round 1.** Signatures for every effect EXCEPT the first settlement (Major) or
  finalization (Final) at stack close. For each partition: SEC (where present), each
  refund tx, fallback + each rollout + each subsequent settlement (multi-Major) — all
  except the unlock. Sent immediately on broadcast.
- **Round 2.** Signature over the unlock tx body (first settlement OR finalization).
  Signed upfront alongside round 1 (the unlock body is known at stack close), but the own
  round-2 ack is **held** by `SlowConsensusActor` until local round-1 confirmation,
  mirroring the fast-side's "scheduled own ack" pattern.

**Why withhold the unlock.** Settlement is the L1 entry point; all dependent txs spend
its outputs. Releasing round-2 sigs before round-1 confirmation would let a peer commit to
the unlock without the rest of the effects being signed. Withholding preserves atomicity:
either every effect is signed (round-1 saturates ⇒ round-2 releases) or nothing is.

### One-phase (all-Minor stack)

- **Sole ack.** Signature over the latest minor's SEC (mandatory) + signatures over each
  minor's refund txs. Sent immediately. No unlock, no round-2.

### Initial (stack 0)

Structurally 2-phase, but its content is exogenous:

- **Round 1.** Signature over the locally-derived fallback tx body.
- **Round 2.** Signature over the head config's initialization tx body + each head peer's
  individual `VKeyWitness`es for utxos spent from their individual addresses (operator-
  supplied funding).

Stack 0 is deferred — the `Bootstrap` constructor injection of stack 0's synthetic inputs
is not yet wired (see "Bootstrap" below).

## Stack closure policy

The slow leader for `stackNum = N+1` (`isSlowLeader(N+1)` holds) closes a stack when both:

1. The previous stack (`N`) is hard-confirmed (`previousStackHardConfirmed == true`,
   armed by the incoming `PreviousStackHardConfirmation(N)`).
2. The ready queue has a non-empty longest-contiguous-from-`lastClosedBlockNum + 1`
   prefix.

The new stack contains **all** of that prefix — any combination of minors / majors / a
final block. There is no "close on Major / Final" simplification; content emerges from
whatever has been paired since the last close.

Slow-leadership round-robin (`isSlowLeader(stackNum) := stackNum % nHeadPeers == peerNum`)
is independent of the fast schedule. Different stack and block sequences advance at
different paces.

## Bootstrap

Stack 0 covers the initial block and the head's L1 init / fallback multisig. Per spec it
runs through the slow cycle; per current code the bootstrap injection is **deferred**.

The plan:
- `StackComposer.Bootstrap(initialBrief, initialBlockResult, syntheticSoftConfirmed0,
  initializationTx)` constructor parameter, processed during `PreStart`. Injects the
  initial brief + result + synthetic soft-confirmation 0 + the head config's init tx.
- `previousStackHardConfirmed = true` synthetically at boot so stack 0 can close
  immediately. Stack 0's hard-confirmation is what arms the chain for stack 1.

Until that is wired, `State.empty` sets `previousStackHardConfirmed = true` and the slow
cycle starts from stack 1 directly. The Initial code paths
(`StackEffects.Unsigned.Initial`, `Round1Payload.Initial`, `Round2Payload.Initial`) exist
but are not exercised at runtime; `Round2Payload.Initial` is intentionally
wire-unsupported in `Codecs.scala` until the boot path lands.

## L1 submission order

`CardanoLiaison` flattens `StackEffects.HardConfirmed.Regular` partitions and submits in
dependency order:

1. **Unlock first** — first `Major`'s settlement OR `Final`'s finalization.
2. **Fallbacks** — one per Major (queued for dead-man's-switch firing; not actively
   submitted, but tracked).
3. **Rollouts** — chained via utxo dependencies (regrouped per backbone via a utxo-chain
   walk).
4. **Refund txs** — post-dated; queued for later submission once their validity start
   times pass.
5. **Standalone evacuation commitments** — NOT submitted. Dormant dispute-only records
   surfaced for the rule-based regime.

## Deterministic effect derivation

Hydrozoa requires that, given the same `BlockResult`s + `StackBrief`, every head peer
derives **byte-identical** effects. The wire only carries the brief (composition); peers
re-derive effect bodies locally. A signature-verification mismatch in
`SlowConsensusActor` therefore means a peer's derivation has diverged — a critical
consensus break, not a transient issue. The actor raises rather than silently dropping;
the response (rule-based fallback / dead-man's-switch) is the system-level handler.

## Not in slow consensus

- **Coil-peer quorum sigs** — Phase A is head-peers-only. Coil peers and the additional
  quorum at both rounds are a future PR.
- **User-facing notification** — `Block.SoftConfirmed` and `Stack.HardConfirmed` will be
  translated into `RequestSequencer` updates so users see request status transitions; not
  yet wired.
- **Refund-tx L1 submission** — `CardanoLiaison` consumes refund txs but the post-dated
  submission timing is queued; user-visible refund-confirmed events are part of the
  `RequestSequencer` follow-up.
- **L2 refund release** — the L2 ledger books refund obligations at minor-block
  soft-confirmation time; no slow-side `JointLedger` callback is needed.
- **Follower divergence fallback** — `tryCloseAsFollower` currently logs and stalls on
  structural divergence; the rule-based fallback wiring is a TODO.
