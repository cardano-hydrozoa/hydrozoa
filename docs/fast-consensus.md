# Fast Consensus

## Overview

The **fast cycle** soft-confirms blocks: a single round of per-peer signatures over each
block header, collected eagerly to give clients an immediate guarantee on the head's
per-block decisions — which requests it included vs. rejected, and which deposits it
absorbed vs. refunded. It does not commit to L1; for that the slow cycle takes over
(`docs/slow-consensus.md`).

Soft-confirmation requires soft-acks from **every** head peer, including the leader's own
— it is a saturation requirement, not a quorum. A single missing ack stalls the brief.

## Terminology

- **ack** (soft) — one peer's Ed25519 signature over `BlockHeader.Section.signingBytes`.
  Per-peer event, transported by `PeerLiaison`, collected by `FastConsensusActor`.
- **confirmation** (soft) — the saturated set of acks, emitted as `Block.SoftConfirmed`.
  Aggregated event, consumed by `BlockWeaver` and by the slow side's `StackComposer`.

`softAck` ≠ `confirmation` — one is a per-peer signature, the other is the all-peers
collection. The same distinction appears on the slow side (hard-ack vs hard-confirmation).

## Block types

`BlockType` lives at `multisig/ledger/block/BlockType.scala`:

| type | role |
|---|---|
| **Initial** | the head's first block, fully reproducible from the head config. Never traverses the fast cycle (every peer derives the same `BlockHeader.Initial` deterministically). |
| **Minor** | affects only L2 ledger state (no treasury rotation / L1 settlement). Carries L2 transactions; may register deposits, each preparing a post-dated refund tx — a safety net to recover the deposit if the head never absorbs it (not an obligation). One soft-ack saturates per peer. |
| **Major** | carries everything Minor does, plus interaction with L1: absorbs matured deposits and withdraws funds from L2 to L1. The accompanying treasury rotation (major-version bump) triggers slow-side L1 settlement. |
| **Final** | terminal block. Triggers head finalization on the slow side. |

`BlockBrief.Next = Minor | Major | Final` is the wire-broadcast composition record (no
block 0 — it never travels). The leader produces it; followers reproduce it locally and
agree by signing the same header bytes.

## Leadership

Round-robin per `blockNum`:

```scala
def isLeader(blockNum: BlockNumber): Boolean =
    blockNum.convert % nHeadPeers == peerNum.convert
```

`nextLeaderBlock(blockNum)` returns the next block number this peer leads, strictly after
`blockNum`. The fast lane's `PeerLiaison` cursor protocol uses it (see "Per-link block
stream" below). Slow leadership (`isSlowLeader(stackNum)`) is the same round-robin keyed
on `stackNum` — independent of the fast schedule.

## Actors

```
┌────────────────────┐
│    BlockWeaver     │  decides the next block (leader/follower),
│ (next-block driver)│  then instructs JointLedger to produce it.
└──────────┬─────────┘
           │ produce block (StartBlock / CompleteBlock)
           ▼
┌────────────────────┐   BlockBrief.Next    ┌────────────────────┐
│    JointLedger     │ ───── DIRECT ──────▶ │    PeerLiaisons    │
│  (block producer   │    (leader only)     │  (one per remote   │
│    & signer)       │                      │   peer; fast +     │
└──────────┬─────────┘                      │    slow lanes)     │
           │ SoftAck (own)                  └─────────┬──────────┘
           │ + BlockBrief                             │ SoftAck (remote, inbound)
           ▼                                          │
┌────────────────────┐ ◀──────────────────────────────┘
│ FastConsensusActor │ ──── SoftAck (own, outbound) ────▶ PeerLiaisons
│    (aggregator)    │      (immediate, or postponed onto the
│                    │       previous block's cell until it confirms)
└──────────┬─────────┘
           │ Block.SoftConfirmed
           ├──▶ BlockWeaver     (feeds the next-block decision — closes the loop)
           ├──▶ StackComposer   (slow-side gate for stack closure)
           ├──▶ JointLedger     (proxies the confirmation to the L2 ledger)
           └──▶ PeerLiaisons    (local SoftConfirmed signal; inert now, kept for Final-block bookkeeping)
```

### `BlockWeaver` (`multisig/consensus/BlockWeaver.scala`)

Decides what the next block looks like — per-block leader/follower mode switch via
`isLeader(nextBlockNum)`. Gates new block production on:
- soft-confirmation of the previous block (wraps `Block.N+1` once `Block.N` is
  soft-confirmed),
- mempool pressure / dead-man timer,
- deposit maturity decisions.

The leader instructs `JointLedger` to produce `BlockBrief.Next`. Followers reproduce the
same brief locally from the same inputs (deterministic).

### `JointLedger` (`multisig/ledger/joint/JointLedger.scala`)

Produces blocks on **every** peer, not just the leader: the leader builds the block from its
inputs and broadcasts the brief; a follower re-produces the same block from the same
(deterministic) inputs and verifies it arrives at the identical brief
(`panicOnMismatchWithExpectedBrief`). It is also the L2 executor (applies each block's L2
transactions) and owns the deposit map, making the per-block deposit decisions (absorb vs.
refund) from `PollResults` — the set of deposit utxos currently visible on L1, which
`CardanoLiaison` polls and forwards through `BlockWeaver` (delivered with the block-completion
command; needed only for regular, non-final blocks). On local block completion
(`completeBlockRegular` / `completeBlockFinal`) it:

1. Broadcasts `BlockBrief.Next` directly to `PeerLiaisons` (leader only) — briefs are not
   routed through `FastConsensusActor`.
2. Signs the brief and sends its own `SoftAck` to the local `FastConsensusActor`.
3. Forwards `BlockBrief.Next` to the local `FastConsensusActor` (so verification has the
   header bytes).
4. Emits `BlockResult` to `StackComposer` (slow side; independent of the soft-ack round).

`BlockResult` is the slow-side's per-block input: brief + evacuation-map diff + payout
obligations + post-dated refund txs. The fast cycle proceeds in parallel; the slow side
only needs the soft-confirmation later.

### `FastConsensusActor` (`multisig/consensus/FastConsensusActor.scala`)

Soft-ack aggregator. Inputs:
- own `SoftAck` from `JointLedger` (leader's path), or own `SoftAck` from local signing
  (follower's path after verifying the incoming brief).
- remote `SoftAck` from `PeerLiaisons`.
- `BlockBrief.Next` from the local `JointLedger` only — never peer-relayed. An incoming
  leader brief lands at the follower's `PeerLiaison`, which routes it to `BlockWeaver`; the
  follower's `JointLedger` then re-produces the brief and forwards its own copy here.

Verifies each soft-ack's signature against the brief's `signingBytes` and accumulates per
`blockNum`. When all head peers' acks are present, emits `Block.SoftConfirmed` to:
- `BlockWeaver` — frees the next-block decision.
- `StackComposer` — paired with the corresponding `BlockResult` to mark the block
  stackable.
- `JointLedger` — proxies the confirmation to the L2 ledger
  (`L2LedgerCommand.ProxyBlockConfirmation`), advancing its confirmed state.
- `PeerLiaisons` — a **local** signal, not a broadcast: confirmations (soft or hard) never
  cross the network. Currently inert (per-remote outbox pruning moved to the `GetMsgBatch`
  receipt signal); the receive case is kept for Final-block bookkeeping.

Also broadcasts the local peer's own `SoftAck` to `PeerLiaisons` for outbound delivery.

### `PeerLiaison` fast lanes (`multisig/consensus/PeerLiaison.scala`)

One liaison per directed remote peer. Carries three fast-lane payloads per link, plus the
slow lanes (`stackBrief`, `hardAck`) covered in the slow-consensus doc:

| lane | family | producer |
|---|---|---|
| `softAck` | next-expected, contiguous per peer | every peer (every block) |
| `blockBrief` | next-expected, sparse round-robin | only `isLeader(blockNum)` |
| `request` | next-expected, contiguous per peer | every peer (user requests) |

The full per-lane batch-protocol summary lives as a `// format: off` table just above
`final case class GetMsgBatch` in `PeerLiaison.scala`. See that table for prune / verify /
advance per-lane invariants.

## Block lifecycle (a Minor block, happy path)

1. **Wrap signal.** `BlockWeaver` observes `Block.SoftConfirmed.N-1`; wraps `N`. Either
   becomes leader (`isLeader(N)`) or follower.
2. **Leader produces brief.** Leader instructs `JointLedger`, which executes the new L2
   transactions, builds `BlockBrief.Next` with header + diff + obligations, then:
   - broadcasts `BlockBrief.Next` to all `PeerLiaisons` (their outboxes' block lane);
   - signs the header (`HeadPeerWallet.mkSoftAck`) and hands the own `SoftAck` to local
     `FastConsensusActor`;
   - emits `BlockResult.N` to local `StackComposer`.
3. **Followers receive the brief** from their peer-link inbox, reproduce the same brief
   locally, sign it, and route their own `SoftAck` through their local `FastConsensusActor`
   (which also broadcasts it out to other `PeerLiaisons`).
4. **`FastConsensusActor` aggregates** soft-acks per `blockNum`. When all peers' acks are in
   (including local), emits `Block.SoftConfirmed.N` to `BlockWeaver` and `StackComposer`.
5. **Slow-side pairing.** `StackComposer` waits on `(BlockResult.N, Block.SoftConfirmed.N)`
   to coincide; the block becomes a candidate for stack closure (see slow-consensus doc).

Major and Final follow the same shape — the difference is what `JointLedger` emits in
`BlockResult` and how `StackComposer` partitions stacks (head-based partition model).
Initial blocks bypass the fast cycle entirely (no soft-ack round; every peer derives the
same `BlockHeader.Initial` from the config).

## Per-link block stream

A peer only broadcasts blocks it leads, so the per-link block sequence is **sparse**:
remote `R` with `n` head peers sends blocks `R, R+n, R+2n, …` (round-robin). The fast
lane's `PeerLiaison` cursor handles this with **next-expected** semantics: the cursor
`current.blockNum` is precomputed to the next block this remote leads, so `verifyAgainst`
does the same exact-match check as every other lane — `received.blockNum == current.blockNum`
— then advances the cursor via `remotePeerId.nextLeaderBlock(received.blockNum)`. Block 0 is
the config-bootstrapped block and is never sent over this channel (`nextLeaderBlock(0)`
advances the cursor past it).

This is the only lane on `PeerLiaison` that is sparse-leader-driven *for blocks*; soft-acks
travel densely (every peer acks every block, so `ackNum == blockNum` is contiguous per
peer). The slow-side `stackBrief` lane mirrors the same sparse round-robin shape (only the
slow-leader of a stack broadcasts its brief).

## Bootstrap

Block 0 (the initial block) is fully determined by the head config. Every peer derives the
same `BlockHeader.Initial` deterministically; there is no soft-ack round. The fast cycle
starts on block 1. The slow cycle stack 0 covers the initial block's L1 work (init tx +
fallback tx multisig); the fast cycle has nothing to do for it.

## Relationship to the slow cycle

The fast cycle is the *gate* for the slow cycle:

- `BlockResult.N` (data) is emitted by `JointLedger` independent of soft-ack.
- `Block.SoftConfirmed.N` (proof) is emitted by `FastConsensusActor` once all peers acked.
- `StackComposer` pairs `(BlockResult.N, Block.SoftConfirmed.N)` and only then treats `N`
  as stackable; the slow leader closes a stack containing the longest contiguous
  soft-confirmed prefix since the previous stack.

The fast cycle may run far ahead of the slow cycle; that is the design — amortize L1 cost
and (eventually) coil round-trips. Both cycles share the same `PeerLiaison` actor (one per
remote) and the same batch-cursor protocol; they only differ in which lanes they push
through it.
