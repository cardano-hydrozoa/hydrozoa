# Fast Consensus

## Overview

The **fast cycle** soft-confirms blocks: a single round of per-peer signatures over each
block header, collected eagerly to give clients an immediate "the head agrees this block
ordered request X at slot Y" guarantee. It does not commit to L1; for that the slow cycle
takes over (`docs/slow-consensus.md`).

Soft-confirmation requires soft-acks from **every** head peer, including the leader's own
— it is a saturation requirement, not a quorum. A single missing ack stalls the brief.

## Terminology

- **ack** (soft) — one peer's Ed25519 signature over `BlockHeader.Section.signingBytes`.
  Per-peer event, transported by `PeerLiaison`, collected by `FastConsensusActor`.
- **confirmation** (soft) — the saturated set of acks, emitted as `Block.SoftConfirmed`.
  Aggregated event, consumed by `BlockWeaver` and by the slow side's `StackComposer`.

`ack` ≠ `confirmation` — one is a per-peer signature, the other is the all-peers
collection. The same distinction appears on the slow side (hard-ack vs hard-confirmation).

## Block types

`BlockType` lives at `multisig/ledger/block/BlockType.scala`:

| type | role |
|---|---|
| **Initial** | the head's first block, fully reproducible from the head config. Never traverses the fast cycle (every peer derives the same `BlockHeader.Initial` deterministically). |
| **Minor** | the common case. Carries L2 transactions and refund obligations. One soft-ack saturates per peer. |
| **Major** | every `nMajor` blocks. Carries everything Minor does, plus a major-version bump that triggers slow-side L1 settlement. |
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
┌─────────────────┐                    ┌──────────────────┐
│  JointLedger    │  BlockBrief.Next   │   PeerLiaisons   │
│  (block producer│ ─────DIRECT──────▶ │  (one per remote │
│   & signer)     │                    │   peer; fast +   │
└──┬──────────────┘                    │   slow lanes)    │
   │ SoftAck (own)                     └──┬───────────────┘
   │ BlockBrief                            │ SoftAck (remote)
   ▼                                       ▼
┌─────────────────┐                    ┌─────────────────┐
│ ConsensusActor  │ ◀── SoftAck ───────│  (remote acks   │
│ (aggregator)    │     remote         │    inbound)     │
└──┬──────────────┘                    └─────────────────┘
   │ Block.SoftConfirmed
   ├──▶ BlockWeaver        (drives next-block decision)
   ├──▶ StackComposer      (slow-side gate for stack closure)
   ├──▶ JointLedger        (proxies the confirmation to the L2 ledger)
   └──▶ PeerLiaisons       (broadcast; inert now, kept for Final-block bookkeeping)
```

### `JointLedger` (`multisig/ledger/joint/JointLedger.scala`)

Block producer (leader role) and L2 executor (every peer). On local block completion
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

### `FastConsensusActor` (`multisig/consensus/ConsensusActor.scala`)

Soft-ack aggregator. Inputs:
- own `SoftAck` from `JointLedger` (leader's path), or own `SoftAck` from local signing
  (follower's path after verifying the incoming brief).
- remote `SoftAck` from `PeerLiaisons`.
- `BlockBrief.Next` from `JointLedger` and from `PeerLiaisons` (peer-relayed).

Verifies each soft-ack's signature against the brief's `signingBytes` and accumulates per
`blockNum`. When all head peers' acks are present, emits `Block.SoftConfirmed` to:
- `BlockWeaver` — frees the next-block decision.
- `StackComposer` — paired with the corresponding `BlockResult` to mark the block
  stackable.
- `JointLedger` — proxies the confirmation to the L2 ledger
  (`L2LedgerCommand.ProxyBlockConfirmation`), advancing its confirmed state.
- `PeerLiaisons` — broadcast outbound. Currently inert (per-remote outbox pruning moved to
  the `GetMsgBatch` receipt signal); the receive case is kept for Final-block bookkeeping.

Also broadcasts the local peer's own `SoftAck` to `PeerLiaisons` for outbound delivery.

### `BlockWeaver` (`multisig/consensus/BlockWeaver.scala`)

Decides what the next block looks like — per-block leader/follower mode switch via
`isLeader(nextBlockNum)`. Gates new block production on:
- soft-confirmation of the previous block (wraps `Block.N+1` once `Block.N` is
  soft-confirmed),
- mempool pressure / dead-man timer,
- deposit maturity decisions.

The leader instructs `JointLedger` to produce `BlockBrief.Next`. Followers reproduce the
same brief locally from the same inputs (deterministic) and sign it.

### `PeerLiaison` fast lanes (`multisig/consensus/PeerLiaison.scala`)

One liaison per directed remote peer. Carries three fast-lane payloads per link, plus the
slow lanes (`stackBrief`, `hardAck`) covered in the slow-consensus doc:

| lane | family | producer |
|---|---|---|
| `softAck` | next-expected, contiguous per peer | every peer (every block) |
| `blockBrief` | last-seen, sparse round-robin | only `isLeader(blockNum)` |
| `request` | next-expected, contiguous per peer | every peer (user requests) |

The full per-lane batch-protocol summary lives as a `// format: off` table just above
`final case class GetMsgBatch` in `PeerLiaison.scala`. See that table for prune / verify /
advance per-lane invariants.

> **TODO (merge `feature/migrate-kzg`):** that branch unified ALL lanes to **next-expected**
> — `blockBrief` (and the slow-side `stackBrief`) become "sparse but next-expected" (cursor
> precomputed to the next leader block via `nextLeaderBlock`), no longer last-seen. When that
> change lands here, update the `blockBrief` row above and the "Per-link block stream" section
> below (and `slow-consensus.md`'s `stackBrief` row) from last-seen → next-expected.

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
lane's `PeerLiaison` cursor handles this with **last-seen** semantics keyed on the remote's
leader schedule — `verifyAgainst` checks `received.blockNum == remotePeerId.nextLeaderBlock(current.blockNum)`,
not `current.increment`. Block 0 is the config-bootstrapped block and is never sent over
this channel.

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
