# Persistence & Crash Recovery

**Status:** Draft · **Whitepaper milestone:** M5 — Feature Complete MVP (2026 May)
· **Branch baseline:** post `feature/slow-consensus` + `migrate-kzg` + `rate-limiter` merge

> [!NOTE]
> The whitepaper does not yet have a dedicated persistence article. The only
> reference is the roadmap: M5 plans to *"add coil consensus, TDX privacy
> features, persistence for consensus data, and crash recovery procedures"*
> ([/gummiworm-whitepaper/introduction/roadmap](https://)). This spec proposes
> the design and flags decision points for review; promotable content should be
> folded back into the whitepaper (likely a new article under
> `single-head-gummiworm-protocol/peer-network`).

> [!NOTE]
> **What is M5?** M5 is the fifth milestone on the Gummiworm roadmap —
> *"Feature Complete MVP"* (2026 May). It rounds out a suitable initial
> minimum-viable, production-grade product for a single Gummiworm head by adding
> four things: **coil consensus**, **TDX privacy features**, **persistence for
> consensus data**, and **crash-recovery procedures**. This spec covers the last
> two. Roadmap context: M4 *Specification* (2026 Apr) precedes it; M6 *Hardening*
> (2026 Jun — testing, then the duration-bounded Sugar Rush launch on a curated
> coil set) follows. M5 is the point at which a single head is feature-complete
> enough to be hardened for production.

---

## 1. Goal & scope

A Gummiworm head peer must survive a process crash, host reboot, or
controlled restart **without losing custody safety and without violating
consensus invariants**. After a restart the peer rejoins the running head,
catches up, and resumes producing/verifying blocks and effects as if it had only
been briefly unresponsive (within the head's inactivity/silence timing budget —
see [timing-rules](https://) `#fallback`).

### In scope (M5)

- Durable storage for the **consensus data** a single peer produces and observes
  (its slice of the replicated set, signing material, derived-state snapshots).
- **Recovery from local persistence only.** On boot a peer reconstructs its
  crash-time state entirely from its own disk — no recovery-specific data request
  to other peers (see Out of scope). Two durability barriers make this sound:
  *write-before-advance* on the receive path (CR8) and *write-before-send* on the
  send path (CR4). After local state is restored the peer reconnects and the
  normal `GetMsgBatch` protocol resumes ordinary forward progress — liveness, not
  a recovery backfill.
- **Equivocation avoidance:** a recovered peer must never re-assign a request ID
  to a different request, never sign two conflicting blocks/effects at the same
  index, and never regress a monotonic counter.
- Crash points covered: between any two actor messages, mid-stack,
  mid-signing-round, after L1 submission but before observing settlement.

### Out of scope (later)

- **Peer-assisted recovery / backfill.** Reconstructing lost local state by asking
  peers to re-send it. Recovery is self-sufficient from local disk; peers are
  **never** a durability backstop. The normal `GetMsgBatch` still runs
  post-recovery for forward progress — what is excluded is any recovery-specific
  re-send path.
- **Coil peers, entirely.** This spec is written for **head peers only**. Coil
  consensus is a separate M5 workstream — the **coil-ready peer node-type** is
  specified in `coil-network.md`; coil-side persistence + recovery will be
  added as a §11 here later.
- TDX sealed storage / encryption-at-rest (the M5 TDX workstream; this spec
  assumes a pluggable "sealed blob" primitive).
- Backup/restore tooling, multi-host replication.
- Rule-based-regime persistence beyond what is needed to *enter* it cleanly
  (`rulebased/persistence/Persistence.scala` is a stub; unify the storage
  interface, defer the rule-based payloads).

---

## 2. Two kinds of actor

The single most useful distinction for recovery: every actor is either a
**boundary actor** or a **consensus actor**, and the two recover by completely
different mechanisms.

| | **Boundary actors** | **Consensus actors** |
|---|---|---|
| Members | RequestSequencer, CardanoLiaison, PeerLiaison | BlockWeaver, JointLedger, StackComposer, FastConsensusActor, SlowConsensusActor |
| Sit at | an external surface: users / L1 / peers | the deterministic interior |
| Own exclusively | the **network** (PeerLiaison) and the **chain** (CardanoLiaison) — the only ways an effect escapes the process | the deterministic fold; they regenerate inter-actor signals when replayed |
| Recover by | **state restoration** (load cursors/counters, re-observe L1) | **base-state seed + replay** — load the base snapshot at the side's ack mark (`softAcked` / `hardAcked`), then the `ReplayActor` re-runs the input tail from that mark + 1 |

Each boundary maps to exactly one external surface: **RequestSequencer** = the
user-facing boundary (requests in), **PeerLiaison** = the other-peers boundary
(messages in/out), **CardanoLiaison** = the Cardano-L1 boundary (effects out,
observations in).

**Why this split is the keystone — and what each side actually touches.** It is
what makes replay (§5) safe, but the line is **not** "only boundaries touch the
store, clock, and chain." Per resource:

- **Network (peer wire) and chain (L1) — boundary-exclusive.** `PeerLiaison` is
  the only network touch, `CardanoLiaison` the only chain touch. These are the
  only ways an effect *escapes the process*, so duplicate-suppression during
  replay is needed at exactly these two places — and it is the **same
  cursor / equivocation filter the boundaries already run in steady state**: a
  re-emitted output `≤` the boundary's cursor is dropped just as a live duplicate
  would be (§5). That is why all actors can start **together** (§5) — the filter,
  not a deferred boundary start, is what keeps replay re-emissions off the wire / chain.
- **Durable store — written by both.** Boundaries persist what crosses them
  (`PeerLiaison`: inbound lane entries (CR8), each value carrying a 12-byte
  `ArrivalStamp` prefix; `RequestSequencer`: assigned requests, CR1/CR4). Consensus
  *producers* persist their authoritative
  **lane outputs once, at creation** (CR4) — `JointLedger` its block briefs +
  soft-acks, `StackComposer` its stacks + hard-acks — and re-save their **passive
  snapshots on cadence**: `JointLedger` the deposits map on every soft ack (§5.2),
  `StackComposer` the treasury / evacuation-map snapshot. Lane records are keyed
  by lane identity, so re-writing one during replay is a harmless overwrite
  (atomic per CR6).
- **Clock — read by consensus too.** `BlockWeaver` stamps block-creation
  start/end times (`realTimeQuantizedInstant`), and `CardanoLiaison` times its L1
  observations. Neither is boundary-exclusive, and both are handled explicitly:
  BlockWeaver uses the **live wall clock** on replay — blocks `≤ softAcked` are not
  re-cut (their times sit in the persisted brief headers) and an in-flight block is
  re-cut fresh (§6) — while CardanoLiaison's clock use is handled by **re-sampling
  L1 live** (§5.5).

So the split rests on two facts, not on a persistence monopoly: **(1)** the only
surfaces an effect can escape through (network, chain) are boundary-owned, so
duplicate-suppression during replay is needed at just those two places; and
**(2)** the recovery *mechanism* differs — the deterministic interior must be
**replayed** to regenerate its inter-actor signal cascade (a bare state-restore
cannot), whereas the boundaries hold directly-restorable state.

---

## 3. Consensus data: the lanes

What the whitepaper calls a *replicated log* is **not** a single Raft-style
totally-ordered log. It is a replicated **set of single-writer lanes** — where
**single-writer means single-writer-*per-entry*** (every entry has exactly one
author; no two peers ever write the same slot), **not** one fixed author for an
entire lane. Two lane shapes realize that:

- **Satellite lanes** (RequestLane, SoftAckLane, HardAckLane) — a **fixed** single
  author owns the whole lane, keyed `(authorPeerId, type, seqNum)` and totally
  ordered within itself.
- **Spines** (BlockLane, StackLane) — one global sequence (`BlockNumber` /
  `StackNumber`) whose **sole writer rotates round-robin**: the leader for index
  *i* is the only peer that writes entry *i*, but leadership rotates across the
  lane. No single peer authors the whole spine, yet every individual entry still
  has exactly one writer (and each peer's own contribution is a **sparse**
  subsequence — gaps where others led; §3.1's "per-writer gaps, globally gap-free").

Either way the defining invariant is **one writer per entry**, so entries never
conflict and every peer eventually holds a copy of *every* entry. "Replicated" =
the set converges across peers; "single-writer" = no write-write race on any entry.
There is **no global cross-author order in the lane layer** — total order over
requests is imposed downstream by fast consensus (block briefs), not by the lanes.

Vocabulary: **lane** = a gap-disciplined sequence with **one writer per entry** (a
fixed author for satellites, a rotating leader for spines); **spine** = the two
round-robin lanes that carry consensus order (blocks, stacks); **replicated set** =
the convergent union of all lanes.

### 3.1 The five lanes

Two are **spines** (round-robin, single global index, rotating sole writer — the
only Raft-like part); three are **satellite** lanes (one independent sequence per
author) attached to a spine.

The spines are **common** — a *single* BlockLane and a *single* StackLane shared
by the whole head — while each satellite is **per author**. So a head of **N**
peers has **2 + 3·N lanes** in total (the 2 shared spines + 3 satellites for each
of N peers), and every peer eventually holds a copy of all of them. That `2 + 3N`
is the figure the recovery indices algorithm works over (§5.3).

| Lane | Writer(s) | Index key | Pacing | Gap rule | Role |
|---|---|---|---|---|---|
| **BlockLane** | head, round-robin | `BlockNumber` (one global line) | round-robin | per-writer gaps; globally gap-free | fast spine |
| **StackLane** | head, round-robin | `StackNumber` (one global line) | round-robin | per-writer gaps; globally gap-free | slow spine (bundles blocks) |
| **RequestLane** | each head peer | `(HeadPeerNumber, RequestNumber)` | author-paced | gap-free | feeds → BlockLane |
| **SoftAckLane** | each head peer | `(HeadPeerNumber, SoftAckNumber)` | coverage-paced | gap-free (one ack per block; `SoftAckNumber` coincides with the block's number) | ratifies BlockLane → soft-confirm |
| **HardAckLane** | each head peer | `(HeadPeerNumber, HardAckNumber)` | coverage-paced | cover every stack — all head peers' acks needed to hard-confirm | ratifies StackLane → hard-confirm |

Three **pacing archetypes**:

- **Round-robin-paced** (BlockLane, StackLane) — one global index, leader rotates
  by `index % nHeadPeers`; any single writer's entries have gaps.
- **Author-paced** (RequestLane) — the writer's own monotonic counter; the writer
  sets cadence.
- **Coverage-paced** (SoftAckLane, HardAckLane) — the writer must cover every entry
  of a *spine* gap-free. SoftAckLane indexes directly by the spine number (one soft
  ack per block); HardAckLane carries the same cover-every-stack obligation but is
  indexed by the author's own counter, since one stack draws several hard acks (per
  partition / round).

Structural facts:

- **Every head peer participates in both fast and slow consensus.** There is no
  fast-only or slow-only head peer: each head peer authors a RequestLane, rotates
  as leader on *both* spines (BlockLane, StackLane), and authors *both* a
  SoftAckLane (fast) and a HardAckLane (slow). So every head peer **writes to all
  five lanes** — *sole* author of its three satellites (Request/Soft/HardAck),
  round-robin author of entries on both spines — and reads all five.
- **Dependency:** RequestLane → BlockLane → StackLane; SoftAckLane ratifies
  BlockLane; HardAckLane ratifies StackLane.

### 3.2 What gets stored, and how recovery treats it

| Datum                                                                             | Producer | Recovery treatment |
|-----------------------------------------------------------------------------------|---|---|
| User requests + assigned `RequestId`                                              | RequestSequencer (own), PeerLiaison (remote) | replayed; own assignments authoritative (CR1) |
| Block briefs                                                                      | JointLedger (own/leader), PeerLiaison (remote) | replayed; own leader briefs authoritative |
| Soft acks (sig over header)                                                       | signed by JointLedger, broadcast by FastConsensusActor; remote via PeerLiaison | replayed; own ack authoritative (CR2) |
| **Block result** (`BlockResult` CF)                                               | JointLedger | per-block JL output (state delta, deposit changes); written at own ack time, keyed by `blockNum`. Lets `StackComposer` rebuild `pending` from disk on restart without JL re-running the band. |
| **Soft confirmation** (`SoftConfirmation` CF)                                     | FastConsensusActor | header + aggregated multisig over the soft-acks, written at confirmation time, keyed by `blockNum`. `softConfirmed` **derives** as `max(SoftConfirmation.key)` — no marker key. Prunes soft-acks. |
| Stack briefs                                                                      | StackComposer (own/leader), PeerLiaison (remote) | replayed; own cut authoritative |
| Hard acks (per-effect, round-1/2/sole)                                            | signed by StackComposer, aggregated by SlowConsensusActor; remote via PeerLiaison | replayed; own ack authoritative (CR2) |
| **Hard confirmation** (`HardConfirmation` CF)                                     | SlowConsensusActor → CardanoLiaison | multisigned effects / SECs / fallbacks **in full**, written at confirmation time, keyed by `stackNum`. `hardConfirmed` **derives** as `max(HardConfirmation.key)` — no marker key. CardanoLiaison submits; evacuation reads. Prunes hard-acks. **R10 evacuation floor.** |
| Deposits map (`DepositMap` CF)                                                    | JointLedger | **snapshotted** (one keyed blob, rewritten on every own soft ack — out-of-order subset, §5.3) |
| Request high-water (`RequestHighWater` CF)                                        | JointLedger | **snapshotted** (one keyed blob — a `Map[HeadPeerNumber, RequestNumber]`, the highest request each peer has had included in a block; rewritten on every own soft ack). The RequestLane recovery cursor is this `+ 1` per peer. Snapshotted, not derived, because retention (§5.1) prunes the old briefs a brief-fold would need (§5.3). |
| Treasury (`Treasury` CF)                                                          | StackComposer | **snapshotted** (one keyed blob, rewritten on every own hard-ack stack-close — rotates per settlement / finalization) |
| Evacuation maps (`EvacuationMap` CF)                                              | StackComposer | **per-block, but only at the blocks that back an on-chain KZG commitment** — every **major** block (its post-diff map is the settlement's `nextKzg`) and every **last-of-partition minor** (the minor that gets a SEC), keyed by `blockNum`. SC folds each block's `evacuationMapDiff` onto the running map and persists the result at those blocks on every own hard-ack stack-close. The maps at other minors commit to nothing on-chain, so the rule-based dispute can never need them. KZG commitment derives from the map, not stored separately. |

Monotonic counters (own `RequestNumber`, produced-brief block numbers, own
`SoftAckNumber` / `HardAckNumber`, `lastClosedStackNum`) are not stored — they
derive from the lanes (`max + 1`). See CR3.

**Three separate artifacts straddling the two paths.** `BlockResult` and
`SoftConfirmation` are fast-side (produced under fast-consensus cadence);
`HardConfirmation` is slow-side. Each lives in its own CF and is written by its
own producer at its own moment. Keeping them split is what lets `softConfirmed` and
`hardConfirmed` be **derived** as `max(CF.key)` — no separate marker storage —
while still giving `StackComposer` what it needs to rebuild on restart:

- **`BlockResult` (JL, at ack time).** Per-block JL output, keyed by `blockNum`.
  Written in JL's own atomic per-soft-ack `WriteBatch` (§6) alongside the soft-ack
  / brief / deposits-snapshot. On restart, `StackComposer` loads `BlockResult`s
  for `(StackLane[hardAcked].lastBlockNum, head]` from disk to rebuild its
  `pending` map — JL itself anchors at `softAcked` and does **not** re-run earlier
  blocks.
- **`SoftConfirmation` (FCA, at confirmation time).** Per-block header +
  aggregated multisig over the threshold-met soft-acks, keyed by `blockNum`.
  Written in FCA's own atomic batch alongside the soft-ack prune. Soft-ack
  signatures are not retained after this — nothing downstream needs them (slow
  side derives stacks from `BlockResult`s; dispute uses hard SECs).
- **`HardConfirmation` (SCA, at confirmation time).** Per-stack multisigned
  effects / SECs / fallbacks **in full**, keyed by `stackNum`. CardanoLiaison
  submits the effects; evacuation reads the SECs. Written in SCA's own atomic
  batch alongside the hard-ack prune. The **R10 evacuation floor** — never
  deleted (§5.1 retention rule).

Storing these as separate artifacts decouples three concerns: when JL writes
(per ack), when FCA writes (per confirmation), and when SCA writes (per
confirmation). It also gives each marker a clean derivation source: scan one CF's
last key, no separate marker record to keep consistent.

---

## 4. Correctness requirements

- **CR1 — No request-ID equivocation.** After recovery the peer must never emit
  `(ownPeerId, n)` bound to a different request than the one already broadcast at
  `n`. ⇒ own `RequestNumber` high-water + the request bytes for every assigned `n`
  must be durable **before the user is told the id** (not merely before broadcast).
  (Defends the *lying-peer proof*, [anti-censorship](https://) `#the-lying-peer-proof`.)
- **CR2 — No conflicting signatures.** Never two soft acks for different headers
  at one block number, nor conflicting hard acks at one stack/partition index.
  ⇒ the equivocation marker lives at the **peer boundary** — "what signature
  crossed the wire" is a PeerLiaison fact. The signer (JointLedger/StackComposer)
  *computes* the signature deterministically (Ed25519 ⇒ re-derivation is not
  equivocation); PeerLiaison's cursor gates what actually leaves. (Structurally a
  slashing-protection database.)
- **CR3 — Monotonic counters never regress.** Own `RequestNumber`, produced-brief
  block numbers, own `SoftAckNumber` / `HardAckNumber`, `lastClosedStackNum`. These are a
  *corollary* of CR4 + recover-as-`max(lane)+1`, not separate durable records:
  anything that left the process is durable, so `max+1` never reissues.
- **CR4 — Write-before-send.** Any datum that, once observed externally, binds
  this peer is persisted **before** the corresponding message leaves a boundary
  (to a `PeerLiaison` outbox, to the user, or to L1).
- **CR6 — Crash-atomicity.** A crash mid-write leaves the store consistent: a
  record is fully present or fully absent (one atomic `WriteBatch`, §7).
- **CR7 — Recovery within the timing budget.** Total downtime (crash → restart →
  caught-up) must fit the head's inactivity/silence margin, or the peer must fail
  safe (stay down / signal evacuation) rather than rejoin stale.
- **CR8 — Write-before-advance (receive path).** An inbound lane entry is durably
  stored **before** the lane's receive cursor advances past it. The cursor never
  points beyond what is on disk. Pairs with CR4: together they make recovery
  self-sufficient from local disk, with no peer-assisted backfill.

---

## 5. Recovery architecture

**Recovery is initialization against a non-empty store.** There is one boot path;
a cold start is the degenerate case where the store is empty. The same path runs
on every boot, so it cannot silently rot, and "test recovery" reduces to "restart
and observe re-convergence" (§9).

**Recovery is therefore re-entrant — a second crash mid-recovery is not special.**
Because recovery *is* the normal boot path, it makes only the same monotonic,
crash-atomic writes that live operation makes (markers/snapshots advance under
one atomic `WriteBatch`, CR6, and only ever forward, CR8/CR3), and the replay
itself is a **deterministic re-derivation** from the persisted lane tail — no
re-processing semantics to get wrong. So a crash partway through recovery
leaves the store at a **valid earlier state**, never a half-advanced one; the
next boot re-runs the identical path from wherever the anchor reached and makes
the same progress.
Each uninterrupted attempt strictly advances (or completes), so recovery converges
to the committed state after finitely many interruptions — exactly the
observational-equivalence property of §9, with the crash placed *inside* the
recovery window.

The boot **starts every actor together** — consensus and boundary alike — and
lets them run. The §2 split is by *recovery mechanism* (replay vs restore), not by
*start order*; the two mechanisms run concurrently:

1. **Consensus actors: base-state seed, then replay.** Each is seeded with its
   base snapshot at the side's ack mark (`softAcked` on the fast side, `hardAcked`
   on the slow; §5.2; empty for the pure aggregators); a central **`ReplayActor`**
   re-runs the admitted input tail through them, rebuilding state *and* the
   inter-actor signal cascade.
2. **Boundary actors restore in parallel.** Each loads its own state directly from
   the store — cursors, counters — or re-observes the outside world (L1). No
   replay; everything they need is on disk (or live on L1) at boot.

**Why start them together (and not boundary-last).** During replay the consensus
interior re-emits its outputs (acks, briefs, effects) deterministically. The
boundaries are *running* while this happens — but each boundary loads its cursor
**before** it processes anything, and every re-emitted output `≤ cursor` is dropped
by the **same cursor / equivocation filter the boundary runs in steady state**,
exactly as a live duplicate would be. A replay re-emission of something already
sent is therefore filtered; a genuinely new (`> cursor`) output is precisely what
forward progress *should* emit. So there is nothing for a separate "boundary-last"
phase to protect — the cursor filter is load-bearing either way, and deferring
boundary start **buys nothing**. We start from the simpler all-together shape and
observe; if a replay re-emission is ever found to leak past the cursor filter, we
can reintroduce a deferral surgically (§10 Q4) — but only if measurement shows it
is needed.

> **Code note (current state, 2026-05).** All-together start is exactly what the
> **existing** actor topology already does: consensus actors reference the
> boundaries through the `Connections` barrier, so the boundaries are in place when
> consensus starts. The design therefore needs **no** topology rearrangement — it
> matches today's code (this is the simplification over the earlier boundary-last
> sketch, which would have required deferring boundary start for no benefit).

**Why replay, not just state-restore, for the interior.** Restoring an actor's
state alone misses the *signals it would have emitted on receiving its inputs*.
Example: you can restore BlockWeaver's mempool by selecting requests not yet in a
block — but BlockWeaver must also *signal JointLedger* on receiving each request,
and a bare state-restore never re-emits those signals, leaving the system torn.
Replay reproduces the whole cascade by construction, so cross-actor state can't
tear.

### 5.1 The four markers — all derived, none stored

Recovery is governed by **four markers** — two kinds (*confirmed* and *acked*),
each with a fast-side and a slow-side variant. They tell a recovering peer where
committed work ends and where its own in-flight work begins. **None of them lives
in its own CF**: each derives from a single-CF scan with no consistency burden of
its own.

| Marker | Side | Definition (and how it's derived) |
|---|---|---|
| **`softConfirmed`** | fast | The highest **soft-confirmed** block — `max(SoftConfirmation.key)` (last key in the `SoftConfirmation` CF; empty store → no soft confirmations). |
| **`softAcked`** | fast | The highest block **we soft-acked** ourselves — `max(SoftAck.softAckNum where peer == own)` (last own key in the `SoftAck` CF). Covers leader *and* follower acks (we ack every block we see, either way). |
| **`hardConfirmed`** | slow | The highest **hard-confirmed** stack — `max(HardConfirmation.key)` (last key in the `HardConfirmation` CF). |
| **`hardAcked`** | slow | The highest stack **we hard-acked** ourselves — derived from the last own entry in the `HardAck` CF (unpack the stack identifier from the value). Covers leader *and* follower acks. |

The two pairs are independent: fast and slow advance at their own pace. Within a
side the relation is **`acked ≥ confirmed`** — we ack a block / stack *before* it
confirms — so the band `[confirmed + 1, acked]` (per side) is
**acked-by-us-but-not-yet-confirmed**. Its pending confirmations complete from
late peer acks arriving live post-recovery (§6 aggregator).

**Each marker is the conceptual anchor for one consensus actor**, even though no
marker key is stored:

| Marker | Anchors | What that actor reads at boot |
|---|---|---|
| `softAcked` | **JointLedger** | the **`DepositMap`** CF (one keyed blob — the deposits map as of our last own soft ack). `previousBlockHeader` is reloaded from `BlockLane[softAcked].brief`. JL is at `Done(softAcked)`; SC's `pending` rebuild is **not** JL's burden — SC loads `BlockResult`s directly (next row). |
| `hardAcked` | **StackComposer** | **`Treasury`** (one blob — cumulative treasury UTXO ref) + **`EvacuationMap`** (keyed by `blockNum`, written only at committed blocks — major / SEC minor; load `EvacuationMap[StackLane[hardAcked].lastBlockNum]` for the current map — that last block is always a committed one for a non-final stack, §5.2). Counters like `lastClosedStackNum` (= `hardAcked`), `lastClosedBlockNum` (from `StackLane[hardAcked].brief`), and `nextOwnHardAckNum` (= `max(own HardAckLane) + 1`) are derived. SC additionally reads `BlockResult` for `(StackLane[hardAcked].lastBlockNum, head]` to rebuild `pending`. |
| `softConfirmed` | **FastConsensusActor** | **Nothing of its own.** Cells `≤ softConfirmed` were dropped the moment they produced their `SoftConfirmation` record; cells `> softConfirmed` are in-flight and rebuilt by replay. |
| `hardConfirmed` | **SlowConsensusActor** | **Nothing of its own.** Same shape: cells dropped at confirmation; in-flight tail rebuilds from replay. |

So the signers / ledgers (JointLedger, StackComposer) read non-trivial passive
state at their `*Acked` mark; the aggregators are **genuinely stateless across
their `*Confirmed` mark**. Re-aggregating the in-flight band is safe because
the aggregators sign nothing — replay just rebuilds the cells as it re-feeds
the band's briefs + acks.

**Replay resumes at the right offset for each actor:**

- **Signers / ledgers** resume at `*Acked + 1` (CR2 — no re-signing what we
  already signed). JointLedger picks up via BlockWeaver's re-drive from
  `softAcked + 1` (§6); StackComposer from `hardAcked + 1`.
- **Aggregators** resume at `*Confirmed + 1` (per §10 Q9 option a) — their cells
  reconstitute as the replayed briefs + acks land. Anything `≤` the confirmed
  mark has already become a `SoftConfirmation` / `HardConfirmation` record and no
  longer needs re-aggregation.
- **BlockWeaver** holds no persistent state; it rebuilds mempool +
  `nextBlockNumber` from the replay tail starting at `softAcked + 1` (driven by
  JointLedger's signed timeline, not by confirmations).

The `2 + 3N` per-lane resume cursors that the `ReplayActor` uses to seek into
the store **derive from the markers** — which derive from CF scans — with no
extra per-lane or per-marker storage (§5.3).

Recovery never *writes* a marker (there is none to write); it derives them at
boot and uses them to drive replay + runtime invariants — ack-pruning, late-ack
discard, the disk-retention floor — which recovery simply inherits. The most
important of those, the retention floor, is detailed in the compaction note
below.

> **Three senses of "compaction" — keep them distinct.**
> 1. **Confirmation write (what writing a `SoftConfirmation` / `HardConfirmation`
>    record triggers).** When N acks aggregate into a confirmation, the aggregator
>    writes the confirmation record (its CF's last key advances ⇒ the marker
>    advances) and drops the now-redundant *per-ack signatures* from `SoftAck` /
>    `HardAck`. The confirmation record itself is **retained**, not deleted — for
>    hard-confirmation it is the full multisigned effects / SECs, i.e. the
>    **R10 evacuation floor**. This prunes *acks*, never the committed region.
> 2. **The recovery base (a logical compacted view).** The base snapshot (§5.2)
>    is a compacted *image* of state at the side's ack mark; the `ReplayActor`
>    replays the entries after it. A read-time construction over a copy — it
>    deletes nothing from the live store.
> 3. **RocksDB native compaction (§7)** — the storage engine merging SST files.
>    Engine-internal, unrelated to either.
>
> **Hard rule:** physical deletion from the durable store **never descends below
> the latest hard-confirmed stack's last block** — that region holds the
> evacuation-critical state (R10). Soft-confirmation only prunes the soft-acks it
> subsumes (blocks themselves stay, the slow side still needs them);
> hard-confirmation is the physical-retention floor.

### 5.2 State recovery: the base snapshots

Replay starts from **per-side passive snapshots at the ack mark**: the fast
snapshots live in `DepositMap` + `RequestHighWater`, the slow in `Treasury` +
`EvacuationMap`. Each snapshot is rewritten in the same atomic `WriteBatch` as its
side's own ack write (deposits + request high-water per own soft ack, treasury +
evac per own hard-ack stack-close), so the snapshotted state is always exactly
aligned with the side's `*Acked` mark and can never be torn from it.

Snapshots carry **only the non-derivable** passive state on each side:

- fast — **`DepositMap`** + **`RequestHighWater`** CFs (JointLedger):
    - `DepositMap` — one keyed blob, the deposits map at `softAcked`. JL is at
      `Done(softAcked)` after restore — `previousBlockHeader` reloads from
      `BlockLane[softAcked].brief`, deposits come from `DepositMap`, nothing else
      needed for JL's own state.
    - `RequestHighWater` — one keyed blob, a `Map[HeadPeerNumber, RequestNumber]`
      giving each peer's highest included request as of `softAcked`. It is *not*
      JointLedger state; it feeds the RequestLane recovery cursors (§5.3), rewritten
      here so it stays aligned with `softAcked` and survives brief pruning.
- slow — **`Treasury`** + **`EvacuationMap`** CFs (StackComposer):
    - `Treasury` — one keyed blob, the cumulative treasury UTXO ref at
      `hardAcked`. Overwritten in place — there's no rule-based scenario that
      needs an older treasury, so a single snapshot suffices.
    - `EvacuationMap` — keyed by `blockNum`, written at each own hard-ack
      stack-close, but **only at the blocks whose map backs an on-chain KZG
      commitment**. SC folds each block's `evacuationMapDiff` onto the running
      map and persists the result at exactly those blocks:
        - every **major** block — its post-diff map is the commitment the
          settlement tx carries as `nextKzg`;
        - every **last-of-partition minor** — the minor that gets a SEC
          (the standalone evacuation commitment), whose KZG is the map after
          that minor's diff.

      No other map is persisted. The rule-based dispute resolves against one of
      these on-chain commitments (a settlement's `nextKzg`, or a SEC), and
      evacuation needs the map *behind that commitment* — so the only maps it can
      ever need are the ones we keep. The maps at intermediate minors commit to
      nothing on-chain, so they are dead weight (this corrects the earlier
      "store every block" sketch — those extra maps were useless). Pruning is
      still bounded: anything strictly older than the last-hard-confirmed major
      can be dropped, since those minors can never be disputed against once the
      next major supersedes them. The set of committed blocks mirrors
      `StackEffectsBuilder.mkEffectsRegular`'s settlement / SEC logic exactly
      (`StackComposer.committedBlockNums`).

      **Load invariant:** the recovering SC reads the cumulative map at
      `hardAcked` as `EvacuationMap[StackLane[hardAcked].lastBlockNum]`, and
      that key is always present — the last block of any **non-final** stack is
      necessarily a major or a last-of-partition minor (a leading minor run that
      reaches stack end ends in its own SEC minor; any later partition is a major
      or the final). A **final** stack drains the map to empty and has no
      successor stack, so no cumulative map is loaded after it.

  SC's pairing maps (`pending` / `ready`) and counters rebuild from lanes +
  `BlockResult` (§6 StackComposer).

The snapshots do **not** carry per-lane cursors (those are derived, §5.3), do
**not** carry the acked-but-unconfirmed band (those briefs / acks are still in
the lane CFs above the side's confirmed mark — unpruned), and do **not** carry
SC's `pending` map (rebuilt from the `BlockResult` CF on restart — JL writes
each block's result at ack time, §3.2, §6 JointLedger, so SC can load
`(StackLane[hardAcked].lastBlockNum, head]` directly). The pending soft / hard
confirmations themselves complete in the **aggregators**
(`FastConsensusActor` / `SlowConsensusActor`) from the persisted briefs +
peers' late acks — the aggregators sign nothing, so they can re-acquire and
re-aggregate the band freely, unlike the signers (§10 Q9).

### 5.3 The indices algorithm: deriving the 2 + 3N lane cursors

To replay, the `ReplayActor` needs an initial cursor for each lane: **2** for
the shared spines (one BlockLane, one StackLane) **+ 3 per head peer** (its
Request / Soft-ack / Hard-ack satellites) = **2 + 3N** (§3.1). Every one of
them **derives from the markers** via Hydrozoa's monotonicity invariants:

- **Requests** — monotonic per peer: *if a block includes Peer A's Request `N`,
  the next block includes only A's requests `> N`.* So A's RequestLane cursor is
  A's highest-ever included request `+ 1`. This high-water is **persisted as a
  small per-peer counter** (one entry per head peer, written as blocks are
  produced) and read directly at boot — *not* recomputed by scanning briefs.
  Folding briefs is unsound: to get *every* peer's high-water you must scan back
  until each peer's latest included request appears (worst case to block 0), and
  once retention (§5.1) prunes old briefs below the hard-confirmed floor they are
  gone. The counter sidesteps both — `N` reads, immune to pruning. (This is the one
  request-side datum that *is* stored; the lane cursors themselves still derive
  from the markers.)
- **Blocks / stacks (the spines) — two floors each, one per consumer role.** Each
  spine is read by **two** roles: the aggregator (FCA / SCA), which resumes at the
  `*Confirmed` mark `+ 1` to rebuild in-flight confirmation cells, and the
  re-deriving ledger (BlockWeaver → JointLedger / StackComposer), which resumes at
  the `*Acked` mark `+ 1` to re-produce / re-close what it has not yet signed. So a
  spine has a **`confirmed` floor** and an **`acked` floor**, with
  `confirmed ≤ acked`. The `ReplayActor` scans the spine once from the lower
  (`confirmed`) floor and feeds the `acked` consumer only the suffix `≥ acked` —
  **the one place the no-re-sign-`≤ *Acked` rule (CR2) lives**, so no consensus
  actor re-filters for it. The fast-side floors are both marker-derived
  (`softConfirmed`; `softAcked`, whose `SoftAckNumber` coincides with the block
  number, §3.1). On the slow side `hardConfirmed` is marker-derived but the **acked
  stack** is *not* — `hardAcked` is a `HardAckNumber` counter, not a `StackNumber`,
  so the acked-stack floor is sourced separately (unpack the stack id from the last
  own `HardAck` value; §10 Q9).
- **Soft acks** — single consumer (FCA). The soft-ack index *coincides with the
  block number* (one ack per block, §3.1), so each peer's SoftAckLane cursor is the
  fast-side confirmed mark `+ 1`, exactly the BlockSpine `confirmed` floor.
- **Requests** — single consumer (BlockWeaver), at the per-peer high-water `+ 1`
  (the persisted counter above).
- **Hard acks** — single consumer (SCA), and the **one lane whose floor does *not*
  derive from a marker.** The HardAckLane is indexed by the author's own
  `HardAckNumber`, not by `StackNumber`, and no marker records the
  `StackNumber → HardAckNumber` correspondence — so there is no marker-findable
  floor. Recovery scans each peer's HardAckLane **from 0** (correct, not minimal).
  Tightening needs a per-peer hard-ack marker or a stack-indexed hard-ack lane;
  deferred (§10 Q9).

So the cursors split into **dual-floor spines + single-floor satellites** (`4 + 3N`
floors over `2 + 3N` lanes). All derive from the markers + the request high-water
counter, except the slow-side acked stack and the HardAck floor (the hard-ack-lane
indexing gap, §10 Q9). None of the cursors is stored.

**The deposits map is the exception — and it is not a lane**, so it has no cursor
to derive. The pending-deposits map mutates on *any* block (a request *adds* a
deposit; a major block *absorbs* an out-of-order **subset**), so it is neither
constant nor a marker-findable suffix and cannot be reconstructed from the lanes.
It is carried instead as **snapshotted passive state** (§5.2), re-written on every
own soft ack — the one place a snapshot is unavoidable on the fast side.

### 5.4 Total order of the replayed streams

Within a lane, order is intrinsic — the lane's own index (RequestNumber;
Block/StackNumber; ack index). To order entries *across* the `2 + 3N` streams,
**every persisted lane value carries a 12-byte big-endian `ArrivalStamp` prefix**
— a `(generation, monotonic)` pair (receipt time for inbound entries, creation for
own ones — §7.1); merging by stamp yields a fixed, durable interleaving — "not
canonical, but correct."

The stamp must stay comparable **across restarts**, which a bare process-monotonic
clock is not (its zero resets each boot). So it pairs a **`generation`** — a
per-process counter persisted in `Cf.Meta` and bumped once at store-open — with
`IO.monotonic` *within* the process: the generation separates restarts (a later
process always sorts after an earlier one), and monotonic orders within one
(strictly increasing, ns resolution, never stepping backward like a wall clock).
Encoded big-endian `[generation : 4][monotonicNanos : 8]`, so raw byte order is
exactly `(generation, monotonic)` order.

That interleaving is the *one that actually happened*, hence already causally valid
(nothing was delivered before its prerequisites — it physically couldn't be). The
consensus tolerates other interleavings too — that robustness is what mechanism (1)
leans on (§5.6) — so the recorded order isn't strictly required for committed-state
correctness. It is kept as a **known-good serialization**: a safety net if
robustness is imperfect, the order the one-by-one oracle (§5.6) replays, and a
faithful record of the run.

**The stamps order streams; they are not a clock.** Time-dependent decisions do
*not* replay off them — block-cut timing uses the live wall clock, with committed
blocks keeping the times in their persisted brief headers and in-flight blocks
re-cut fresh (§6).

### 5.5 The L1 boundary is re-sampled live, not replayed

Not every input is replayed from the log. The **L1 boundary** (CardanoLiaison
poll results) is **re-acquired live at replay time**, and the result may differ
from the pre-crash value — which is correct:

- **Below the confirmed marks (`≤ softConfirmed` and `≤ hardConfirmed`):**
  committed; loaded from the materialized confirmations, never re-verified
  against fresh polls. No agreed block / stack is reverted because L1 moved.
- **Above the confirmed marks (the in-flight tail):** re-verified against
  *current* L1 truth. If a deposit seen pre-crash is gone now, the peer
  legitimately fails to confirm, diverges, maybe falls back — "verify the
  block / stack NOW, not back then." That is the protocol reacting to reality,
  not a replay defect.

Consequence: the L1 boundary contributes **nothing** to the replay log — it is a
live source, not a recorded one. (A soft-confirmed-then-reorged deposit is caught
by the hard side / fallback, not by re-verifying soft confirmations.)

**The `ReplayActor` performs the initial L1 sample itself — it does not wait on
CardanoLiaison.** BlockWeaver (and hence JointLedger's deposit decisions) needs a
`PollResults` — the set of utxos at the head's multisig address — to make
forward progress, and after a restart it has none. CardanoLiaison *will* poll and
forward one on its own cadence (its poll tick / next-effect trigger), but there is
**no guarantee it does so promptly**, and replay must not stall waiting on the
boundary's timer. So the `ReplayActor` reads the L1 state **directly from
`CardanoBackend`** (`utxosAt(config.initializationTx.treasuryProduced.address)` →
the utxo ids) and sends a `PollResults` straight to BlockWeaver at replay start —
byte-for-byte the same message CardanoLiaison emits in steady state
(`CardanoLiaison.runEffects` → `blockWeaver ! PollResults(...)`), just sourced
eagerly by recovery instead of on the poll timer. Subsequent live `PollResults`
from CardanoLiaison flow normally once it is running. This is still the **live**
L1 read of §5.5 ("re-sampled live, not replayed") — merely *initiated by* the
`ReplayActor` rather than awaited from the boundary — so it carries the same
above/below-the-confirmed-mark semantics.

### 5.6 The replay mechanism

**Replay re-feeds lanes, and only lanes.** The replicated set's lane entries
(§3.1) are the sole replay input; every *non-lane* input is reproduced, not
replayed — inter-actor signals (`StartBlock`/`CompleteBlock`, the soft-confirm
fan-out, `GetState`) regenerate from the interior cascade, and L1 poll results
are re-sampled live (§5.5) — with the `ReplayActor` seeding the first
`PollResults` into BlockWeaver straight from `CardanoBackend`, rather than waiting
on CardanoLiaison's poll cadence. So an actor whose inputs are *all* non-lane signals
gets base state but no tail — JointLedger, for one, reads no lane (BlockWeaver
drives it), so the `ReplayActor` never feeds it (§6). This holds for either
mechanism below.

Two candidate mechanisms for *how* to feed that lane tail; we target **(1)** and
keep **(2)** as a test oracle.

1. **Pre-populate mailboxes ("crash as a state").** Create the consensus actors
   suspended, seed each with its passive base state, and drop the total-ordered
   **lane-entry tail** into the mailboxes of the actors that read those lanes (e.g.
   BlockWeaver the request lanes; the ack-aggregators the ack lanes). Then open
   the start barrier and let them run concurrently to the crash state. Fast,
   reuses the existing `Deferred[Connections]` barrier (though a **separate
   suspend barrier** may be needed to hold actors during seeding — §8, §10 Q4),
   and unifies replay with normal operation (cold boot = the empty-seed case).
   Boundary actors start *together* with the consensus actors (§5); during replay
   the interior's external re-emissions reach the running boundaries but are
   dropped by the same cursor filter that suppresses live duplicates (each
   boundary loads its cursor before processing anything; §5). What is restored is
   the **base snapshot + lane-entry tail in mailboxes**; everything else regenerates
   as the tail is processed — you don't capture internal in-flight messages.
2. **One-by-one + quiescence** *(test oracle, not for production)*. The `ReplayActor`
   sends one input, waits for the system to settle, then the next. A single
   deterministic serialization, easy to reason about — but slow, and it needs a
   global quiescence oracle (cheap under TestControl via the cede-settle trick,
   awkward on the live runtime). **Use it as the reference oracle:** replay
   deterministically one-at-a-time in a test, assert the concurrent run (1) lands on
   the same committed state.

Prerequisite for (1), already required: **interleaving-robust consensus** —
concurrent replay finds *a* valid schedule; it must reach the same *committed*
state under any of them. Live operation is already concurrent message arrival
from N peers, so this holds or the consensus is wrong.

### 5.7 The recovery-priority ladder (graceful degradation)

The durable set is layered so that **restoration priority is requirement
priority**: a partial restore still buys a precise, bounded capability, with
custody safety as the non-negotiable floor.

| Restore through | Unlocks | Requirement |
|---|---|---|
| evacuation maps + SECs (the standalone evacuation commitments) + their signatures + fallbacks | fallback, vote, **evacuate** | **R10** |
| + happy-path effects | follow the happy path (TTL permitting) | **R9** |
| + stack briefs/results + soft/hard confirmation markers | realize fast-consensus decisions | **R8** |
| + block briefs + deposit maps + user requests | process known requests — **follow or lead** | R8 |

Leading needs nothing beyond the bottom rung's mempool plus the live wall clock, so
it unlocks together with following. The top rung is the custody floor — even a peer
that restored nothing else can still fallback, vote, and evacuate.

---

## 6. Per-actor recovery contracts

Each contract has four fields — **State**, **Recover** (how it is rebuilt),
**Inputs**, **Persists** — and is grouped by §2's split.

### Boundary actors — *restore only*

#### RequestSequencer

> The class is still named `EventSequencer` (rename pending — TODO in source).

- **State:** next `RequestNumber` (`nLedgerEvent`, starts `0`).
- **Recover:** `next = max(persisted own RequestWithId).requestNum + 1` (empty →
  `0`). CR3 is a corollary of CR4 here.
- **Inputs:** `UserRequest.Sync` — user-originated, droppable (crash before
  persist → user resubmits).
- **Persists:** the produced `UserRequestWithId`, **before `dResponse.complete(id)`**
  — before the user is told the id, not merely before broadcast (CR1/CR4).
  - ⚠ **Finding (current code):** it completes `dResponse` then sends, persisting
    nothing. The barrier must precede telling the user the id.

#### PeerLiaison

- **State:** per-remote cursors (the `GetMsgBatch` fields — `batchNum`,
  `requestNum`, `blockNum`, `softAckNum`, `hardAckNum`), in/out queues,
  current requesting batch.
- **Recover:** cursors via `max(persisted …)`; queues **empty**; the outbox is a
  **DB-backed view** (on `GetMsgBatch` from R, read the persisted own-produced
  prefix `[R's cursor, head]`). In steady state the queue is a write-through
  cache, so a cold cache *is* recovery — same procedure. The whitepaper already
  prunes these outboxes by **remote `GetMsgBatch` cursors**, not local
  confirmation, *"so that messages can be retransmitted if needed during recovery
  scenarios"* ([peer-network](https://) `#outbox-queues-and-confirmation`);
  persistence extends that retransmissibility across a process restart, not just a
  transient disconnect.
- **Inputs:** remote lane entries — **cursor-gated (CR8)**.
- **Persists:** inbound remote lane entries (CR8); each persisted value carries
  a **12-byte `ArrivalStamp` prefix** (§5.4, §7.1) — `(generation, monotonic)` at
  receipt. The one actor that durably stores data it did not produce.

#### CardanoLiaison

- **State:** `targetState` (`Active(treasuryUtxoId)` | `Finalized`), `effectInputs:
  Map[TransactionInput, EffectId]` (the "which effect spends this input" index),
  `happyPathEffects: TreeMap[EffectId, HappyPathEffect]`, `fallbackEffects:
  Map[BlockVersion.Major, FallbackTx]`.
- **Recover:** fold over the **`HardConfirmation` CF** (§3.2, multisigned effects /
  SECs / fallbacks in full); `targetState` seeded from
  `config.initializationTx.treasuryProduced.utxoId` until stack-0 hard-confirms;
  effects faulted in lazily. **Submission progress is recovered from L1 itself**
  (poll → observe the current treasury utxo) — L1 is the durable truth,
  re-sampled live (§5.5), so there is no own progress marker.
- **Inputs:** `Stack.HardConfirmed`, `Timeout` (poll tick).
- **Persists:** nothing of its own (signs nothing ⇒ no CR2; submits multisigned
  txs). The lazy load is `effectInputs(observedInput) → happyPathEffects(effectId)`.

### Consensus actors — *replayed*

#### BlockWeaver

- **State:** role FSM (`DecidingRole` → `Leader`/`Follower`), `mempool`,
  `nextBlockNumber`, transient `pollResults` / finalization trigger / `Wakeup`.
- **Recover (replay):** base = state at `softAcked`; replay `[softAcked + 1, head]`.
  Mempool and `nextBlockNumber` follow from the replay. **Block-creation timing
  uses the live wall clock**: blocks `≤ softAcked` are not re-led (their
  `BlockCreationStart/EndTime` already sit in the persisted brief headers), and a
  block left mid-`Producing` at the crash was never saved or acked, so BlockWeaver
  just **cuts a fresh block** under the current clock — a never-committed cut may
  legitimately differ, and only committed state must match (§9). The transient
  `pollResults` is seeded **by the `ReplayActor`**, which reads L1 directly from
  `CardanoBackend` and sends the first `PollResults` rather than waiting on
  CardanoLiaison's poll cadence (§5.5) — so deposit decisions can proceed
  immediately on replay.
- **Inputs:** `UserRequestWithId`, `BlockBrief.Next`, `Block.SoftConfirmed`,
  `PollResults` (the first one from the `ReplayActor`'s direct `CardanoBackend`
  read; later ones live from CardanoLiaison — §5.5), `Wakeup`, finalization trigger.
- **Persists:** nothing — authors no lane; drives `JointLedger` via
  `StartBlock`/`CompleteBlock{Regular,Final}`, which authors the brief.

#### JointLedger  *(snapshot-bearing)*

Post-split, owns only the fast-side state; treasury moved to StackComposer.

- **State:** `Done(previousBlockHeader, deposits)` | `Producing(previousBlockHeader,
  deposits, l2LedgerState, startTime, userRequestState)`.
- **Recover:** restore `Done(previousBlockHeader, deposits)` from two sources —
  load the **`deposits` map** from the snapshot bundled with the `softAcked` ack
  (§5.1, §5.2), and reload `previousBlockHeader` from `BlockLane[softAcked]` (its
  brief is already persisted there; no need to duplicate it in the snapshot).
  Those two are JointLedger's whole passive state. (The acked-but-unconfirmed
  band `[softConfirmed + 1, softAcked]` is **not** JointLedger state: those
  blocks are already applied — JointLedger is at `Done(softAcked)` — and their
  pending soft-confirmations complete in the **aggregator**, not here; §5.2.)
  JointLedger is **not fed by the `ReplayActor`**; BlockWeaver replays its own
  tail and re-drives JointLedger via `StartBlock`/`CompleteBlock` + forwarded
  requests, reproducing the `[softAcked + 1, head]` blocks. A mid-`Producing`
  block is discarded and re-produced (a never-committed cut is safely redone).
- **Inputs (all interior signals — none from the `ReplayActor`):** from
  **BlockWeaver** — forwarded `UserRequestWithId` (deposits fold into `deposits`)
  and `StartBlock`/`CompleteBlock{Regular,Final}`; from the fast aggregator —
  `Block.SoftConfirmed.Next`. (`GetState.Sync` is a defined query with no current
  sender.)
- **Persists:** on each own soft ack, one atomic `WriteBatch` (CR4/CR6/CR8)
  spanning five CFs:
    - the own `BlockBrief` → **`Block`** CF (leader only — BlockLane author),
    - the own soft-ack → **`SoftAck`** CF (SoftAckLane author),
    - the per-block **`BlockResult`** → **`BlockResult`** CF (keyed by
      `blockNum`). Written at ack time — *not* at soft-confirmation time — so
      `StackComposer` can rebuild its `pending` map from disk on restart by
      loading `(StackLane[hardAcked].lastBlockNum, head]` directly, instead of
      relying on JointLedger to re-emit results below `softAcked` (§3.2),
    - the current **deposits snapshot** → **`DepositMap`** CF (single keyed
      blob; overwrites the previous snapshot),
    - the current **request high-water** → **`RequestHighWater`** CF (single keyed
      blob, a `Map[HeadPeerNumber, RequestNumber]`; fold this block's included
      request ids into the running per-peer max and overwrite). Feeds the
      RequestLane recovery cursors (§5.3); snapshotted here so it stays aligned
      with `softAcked` and survives brief pruning.

  Bundling deposits + request high-water here keeps both snapshots aligned with
  the `softAcked` anchor (our last soft-ack) and un-tearable from it. No marker key
  is written:
  `softAcked = max(SoftAck.softAckNum where peer == own)` derives from the
  `SoftAck` CF on restart (§5.1). Soft-**confirmation** is *not* written here
  — that is `FastConsensusActor`'s job (below). The own soft-ack's
  equivocation guard is a PeerLiaison-boundary fact per CR2; JointLedger
  computes the signature.
- **L2 co-anchoring (keyed by an L2 serial number — *not* by any ack).**
  `Producing.l2LedgerState` is WIP; the committed L2 state lives in the `L2Ledger`
  black box (its own persistence). That black box **knows nothing of acks, blocks,
  stacks, or confirmations**, so its persistence is keyed by something intrinsic to
  *its own* operation, never by a consensus marker like `softAcked`. The key is the
  L2's own **monotonic serial number** — a commit counter it increments on each
  state-mutating command. (A content hash of the evacuation map was considered and
  dropped: no cheap whole-map hash exists — only a slow `kzgCommitment` BLS pairing
  and a private per-entry blake2b — and the EUTXO ledger holds `activeUtxos` + diffs,
  not an assembled map, so a content key would cost a projection + digest per commit.
  Recovery needs only a stable per-commit key, which the serial gives for free.) The
  L2 reconstructs from `(initial state, target serial)`; the interface is
  `restoreTo(serial)`. The **consensus → L2 translation lives entirely on the
  JointLedger side**: the L2 returns its new serial on each commit, JL records per
  own soft-ack the serial for that block (durable alongside its own snapshot), and on
  recover restores `Done(softAcked)` then calls `restoreTo(thatBlock'sSerial)`. The
  co-anchoring requirement is still exact — the two must agree on the same committed
  point — but the membrane is never crossed by an ack; only a serial passes.
  - **Cost / optimization.** Full L2 state is far larger than the deposits map, so
    snapshotting it at *every* commit may be too expensive. The `L2Ledger` may
    instead snapshot **less frequently** and restore-to-serial by loading its nearest
    snapshot `≤ serial` and replaying its own command log forward to the target — the
    usual snapshot-interval-vs-replay-length tradeoff, internal to the black box. The
    mechanism is delegated / out-of-scope here; the contract is the
    `restoreTo(serial)` interface plus JL holding the soft-ack → serial mapping.

#### FastConsensusActor, SlowConsensusActor

- **Aggregators that write the confirmation record.** Each holds in-flight cells
  (per-block soft-ack cells / per-stack hard-ack cells); when a cell reaches
  threshold it writes its confirmation record in one atomic `WriteBatch` (CR4):
    - `FastConsensusActor`: write the **`SoftConfirmation`** record (block header +
      aggregated multisig over the soft-acks) to the **`SoftConfirmation`** CF
      (keyed by `blockNum`), and prune the now-redundant soft-acks for this
      block from `SoftAck`. No marker key is written — `softConfirmed` derives
      as `max(SoftConfirmation.key)` (§5.1).
    - `SlowConsensusActor`: write the **`HardConfirmation`** record (multisigned
      effects / SECs / fallbacks in full) to the **`HardConfirmation`** CF
      (keyed by `stackNum`), and prune the now-redundant hard-acks from
      `HardAck`. CardanoLiaison submits the effects from this record. No marker
      key written — `hardConfirmed` derives as `max(HardConfirmation.key)`.

  See §3.2.
- **Recover (replay):** rebuild only the **in-flight cells** above the side's
  confirmed mark — FastConsensusActor for cells `> softConfirmed`,
  SlowConsensusActor for cells `> hardConfirmed` — from briefs + acks. Re-acquire
  the acked-but-unconfirmed band (`[softConfirmed + 1, softAcked]` on the fast
  side; the slow counterpart) per §10 Q9 (signing nothing, the aggregator may
  re-aggregate freely). The aggregator does **not** reload past
  confirmation records: a cell is dropped once it confirms, so confirmations
  below the confirmed mark are not aggregator state — they were persisted for
  **downstream consumers** (CardanoLiaison folds over `HardConfirmation`,
  evacuation reads SECs), who load them. The confirmed mark alone gives the
  floor below which a late ack is ignored. The own-ack signatures belong to
  the signers (JointLedger / StackComposer), guarded at the PeerLiaison
  boundary (CR2).

#### StackComposer  *(snapshot-bearing)*

The slow-side mirror of JointLedger. It pairs each `BlockResult` (from JointLedger)
with its `Block.SoftConfirmed` (from FastConsensusActor) by `blockNum`, closes a
stack from the longest contiguous prefix of paired blocks (leader) or from exactly
the leader's announced range (follower), derives the `StackEffects` (settlement /
fallback / rollouts / SECs / post-dated refunds, threading the treasury and folding
the evacuation map for KZG), and **signs this peer's hard-acks** for the stack.

- **State** (`StackComposer.State`): the pairing maps `pending` / `ready`;
  `inboundLeaderBrief` (remote leader `StackBrief`s); `lastClosedStackNum` /
  `lastClosedBlockNum`; `previousStackHardConfirmed` (the single-flight gate —
  close `n+1` only after stack `n` hard-confirms); `nextOwnHardAckNum`; and the two
  **cumulative, non-derivable** values — **`treasury`** (the `MultisigTreasuryUtxo`
  chain, rotated on every settlement / finalization) and **`evacuationMap`** (the
  cumulative L2 evacuation map; the KZG commitment is computed from it). These two are the
  slow-side analogue of JointLedger's deposits map.
- **Recover:** load **`treasury`** from the `Treasury` CF and **`evacuationMap`**
  from `EvacuationMap[StackLane[hardAcked].lastBlockNum]` — the cumulative
  non-derivable slow-side state, as of `hardAcked`. That key is always present
  for a non-final `hardAcked` stack (its last block is always a committed block
  — §5.2 load invariant). (Older committed `EvacuationMap` entries stay on disk
  for the rule-based evacuation read — not loaded into SC state.) The
  counters are *derived*, not stored:
  `lastClosedStackNum = hardAcked`; `lastClosedBlockNum =
  StackLane[hardAcked].brief.lastBlockNum`; `nextOwnHardAckNum = max(own
  HardAckLane) + 1` (CR3); `previousStackHardConfirmed = (hardAcked ≤
  hardConfirmed)`, re-armed when that stack's `Stack.HardConfirmed` arrives /
  loads. `pending` rebuilds **directly from disk** by scanning the
  **`BlockResult`** CF over `(StackLane[hardAcked].lastBlockNum, head]` (JL
  wrote each block's result at ack time, §6 JointLedger); `Block.SoftConfirmed`s
  for the band come from FCA's in-flight replay. `inboundLeaderBrief` comes
  from the replayed **StackLane**. Then close stacks `> hardAcked` — never
  `≤ hardAcked` (no re-signing, CR2).
- **Bootstrap vs recovery (one seam, two seeds).** On a **cold** store `PreStart`
  runs `bootstrapInitialStack`: stack 0's init + fallback are derived from `config`
  (no `StackBrief` is broadcast — every peer derives it identically), seeding
  `treasury` / `evacuationMap` from `config.initializationTx.treasuryProduced` /
  `config.initialEvacuationMap`. On a **non-empty** store that genesis seed is
  replaced by the loaded snapshot, and stack 0 — long since hard-confirmed — is
  **not** recomposed. (Cold start is the degenerate case, §5.)
- **Inputs:** `BlockResult` (JointLedger), `Block.SoftConfirmed` (FastConsensusActor),
  `StackBrief` (remote leader, via PeerLiaison — the StackLane), `Stack.HardConfirmed`
  (SlowConsensusActor — arms the next close).
- **Persists:** at each stack close, one atomic `WriteBatch` (CR4/CR6/CR8)
  spanning four CFs:
    - the own `StackBrief` → **`Stack`** CF (leader only — StackLane author),
    - this peer's `HardAck`s for the stack (round-1 + round-2, or the sole ack)
      → **`HardAck`** CF (HardAckLane author),
    - the rotated **`treasury`** as of that close → **`Treasury`** CF (single
      keyed blob; overwrites the previous),
    - **one `EvacuationMap` entry per committed block** (each major + each
      last-of-partition SEC minor) of the closing stack's prefix, keyed by
      `blockNum` → **`EvacuationMap`** CF. SC walks the prefix from the prior
      `hardAcked` map, folds each block's `evacuationMapDiff`
      (`BlockResult.evacuationMapDiff`), and persists the running map only at
      the committed blocks — the ones whose map backs an on-chain commitment
      (`StackComposer.committedBlockNums`, mirroring
      `StackEffectsBuilder.mkEffectsRegular`). The intermediate minors' maps
      back nothing on-chain, so the dispute can never need them (see §5.2).

  Bundling the two snapshots with the acks keeps them aligned with the
  `hardAcked` anchor — exactly as deposits ride JointLedger's soft-ack write.
  The `StackEffects` themselves are **not** persisted — they re-derive from
  the prefix + `treasury` + `evacuationMap` (`StackEffectsBuilder`).
  Hard-**confirmation** is *not* written here — it is `SlowConsensusActor`'s
  output (above). Round-2 is signed at close but released by SlowConsensusActor
  only after local round-1 confirmation, so it must be durable **before** that
  release (CR4). The own hard-ack equivocation guard is a PeerLiaison-boundary
  fact per CR2; StackComposer computes the signatures (`EffectSigner`,
  Ed25519-deterministic ⇒ re-derivation is not equivocation).

---

## 7. Storage design — RocksDB

We will use **RocksDB** — an embedded ordered key-value store. A short primer
first, then how we use it.

**Primer — RocksDB in 101 terms** *(skip if familiar).*

RocksDB is a C++ library you link into your process (no separate server), reached
from the JVM via **RocksJava** (JNI). Only **one process** can open a given DB
for writing — it takes an exclusive `LOCK` file in the DB directory; other
processes can open the same DB `secondary` read-only. We are single-JVM-per-peer,
so this fits naturally. Keys and values are arbitrary bytes; records are sorted
lexicographically by key, and that sort is the only "index" — there is no SQL, no
secondary indexes, no joins.

What makes it fast is the **LSM (log-structured merge-tree)** shape of its
storage. Every put becomes two cheap operations — a sequential append to a
**WAL** (write-ahead log) file on disk, and an insert into a **MemTable**, a
sorted skiplist held in RAM. When the MemTable fills, it is flushed to disk as
an immutable **SSTable** (sorted file with its own index + Bloom filter), and a
background **compaction** process keeps merging SSTables into fewer, larger
files arranged in **levels** L0, L1, …, each roughly 10× larger than the previous.

```
   put(key, value) / write(WriteBatch)
        │
        ▼
   ┌───────────────────────────────────────────────┐
   │  ① append to WAL    (sequential file on disk) │
   │  ② insert MemTable  (sorted skiplist, in RAM) │
   └───────────────────────────────────────────────┘
                       │
       MemTable fills → flush as immutable SSTable
                       ▼
   ┌─────────────────────────────────────────────────────┐
   │  L0   [SST] [SST] [SST]      ← newest, may overlap  │
   │  L1   [SST][SST][SST][SST]   ← sorted, no overlap   │
   │  L2   …                       ← ~10× larger         │
   │                                                     │
   │   ▲  background compaction merges + dedupes         │
   └─────────────────────────────────────────────────────┘
```

A **read** checks the MemTable first, then the SSTables newest-to-oldest, asking
each SSTable's **Bloom filter** "could this key be here?" — the filter cheaply
answers "definitely not" and lets the read skip an SSTable without touching its
data blocks, so a missing-key check usually hits at most one file on disk.

Two further mechanisms we lean on:

- **`WriteBatch`** — a group of puts/deletes committed **atomically** as a single
  WAL record. All operations land or none, even after a crash. This is our
  durability barrier for CR4/CR6/CR8 (e.g. the per-soft-ack bundle
  `{brief, ack, BlockResult, DepositMap, RequestHighWater}` lands together across
  five CFs).
- **Column families (CFs)** — logically separate keyspaces inside one DB, each
  with its own MemTable + its own SSTables, all **sharing the WAL**. The shared
  WAL is exactly what makes a `WriteBatch` atomic *across* CFs. Splitting a
  workload across multiple CFs (vs one big keyspace with a tag-byte discriminant)
  buys:
    - **Per-CF tuning** — block size, Bloom-filter bits/policy, compression,
      MemTable size, and compaction style are all CF-scoped. Workloads with very
      different shapes (small high-churn ack signatures vs larger variable user
      requests vs sparse spine briefs) can be tuned independently.
    - **Compaction isolation** — each CF has its own background compaction queue,
      so a write burst on one CF doesn't stall compaction on another.
    - **Bloom-filter scoping** — each SSTable's Bloom filter covers only its own
      CF's keys. A missing-key check in one CF doesn't pay for SSTables in
      others. A prefix extractor (e.g. the `[peer:1]` byte on satellite CFs, §7.1)
      further lets a scan skip SSTables that hold no entries for a given peer.
    - **Tag-byte savings** — with the CF as the lane-type discriminant, the
      encoded key drops the tag byte entirely (§7.1).
    - **Per-CF metrics + backup granularity** — observability and (later) backups
      are CF-level.

Our store opens **13 CFs** — singular names throughout, no marker CF (every
marker derives from a single-CF scan, §5.1):

```
   ┌── one RocksDB instance (per-peer directory) ──────────────────────────┐
   │                                                                       │
   │   ┌─────────── shared WAL ───────────┐                                │
   │   └──────────────────────────────────┘                                │
   │                                                                       │
   │   each CF has its own MemTable + own SSTables                         │
   │                                                                       │
   │   Lane CFs (§7.1):                                                    │
   │     ┌──────┐ ┌──────┐ ┌───────┐ ┌────────┐ ┌────────┐                 │
   │     │Block │ │Stack │ │Request│ │SoftAck │ │HardAck │                 │
   │     └──────┘ └──────┘ └───────┘ └────────┘ └────────┘                 │
   │                                                                       │
   │   JL working data + aggregator outputs (spine-indexed):               │
   │     ┌────────────┐ ┌─────────────────┐ ┌─────────────────┐            │
   │     │BlockResult │ │SoftConfirmation │ │HardConfirmation │            │
   │     └────────────┘ └─────────────────┘ └─────────────────┘            │
   │                                                                       │
   │   Per-side snapshots:                                                 │
   │     ┌───────────┐ ┌────────────────┐ ┌────────┐ ┌─────────────┐       │
   │     │DepositMap │ │RequestHighWater│ │Treasury│ │EvacuationMap│       │
   │     └───────────┘ └────────────────┘ └────────┘ └─────────────┘       │
   │                                                                       │
   │   Metadata:                                                           │
   │     ┌──────┐                                                          │
   │     │ Meta │                                                          │
   │     └──────┘                                                          │
   │                                                                       │
   │   (arrival stamps ride inline as a 12-byte prefix on each            │
   │    lane value — not a separate CF; see §5.4, §7.1)                    │
   └───────────────────────────────────────────────────────────────────────┘
```

RocksDB's own crash recovery is mechanical: on open it replays the WAL from where
the most recent SSTable flush stopped, reconstructing the MemTable. *Our*
recovery (§5–§6) builds on top of that — once RocksDB hands us a consistent view
of the committed bytes in our CFs, we rebuild actor state from there.

**Three operational facts worth knowing.**

- **Every RocksJava call can block.** Not just writes — a cache-missing `Get`
  does synchronous disk I/O, and each iterator `Next` can cross an SSTable
  boundary and block. Writes can block on the WAL `fsync` *and* on **write
  stalls** — RocksDB throttles or pauses writes inside the `Write` call when L0
  has too many SSTables or pending-compaction bytes are high. So an
  `IO.blocking { db.put(...) }` can sit for a long, unbounded-ish time under
  sustained write pressure. In our impl every call (including each
  `Cursor.next`) goes through `IO.blocking`; when load forces tuning, the knobs
  are `max_write_buffer_number`, the L0 stall / slowdown triggers, and the
  parallel-compaction count.
- **`RocksDB.Snapshot` is in-process only.** It's an in-memory, process-lifetime
  construct — it does *not* survive a restart. Our recovery point is defined by
  the **derived `*Acked` marks** (§5.1 — from lane-CF scans), never by a
  `RocksDB.Snapshot`. Keep the distinction firm: "RocksDB snapshot" = a
  consistent live read view across ongoing writes; "our base snapshot" = the
  persisted passive-state blob (in `DepositMap` / `RequestHighWater` / `Treasury` /
  `EvacuationMap`) at the ack mark.
- **The blocking pool is unbounded by default.** Cats-effect's `IO.blocking`
  shifts onto a *cached* (unbounded) executor by default — fine for moderate
  throughput, but a flood of concurrent RocksDB calls can spawn many OS
  threads. If throughput becomes the bottleneck we can route RocksDB calls
  through a dedicated bounded executor via `IO.evalOn`, or front them with a
  semaphore.

**How we use it.**

- **Lanes** = key `LaneKey` (a `LaneId` + the within-lane index; §7.1) ⇒ replay is a
  **range scan from a cursor**, RocksDB's sweet spot; the access pattern is
  range-scan-by-lane, not ad-hoc SQL.
- **Durability barriers** = one atomic **`WriteBatch`** spanning every CF the
  step touches + WAL sync ⇒ clean CR4/CR8/CR6.
- **Confirmation-driven ack-pruning** = at confirmation, the aggregator writes
  its `SoftConfirmation` / `HardConfirmation` record and deletes the
  now-redundant per-ack keys in the same `WriteBatch`. Physical deletion is
  **bounded by the last hard-confirmed stack's last block** — never delete the
  confirmation records, the `BlockResult` records below that floor, or anything
  else that would breach the R10 evacuation floor (§5.1 note). The storage
  engine's own **native (SST) compaction** runs underneath and is a separate,
  internal matter.
- **Column families.** Thirteen CFs — five lane (`Block`, `Stack`, `Request`,
  `SoftAck`, `HardAck`; §7.1), three spine-indexed working / confirmation
  (`BlockResult`, `SoftConfirmation`, `HardConfirmation`), four snapshot CFs
  (`DepositMap` + `RequestHighWater` + `Treasury` single keyed blobs;
  `EvacuationMap` keyed per committed block), and one metadata
  (`Meta`, store version + the arrival-stamp generation counter, §5.4). The
  generic benefits of CF-per-concern (per-CF
  tuning, compaction isolation, scoped Bloom filters, tag-byte savings) are
  enumerated in the primer above. **Atomicity across CFs is preserved**:
  `WriteBatch` covers puts in multiple CFs as one WAL record, so the per-soft-ack
  bundle `{Block, SoftAck, BlockResult, DepositMap, RequestHighWater}` lands as one
  transaction even though its pieces live in five CFs.
- **Snapshots** = `DepositMap` / `Treasury` are single keyed blobs in their own
  CF, overwritten in place; `EvacuationMap` is keyed by `blockNum`, one entry per
  committed block (§3.2).

**Per-CF profile — what CF-per-concern buys us.** The thirteen CFs have very
different workload shapes, and the win of splitting them is that each one's
tuning knobs (block size, Bloom-filter bits / prefix extractor, MemTable size,
compaction style) can match its shape. Concretely:

| CF | Workload shape | Concrete payoff of having its own CF |
|---|---|---|
| `Block`, `Stack` | sparse spine — one small entry per block / stack, low write rate, heavily *read* (everyone consults briefs, replay scans them in order) | high bits/key Bloom is almost free (few keys); index + Bloom pinned in cache; range-scan-from-cursor on the spine is the native access pattern |
| `Request` | high write rate (every user request), variable-sized payloads, scans always start with a `[peer:1]` prefix (BlockWeaver pulling a peer's requests for a block) | `[peer:1]` **prefix extractor** lets a scan skip SSTables that hold no entries for that peer; larger MemTable absorbs request bursts without stalling other lanes |
| `SoftAck`, `HardAck` | very high write rate (N peers × every block / stack-round), small fixed-size entries (Ed25519 sigs), heavy churn — pruned by confirmation-driven ack-pruning | small block size + aggressive compaction in *this* CF only (Request bursts don't push acks around); strong Bloom (small keys → cheap bits/key); `*Acked` markers derive from the last own key here |
| `BlockResult` | one entry per block (JL's per-block output), keyed by `blockNum`, scanned at recovery to rebuild `StackComposer.pending`, low live read pressure | scan-optimized profile; values larger than acks but smaller than snapshots — independent compaction shape |
| `SoftConfirmation` | one entry per soft-confirmed block (FCA aggregate output), keyed by `blockNum`, low write rate (one per confirmation event); `softConfirmed = max(key)` | a single `SeekToLast` derives the marker for free; small + sparse — cheap to keep |
| `HardConfirmation` | one entry per hard-confirmed stack (multisigned effects / SECs / fallbacks), keyed by `stackNum`, the R10 evacuation floor — physical deletion never descends here; folded sequentially at recovery (CardanoLiaison + rule-based regime, §6, §10 Q6) | scan-optimized; `hardConfirmed = max(key)` from `SeekToLast`; **never compacted past — physical-retention floor** |
| `DepositMap` | one key (`Meta`-like — single keyed blob), medium-sized blob, **rewritten on every own soft ack** (high churn on a single key) | RocksDB-friendly single-key churn (large MemTable absorbs the overwrites; compaction collapses the chain quickly); isolated from ack-CF compaction |
| `RequestHighWater` | one key — single keyed blob holding `Map[HeadPeerNumber, RequestNumber]` (small, `N` entries), **rewritten on every own soft ack** (same cadence as `DepositMap`) | tiny single-key churn; read once at boot to seed the `N` RequestLane recovery cursors (§5.3); its own CF keeps the churn off the lane / ack compaction queues |
| `Treasury` | one key, rewritten only at own hard-ack stack-close (slow cadence); small (UTXO ref) | low write rate ⇒ tiny compaction footprint; own CF makes it trivially backupable / restorable as part of the R10 floor |
| `EvacuationMap` | keyed by `blockNum`; one entry per **committed** block (each major + each last-of-partition SEC minor — the only maps that back an on-chain commitment), written in batches at own hard-ack stack-close (the closing stack's committed blocks in one `WriteBatch`); each map is cumulative L2 state | scan-optimized profile (range-read for evacuation reconstruction); pruning is bounded — anything strictly older than the last-hard-confirmed major can be dropped, since those minors can never be disputed against once the next major supersedes them |
| `Meta` | store-level keys: the schema version and the **arrival-stamp `generation`** — a per-process counter read+incremented+persisted once at store-open (§5.4), so this process's lane stamps sort after earlier ones across restarts | no tuning needed; one obvious place for store-level metadata |

A few benefits worth calling out separately:

- **Compaction isolation pays here for real.** Request lanes can burst (a user
  request storm) and ack lanes churn (every confirmation prunes a batch). With
  one big keyspace those two would share a compaction queue and starve each
  other; per-CF queues let them be independent.
- **The atomic `WriteBatch` across CFs is what lets the per-soft-ack bundle
  `{Block, SoftAck, BlockResult, DepositMap, RequestHighWater}` land as one
  transaction**, even though its pieces live in five CFs. Without CF-cross-atomicity we'd need a
  manual two-phase scheme; with it the durability barrier (CR4/CR6/CR8) is one
  batch. The SC per-hard-ack-close bundle `{Stack, HardAck, Treasury, EvacuationMap}`
  is the slow-side mirror.
- **The R10 floor is a clean subset.** P2 (the first-priority milestone) writes
  `HardConfirmation` + `Treasury` + `EvacuationMap` and reads them on rule-based
  handover — no entanglement with fast-side lane CFs. If we ever want to back up
  *just* the evacuation floor, it's three CFs, not a key-range carve-out.
- **Self-documenting layout.** "Where do block briefs live?" → `Cf.Block`.
  "Where do `BlockResult`s live?" → `Cf.BlockResult`. The 13-CF inventory is the
  persistence map of the system, enumerable in one place (`Cf.scala`).
- **No marker CF saves a write per confirmation.** The aggregator's
  confirmation write *is* the marker advance (the new last key in the
  confirmation CF). Fewer CFs to keep consistent, fewer keys per confirmation
  WriteBatch, derivation is a single `SeekToLast`.

We don't set most of those tuning knobs on day 1 — RocksDB defaults are fine for
P1. The win is having the **option** to tune each CF independently when profiling
reveals a hotspot later, instead of being stuck with one-size keyspace knobs.

Notes / decisions:

- **Native dependency** (RocksJava / JNI) — accepted.
- The "schema" is now **key-layout design**; **lane values reuse the existing wire
  codecs** (`consensus/transport/Codecs.scala`) under a **12-byte `ArrivalStamp`
  prefix** (§5.4, §7.1) — strip the prefix and you have the byte-identical wire
  form (one codec to test, byte-identical forward path). Non-lane CF values
  (`BlockResult` / `SoftConfirmation` / `HardConfirmation` / `DepositMap` /
  `Treasury` / `EvacuationMap` / `Meta`) need their own codecs (no wire form,
  no stamp prefix).
- **Interface:** a `Persistence[F[_]]` capability (tagless / cats-effect), injected
  like `CardanoBackend` — `MultisigRegimeManager` already reserves a
  `Dependencies.Persistence` enum case and termination handler, so the seam exists.
- **Layout:** one store per head instance, keyed by head ID, path from `NodeConfig`.
- **Versioning** from day one; recovery refuses to load an incompatible version.

### 7.1 Key layout — lane IDs

Two types, kept distinct: a **`LaneId`** names a lane (the range-scan prefix); a
**`LaneKey`** is a full addressable entry (`LaneId` + the within-lane index). `LaneId`
is the cursor/scan unit — §5.3 derives exactly one resume cursor per lane.

```scala
/** Identifies one single-writer lane = the range-scan prefix.
  * Spines are head-global (no peer); satellites are per author. */
enum LaneId:
    case BlockSpine                      // the one block spine
    case StackSpine                      // the one stack spine
    case Request(peer: HeadPeerNumber)
    case SoftAck(peer: HeadPeerNumber)
    case HardAck(peer: HeadPeerNumber)

/** A full entry key = LaneId + within-lane index (the spine "tuple" / satellite "triple"). */
enum LaneKey:
    case Block  (num: BlockNumber)
    case Stack  (num: StackNumber)
    case Request(peer: HeadPeerNumber, num: RequestNumber)
    case SoftAck(peer: HeadPeerNumber, num: SoftAckNumber)
    case HardAck(peer: HeadPeerNumber, num: HardAckNumber)

    def laneId: LaneId = this match
        case Block(_)      => LaneId.BlockSpine
        case Stack(_)      => LaneId.StackSpine
        case Request(p, _) => LaneId.Request(p)
        case SoftAck(p, _) => LaneId.SoftAck(p)
        case HardAck(p, _) => LaneId.HardAck(p)
```

**Each lane type is its own column family**, so the **CF is the discriminant** —
the in-memory `LaneKey.laneId` maps to a CF handle and the encoded byte key drops
the type tag. Within a CF the index is **big-endian, fixed-width**, so lexicographic
byte order matches numeric index order (what makes a range scan correct):

| CF | key bytes |
|---|---|
| `Block` (spine) | `[blockNum : 4]` |
| `Stack` (spine) | `[stackNum : 4]` |
| `Request`       | `[peer : 1][requestNum : 8]` |
| `SoftAck`       | `[peer : 1][softAckNum : 4]` |
| `HardAck`       | `[peer : 1][hardAckNum : 4]` |

Widths follow the value ranges: `HeadPeerNumber < 2⁸` (1 byte); `Block` / `Stack` /
`SoftAck` / `HardAckNumber` are non-negative `Int` (4 bytes); `RequestNumber < 2⁴⁰`,
carried as an 8-byte `Long`. Peers stay **multiplexed inside a satellite CF** — the
`[peer:1]` prefix gives per-peer range scans (`Seek([peer])`, iterate while peer
matches); per-peer CFs would multiply file/MemTable count without buying anything.

**Range scan / cursor.** Scans are scoped to the CF: spine scan = the whole CF in
index order; satellite scan = prefix `[peer]`. To replay a lane from its cursor,
position an iterator on the lane's CF at `[cursorBE]` (spine) / `[peer][cursorBE]`
(satellite). Each of the `2 + 3N` replay cursors (§5.3) is a `LaneKey` — its
`laneId` picks the CF, the index gives the seek key.

**Values.** Lane values are framed as `[arrivalStamp : 12][wirePayload …]` — the
durable `ArrivalStamp` (`[generation : 4][monotonicNanos : 8]` big-endian; §5.4) is
a fixed prefix on the wire-codec payload. Stripping the prefix gives the bytes that
go on the wire — there is no separate `ArrivalStamps` CF. Non-lane CFs (`BlockResult`,
`SoftConfirmation`, `HardConfirmation`, `DepositMap`, `RequestHighWater`,
`Treasury`, `EvacuationMap`, `Meta`) carry no stamp prefix.

---

## 8. Boot sequence

Executed in/around `MultisigRegimeManager.preStartLocal`, before
`pendingConnections.complete`. **All actors start together**; the two recovery
mechanisms (replay / restore) run concurrently (§5).

1. Open the store; verify version; **bump the arrival-stamp `generation`** — read
   the `Cf.Meta` counter, increment, persist (once per store-open), so every lane
   stamp this process writes sorts after every earlier process's (§5.4). Absent
   store → cold start (the degenerate replay: empty base, empty mailboxes), and the
   generation simply starts at 1.
2. **Derive the four markers** by CF scan (§5.1): `softConfirmed =
   max(SoftConfirmation.key)`, `hardConfirmed = max(HardConfirmation.key)`,
   `softAcked = max(own SoftAck key)`, `hardAcked` from the last own `HardAck`.
   **Load base snapshots** for the consensus actors — `DepositMap` +
   `RequestHighWater` (JL; the latter feeds the RequestLane cursors, §5.3),
   `Treasury` + `EvacuationMap` (SC; the latter at
   `EvacuationMap[StackLane[hardAcked].lastBlockNum]`, always present for a
   non-final stack — §5.2). Validate invariants (markers consistent with the
   snapshot blobs being aligned to their anchors; counter monotonicity). On
   violation → **fail safe**.
3. **`ReplayActor` seeds the consensus actors' mailboxes** with the total-ordered
   lane-entry tail, **using per-actor resume cursors** (§5.1, §10 Q9 option a):
   signers / ledger from `*Acked + 1` (fast: `softAcked + 1`; slow: `hardAcked + 1`
   — no re-signing, CR2); aggregators from `*Confirmed + 1` (fast:
   `softConfirmed + 1`; slow: `hardConfirmed + 1`), so the acked-but-unconfirmed
   band is re-aggregated as peers' late acks arrive. StackComposer also reads
   `BlockResult` for `(StackLane[hardAcked].lastBlockNum, head]` to rebuild
   `pending` (§6 StackComposer). The `ReplayActor` also reads L1 **directly from
   `CardanoBackend`** (`utxosAt(treasuryAddress)`) and seeds the first
   `PollResults` into BlockWeaver, so deposit decisions proceed immediately rather
   than waiting on CardanoLiaison's poll cadence (§5.5). A **suspend barrier** may
   be needed to hold the consensus actors until seeding completes (§10 Q4).
4. **Open the start barrier:** every actor — consensus *and* boundary — runs.
   Consensus actors rebuild state + cascades from the seeded tail; their external
   re-emissions (acks, briefs, effects) reach the already-running boundaries.
5. **Boundaries restore + filter, concurrently.** RequestSequencer's counter from
   the store, PeerLiaison's per-remote cursors, CardanoLiaison's target — each
   loaded **before** the boundary processes anything. A replay re-emission
   `≤ cursor` is dropped by the same cursor / equivocation filter that runs in
   steady state; a `> cursor` output is genuine forward progress and goes out
   normally (§5).
6. **Reconcile L1:** `CardanoLiaison` polls `CardanoBackend` on its own cadence
   (the `ReplayActor` already supplied the first sample in step 3); the in-flight
   tail re-verifies against live L1 (§5.5) and may diverge; detect whether the
   head moved to the rule-based regime while we were down (→ hand off to the
   rule-based path, which reads this store once then runs off L1 (§10 Q6) —
   not a multisig resume).
7. Resume `GetMsgBatch` over the connected `PeerLiaison`s from restored cursors;
   catch up to current head height (ordinary liveness, not backfill).
8. Resume normal participation.

If step 7 can't complete within the timing budget (CR7), abort and signal the
operator / trigger evacuation rather than rejoin stale.

---

## 9. Failure scenarios to specify & test

- Crash after assigning a request ID but before the user was told (CR1).
- Crash after signing a soft/hard ack but before it crossed the peer boundary
  (CR2/CR4) — re-derivation must reproduce the identical signature, not a conflict.
- Crash mid-stack: round-1 acks persisted, round-2 not yet released.
- Crash after L1 submission, before observing the tx — re-submission is a
  no-op (the L1 layer rejects the duplicate `txId`); CardanoLiaison observes
  the original landing on the next poll.
- Crash during a snapshot write (CR6: atomic; fall back to previous snapshot +
  longer replay).
- Crash **during recovery itself** (a second crash mid-replay): the next boot
  must converge. Recovery is re-entrant — it only advances markers/snapshots
  monotonically and atomically (CR6/CR8/CR3) and replay is deterministic
  re-derivation, so a partially-completed recovery is just a valid earlier
  state to recover from again (§5). Test: inject crashes at successive points
  *within* the recovery window and assert the same final committed state.
- L1 moved while down: in-flight tail re-verifies against current truth and
  legitimately diverges/falls back (§5.5) — assert this is *not* treated as
  corruption.
- Long downtime → head went to rule-based regime while down → recovery must detect
  and not re-enter multisig (CR7, §8 step 6 — post-barrier L1 reconciliation).
- Torn/corrupt record, or counter regression on load → refuse start.
- Head-wide restart: all peers crash and recover — does the protocol re-converge
  purely from persisted state + replay?

**Testing strategy.** Extend the model-based integration suites (stage1 / stage4)
with a **crash-restart action** that kills and reconstructs a peer from its store
mid-run, asserting the consensus invariants still hold. Single-actor restart tests
the per-actor contract; **whole-node reboot** tests cross-actor re-convergence;
**crash-window fault injection** tests the CR4/CR8 barriers. Run the one-by-one
replay (§5.6 mechanism 2) as the deterministic oracle the concurrent run is checked
against. Property: *for any crash point, recovered committed state is
observationally equivalent to the no-crash run.*

---

## 10. Open questions

1. ~~**Backend**~~ — **resolved:** RocksDB (§7).
2. ~~**Snapshot cadence**~~ — **resolved:** snapshots anchored at the side's
   ack mark (`softAcked` / `hardAcked`); `DepositMap` re-saved on every own
   soft ack (bundled with that block's brief/ack/`BlockResult`); `Treasury` and
   `EvacuationMap` re-saved on every own hard-ack stack-close. `SoftConfirmation`
   / `HardConfirmation` writes govern ack-pruning, bounded by the last
   hard-confirmed stack's last block — never deletes below it (§5.1–§5.3).
3. ~~**Recovery wiring**~~ — **resolved:** central `ReplayActor` + boundary
   state-restore; not per-actor self-load, not pure message-journaling (§5).
4. **Replay mechanism** — **mostly settled:** pre-populate-mailboxes (§5.6
   mechanism 1) is the target and is *loosely expected to work* on the real
   substrate (validate empirically). **Barriers:** all actors start **together**
   (§5) — boundaries are *not* deferred, since the steady-state cursor filter
   already suppresses replay re-emissions, so a boundary-last phase buys nothing.
   The only barrier still in question is a **suspend barrier** to hold the
   consensus actors while the `ReplayActor` seeds their mailboxes; the existing
   `Connections` barrier may already suffice (then we add nothing). **Revisit
   trigger:** if measurement ever shows a replay re-emission leaking past a
   boundary's cursor filter onto the wire / chain, reintroduce a deferred
   boundary start surgically — but start from the simpler all-together shape.
5. ~~**fsync granularity**~~ — **resolved as a per-CF gradient.** Not one
   global policy — `WriteOptions::sync` is chosen per `Write` call, so we set it
   based on which CF (and what protocol promise) the write backs:
   - **Always `sync=true` (R10-critical):** `HardAck`, `HardConfirmation`,
     `Treasury`, `EvacuationMap`. Losing one of these to a power loss would let
     us forget a hard-ack or hard-confirmation, or roll back the slow-side
     snapshot anchor — violating the protocol's external-effect safety premise.
     Group-commit happens naturally (the slow-consensus pipeline batches these),
     so the fsync cost amortizes across the batch.
   - **`sync=true`, group-committed (soft-side protocol promises):** `SoftAck`,
     `BlockResult`, `SoftConfirmation`, `DepositMap`. Soft acks are protocol
     promises too, but losing one to a power loss is **recoverable via
     re-replication** (no external-effect violation), so we accept the fsync
     cost but rely on group commit (concurrent soft-acks coalesce into one
     fsync) to amortize.
   - **`sync=false` (sync optional, page-cache durable):** `Request` and the
     request-ID assignment. Without fsync the WAL write still lands in the
     **OS page cache**, which survives a *process* crash (the kernel owns the
     bytes), so a Hydrozoa crash with the host still powered recovers fine —
     RocksDB replays the WAL on restart. The forfeit is power-loss /
     kernel-panic durability for exactly those records. ⚠ For request-ID this
     trades against **CR1** (a power loss after telling the user an id but
     before the page flushes could re-assign) — accept only if power-loss is
     outside the durability promise; **decide explicitly**.
   (RocksJava is *embedded / in-process*, not a separate process — the
   `sync=false` path's process-crash durability comes from the OS page cache
   being kernel-owned and outliving the JVM, not from a separate writer
   staying up.)
6. ~~**Unify with `rulebased/persistence`**~~ — **resolved: one store, no second
   persistence.** The rule-based regime **persists nothing**; on entry it **reads
   the multisig store once**, then pursues its objective purely from that snapshot +
   live L1 polling (iterating on what it observes). So `rulebased/persistence` needs
   only a **read path** over the same store, never its own write store.
7. **TDX interaction** — does sealed/attested storage relax any CR? **Open — no
   decision yet**; coordinate with the M5 TDX workstream.
8. **Whitepaper promotion** — **direction set:** the article has **high-level parts
   that can eventually join the whitepaper** (architecture — boundary/consensus
   split, markers, replay, the lanes) and **low-level parts that stay in
   implementation docs** (RocksDB key layout, codec reuse, fsync/barrier mechanics).
   Split along that seam when promoting.
9. **Band-confirmation completion / aggregator resume point.** Signers / ledger
   resume at the ack mark + 1 (fast: `softAcked + 1`, slow: `hardAcked + 1`) to
   protect the signers (no re-signing, CR2). But the ack-aggregators must still
   complete the acked-but-unconfirmed band (fast: `[softConfirmed + 1, softAcked]`;
   slow counterpart) as peers' late acks arrive — and those cells aren't rebuilt
   by an ack-mark-+1 replay. Options: **(a)** the aggregators replay from the
   confirmed mark + 1 (fast: `softConfirmed + 1`; slow: `hardConfirmed + 1`)
   — they sign nothing, so re-aggregating is safe — while the signers /
   ledger keep their ack-mark-+1 resume point, giving a per-actor resume scheme;
   **(b)** the aggregator **re-reads** the band's briefs / acks from the persisted
   lanes on demand. Decide — (a) looks cleaner.

   **Related (surfaced in R1): the HardAckLane scan floor.** Resume *points* aside,
   the slow side also lacks a derivable scan *floor* for the per-peer hard-ack
   lanes. The HardAckLane is indexed by the author's own `HardAckNumber`, not by
   `StackNumber`, and no marker records the `StackNumber → HardAckNumber`
   correspondence — so unlike the other four lane families (§5.3) its cursor cannot
   be derived from `hardConfirmed` / `hardAcked`. R1 scans these lanes **from 0**
   (correct, not minimal). A tight floor needs one of: a per-peer hard-ack marker;
   a stack-indexed hard-ack lane; or reading each entry's embedded stack id while
   scanning and stopping once it passes the band. Decide alongside (a)/(b).

---

**Priority.** Sequence by **custody value, not by data flow.** After the storage
skeleton (P1), the first deliverable is **persisting what the rule-based regime
reads** — the hard-confirmed effects / SECs / fallbacks (the **R10 evacuation
floor**) plus the read path that loads them once on handover (§5.7 top rung, §10 Q6).
With that durable, a crashed peer can always fall back, vote, and evacuate even if
nothing else is restored. Everything after it is liveness / performance and may be
built **in any order**.

| Step | Deliverable |
|---|---|
| P1 | **Foundation (prerequisite for all below).** `Persistence[F]` + RocksDB backend skeleton; `LaneId`/`LaneKey` layout (§7.1); versioning; wired through `MultisigRegimeManager.Dependencies.Persistence`. |
| **P2 — first priority** | **The rule-based regime's read-set = the R10 floor.** SlowConsensusActor writes `HardConfirmation` records **in full** (multisigned effects / SECs / fallbacks) as it produces them (CR4); StackComposer's `Treasury` + `EvacuationMap` snapshots ride alongside (§6 StackComposer); the rule-based regime's **read path** loads `HardConfirmation` + `Treasury` + `EvacuationMap` once on handover and runs off live L1 (§10 Q6). Custody-safe in isolation — needs no replay, no fast-side state. |
| *the rest — any order* | |
| Pa | Boundary persistence: RequestSequencer write-before-tell-user (CR1/CR4); PeerLiaison inbound write-before-advance + cursors (CR8), with the 12-byte `ArrivalStamp` prefix on each lane value (§5.4). |
| Pb | Equivocation guard at the peer boundary (CR2) + counter recovery (CR3); unit tests. |
| Pc | Fast-side per-block persistence (JointLedger): per-soft-ack `WriteBatch` over `Block` + `SoftAck` + `BlockResult` + `DepositMap` + `RequestHighWater` (§6 JointLedger); plus `FastConsensusActor`'s confirmation write to `SoftConfirmation` + soft-ack pruning. The `BlockResult` CF is what lets `StackComposer` rebuild `pending` from disk on restart (§5.2); `RequestHighWater` feeds the RequestLane recovery cursors (§5.3). |
| Pd | `ReplayActor` + total-order merge (§5.4) + indices algorithm (§5.3); pre-populate-mailboxes mechanism + suspend barrier (§5.6); seeds the first `PollResults` straight from `CardanoBackend` so deposit decisions don't wait on CardanoLiaison's poll cadence (§5.5). |
| Pe | L1 reconciliation + live re-sample in CardanoLiaison (§5.5, §8 step 6 — post-barrier). |
| Pf | Boot sequence end-to-end (§8); fail-safe paths (CR6/CR7). |
| Pg | Crash-restart integration action + one-by-one oracle + observational-equivalence property (§9). |
| — | Slow-side schema (over `StackComposer` types) — unblocked: the slow-consensus types and Bootstrap have landed (§6 StackComposer). |

---

## References

- Whitepaper: `introduction/roadmap` (M5); `single-head-gummiworm-protocol/peer-network`
  (replicated log, outbox/cursor recovery note); `…/replicated-state-machine`
  (deterministic replay); `…/consensus/{fast,slow}-consensus`, `…/finality`;
  `future-work/user-experience/anti-censorship` (lying-peer proof).
- Code: `multisig/MultisigRegimeManager.scala` (actor topology, `Persistence`
  dependency seam); `multisig/consensus/{FastConsensusActor,SlowConsensusActor,StackComposer,
  BlockWeaver,PeerLiaison,EventSequencer,CardanoLiaison}.scala`;
  `multisig/ledger/joint/JointLedger.scala`; `multisig/consensus/transport/Codecs.scala`;
  `rulebased/persistence/Persistence.scala` (stub).
- Bootstrap (stack-0 init + fallback): `StackComposer.bootstrapInitialStack` — the
  cold-start seed; recovery loads the snapshot instead (§6 StackComposer).
