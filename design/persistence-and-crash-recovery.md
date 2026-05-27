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
  consensus is a separate M5 workstream that is not built yet; coil-peer recovery
  will be specified when it exists.
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
| Recover by | **state restoration** (load cursors/counters, re-observe L1) | **base-state seed + replay** — load the base snapshot at `start`, then the `ReplayActor` re-runs the input tail from `start+1` |

Each boundary maps to exactly one external surface: **RequestSequencer** = the
user-facing boundary (requests in), **PeerLiaison** = the other-peers boundary
(messages in/out), **CardanoLiaison** = the Cardano-L1 boundary (effects out,
observations in).

**Why this split is the keystone — and what each side actually touches.** It is
what makes replay (§5) safe, but the line is **not** "only boundaries touch the
store, clock, and chain." Per resource:

- **Network (peer wire) and chain (L1) — boundary-exclusive.** `PeerLiaison` is
  the only network touch, `CardanoLiaison` the only chain touch. These are the
  only ways an effect *escapes the process*, which is exactly why the replay
  membrane (§5.1) — drop anything `≤ cursor` — lives at just these two boundaries.
- **Durable store — written by both.** Boundaries persist what crosses them
  (`PeerLiaison`: inbound lane entries + arrival stamps, CR8; `RequestSequencer`:
  assigned requests, CR1/CR4). Consensus *producers* persist their authoritative
  **lane outputs once, at creation** (CR4) — `JointLedger` its block briefs +
  soft-acks, `StackComposer` its stacks + hard-acks — and re-save their **passive
  snapshots on cadence**: `JointLedger` the deposits map on every soft ack (§5.3),
  `StackComposer` the treasury/evac snapshot. Lane records are keyed by lane
  identity and idempotent (CR5/CR6), so re-writing one during replay is a harmless
  overwrite — the store needs no membrane, only the *external* surfaces do.
- **Clock — read by consensus too.** `BlockWeaver` stamps block-creation
  start/end times (`realTimeQuantizedInstant`), and `CardanoLiaison` times its L1
  observations. Neither is boundary-exclusive, and both are handled explicitly:
  BlockWeaver's clock-dependence is made replayable by the **recorded arrival
  stamps** (§5.5) — replay reads recorded times, not the live clock — while
  CardanoLiaison's is handled by **re-sampling L1 live** (§5.6).

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
is the figure the recovery indices algorithm works over (§5.4).

| Lane | Writer(s) | Index key | Pacing | Gap rule | Role |
|---|---|---|---|---|---|
| **BlockLane** | head, round-robin | `BlockNumber` (one global line) | round-robin | per-writer gaps; globally gap-free | fast spine |
| **StackLane** | head, round-robin | `StackNumber` (one global line) | round-robin | per-writer gaps; globally gap-free | slow spine (bundles blocks) |
| **RequestLane** | each head peer | `(HeadPeerId, RequestNumber)` | author-paced | gap-free | feeds → BlockLane |
| **SoftAckLane** | each head peer | `(HeadPeerId, BlockNumber)` | coverage-paced | gap-free (one ack per block) | ratifies BlockLane → soft-confirm |
| **HardAckLane** | each head peer | `(HeadPeerId, AckNumber)` | coverage-paced | cover every stack — all head peers' acks needed to hard-confirm | ratifies StackLane → hard-confirm |

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

| Datum | Producer | Recovery treatment |
|---|---|---|
| User requests + assigned `RequestId` | RequestSequencer (own), PeerLiaison (remote) | replayed; own assignments authoritative (CR1) |
| Block briefs | JointLedger (own/leader), PeerLiaison (remote) | replayed; own leader briefs authoritative |
| Soft acks (sig over header) | signed by JointLedger, broadcast by FastConsensusActor; remote via PeerLiaison | replayed; own ack authoritative (CR2) |
| **Soft confirmations** | FastConsensusActor (aggregate) | **materialized**: marker + block-results (no sigs); prunes soft-acks |
| Stack briefs | StackComposer (own/leader), PeerLiaison (remote) | replayed; own cut authoritative |
| Hard acks (per-effect, round-1/2/sole) | signed by StackComposer, aggregated by SlowConsensusActor; remote via PeerLiaison | replayed; own ack authoritative (CR2) |
| **Hard confirmations / multisigned effects** | SlowConsensusActor → CardanoLiaison | **materialized in full**: CardanoLiaison submits, evacuation reads; prunes hard-acks |
| Deposits map | JointLedger | **snapshotted** (out-of-order subset, §5.4) |
| Treasury / KZG / evac map | StackComposer | **snapshotted** |
| Monotonic counters | (various) | **derived** from the lanes (`max + 1`); never a separate record |

**Confirmations are stored, not just derived.** A confirmation is the aggregate
of N acks; storing the aggregate lets the constituent **acks** be pruned (the acks
are the log tail, the confirmation is the snapshot of them) — the aggregate itself
is **retained**, not deleted (§5.2 note: "compaction" prunes acks, never the
committed region; physical deletion never descends below hard-confirm).
**Soft** confirmations are stored as *marker + block-results, without the
signatures*: nothing downstream needs the soft-ack sigs after the fact (the slow
side derives stacks from the block results; dispute uses hard-confirmed SECs).
**Hard** confirmations are stored *in full* (the multisigned effects/SECs):
CardanoLiaison submits them and evacuation reads them.

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
  block numbers, own soft/hard `AckNumber`s, `lastClosedStackNum`. These are a
  *corollary* of CR4 + recover-as-`max(lane)+1`, not separate durable records:
  anything that left the process is durable, so `max+1` never reissues.
- **CR4 — Write-before-send.** Any datum that, once observed externally, binds
  this peer is persisted **before** the corresponding message leaves a boundary
  (to a `PeerLiaison` outbox, to the user, or to L1).
- **CR5 — Idempotent replay.** Replay re-delivers already-processed messages, and
  the rewound tail re-runs slightly-overlapping work. Every consumer must be
  idempotent keyed by `(authorPeerId, type, seqNum)`; re-submitting an L1 effect
  that already landed is a no-op (tx-id idempotency).
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
crash-atomic writes that live operation makes (markers/snapshots advance under one
atomic `WriteBatch`, CR6, and only ever forward, CR8/CR3), and its replay
re-processing is idempotent (CR5). So a crash partway through recovery leaves the
store at a **valid earlier state**, never a half-advanced one; the next boot re-runs
the identical path from wherever the anchor reached and makes the same progress.
Each uninterrupted attempt strictly advances (or completes), so recovery converges
to the committed state after finitely many interruptions — exactly the
observational-equivalence property of §9, with the crash placed *inside* the
recovery window.

The boot has two halves, mirroring §2:

- **Boundary actors restore.** They load their own state directly — cursors,
  counters — or re-observe the outside world (L1). No replay.
- **Consensus actors: base-state seed, then replay.** Each is seeded with its
  base snapshot at `start` (§5.3; empty for the pure aggregators); a central
  **`ReplayActor`** then re-runs the admitted input tail through them, rebuilding
  state *and* the inter-actor signal cascade.

**Why replay, not just state-restore, for the interior.** Restoring an actor's
state alone misses the *signals it would have emitted on receiving its inputs*.
Example: you can restore BlockWeaver's mempool by selecting requests not yet in a
block — but BlockWeaver must also *signal JointLedger* on receiving each request,
and a bare state-restore never re-emits those signals, leaving the system torn.
Replay reproduces the whole cascade by construction, so cross-actor state can't
tear.

### 5.1 The membrane: why the split makes replay safe

During replay the consensus interior re-emits its outputs (acks, briefs, effects)
toward the boundaries. But the **network** boundary (PeerLiaison) has **restored its
cursor to the post-crash point**, so every replay-spilled message is `≤ cursor` and
is **dropped**; only genuinely new output (`> cursor`) leaves. Therefore:

- the consensus interior needs **no `recoveryMode` flag** — it just re-executes;
- duplicate external I/O is suppressed at the **two escape surfaces** (§2): the
  **network** boundary by PeerLiaison's restored cursor (the membrane proper), and
  the **chain** boundary by L1 idempotency + live re-sampling (CR5, §5.6), not a
  cursor. The **user** boundary (RequestSequencer) sees no replay-spill — user
  requests are not replayed (§6).

The one mechanism requirement: a boundary actor must restore its cursor **before**
it drains any replay-spill from its mailbox. A start barrier covers this — and we
can add one if the actor substrate doesn't already gate it.

### 5.2 The two markers

| Marker | Is | Governs |
|---|---|---|
| **done** | last soft-confirmation (fast) / hard-confirmation (slow) | **confirmation materialization** — the N acks become the aggregate (soft = marker+results; hard = full effects/SECs, **retained**) and the redundant per-ack signatures are pruned; below it is committed and never re-verified. *Not* a region delete — see the note below. |
| **start** | our own last soft-block-ack / hard-stack-ack — the block/stack *we constructed and signed* | the **snapshot anchor** and the **replay resume point** (replay runs from `start + 1`) |

The marks mirror each other across the fast/slow boundary. Their relationship is
**`start ≥ done`**: we ack a block/stack *before* it confirms, so the band
`[done+1, start]` is acked-by-us-but-not-yet-confirmed. We resume replay at
`start + 1` precisely so we never re-decide or re-sign anything we already put our
signature behind (CR2); the band's pending confirmations simply complete as
peers' acks arrive live post-recovery.

> **Three senses of "compaction" — keep them distinct.**
> 1. **Confirmation materialization (what `done` triggers).** When N acks aggregate
>    into a confirmation, store the aggregate and drop the now-redundant *per-ack
>    signatures*. The aggregate is **retained**, not deleted — for hard-confirmation
>    it is the full multisigned effects/SECs, i.e. the **R10 evacuation floor**. This
>    prunes *acks*, never the committed region.
> 2. **The recovery base (a logical compacted view).** The base snapshot (§5.3) is a
>    compacted *image* of state at `start`; the `ReplayActor` replays the entries
>    after it. A read-time construction over a copy — it deletes nothing from the
>    live store.
> 3. **RocksDB native compaction (§7)** — the storage engine merging SST files.
>    Engine-internal, unrelated to either.
>
> **Hard rule:** physical deletion from the durable store **never descends below the
> latest hard-confirmed block** — that region holds the evacuation-critical state
> (R10). The fast `done` (soft-confirmation) prunes only soft-acks (blocks stay,
> the slow side still needs them); the slow `done` (hard-confirmation) is the
> physical-retention floor.

### 5.3 State recovery: the base snapshot

Replay starts from a **base snapshot of the passive state at `start`**, and the
**ack is stored together with that state** so the snapshot is self-locating (it
knows which block/stack it is the state *after*, hence where replay resumes).

The snapshot carries the **non-derivable** passive state:

- the **deposits map** (JointLedger) — re-saved on **every own soft ack**, in the
  same atomic write as that block's brief / ack / results (§6), so the snapshotted
  map is always exactly the deposits as of the `start` anchor and can never be torn
  from it;
- the **unconfirmed briefs/results** for the `[done+1, start]` band (so the
  acked-but-unconfirmed blocks/stacks survive — their confirmations finish from
  peers' acks arriving live);
- the slow-side **treasury / evacuation / KZG** state (StackComposer);
- the **`start` ack** itself.

It does **not** carry per-lane cursors — those are derived (§5.4).

### 5.4 The indices algorithm: deriving the 2 + 3N lane cursors

To replay, the `ReplayActor` needs a start cursor for each lane: **2** for the
shared spines (one BlockLane, one StackLane) **+ 3 per head peer** (its Request /
Soft-ack / Hard-ack satellites) = **2 + 3N** (§3.1). Every one of them **derives
from the markers** via Hydrozoa's monotonicity invariants:

- **Requests** — monotonic per peer: *if a block includes Peer A's Request `N`,
  the next block includes only A's requests `> N`.* So A's RequestLane cursor is
  the highest `N` from A in any block `≤ start`, `+ 1` — derivable, not stored.
- **Blocks / stacks** — sequential and monotonic ⇒ cursor = `start + 1`.
- **Acks** — tied to block/stack numbers ⇒ derived from the block/stack marker.

So all `2 + 3N` lane cursors fall out of the markers; none is stored.

**The deposits map is the exception — and it is not a lane**, so it has no cursor
to derive. The pending-deposits map mutates on *any* block (a request *adds* a
deposit; a major block *absorbs* an out-of-order **subset**), so it is neither
constant nor a marker-findable suffix and cannot be reconstructed from the lanes.
It is carried instead as **snapshotted passive state** (§5.3), re-written on every
own soft ack — the one place a snapshot is unavoidable on the fast side.

### 5.5 Total order, and the time base

Replay needs a total order over the `2 + 3N` streams. **Stamp every inbound entry
with local monotonic arrival time on receipt, and persist the stamp**; merging
the streams by stamp yields a fixed, durable interleaving — "not canonical but
correct," which is all a correct concurrent replay needs.

The stamps do **double duty**: they are the deterministic **time base**, so
time-dependent decisions (e.g. when BlockWeaver cuts a block) replay off the
*recorded* arrival times instead of the live wall clock. This is what makes the
leader's timing reproducible across a restart — raw `IO.monotonic` is meaningless
across reboots, but a persisted arrival stamp is not.

### 5.6 The L1 boundary is re-sampled live, not replayed

Not every input is replayed from the log. The **L1 boundary** (CardanoLiaison
poll results) is **re-acquired live at replay time**, and the result may differ
from the pre-crash value — which is correct:

- **Below `done`:** committed; loaded from the materialized confirmations, never
  re-verified against fresh polls. No agreed block/stack is reverted because L1
  moved.
- **Above `done` (the in-flight tail):** re-verified against *current* L1 truth.
  If a deposit seen pre-crash is gone now, the peer legitimately fails to confirm,
  diverges, maybe falls back — "verify the block/stack NOW, not back then." That
  is the protocol reacting to reality, not a replay defect.

Consequence: the L1 boundary contributes **nothing** to the replay log — it is a
live source, not a recorded one. (A soft-confirmed-then-reorged deposit is caught
by the hard side / fallback, not by re-verifying soft confirmations.)

### 5.7 The replay mechanism

**Replay re-feeds lanes, and only lanes.** The replicated set's lane entries
(§3.1) are the sole replay input; every *non-lane* input is reproduced, not
replayed — inter-actor signals (`StartBlock`/`CompleteBlock`, the soft-confirm
fan-out, `GetState`) regenerate from the interior cascade, and L1 poll results
are re-sampled live (§5.6). So an actor whose inputs are *all* non-lane signals
gets base state but no tail — JointLedger, for one, reads no lane (BlockWeaver
drives it), so the `ReplayActor` never feeds it (§6). This holds for either
mechanism below.

Two candidate mechanisms for *how* to feed that lane tail; we target **(1)** and
keep **(2)** as a test oracle.

1. **Pre-populate mailboxes ("crash as a state").** Create the consensus actors
   suspended, seed each with its passive base state, and drop the total-ordered
   **lane-entry tail** into the mailboxes of the actors that read those lanes (e.g.
   BlockWeaver the request lanes; the ack-aggregators the ack lanes). Then open the
   start barrier and let them run concurrently to the crash state. Fast, reuses the
   existing `Deferred[Connections]` barrier (though a **separate suspend barrier**
   may be needed to hold actors during seeding, and because `CardanoLiaison` cannot
   poll L1 until the connections barrier opens — §8, §10 Q4), and unifies replay
   with normal operation (cold boot = the empty-seed case). What is restored is the
   **base snapshot + lane-entry tail in mailboxes**; everything else regenerates as
   the tail is processed — you don't capture internal in-flight messages.
2. **One-by-one + quiescence** *(test oracle, not for production)*. The `ReplayActor`
   sends one input, waits for the system to settle, then the next. A single
   deterministic serialization, easy to reason about — but slow, and it needs a
   global quiescence oracle (cheap under TestControl via the cede-settle trick,
   awkward on the live runtime). **Use it as the reference oracle:** replay
   deterministically one-at-a-time in a test, assert the concurrent run (1) lands on
   the same committed state.

Prerequisites for (1), both already required:
- **Interleaving-robust consensus** — concurrent replay finds *a* valid schedule;
  it must reach the same *committed* state under any of them. Live operation is
  already concurrent message arrival from N peers, so this holds or the consensus
  is wrong.
- **Idempotent re-processing** (CR5) — the rewound tail redoes a little work; that
  must be safe.

### 5.8 The recovery-priority ladder (graceful degradation)

The durable set is layered so that **restoration priority is requirement
priority**: a partial restore still buys a precise, bounded capability, with
custody safety as the non-negotiable floor.

| Restore through | Unlocks | Requirement |
|---|---|---|
| evacuation commitments + SECs + their signatures + fallbacks | fallback, vote, **evacuate** | **R10** |
| + happy-path effects | follow the happy path (TTL permitting) | **R9** |
| + stack briefs/results + soft/hard confirmation markers | realize fast-consensus decisions | **R8** |
| + block briefs + deposit maps + user requests | process known requests as **follower** | R8 |
| + per-request / per-batch arrival stamps | process known requests as **leader** | — |

The bottom rung is exactly §5.5's time base; the top rung is the custody floor —
even a peer that restored nothing else can still fallback, vote, and evacuate.

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
- **Persists:** inbound remote lane entries (CR8), each with its **local arrival
  stamp** (§5.5). The one actor that durably stores data it did not produce. Its
  restored cursors are also the **membrane** (§5.1) that drops replay-spill.

#### CardanoLiaison

- **State:** `targetState` (`Active(treasuryUtxoId)` | `Finalized`), `effectInputs:
  Map[TransactionInput, EffectId]` (the "which effect spends this input" index),
  `happyPathEffects: TreeMap[EffectId, HappyPathEffect]`, `fallbackEffects:
  Map[BlockVersion.Major, FallbackTx]`.
- **Recover:** fold over the **stored hard-confirmations** (§3.2, materialized in
  full); `targetState` seeded from `config.initializationTx.treasuryProduced.utxoId`
  until stack-0 hard-confirms; effects faulted in lazily. **Submission progress is
  recovered from L1 itself** (poll → observe the current treasury utxo) — L1 is
  the durable truth, re-sampled live (§5.6), so there is no own progress marker.
- **Inputs:** `Stack.HardConfirmed`, `Timeout` (poll tick).
- **Persists:** nothing of its own (signs nothing ⇒ no CR2; submits multisigned
  txs). The lazy load is `effectInputs(observedInput) → happyPathEffects(effectId)`.

### Consensus actors — *replayed*

#### BlockWeaver

- **State:** role FSM (`DecidingRole` → `Leader`/`Follower`), `mempool`,
  `nextBlockNumber`, transient `pollResults` / finalization trigger / `Wakeup`.
- **Recover (replay):** base = state at `start`; replay `[start+1, head]`. Mempool
  and `nextBlockNumber` follow from the replay; leadership timing follows the
  recorded arrival stamps (§5.5), not the live clock.
- **Inputs:** `UserRequestWithId`, `BlockBrief.Next`, `Block.SoftConfirmed`,
  `PollResults`, `Wakeup`, finalization trigger.
- **Persists:** nothing — authors no lane; drives `JointLedger` via
  `StartBlock`/`CompleteBlock{Regular,Final}`, which authors the brief.

#### JointLedger  *(snapshot-bearing)*

Post-split, owns only the fast-side state; treasury moved to StackComposer.

- **State:** `Done(previousBlockHeader, deposits)` | `Producing(previousBlockHeader,
  deposits, l2LedgerState, startTime, userRequestState)`.
- **Recover:** seed the base snapshot at our `start` ack — `previousBlockHeader`,
  the `deposits` map (§5.4), and the **unconfirmed briefs** in `[done+1, start]`
  (§5.3). JointLedger is **not fed by the `ReplayActor`**; BlockWeaver replays its
  own tail and re-drives JointLedger via `StartBlock`/`CompleteBlock` + forwarded
  requests, reproducing the `[start+1, head]` blocks. A mid-`Producing` block is
  discarded and re-produced (a never-committed cut is safely redone).
- **Inputs (all interior signals — none from the `ReplayActor`):** from
  **BlockWeaver** — forwarded `UserRequestWithId` (deposits fold into `deposits`)
  and `StartBlock`/`CompleteBlock{Regular,Final}`; from the fast aggregator —
  `Block.SoftConfirmed.Next`. (`GetState.Sync` is a defined query with no current
  sender.)
- **Persists:**
  - (1) **On each own soft ack — one atomic `WriteBatch` (CR4/CR6/CR8):** the own
    `BlockBrief` (leader only — BlockLane author), the own soft-ack, the block
    results, and the current **deposits snapshot**. Bundling deposits here keeps the
    snapshot aligned with the `start` anchor (our last ack) and un-tearable from it.
  - (2) **On soft-confirmation:** the **soft-confirm materialization** (marker +
    block-results), pruning the now-redundant soft-acks (the blocks themselves stay
    — the slow side needs them until hard-confirm; §5.2 note).
  - The own soft-ack's equivocation guard is a PeerLiaison-boundary fact per CR2;
    JointLedger computes the signature.
- **L2 co-anchoring (load by ack, like the deposits map).**
  `Producing.l2LedgerState` is WIP; the committed L2 state lives in the `L2Ledger`
  black box (its own persistence). Recovery loads it the **same way as the deposits
  map**: given our `start` ack (the latest block we constructed and signed), restore
  the L2 state **as of that block**. The co-anchoring requirement is exact — the
  `L2Ledger` must return at the *same* block boundary as JointLedger's recovered
  `Done`/`start`, or the two tear.
  - **Cost / optimization.** Full L2 state is far larger than the deposits map, so
    snapshotting it on *every* ack (the deposits cadence) may be too expensive. The
    `L2Ledger` may instead snapshot **less frequently** and restore-to-`start` by
    loading its nearest snapshot `≤ start` and replaying its own block operations
    forward to the ack boundary — the usual snapshot-interval-vs-replay-length
    tradeoff, internal to the black box. The mechanism is delegated/out-of-scope
    here; the contract is the **load-by-ack interface** plus the **shared boundary**.

#### FastConsensusActor, SlowConsensusActor

- Pure aggregators: per-block soft-ack cells / per-stack hard-ack cells. **Recover
  (replay):** cells rebuild from the replayed briefs + acks; nothing of their own
  is persisted. The own-ack signature is computed deterministically; the
  equivocation guard is the PeerLiaison-boundary cursor (CR2).

#### StackComposer  *(snapshot-bearing — STUB)*

The remaining snapshot-bearing actor: treasury / evac map / KZG + the materialized
hard-confirmations. **Contract deferred** — its `Stack`/`StackEffects`/`HardAck`/KZG
types are still in flux and the `Bootstrap` boot path is unbuilt (`StackComposer`
and `HardAckSigningPlan` throw on `StackEffects.Initial`); pin the contract once
those types settle and Bootstrap lands. Recovery and Bootstrap must be co-designed
(both seed `StackComposer` from non-cold state), Bootstrap first.

---

## 7. Storage design — RocksDB

We will use **RocksDB** (embedded LSM key-value store). It fits this design
better than a relational store:

- **Lanes** = key `(laneId, index)` ⇒ replay is a **range scan from a cursor**,
  RocksDB's sweet spot; the access pattern is range-scan-by-lane, not ad-hoc SQL.
- **Durability barriers** = one atomic **`WriteBatch`** (entry + cursor/marker
  advance) + WAL sync ⇒ clean CR4/CR8/CR6.
- **Marker-driven ack-pruning** = on a confirmation, delete the now-redundant
  per-ack keys subsumed by the materialized aggregate. This is **bounded by the
  hard-confirm `done`** — never delete the materialized confirmations or anything
  below the latest hard-confirmed block (the R10 evacuation floor; §5.2 note). The
  storage engine's own **native (SST) compaction** runs underneath and is a
  separate, internal matter.
- **Column families** separate lanes / snapshots / markers / arrival-stamp index.
- **Snapshots and markers** = plain keyed blobs.

Notes / decisions:

- **Native dependency** (RocksJava / JNI) — accepted.
- The "schema" is now **key-layout design**; **values reuse the existing wire
  codecs** (`consensus/transport/Codecs.scala`) so persisted bytes == wire bytes
  (one serialization to test, byte-identical replay/forward). Snapshots need their
  own codecs (no wire form).
- **Interface:** a `Persistence[F[_]]` capability (tagless / cats-effect), injected
  like `CardanoBackend` — `MultisigRegimeManager` already reserves a
  `Dependencies.Persistence` enum case and termination handler, so the seam exists.
- **Layout:** one store per head instance, keyed by head ID, path from `NodeConfig`.
- **Versioning** from day one; recovery refuses to load an incompatible version.

---

## 8. Boot sequence

Executed in/around `MultisigRegimeManager.preStartLocal`, before
`pendingConnections.complete`:

1. Open the store; verify version. Absent → cold start (the degenerate replay:
   empty base, empty mailboxes).
2. **Restore boundary actors** from the store: RequestSequencer's counter,
   PeerLiaison's cursors (this is the membrane, §5.1), CardanoLiaison's target.
3. **Load base snapshots** for the consensus actors (deposits map, unconfirmed
   briefs/results, treasury/evac, the `start` ack) and validate invariants
   (counters monotone, snapshot position consistent). On violation → **fail safe**.
4. **`ReplayActor` pre-populates** the suspended consensus actors' mailboxes with
   the total-ordered input tail from `start+1` (§5.5, §5.7). A **suspend barrier**
   may be needed to hold them until seeding completes (§10 Q4).
5. **Open the connections barrier:** consensus actors run, rebuilding state +
   cascades; the restored boundary cursors drop replay-spill (§5.1). Opening the
   barrier is also what lets `CardanoLiaison` learn its `BlockWeaver` target — so it
   **cannot poll L1 before this point** (§10 Q4).
6. **Reconcile L1 (only now possible):** `CardanoLiaison` polls `CardanoBackend`;
   the in-flight tail re-verifies against live L1 (§5.6) and may diverge; detect
   whether the head moved to the rule-based regime while we were down (→ hand off to
   the rule-based path, which reads this store once then runs off L1 (§10 Q6) — not
   a multisig resume).
7. Reconnect `PeerLiaison`s; resume `GetMsgBatch` from restored cursors; catch up
   to current head height (ordinary liveness, not backfill).
8. Resume normal participation.

If step 7 can't complete within the timing budget (CR7), abort and signal the
operator / trigger evacuation rather than rejoin stale.

---

## 9. Failure scenarios to specify & test

- Crash after assigning a request ID but before the user was told (CR1).
- Crash after signing a soft/hard ack but before it crossed the peer boundary
  (CR2/CR4) — re-derivation must reproduce the identical signature, not a conflict.
- Crash mid-stack: round-1 acks persisted, round-2 not yet released.
- Crash after L1 submission, before observing the tx (CR5: no double-submit).
- Crash during a snapshot write (CR6: atomic; fall back to previous snapshot +
  longer replay).
- Crash **during recovery itself** (a second crash mid-replay): the next boot must
  converge. Recovery is re-entrant — it only advances markers/snapshots
  monotonically and atomically (CR6/CR8/CR3) and re-processes idempotently (CR5),
  so a partially-completed recovery is just a valid earlier state to recover from
  again (§5). Test: inject crashes at successive points *within* the recovery
  window and assert the same final committed state.
- L1 moved while down: in-flight tail re-verifies against current truth and
  legitimately diverges/falls back (§5.6) — assert this is *not* treated as
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
replay (§5.7 mechanism 2) as the deterministic oracle the concurrent run is checked
against. Property: *for any crash point, recovered committed state is
observationally equivalent to the no-crash run.*

---

## 10. Open questions

1. ~~**Backend**~~ — **resolved:** RocksDB (§7).
2. ~~**Snapshot cadence**~~ — **resolved:** base anchored at our `start` ack;
   deposits map re-saved on every own soft ack (bundled with that block's
   brief/ack/results); `done` (confirmation) governs materialization + ack-pruning,
   bounded by hard-confirm — never deletes below it (§5.2–§5.4).
3. ~~**Recovery wiring**~~ — **resolved:** central `ReplayActor` + boundary
   state-restore; not per-actor self-load, not pure message-journaling (§5).
4. **Replay mechanism** — **mostly settled:** pre-populate-mailboxes (§5.7
   mechanism 1) is the target and is *loosely expected to work* on the real
   substrate (validate empirically). **Barrier:** the existing connections barrier
   is probably enough for the interior, but a **separate suspend barrier may be
   needed** — `CardanoLiaison` cannot poll L1 until the connections barrier has
   opened (it must first learn which `BlockWeaver` to route poll results to). So
   **L1 reconciliation cannot precede the barrier open** (reflected in §8).
5. ~~**fsync granularity**~~ — **resolved for the fast path, with a caveat:**
   fast-consensus entries and request-ID assignment **need not be fsync'd**. Without
   fsync a WAL write still lands in the **OS page cache**, which **survives a
   process crash** (the kernel owns it), so a Hydrozoa crash with the host still
   powered recovers fine — RocksDB replays the WAL on restart. The forfeit is
   **power-loss / kernel-panic durability** for exactly those un-fsync'd records.
   ⚠ For request-ID this trades against **CR1** (a power-loss after telling the user
   an id but before the page flushes could re-assign) — accept only if power-loss is
   outside the durability promise; **decide explicitly**. (RocksJava is
   *embedded/in-process*, not a separate process — the safety comes from the OS page
   cache, not from a separate writer surviving the crash.)
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

---

## 11. Milestone plan (M5 sub-plan)

| Step | Deliverable |
|---|---|
| P1 | `Persistence[F]` + RocksDB backend skeleton; key layout; versioning; wired through `MultisigRegimeManager.Dependencies.Persistence`. |
| P2 | Boundary persistence: RequestSequencer write-before-tell-user (CR1/CR4); PeerLiaison inbound write-before-advance + arrival stamps + cursors (CR8); the membrane (§5.1). |
| P3 | Equivocation guard at the peer boundary (CR2) + counter recovery (CR3); unit tests. |
| P4 | Base snapshots: deposits map + unconfirmed briefs (JointLedger); confirmation materialization (soft = marker+results, hard = full). |
| P5 | `ReplayActor` + total-order merge (§5.5) + indices algorithm (§5.4); pre-populate-mailboxes mechanism + barrier (§5.7). |
| P6 | L1 reconciliation + live re-sample in CardanoLiaison (§5.6, §8 step 6 — post-barrier). |
| P7 | Boot sequence end-to-end (§8); fail-safe paths (CR6/CR7). |
| P8 | Crash-restart integration action + one-by-one oracle + observational-equivalence property (§9). |
| — | Slow-side schema (P4/P5 over `StackComposer` types) gated on slow-consensus type stabilization + Bootstrap landing. |

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
- `.scratch/slow-consensus-plan.md` (Bootstrap injection — **unbuilt**; co-design
  recovery with it).
