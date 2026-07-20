# Persistence & Crash Recovery

**Status:** Draft · **Whitepaper milestone:** M5 — Feature Complete MVP (2026 May)
· **Branch baseline:** post `feature/slow-consensus` + `migrate-kzg` + `rate-limiter`
+ `feature/coil` (PR #454) merge — coil peers are now in scope here

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

A Gummiworm peer — **head or coil** — must survive a process crash, host reboot, or
controlled restart **without losing custody safety and without violating
consensus invariants**. After a restart the peer rejoins the running head,
catches up, and resumes producing/verifying blocks and effects as if it had only
been briefly unresponsive (within the head's inactivity/silence timing budget —
see [timing-rules](https://) `#fallback`).

**Head and coil peers share one recovery architecture.** A coil peer keeps the
**same journal structure and the same store** as a head peer (`coil-network.md` §2),
so it recovers by the same machinery described here. The two node types diverge in
a handful of precise, enumerated places — a coil peer never leads either spine,
authors no user requests, **authors no soft-ack** (its fast-side recovery anchors on
the `BlockResult` CF, [§6](#6-per-actor-recovery-contracts) `JointLedger`), and its own hard-acks are re-sequenced onto a hub's `HubHardAck`
journal. Hub head peers carry one extra
recovery contract — the `CoilAckSequencer` re-sequencing consensus actor ([§6](#6-per-actor-recovery-contracts)).

### In scope (M5)

- **Both peer types — head and coil.** Durable storage and crash recovery for head
  peers *and* coil peers, over the one shared store. Coil-specific deltas are called
  out inline (search "coil"); the hub-side `CoilAckSequencer` / coil-link liaisons
  are covered in [§6](#6-per-actor-recovery-contracts).
- Durable storage for everything a peer needs to rebuild its crash-time state,
  which splits into two kinds:
    - **The common (consensus) data set** — this peer's replica of the *replicated
      set*: the append-only journals that converge across **all** peers (the
      block / stack spines + the request / soft-ack / hard-ack satellites, where the
      hard-ack satellite is `PeerId`-keyed so it spans head and coil authors, plus the
      `HubHardAck` journal; [§3](#3-consensus-data-the-journals)). Every peer eventually holds
      the same set; it is the common, network-disseminated consensus data.
    - **Peer-local derived data** — what *this* peer computes and keeps for its own
      recovery, **never shared** and not part of the replicated set: per-block
      `BlockResult`s, the soft / hard confirmation records, the passive snapshots
      (deposits map, treasury, evacuation maps), the request high-water and L2
      command number, the sequencer counters / indices (`RequestSequencer`,
      `CoilAckSequencer`), and store metadata ([§3.3](#33-what-gets-stored-and-how-recovery-treats-it), [§5.2](#52-state-recovery-the-base-snapshots)). Each peer derives these
      from the common set; they differ peer to peer (different `hardAcked` marks,
      different in-flight bands) and are stored only because re-deriving them from
      scratch on every boot would be unsound or too slow ([§5.2](#52-state-recovery-the-base-snapshots)).
  The line matters for recovery: the common set is replayed (and, post-recovery,
  re-converges over `GetMsgBatch`); the peer-local data is restored from this peer's
  own disk and is **never** requested from anyone else (next bullet).
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
- **Permissionless coil-peer marketplace** (registry, bonds, rent, dynamic
  membership). Coil-peer *recovery* is in scope (threaded throughout); the
  marketplace mechanics that would let coil membership change under a peer are a
  separate future-work spec (`coil-network.md` §6.1).
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
| Members | `RequestSequencer`, `CardanoLiaison`, `PeerLiaison*` | `BlockWeaver`, `JointLedger`, `StackComposer`, `FastConsensusActor`, `SlowConsensusActor`, `CoilAckSequencer` (hub-only) |
| Sit at | an external surface: users / L1 / peers | the deterministic interior |
| Own exclusively | the **network** (`PeerLiaison*`, consensus messages) and the **chain** (`CardanoLiaison`, L1 effects) — the only surfaces anything leaves the process through | the deterministic fold; they regenerate inter-actor signals when replayed |
| Recover by | **state restoration** (load cursors/counters, re-observe L1) | **base-state seed + replay** — load the base snapshot at the side's ack mark (`fastBlockMark` / `hardAcked`), then the `ReplayActor` re-runs the input tail from that mark + 1 |

Each boundary maps to exactly one external surface: **`RequestSequencer*`* = the
user-facing boundary (requests in), **`PeerLiaison*`** = the other-peers boundary
(messages in/out), **`CardanoLiaison*`* = the Cardano-L1 boundary (effects out,
observations in).

> **`PeerLiaison*` — three shapes, one role.** There is no single `PeerLiaison*`
> actor anymore: the peer↔peer boundary is realized by three actors —
> `PeerLiaisonHeadToHead` (the symmetric head mesh), `PeerLiaisonHubToCoil`
> (hub → coil, serving the full population stream), and `PeerLiaisonCoilToHub`
> (coil → hub, serving only the coil peer's own hard-ack). `PeerLiaison*` denotes
> all three; they share the pull-based `GetMsgBatch` / `NewMsgBatch` protocol and
> the same outbox-as-DB-view recovery contract ([§6](#6-per-actor-recovery-contracts)), differing only in which lane
> set each carries (`coil-network.md` §5.5). Where a recovery point is specific to
> one shape, it is named explicitly.

> **Coil peers and hubs in this model.** A **coil peer** runs the head-peer
> consensus actors in a strict follower subset (`coil-network.md` §5.2) over the
> *same* store, so its consensus actors recover by the same base-state-seed +
> replay; the only divergences are enumerated where they bite (no own user
> requests; **no own soft-ack** — its fast side anchors on its `BlockResult` mark,
> [§6](#6-per-actor-recovery-contracts) `JointLedger`; own hard-acks in its `HardAck(PeerId.Coil)` CF, [§6](#6-per-actor-recovery-contracts) `StackComposer`). A **hub** head peer carries one
> extra consensus actor — **`CoilAckSequencer`**, a re-sequencer that assigns a per-hub
> sequence number to its coil peers' hard-acks and authors the `HubHardAck` journal; like
> the other consensus actors the `ReplayActor` re-feeds it on boot, while it restores its
> own sequence scalar directly ([§6.2.5](#625-coilacksequencer)). The hub's stateless
> **`CoilRelay`** fan-out holds no cursor or buffer and so has *nothing* to
> recover — the boundary/consensus split simply does not apply to it.

**Why this split is the keystone.** It is what makes replay ([§5](#5-recovery-architecture)) safe. The line is
**not** a persistence monopoly — both sides write the store (boundaries persist what
crosses them, CR1/CR8; consensus producers persist their journal outputs at creation,
CR4). It rests on two facts about *recovery*:

- **The recovery mechanism differs.** The deterministic interior cannot be brought
  back by loading state alone: its actors regenerate a cascade of inter-actor signals
  — briefs, acks, confirmations — that a bare state-restore would not reproduce. So
  consensus actors are **replayed**: seed the base snapshot at the side's ack mark
  (`fastBlockMark` / `hardAcked`), then the `ReplayActor` re-runs the input tail from that
  mark + 1. Boundaries hold directly-restorable state — cursors, counters, the
  outbox-as-DB-view — and need no replay.
- **Only boundaries emit anything outside the process.** There are two external
  surfaces: the peer wire (`PeerLiaison*`, carrying consensus messages to other peers)
  and the chain (`CardanoLiaison`, submitting *effects* — L1 txs / SECs). Replay's
  duplicate-suppression is therefore needed at exactly those two places — and it is the
  **same cursor / equivocation filter the boundaries already run in steady state**: a
  re-emitted output `≤` the boundary's cursor is dropped just as a live duplicate would
  be. That filter — not a deferred boundary start — is why all actors can safely start
  **together** ([§5](#5-recovery-architecture)).

---

## 3. Consensus data: the journals

What the whitepaper calls a *replicated log* is **not** a single Raft-style
totally-ordered log. It is a replicated **set of append-only journals**, each entry
authored by exactly one peer (no two peers ever write the same slot) — **not**
necessarily one fixed author for an entire journal. Two journal shapes realize that:

- **Spine journals** (BlockSpine, StackSpine) — one global sequence (`BlockNumber` /
  `StackNumber`) whose **author rotates round-robin**: the leader for index
  *i* is the only peer that writes entry *i*, but leadership rotates across the
  spine. No single peer authors the whole spine, yet every individual entry still
  has exactly one author (and each peer's own contribution is a **sparse**
  subsequence — gaps where others led; [§3.1](#31-spines)'s "per-writer gaps, globally gap-free").
- **Satellite journals** (Request, SoftAck, HardAck, HubHardAck) — a **fixed** single
  author owns the whole journal, keyed by its own monotonic `seqNum` and totally
  ordered within itself.

Either way the defining invariant is **one author per entry**: entries are appended,
never mutated, and never conflict, and every peer eventually holds a copy of *every*
entry. "Replicated" = the set converges across peers; one author per entry = no
write-write race on any slot.
There is **no global cross-author order in the journal layer** — total order over
requests is imposed downstream by fast consensus (block briefs), not by the journals.

> **Vocabulary — "journal" vs "lane".** A **journal** is an append-only sequence in
> the *persistence/recovery* realm: for a satellite, one author, **one column family (CF)**, keyed by
> the author's own `seqNum`; the **replicated set** is the convergent union of all
> journals; a **spine** is one of the two round-robin journals that carry consensus
> order (blocks, stacks). The word **lane** is reserved for the *peer-liaison
> transport* view — what a `PeerLiaison*` pulls and serves over `GetMsgBatch`
> (`LaneOutbound` / `LaneInbound`). A liaison serves a journal's entries to a remote
> *as a lane*; on disk that same sequence is a *journal* — and the persistence types are
> named accordingly: `JournalId` / `JournalKey` / `JournalScan` / `JournalValue` ([§3](#3-consensus-data-the-journals) vocabulary).

### 3.1 Spines

Two journals are **spines** (round-robin, single global index, rotating sole writer
— the only Raft-like part): the BlockSpine (fast) and the StackSpine (slow). The
spines are **common** — a *single* BlockSpine and a *single* StackSpine for the whole
head. Each spine is one global sequence with a single global index; the author
rotates round-robin, so no single peer authors the whole spine while every individual
entry still has exactly one author.

| Journal | Writer(s) | Per-author CF? | Index key (within CF) | Pacing | Gap rule | Role |
|---|---|---|---|---|---|---|
| **BlockSpine** | head, round-robin | no (one global) | `BlockNumber` | round-robin | per-writer gaps; globally gap-free | fast spine |
| **StackSpine** | head, round-robin | no (one global) | `StackNumber` | round-robin | per-writer gaps; globally gap-free | slow spine (bundles blocks) |

Pacing archetype:

- **Round-robin-paced** (BlockSpine, StackSpine) — one global index, leader rotates
  by `index % nHeadPeers`; any single writer's entries have gaps.

Structural fact:

- **Every head peer rotates as leader on both spines.** There is no spine a head peer
  is excluded from: each head peer is the round-robin author of entries on *both*
  spines (BlockSpine, StackSpine), and reads both.

### 3.2 Satellites

The rest are **satellite** journals (one independent sequence per author) attached to
a spine: Request, SoftAck, HardAck, and HubHardAck.

**Each satellite author gets its own CF** — the journals are **not** multiplexed by
an author-prefix into one CF. One author's CF receives only that author's
monotonic stream, so it is a clean append-only sequence (non-overlapping L0 SSTables
→ near-zero compaction), which matters at the request rate ([§7](#7-storage-design--rocksdb)). The CF *is* the
author discriminant, so the on-disk key is just the within-author index.
Consequently the **CF set is derived from head config at open** — fixed for a head
instance's lifetime (membership changes by closing and re-opening a fresh head, so
the set never changes under a running store, [§7](#7-storage-design--rocksdb)).

Each satellite is **per author**. A head of **N** peers therefore has **2 spines +
3·N satellite CFs** (Request / SoftAck / HardAck per head peer), and every peer
eventually holds a copy of all of them. That `2 + 3N` is the head-mesh figure the
recovery indices algorithm works over ([§5.3](#53-the-indices-algorithm-deriving-the-2--3n--h-journal-cursors)).

With **C** coil peers across **H** hubs the disseminated set grows by **H** hubs'
`HubHardAck` CFs (`SlowConsensusActor` reads them for the coil quorum). A coil peer
authors **only** its own `HardAck(PeerId.Coil)` journal (it never leads a spine, authors
no request, and **authors no soft-ack** — [§2](#2-two-kinds-of-actor)), and that journal reaches the population
only re-sequenced onto a hub's `HubHardAck`. So a *head* peer works over
`2 + 3N + H` journals; a *coil* peer over `2 + 3N + H` (the disseminated population)
**plus its own** `HardAck(PeerId.Coil)` CF; a *hub* additionally holds a `HardAck(PeerId.Coil)`
receive CF per coil peer it hubs ([§5.3](#53-the-indices-algorithm-deriving-the-2--3n--h-journal-cursors)).

| Journal | Writer(s) | Per-author CF? | Index key (within CF) | Pacing | Gap rule | Role |
|---|---|---|---|---|---|---|
| **Request** | each head peer | one per head peer | `RequestNumber` | author-paced | gap-free | feeds → BlockSpine |
| **SoftAck** | each head peer | one per head peer | `SoftAckNumber` | coverage-paced | gap-free (one ack per block; `SoftAckNumber` coincides with the block's number) | ratifies BlockSpine → soft-confirm |
| **HardAck** | each head peer **and** each coil peer (author = `PeerId`); a hub also keeps a receive CF per coil peer it hubs | one per author (`PeerId`) | `HardAckNumber` | coverage-paced (head) / author-paced (coil) | covers every stack | ratifies StackSpine → hard-confirm; a coil peer's acks reach the quorum re-sequenced onto `HubHardAck` |
| **HubHardAck** | each hub | one per hub | `HubHardAckNumber` | hub-paced | gap-free | the re-sequenced, *disseminated* coil-ack journal the head mesh + hub→coil links carry; `SlowConsensusActor` reads it to reach the coil quorum |

`SoftAck` is **head-only** — a coil peer authors none, and recovers its fast side
from the `fastBlockMark = max(BlockResult.key)` anchor instead ([§6](#6-per-actor-recovery-contracts)
`JointLedger`), so no coil identity enters the `SoftAck` journal.

**The `HardAck` journal is `PeerId`-keyed** — one CF per author, the author being a
`PeerId`, so a head peer's `HardAck(PeerId.Head)` and a coil peer's
`HardAck(PeerId.Coil)` are distinct CFs within the one journal kind. A coil peer's
hard-acks are its only authored journal, and they feed the hub re-sequencing pipeline.
A **hub** also keeps a receive copy of each of its coil peers' acks in that same
`HardAck(PeerId.Coil)` CF (on the hub, where that coil peer doesn't run). The
`HubHardAck` journal is a **separate** kind — the hub's re-sequenced, disseminated
coil-ack spine. Only `HubHardAck` is disseminated across the network — a coil peer's
raw `HardAck(PeerId.Coil)` journal reaches the population only *re-sequenced* onto a
hub's `HubHardAck` (the raw copy a hub keeps is for hub-crash survival, [§6](#6-per-actor-recovery-contracts)
`CoilAckSequencer`).

> **Coil journals are slow-side only.** A coil peer touches none of the fast-side
> author journals: no `Request`, no `SoftAck` (its fast recovery anchors on the
> `BlockResult` CF, [§6](#6-per-actor-recovery-contracts) `JointLedger`). Its only authored journal is its
> slow-side `HardAck(PeerId.Coil)` CF. The head/coil distinction it carries is the
> `PeerId` author, not a separate journal kind: a head peer's acks ride
> `HardAck(PeerId.Head)`, a coil peer's ride `HardAck(PeerId.Coil)`, and the dissemination
> difference (a coil ack travels up to its hub, which re-sequences it onto `HubHardAck`)
> is a property of the author, not of the CF kind. The per-author CF split keeps the
> hub's receive log + re-sequence pipeline isolated from the mesh-broadcast head acks.

Pacing archetypes:

- **Author-paced** (Request, a coil peer's `HardAck`) — the writer's own monotonic
  counter; the writer sets cadence.
- **Coverage-paced** (SoftAck, head HardAck) — the writer must cover every entry
  of a *spine* gap-free. SoftAck indexes directly by the spine number (one soft
  ack per block); HardAck carries the same cover-every-stack obligation but is
  indexed by the author's own counter, since one stack draws several hard acks (per
  partition / round).

Structural facts:

- **Every head peer authors all three of its satellites.** Each head peer is the
  *sole* author of its Request journal, its SoftAck journal (fast), and its HardAck
  journal (slow), and reads all of them — there is no fast-only or slow-only head peer.
- **A coil peer authors only its own `HardAck(PeerId.Coil)`.** It never leads a spine,
  authors no Request and no SoftAck, and produces no disseminated ack. Its sole authored
  journal is `HardAck(PeerId.Coil)` (its slow-side acks, re-sequenced onto a hub's `HubHardAck`
  for the population); its fast-side recovery anchors on the `BlockResult` CF
  ([§6](#6-per-actor-recovery-contracts) `JointLedger`), not on any ack it authors. It *reads* the whole disseminated
  population.
- **A hub additionally authors `HubHardAck`.** A hub is a head peer (so it writes all
  the head journals) that also serves coil peers: its `CoilAckSequencer` re-sequences
  its coil peers' hard-acks into the disseminated `HubHardAck` journal the population
  reads to reach the coil quorum ([§6](#6-per-actor-recovery-contracts)).
- **Dependency:** Request journal → BlockSpine → StackSpine; SoftAck journal ratifies
  BlockSpine; HardAck journal (+ the coil quorum via `HubHardAck`) ratifies StackSpine.

### 3.3 What gets stored, and how recovery treats it

| Datum                                                                             | Producer | Recovery treatment |
|-----------------------------------------------------------------------------------|---|---|
| User requests + assigned `RequestId`                                              | `RequestSequencer` (own), `PeerLiaison*` (remote) | replayed; own assignments authoritative (CR1) |
| Block briefs                                                                      | `JointLedger` (own/leader), `PeerLiaison*` (remote) | replayed; own leader briefs authoritative |
| Soft acks (sig over header, `SoftAck` journal, one CF per **head** peer)            | signed + persisted by `JointLedger` (**head peers only**) at creation (own `SoftAck` lane, before the handoff — CR4); announced by `FastConsensusActor`; remote acks via `PeerLiaison*` | replayed; own ack authoritative (CR2). Each head peer has its own `SoftAck` CF keyed by `SoftAckNumber`; on a head peer `max(own SoftAck CF)` coincides with `fastBlockMark = max(BlockResult)`. A coil peer authors none (it anchors its fast side on `BlockResult`, [§6](#6-per-actor-recovery-contracts) `JointLedger`). |
| **Block result** (`BlockResult` CF)                                               | `JointLedger` | per-block JL **deltas** (evac-map diff, payouts, refunds, absorbed deposits, fallback time); keyed by `blockNum`. The `brief` is **not** stored here — it already lives in the `Block` journal, rehydrated from there at recovery (`BlockResult.fromPersisted`), so it is not duplicated across two journals; the live `JL → StackComposer` message still carries it. Lets `StackComposer` rebuild `pending` from disk on restart without JL re-running the band. Written on **every** peer (head and coil); `max(BlockResult.key)` is the fast-side recovery anchor `fastBlockMark` ([§6](#6-per-actor-recovery-contracts) `JointLedger`), identical on head and coil. On a head peer it equals `max(own SoftAck)`; a coil peer authors no soft-ack. |
| **Soft confirmation** (`SoftConfirmation` CF)                                     | `FastConsensusActor` | header + aggregated multisig over the soft-acks, written at confirmation time, keyed by `blockNum`. The value is `Timestamped` with the node-local confirmation moment as an **arrival stamp** (not a wall clock — diagnostic, never consensus state; converted to wall-clock time on read via the per-generation zero-time anchor). `softConfirmed` **derives** as `max(SoftConfirmation.key)` — no marker key. Prunes soft-acks. Aggregates **head-peer** acks only (the coil quorum sits on the slow side). |
| Stack briefs                                                                      | `StackComposer` (own/leader), `PeerLiaison*` (remote) | replayed; own cut authoritative |
| Hard acks (per-effect, round-1/2/sole)                                            | signed + persisted by `StackComposer` at creation (own `HardAck` lane + `UnsignedStack`, before the handoff — CR4); announced + aggregated by `SlowConsensusActor`; remote acks via `PeerLiaison*` | replayed; own ack authoritative (CR2). The `HardAck` journal is `PeerId`-keyed: a head peer's own acks ride `HardAck(PeerId.Head)`, a coil peer's own ride `HardAck(PeerId.Coil)` (next row). |
| **Unsigned stack** (`UnsignedStack` CF)                                           | `StackComposer` | the closed `Stack.Unsigned` (brief + locally-derived effects), keyed by `stackNum`, written **before** the handoff to `SlowConsensusActor` so SCA re-forms its in-flight cell on recovery (a `HardAck` signs the effects, which a `StackBrief` alone does not carry; [§6](#6-per-actor-recovery-contracts) `StackComposer`). Written on every close, head or coil. |
| **Coil hard acks** (`HardAck(PeerId.Coil)` CF, coil peers)                        | `StackComposer` (coil peer, own); a hub's `PeerLiaisonHubToCoil` writes its coil peers' *inbound* acks here | keyed by `HardAckNumber` within the per-coil-peer CF (the coil author is the CF, `HardAck(PeerId.Coil)`). A coil peer's own slow-side ack journal — `hardAcked` derives from its last own entry, exactly as a head peer's derives from `HardAck(PeerId.Head)`. The hub's receive copy is its durable receive log, from which it stamps any unstamped tail after a crash ([§6](#6-per-actor-recovery-contracts) `CoilAckSequencer`). One entry per stack — a coil peer hard-acks every stack (skip-stack optimization deferred, [§2](#2-two-kinds-of-actor)). |
| **Hub hard acks** (`HubHardAck` CF, hubs)                                         | `CoilAckSequencer` (hub) | the re-sequenced, disseminated coil-ack journal, keyed `(HubHeadPeerNumber, HubHardAckNumber)`; carries a `HardAckWithId`. The hub's `nextSeq` derives as `max(HubHardAck where hub == own) + 1` — no stored counter. `SlowConsensusActor` reads all hubs' journals to reach the coil quorum ([§6](#6-per-actor-recovery-contracts) `CoilAckSequencer`). |
| **Hard confirmation** (`HardConfirmation` CF)                                     | `SlowConsensusActor` → `CardanoLiaison` | multisigned effects / SECs / fallbacks **in full**, written at confirmation time, keyed by `stackNum`. The value is `Timestamped` with the node-local confirmation moment as an **arrival stamp** (not a wall clock — diagnostic, never consensus state; converted to wall-clock time on read via the per-generation zero-time anchor). `hardConfirmed` **derives** as `max(HardConfirmation.key)` — no marker key. `CardanoLiaison` submits; evacuation reads. Prunes hard-acks. **R10 evacuation floor.** |
| **Request → block index** (`RequestBlockIndex` CF)                                | `JointLedger` | reverse index, keyed `(author, requestNum)`: the block that locally processed the request plus its validity verdict. Written in the same atomic bundle as the block. Never read by recovery — query support only. |
| **Block → stack index** (`BlockStackIndex` CF)                                    | `SlowConsensusActor` | reverse index, keyed by `blockNum`: the stack that hard-confirmed the block, one row per block in the stack's range, written in the same atomic batch as the stack's `HardConfirmation`. Never read by recovery — query support only. |
| Deposits map (`DepositMap` CF)                                                    | `JointLedger` | **snapshotted** (one keyed blob, rewritten on every own soft ack — out-of-order subset, [§5.3](#53-the-indices-algorithm-deriving-the-2--3n--h-journal-cursors)) |
| Request high-water (`RequestHighWater` CF)                                        | `JointLedger` | **per-block** — keyed by `blockNum`, a `Map[HeadPeerNumber, RequestNumber]` giving the highest request from each peer included in any block `≤ blockNum` (cumulative, monotone in `blockNum`). One entry per own soft-ack, bundled with that block. The Request journal recovery cursor is `RequestHighWater[fastBlockMark] + 1` per peer. Persisted (not derived) because retention ([§5.1](#51-the-markers--all-derived-none-stored)) prunes the old briefs a brief-fold would need ([§5.3](#53-the-indices-algorithm-deriving-the-2--3n--h-journal-cursors)); block-keyed (not a singleton) so the high-water at any committed block is recoverable. |
| L2 command number (`L2CommandNumber` CF)                                          | `JointLedger` | **per-block** — keyed by `blockNum`, a single `Long`: the L2 ledger's commit counter reached after that block's L2 commits. One entry per own soft-ack, bundled with that block. On recover `JointLedger` reads `L2CommandNumber[fastBlockMark]` and calls `l2Ledger.restoreTo` to co-anchor the committed L2 state ([§6](#6-per-actor-recovery-contracts)). |
| Treasury (`Treasury` CF)                                                          | `StackComposer` | **snapshotted** (one keyed blob, rewritten on every own hard-ack stack-close — rotates per settlement / finalization) |
| Evacuation maps (`EvacuationMap` CF)                                              | `StackComposer` | **per-block, but only at the blocks that back an on-chain KZG commitment** — every **major** block (its post-diff map is the settlement's `nextKzg`) and every **last-of-partition minor** (the minor that gets a SEC), keyed by `blockNum`. SC folds each block's `evacuationMapDiff` onto the running map and persists the result at those blocks on every own hard-ack stack-close. The maps at other minors commit to nothing on-chain, so the rule-based dispute can never need them. KZG commitment derives from the map, not stored separately. |

Monotonic counters (own `RequestNumber`, produced-brief block numbers, own
`SoftAckNumber` / `HardAckNumber`, `lastClosedStackNum`) are not stored — they
derive from the journals (`max + 1`). See CR3.

**Three separate artifacts straddling the two paths.** `BlockResult` and
`SoftConfirmation` are fast-side (produced under fast-consensus cadence);
`HardConfirmation` is slow-side. Each lives in its own CF and is written by its
own producer at its own moment. Keeping them split is what lets `softConfirmed` and
`hardConfirmed` be **derived** as `max(CF.key)` — no separate marker storage —
while still giving `StackComposer` what it needs to rebuild on restart:

- **`BlockResult` (JL, at ack time).** Per-block JL output, keyed by `blockNum`.
  Written in JL's own atomic per-soft-ack `WriteBatch` ([§6](#6-per-actor-recovery-contracts)) alongside the soft-ack
  / brief / deposits-snapshot. On restart, `StackComposer` loads `BlockResult`s
  for `(StackSpine[hardAcked].lastBlockNum, head]` from disk to rebuild its
  `pending` map — JL itself anchors at `fastBlockMark` and does **not** re-run earlier
  blocks.
- **`SoftConfirmation` (FCA, at confirmation time).** Per-block header +
  aggregated multisig over the threshold-met soft-acks, keyed by `blockNum`.
  Written in FCA's own atomic batch alongside the soft-ack prune. Soft-ack
  signatures are not retained after this — nothing downstream needs them (slow
  side derives stacks from `BlockResult`s; dispute uses hard SECs).
- **`HardConfirmation` (SCA, at confirmation time).** Per-stack multisigned
  effects / SECs / fallbacks **in full**, keyed by `stackNum`. `CardanoLiaison`
  submits the effects; evacuation reads the SECs. Written in SCA's own atomic
  batch alongside the hard-ack prune. The **R10 evacuation floor** — never
  deleted ([§5.1](#51-the-markers--all-derived-none-stored) retention rule).

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
  crossed the wire" is a `PeerLiaison*` fact. The signer (`JointLedger`/`StackComposer`)
  *computes* the signature deterministically (Ed25519 ⇒ re-derivation is not
  equivocation); `PeerLiaison*`'s cursor gates what actually leaves. (Structurally a
  slashing-protection database.)
- **CR3 — Monotonic counters never regress.** Own `RequestNumber`, produced-brief
  block numbers, own `SoftAckNumber` / `HardAckNumber`, `lastClosedStackNum`. These are a
  *corollary* of CR4 + recover-as-`max(journal)+1`, not separate durable records:
  anything that left the process is durable, so `max+1` never reissues.
- **CR4 — Write-before-send.** Any datum that, once observed externally, binds
  this peer is persisted **before** the corresponding message leaves a boundary
  (to a `PeerLiaison*` outbox, to the user, or to L1).
- **CR6 — Write atomicity.** A crash mid-write leaves the store consistent: a
  record is fully present or fully absent (one atomic `WriteBatch`, [§7](#7-storage-design--rocksdb)).
- **CR7 — Recovery within the timing budget.** Total downtime (crash → restart →
  caught-up) must fit the head's inactivity/silence margin, or the peer must fail
  safe (stay down / signal evacuation) rather than rejoin stale.
- **CR8 — Write-before-advance (receive path).** An inbound journal entry is durably
  stored **before** the journal's receive cursor advances past it. The cursor never
  points beyond what is on disk. Pairs with CR4: together they make recovery
  self-sufficient from local disk, with no peer-assisted backfill.

---

## 5. Recovery architecture

**Recovery is initialization against a non-empty store.** There is one boot path;
a cold start is the degenerate case where the store is empty. The same path runs
on every boot, so it cannot silently rot, and "test recovery" reduces to "restart
and observe re-convergence" ([§9](#9-failure-scenarios-to-specify--test)).

**Recovery is therefore re-entrant — a second crash mid-recovery is not special.**
Because recovery *is* the normal boot path, it makes only the same monotonic,
atomic writes that live operation makes (markers/snapshots advance under
one atomic `WriteBatch`, CR6, and only ever forward, CR8/CR3), and the replay
itself is a **deterministic re-derivation** from the persisted journal tail — no
re-processing semantics to get wrong. So a crash partway through recovery
leaves the store at a **valid earlier state**, never a half-advanced one; the
next boot re-runs the identical path from wherever the anchor reached and makes
the same progress.
Each uninterrupted attempt strictly advances (or completes), so recovery converges
to the committed state after finitely many interruptions — exactly the
observational-equivalence property of [§9](#9-failure-scenarios-to-specify--test), with the crash placed *inside* the
recovery window.

The boot **starts every actor together** — consensus and boundary alike — and
lets them run. The [§2](#2-two-kinds-of-actor) split is by *recovery mechanism* (replay vs restore), not by
*start order*; the two mechanisms run concurrently:

1. **Consensus actors: base-state seed, then replay.** Each is seeded with its
   base snapshot at the side's ack mark (`fastBlockMark` on the fast side, `hardAcked`
   on the slow; [§5.2](#52-state-recovery-the-base-snapshots); empty for the pure aggregators); a central **`ReplayActor`**
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
can reintroduce a deferral surgically ([§10](#10-open-questions) Q1) — but only if measurement shows it
is needed. The all-together start needs **no** topology change: consensus actors
reference the boundaries through the `Connections` barrier (`pendingConnections`),
so the boundaries are already in place when consensus starts.

**Why replay, not just state-restore, for the interior.** Restoring an actor's
state alone misses the *signals it would have emitted on receiving its inputs*.
Example: you can restore `BlockWeaver`'s mempool by selecting requests not yet in a
block — but `BlockWeaver` must also *signal `JointLedger*` on receiving each request,
and a bare state-restore never re-emits those signals, leaving the system torn.
Replay reproduces the whole cascade by construction, so cross-actor state can't
tear.

### 5.1 The markers — all derived, none stored

Recovery is governed by **four markers** (`softConfirmed`, `fastBlockMark`,
`hardConfirmed`, `hardAcked`) — a *confirmed* mark and an *acked* mark on each of the
fast and slow sides (the fast-side acked mark is the `BlockResult` anchor
`fastBlockMark`). Together they tell a recovering peer where committed
work ends and where its own in-flight work begins. **None of them lives in its own
CF**: each derives from a single-CF scan with no consistency burden of its own.

> The code's `Markers` value bundles a fifth field, **`nextRequestNumber`** — the
> `RequestSequencer` counter (`max(own Request.key) + 1`; `RequestNumber(0)` cold, or on a
> coil peer, which assigns none — [§6.1.1](#611-requestsequencer)). It rides the same boot
> read but is a boundary counter, not part of the fast/slow *confirmed* / *acked* structure
> below.

| Marker | Side | Definition (and how it's derived) |
|---|---|---|
| **`softConfirmed`** | fast | The highest **soft-confirmed** block — `max(SoftConfirmation.key)` (last key in the `SoftConfirmation` CF; empty store → no soft confirmations). Identical on head and coil (both aggregate the head-peer soft-acks into `SoftConfirmation`). |
| **`fastBlockMark`** | fast | The fast-side **acked** mark — the highest block we durably finalized — `fastBlockMark = max(BlockResult.key)`, derived by `Markers.recoverFastBlockMark`. **Identical on head and coil**: the `BlockResult` CF is written every block by every peer, so on a **head** peer `max(BlockResult)` equals `max(own SoftAck CF)` (both written in the same atomic per-block batch), and a **coil** peer authors no soft-ack at all. `JointLedger`'s recover is one routine off this mark on either peer type ([§6](#6-per-actor-recovery-contracts) `JointLedger`). |
| **`hardConfirmed`** | slow | The highest **hard-confirmed** stack — `max(HardConfirmation.key)` (last key in the `HardConfirmation` CF). Identical on head and coil. |
| **`hardAcked`** | slow | The highest stack **we hard-acked** ourselves — derived from the last own entry in the **`HardAck(own)`** CF, the unified `PeerId`-keyed hard-ack journal: a head peer reads `HardAck(PeerId.Head)`, a coil peer `HardAck(PeerId.Coil)` (unpack the stack identifier from the value). One derivation, `Markers.recoverHardAcked(backend, own)`, for both peer types. Covers leader *and* follower acks. On a coil peer the journal is dense in `StackNumber` — it hard-acks every stack (skip-stack optimization deferred, [§2](#2-two-kinds-of-actor)) — so `max` over its entries gives `hardAcked` directly, as on a head peer. |

The two pairs are independent: fast and slow advance at their own pace. Within a
side the relation is **`acked ≥ confirmed`** — we ack a block / stack *before* it
confirms — so the band `[confirmed + 1, acked]` (per side) is
**acked-by-us-but-not-yet-confirmed**. Its pending confirmations complete from
late peer acks arriving live post-recovery ([§6](#6-per-actor-recovery-contracts) aggregator).

On a **coil** peer the marks keep the same meaning and the same `acked ≥ confirmed`
relation, with the slow acked mark derived from `HardAck(PeerId.Coil)`. The fast-side band
`[softConfirmed + 1, fastBlockMark]` is then "blocks the coil peer durably finalized,
but the head quorum has not yet soft-confirmed" — the coil peer is not a
soft-confirmation input, so the band closes purely as head-peer acks arrive, exactly
as it would for a non-leading head follower.

**Each marker is the conceptual anchor for one consensus actor**, even though no
marker key is stored:

| Marker | Anchors | What that actor reads at boot |
|---|---|---|
| `fastBlockMark` | **`JointLedger*`* | the **`DepositMap`** CF (one keyed blob — the deposits map as of our last own soft ack). `previousBlockHeader` is reloaded from `BlockSpine[fastBlockMark].brief`. JL is at `Done(fastBlockMark)`; SC's `pending` rebuild is **not** JL's burden — SC loads `BlockResult`s directly (next row). |
| `hardAcked` | **`StackComposer*`* | **`Treasury`** (one blob — cumulative treasury UTXO ref) + **`EvacuationMap`** (keyed by `blockNum`, written only at committed blocks — major / SEC minor; load `EvacuationMap[StackSpine[hardAcked].lastBlockNum]` for the current map — that last block is always a committed one for a non-final stack, [§5.2](#52-state-recovery-the-base-snapshots)). Counters like `lastClosedStackNum` (the closing stack, its number unpacked from the last own `HardAck(own)` value), `lastClosedBlockNum` (from `UnsignedStack[lastClosedStackNum].brief.lastBlockNum` — every peer writes it on every close, so this also works for a follower), and `nextOwnHardAckNum` (= `max(own HardAck journal) + 1`) are derived. SC additionally reads `BlockResult` for `(StackSpine[hardAcked].lastBlockNum, head]` to rebuild `pending`. |
| `softConfirmed` | **`FastConsensusActor*`* | **Nothing of its own.** Cells `≤ softConfirmed` were dropped the moment they produced their `SoftConfirmation` record; cells `> softConfirmed` are in-flight and rebuilt by replay. |
| `hardConfirmed` | **`SlowConsensusActor*`* | **Nothing of its own.** Same shape: cells dropped at confirmation; in-flight tail rebuilds from replay. |

So the signers / ledgers (`JointLedger`, `StackComposer`) read non-trivial passive
state at their `*Acked` mark; the aggregators are **genuinely stateless across
their `*Confirmed` mark**. Re-aggregating the in-flight band is safe because
the aggregators sign nothing — replay just rebuilds the cells as it re-feeds
the band's briefs + acks.

**Replay resumes at the right offset for each actor:**

- **Signers / ledgers** resume at the acked mark `+ 1` (CR2 — no re-signing what we
  already signed). `JointLedger` picks up via `BlockWeaver`'s re-drive from
  `fastBlockMark + 1` ([§6](#6-per-actor-recovery-contracts)); `StackComposer` from `hardAcked + 1`.
- **Aggregators** resume at `*Confirmed + 1` — their cells
  reconstitute as the replayed briefs + acks land. Anything `≤` the confirmed
  mark has already become a `SoftConfirmation` / `HardConfirmation` record and no
  longer needs re-aggregation.
- **`BlockWeaver*`* holds no persistent state; it rebuilds mempool +
  `nextBlockNumber` from the replay tail starting at `fastBlockMark + 1` (driven by
  `JointLedger`'s signed timeline, not by confirmations).

The `2 + 3N + H` per-journal resume cursors that the `ReplayActor` uses to seek into
the store **derive from the markers** — which derive from CF scans — with no
extra per-journal or per-marker storage ([§5.3](#53-the-indices-algorithm-deriving-the-2--3n--h-journal-cursors)).

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
> 2. **The recovery base (a logical compacted view).** The base snapshot ([§5.2](#52-state-recovery-the-base-snapshots))
>    is a compacted *image* of state at the side's ack mark; the `ReplayActor`
>    replays the entries after it. A read-time construction over a copy — it
>    deletes nothing from the live store.
> 3. **RocksDB native compaction ([§7](#7-storage-design--rocksdb))** — the storage engine merging SST files.
>    Engine-internal, unrelated to either.
>
> **Hard rule:** physical deletion from the durable store **never descends below
> the latest hard-confirmed stack's last block** — that region holds the
> evacuation-critical state (R10). Soft-confirmation only prunes the soft-acks it
> subsumes (blocks themselves stay, the slow side still needs them);
> hard-confirmation is the physical-retention floor.

### 5.2 State recovery: the base snapshots

Replay starts from **per-side passive state at the ack mark**: the fast side
persists `DepositMap` (singleton snapshot) + `RequestHighWater` (per-block), the
slow `Treasury` (singleton) + `EvacuationMap` (per committed block). Each is
written in the same atomic `WriteBatch` as its side's own ack write (deposits +
request high-water per own soft ack, treasury + evac per own hard-ack
stack-close), so the state at the side's `*Acked` mark is always exactly aligned
with it and can never be torn from it.

These carry **only the non-derivable** passive state on each side:

- fast — **`DepositMap`** + **`RequestHighWater`** CFs (`JointLedger`):
    - `DepositMap` — one keyed blob, the deposits map at `fastBlockMark`. JL is at
      `Done(fastBlockMark)` after restore — `previousBlockHeader` reloads from
      `BlockSpine[fastBlockMark].brief`, deposits come from `DepositMap`, nothing else
      needed for JL's own state.
    - `RequestHighWater` — keyed by `blockNum`, a `Map[HeadPeerNumber, RequestNumber]`
      giving each peer's highest included request as of that block; recovery reads
      the entry at `fastBlockMark`. It is *not* `JointLedger` state; it feeds the
      Request journal recovery cursors ([§5.3](#53-the-indices-algorithm-deriving-the-2--3n--h-journal-cursors)), written here so the `fastBlockMark` entry
      stays aligned with the soft-ack and survives brief pruning.
- slow — **`Treasury`** + **`EvacuationMap`** CFs (`StackComposer`):
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
      `hardAcked` as `EvacuationMap[StackSpine[hardAcked].lastBlockNum]`, and
      that key is always present — the last block of any **non-final** stack is
      necessarily a major or a last-of-partition minor (a leading minor run that
      reaches stack end ends in its own SEC minor; any later partition is a major
      or the final). A **final** stack drains the map to empty and has no
      successor stack, so no cumulative map is loaded after it.

  SC's pairing maps (`pending` / `ready`) and counters rebuild from journals +
  `BlockResult` ([§6](#6-per-actor-recovery-contracts) `StackComposer`).

The snapshots do **not** carry per-journal cursors (those are derived, [§5.3](#53-the-indices-algorithm-deriving-the-2--3n--h-journal-cursors)), do
**not** carry the acked-but-unconfirmed band (those briefs / acks are still in
the journal CFs above the side's confirmed mark — unpruned), and do **not** carry
SC's `pending` map (rebuilt from the `BlockResult` CF on restart — JL writes
each block's result at ack time, [§3.3](#33-what-gets-stored-and-how-recovery-treats-it), [§6](#6-per-actor-recovery-contracts) `JointLedger`, so SC can load
`(StackSpine[hardAcked].lastBlockNum, head]` directly). The pending soft / hard
confirmations themselves complete in the **aggregators**
(`FastConsensusActor` / `SlowConsensusActor`) from the persisted briefs +
peers' late acks — the aggregators sign nothing, so they can re-acquire and
re-aggregate the band freely, unlike the signers.

### 5.3 The indices algorithm: deriving the 2 + 3N + H journal cursors

To replay, the `ReplayActor` needs an initial cursor for each journal: **2** for
the spines (one BlockSpine, one StackSpine), **+ 3 per head peer** (its
Request / Soft-ack / Hard-ack satellites), **+ 1 per hub** (its `HubHardAck`
journal, which every peer's SCA reads to reach the coil quorum) = **2 + 3N + H**
([§3.2](#32-satellites)); a coil peer also replays its own `HardAck(PeerId.Coil)` journal. Every one of them
**derives from the markers** via Hydrozoa's monotonicity invariants:

- **Requests** — monotonic per peer: *if a block includes Peer A's Request `N`,
  the next block includes only A's requests `> N`.* So A's Request journal cursor is
  A's highest-ever included request `+ 1`. This high-water is **persisted in the
  `RequestHighWater` CF** (per-block, keyed by `blockNum`, holding a per-peer map;
  written as blocks are produced) and read directly at boot from the `fastBlockMark`
  entry (`recoverHighWater` keys on `fastBlockMark`) — *not* recomputed by scanning briefs.
  Folding briefs is unsound: to get *every* peer's high-water you must scan back
  until each peer's latest included request appears (worst case to block 0), and
  once retention ([§5.1](#51-the-markers--all-derived-none-stored)) prunes old briefs below the hard-confirmed floor they are
  gone. The counter sidesteps both — `N` reads, immune to pruning. (This is the one
  request-side datum that *is* stored; the journal cursors themselves still derive
  from the markers.)
- **Blocks / stacks (the spines) — two floors each, one per consumer role.** Each
  spine is read by **two** roles: the aggregator (FCA / SCA), which resumes at the
  `*Confirmed` mark `+ 1` to rebuild in-flight confirmation cells, and the
  re-deriving ledger (`BlockWeaver` → `JointLedger` / `StackComposer`), which resumes at
  the `*Acked` mark `+ 1` to re-produce / re-close what it has not yet signed. So a
  spine has a **`confirmed` floor** and an **`acked` floor**, with
  `confirmed ≤ acked`. The `ReplayActor` scans the spine once from the lower
  (`confirmed`) floor and feeds the `acked` consumer only the suffix `≥ acked` —
  **the one place the no-re-sign-`≤ acked` rule (CR2) lives**, so no consensus
  actor re-filters for it. On the fast side the `confirmed` floor is marker-derived
  (`softConfirmed`) and the ledger (`acked`) floor is `fastBlockMark + 1`
  (`blockSpineForLedger`), the `BlockResult`-derived anchor. On the slow side
  `hardConfirmed` is marker-derived but the **acked stack** is *not* — `hardAcked`
  is a `HardAckNumber` counter, not a `StackNumber`, so the acked-stack floor is
  sourced separately (unpack the stack id from the last own `HardAck` value).
- **Soft acks** — single consumer (FCA). The soft-ack index *coincides with the
  block number* (one ack per block, [§3.2](#32-satellites)), so each peer's SoftAck journal cursor is the
  fast-side confirmed mark `+ 1`, exactly the BlockSpine `confirmed` floor.
- **Requests** — single consumer (`BlockWeaver`), at the per-peer high-water `+ 1`
  (the persisted counter above).
- **Hard acks** — single consumer (SCA), and the **one journal whose floor does *not*
  derive from a marker.** The HardAck journal is indexed by the author's own
  `HardAckNumber`, not by `StackNumber`, and no marker records the
  `StackNumber → HardAckNumber` correspondence — so there is no marker-findable
  floor. Recovery scans each peer's HardAck journal **from 0** (correct, not minimal).
  Tightening needs a per-peer hard-ack marker or a stack-indexed hard-ack journal;
  deferred.
- **Hub hard acks** (and a coil peer's own **`HardAck(PeerId.Coil)`** journal) — like
  `HardAck`, indexed by a `HardAckNumber` (per hub / per coil peer), with no marker-findable
  `StackNumber` floor, so recovery scans each such journal **from 0**. The single
  `ReplayActor.replay(own: PeerId, …)` (via `ReplayCursors`) scans + routes these on
  both peer types — the hubs' `HubHardAck` always, and on a coil peer additionally its
  own `HardAck(PeerId.Coil)` journal (`ReplayCursors.ownCoilHardAck`); the floor-tightening
  is the same hard-ack indexing gap.

So the cursors split into **dual-floor spines + single-floor satellites** (`4 + 3N`
floors over the `2 + 3N` head journals, plus single-floor `H` hub + coil hard-ack
journals). All derive from the markers + the request high-water counter, except the
slow-side acked stack and the hard-ack floors (the hard-ack-journal indexing gap). None of the cursors is stored.

**The deposits map is the exception — and it is not a journal**, so it has no cursor
to derive. The pending-deposits map mutates on *any* block (a request *adds* a
deposit; a major block *absorbs* an out-of-order **subset**), so it is neither
constant nor a marker-findable suffix and cannot be reconstructed from the journals.
It is carried instead as **snapshotted passive state** ([§5.2](#52-state-recovery-the-base-snapshots)), re-written on every
own soft ack — the one place a snapshot is unavoidable on the fast side.

**These index reads need no cross-CF atomicity.** They are taken journal by journal
(each `highWater` / marker is its own `lastKey` scan), never under one snapshot. That
is safe because every journal is **append-only** — entries are appended in index order
and never mutated ([§3.2](#32-satellites), [§7.1](#71-key-layout--journal-ids)): a reader — even one running concurrently
with the writer, e.g. a liaison re-seeding its lanes after the start barrier — sees a
consistent prefix. It can at worst miss the newest append, never a torn entry, and a
stale high-water self-corrects through the normal `append` / `backfill` path (and the
re-pull cursor protocol). The one read carrying a **cross-CF invariant** — the
markers' `confirmed ≤ acked` — is derived **before** the start barrier, while no
writer is producing yet ([§8](#8-boot-sequence)), and is fail-safed by `validateInvariants`.

### 5.4 Total order of the replayed streams

Within a journal, order is intrinsic — the journal's own index (RequestNumber;
Block/StackNumber; ack index). To order entries *across* the `2 + 3N + H` streams,
**every persisted journal value carries a 12-byte big-endian `ArrivalStamp` prefix**
— a `(generation, monotonic)` pair (receipt time for inbound entries, creation for
own ones — [§7.1](#71-key-layout--journal-ids)); merging by stamp yields a fixed, durable interleaving — "not
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
leans on ([§5.6](#56-the-replay-mechanism)) — so the recorded order isn't strictly required for committed-state
correctness. It is kept as a **known-good serialization**: a safety net if
robustness is imperfect, the order the one-by-one oracle ([§5.6](#56-the-replay-mechanism)) replays, and a
faithful record of the run.

**The stamps order streams; they are not a clock.** Time-dependent decisions do
*not* replay off them — block-cut timing uses the live wall clock, with committed
blocks keeping the times in their persisted brief headers and in-flight blocks
re-cut fresh ([§6](#6-per-actor-recovery-contracts)).

### 5.5 The L1 boundary is re-sampled live, not replayed

Not every input is replayed from the log. The **L1 boundary** (`CardanoLiaison`
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
`CardanoLiaison`.** `BlockWeaver` (and hence `JointLedger`'s deposit decisions) needs a
`PollResults` — the set of utxos at the head's multisig address — to make
forward progress, and after a restart it has none. `CardanoLiaison` *will* poll and
forward one on its own cadence (its poll tick / next-effect trigger), but there is
**no guarantee it does so promptly**, and replay must not stall waiting on the
boundary's timer. So the `ReplayActor` reads the L1 state **directly from
`CardanoBackend`** (`utxosAt(config.initializationTx.treasuryProduced.address)` →
the utxo ids) and sends a `PollResults` straight to `BlockWeaver` at replay start —
byte-for-byte the same message `CardanoLiaison` emits in steady state
(`CardanoLiaison.runEffects` → `blockWeaver ! PollResults(...)`), just sourced
eagerly by recovery instead of on the poll timer. Subsequent live `PollResults`
from `CardanoLiaison` flow normally once it is running. This is still the **live**
L1 read of [§5.5](#55-the-l1-boundary-is-re-sampled-live-not-replayed) ("re-sampled live, not replayed") — merely *initiated by* the
`ReplayActor` rather than awaited from the boundary — so it carries the same
above/below-the-confirmed-mark semantics.

### 5.6 The replay mechanism

**Replay re-feeds journals, and only journals.** The replicated set's journal entries
([§3.2](#32-satellites)) are the sole replay input; every *non-journal* input is reproduced, not
replayed — inter-actor signals (`StartBlock`/`CompleteBlock`, the soft-confirm
fan-out, `GetState`) regenerate from the interior cascade, and L1 poll results
are re-sampled live ([§5.5](#55-the-l1-boundary-is-re-sampled-live-not-replayed)) — with the `ReplayActor` seeding the first
`PollResults` into `BlockWeaver` straight from `CardanoBackend`, rather than waiting
on `CardanoLiaison`'s poll cadence. So an actor whose inputs are *all* non-journal signals
gets base state but no tail — `JointLedger`, for one, reads no journal (`BlockWeaver`
drives it), so the `ReplayActor` never feeds it ([§6](#6-per-actor-recovery-contracts)). This holds for either
mechanism below.

Two candidate mechanisms for *how* to feed that journal tail; we target **(1)** and
keep **(2)** as a test oracle.

1. **Pre-populate mailboxes ("crash as a state").** Create the consensus actors
   suspended, seed each with its passive base state, and drop the total-ordered
   **journal-entry tail** into the mailboxes of the actors that read those journals (e.g.
   `BlockWeaver` the request journals; the ack-aggregators the ack journals). Then open
   the start barrier and let them run concurrently to the crash state. Fast,
   reuses the existing `Deferred[Connections]` barrier (though a **separate
   suspend barrier** may be needed to hold actors during seeding — [§8](#8-boot-sequence), [§10](#10-open-questions) Q1),
   and unifies replay with normal operation (cold boot = the empty-seed case).
   Boundary actors start *together* with the consensus actors ([§5](#5-recovery-architecture)); during replay
   the interior's external re-emissions reach the running boundaries but are
   dropped by the same cursor filter that suppresses live duplicates (each
   boundary loads its cursor before processing anything; [§5](#5-recovery-architecture)). What is restored is
   the **base snapshot + journal-entry tail in mailboxes**; everything else regenerates
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
**Inputs**, **Persists** — and is grouped by [§2](#2-two-kinds-of-actor)'s split.

### 6.1 Boundary actors

These recover by **state restoration only** — reload cursors / counters / snapshots and
re-observe L1. They own the process's external surfaces (the peer network, the chain), so
there is nothing to replay into them.

#### 6.1.1 `RequestSequencer`

- **State:** next `RequestNumber` (`nextRequestNumRef`, starts `0`).
- **Recover:** `next = max(persisted own RequestWithId).requestNum + 1` (empty →
  `0`). CR3 is a corollary of CR4 here.
- **Inputs:** `UserRequest.Sync` — user-originated, droppable (crash before
  persist → user resubmits).
- **Persists:** the produced `UserRequestWithId`, **before `dResponse.complete(id)`**
  — before the user is told the id, not merely before broadcast (CR1/CR4).
- **Coil peer:** **inert.** It fills the shared `Connections` slot but no HTTP
  surface routes requests to it and a coil peer authors none (`coil-network.md`
  §5.2), so on a coil peer it has nothing to persist or recover.

#### 6.1.2 `PeerLiaison*` (three shapes)

A liaison instance serves exactly **one** remote peer (the peer↔peer boundary is three
shapes over one shared `GetMsgBatch` / `NewMsgBatch` protocol, `coil-network.md` §5.5).
All three recover the **same way** — restore the outbound high-water and the inbound
receive cursors, leave the queues empty, and serve the outbox as a **DB-backed view** —
differing only in which lanes each serves.

- **State:**
  - the **receive cursors** (one per lane) — the `GetMsgBatch` fields: `batchNum`,
    `requestNum`, `blockNum`, `softAckNum`, `hardAckNum`, and `hubHardAckNum` where the
    link carries it;
  - the **outbound outbox** queue(s). There is **no inbound queue** — an inbound entry
    is verified against its cursor and forwarded immediately (persisted then dispatched),
    never held;
  - the **current requesting batch** (the in-flight pull awaiting a `NewMsgBatch`).
- **Recover.** Everything that ever reaches a liaison outbox is **persisted before
  it is appended** (CR4 — nothing leaves the process undurable), so the journal is the
  source of truth and the outbox is only a cache. Recovery therefore restores almost
  nothing:
  - **Queue stays empty.** No payloads are eagerly seeded; it fills only as live
    production appends new items, and `reply` hot-loads anything else from the journal
    on demand.
  - **One scalar is restored** — the high-water number (`lastAppended = max(journal
    key)`, payload-free — `LaneOutgoingBacking.highWater`). It is not state to serve; it
    exists so (a) the first post-crash `append` is legal — live production resumes at
    `high-water + 1`, and `append` is gap-free (requires `next(lastAppended)`), so a
    cold `None` would make it throw — and (b) it is `reply`'s out-of-bounds bound
    (`next(lastAppended)`) for a remote that re-pulls before we append anything.
  - **Serving is a DB-backed view.** On `GetMsgBatch` from R, `LaneOutbound.reply`
    returns the in-memory tail if it holds R's cursor, else hot-loads the prefix from
    the journal (`LaneOutgoingBacking.backfill`). Because every served entry is persisted, the
    journal always backs it — the cache need never be authoritative.
  - **`append` is idempotent below the high-water.** A consensus actor re-emitting an
    already-durable entry during replay (e.g. `SlowConsensusActor` re-broadcasting the
    in-flight stack's round-1 ack, number `n1`, after the lane restored its high-water
    to round-2 `= n1 + 1`) is a **no-op**, not an out-of-order error — the lane serves
    that entry from the journal. The strict gap-free check applies only to genuinely new
    items (above the high-water).
  - **Inbound receive cursors are restored**, to `next(max(persisted journal))` — the
    inbound counterpart of the high-water (`LaneInbound.restoreCursor`, off a
    `LaneIncomingCursors` read — whole-CF `lastKey`, no `keep` and no payload decode;
    `next` is `+1` on a contiguous lane, the remote's leader schedule on a sparse spine). This is **load-bearing, not an optimization**:
    each inbound journal entry is persisted before its cursor advanced (CR8), and the
    `ReplayActor` re-feeds the consensus actors from those journals on boot — so if the
    lane stayed at its cold initial cursor it would **re-pull and re-dispatch** the same
    entries the `ReplayActor` already replayed (the liaison has no dedup; it forwards
    whatever `verify` accepts). With the cursor restored, the remote is re-pulled only
    for **new** items, and a stale re-serve of an already-held entry falls *below* the
    cursor, so `verify` rejects it (never dispatched). The receiving liaison is the only
    side that restores this; `ReplayActor` remains the single consensus re-feeder.
- In steady state the queue is a write-through cache, so a cold cache *is* recovery —
  same procedure. The whitepaper already prunes these outboxes by **remote
  `GetMsgBatch` cursors**, not local confirmation, *"so that messages can be
  retransmitted if needed during recovery scenarios"* ([peer-network](https://)
  `#outbox-queues-and-confirmation`); persistence extends that retransmissibility
  across a process restart, not just a transient disconnect. Reading from the
  store on demand (rather than eagerly seeding the whole own production) also
  bounds the in-memory outbox to the live tail.
- **Inputs:** remote lane entries — **cursor-gated (CR8)**.
- **Persists:** each inbound remote lane entry into its journal (CR8); each persisted
  value carries a **12-byte `ArrivalStamp` prefix** ([§5.4](#54-total-order-of-the-replayed-streams), [§7.1](#71-key-layout--journal-ids)) — `(generation,
  monotonic)` at receipt. The boundary that durably stores data it did not produce.
  `PeerLiaisonHubToCoil` additionally writes its coil peer's inbound `HardAck` to
  the hub's `HardAck(PeerId.Coil)` receive journal (so the hub can stamp a
  received-but-unstamped ack from the store after a crash — [§6](#6-per-actor-recovery-contracts) `CoilAckSequencer`),
  the one extra inbound-persist among the three shapes; it also restores that lane's
  **receive cursor** to `max(HardAck(PeerId.Coil)) + 1` on boot, so a coil is re-pulled
  only for new acks.

| Shape | Outbox lanes (served) | Inbound lanes (persisted) |
|---|---|---|
| `PeerLiaisonHeadToHead` (head ↔ head) | this head peer's own production: `Block` (sparse, own-led), `Stack` (sparse), `Request`, `SoftAck`, `HardAck`, and `HubHardAck` if it is a hub | the remote head peer's same set |
| `PeerLiaisonHubToCoil` (hub → coil) | the **full** population: `Block` (contiguous), `Stack`, `Request ×N`, `SoftAck ×N`, head `HardAck ×N`, `HubHardAck ×H` | the coil peer's own `HardAck` → the hub's `HardAck(PeerId.Coil)` receive CF |
| `PeerLiaisonCoilToHub` (coil → hub) | this coil peer's own `HardAck` (one lane) | the **full** population (mirror of `HubToCoil` outbound) |

> **Code reality (2026-06).** All three shapes recover the same way: each outbound
> lane is built with a `LaneOutgoingBacking` over its journal, `preStart` restores the
> lane high-waters (`seedHighWater`), and `LaneOutbound.reply` hot-loads below the
> in-memory floor (`LaneOutgoingBacking.backfill`); inbound receive cursors restore off a
> `LaneIncomingCursors` read. No lane eagerly seeds its whole own
> production. The earlier `recover` → `OutboxSeed` → `LaneOutbound.seed` (eager,
> `HeadToHead`-only) is gone (GUM-153).

#### 6.1.3 `CardanoLiaison`

- **State:**
  - `targetState` — `Active(treasuryUtxoId)` | `Finalized`;
  - `effectInputs: Map[TransactionInput, EffectId]` — the "which effect spends this input" index;
  - `happyPathEffects: TreeMap[EffectId, HappyPathEffect]`;
  - `fallbackEffects: Map[BlockVersion.Major, FallbackTx]`.
- **Recover:** fold over the **`HardConfirmation` CF** ([§3.3](#33-what-gets-stored-and-how-recovery-treats-it), multisigned effects /
  SECs / fallbacks in full); `targetState` seeded from
  `config.initializationTx.treasuryProduced.utxoId` until stack-0 hard-confirms;
  effects faulted in lazily. **Submission progress is recovered from L1 itself**
  (poll → observe the current treasury utxo) — L1 is the durable truth,
  re-sampled live ([§5.5](#55-the-l1-boundary-is-re-sampled-live-not-replayed)), so there is no own progress marker.
- **Inputs:** `Stack.HardConfirmed`, `Timeout` (poll tick).
- **Persists:** nothing of its own (signs nothing ⇒ no CR2; submits multisigned
  txs). The lazy load is `effectInputs(observedInput) → happyPathEffects(effectId)`.
- **Coil peer:** **identical.** `CardanoLiaison` runs unchanged on a coil peer — it
  submits happy-path + fallback effects (R8/R9) and recovers off the same
  `HardConfirmation` CF + live L1, with no role gate (`coil-network.md` §7).

### 6.2 Consensus actors

These recover by **base-state seed + replay** — load the base snapshot at the side's ack
mark (`fastBlockMark` / `hardAcked`), then the `ReplayActor` re-runs the input tail from that
mark onward, regenerating the inter-actor signals deterministically.

#### 6.2.1 `BlockWeaver`

- **State:**
  - the role FSM (`DecidingRole` → `Leader` / `Follower`);
  - `mempool`;
  - `nextBlockNumber`;
  - transient `pollResults` / finalization trigger / `Wakeup`.
- **Recover (replay):** base = state at `fastBlockMark`; replay `[fastBlockMark + 1, head]`.
  Mempool and `nextBlockNumber` follow from the replay. **Block-creation timing
  uses the live wall clock**: blocks `≤ fastBlockMark` are not re-led (their
  `BlockCreationStart/EndTime` already sit in the persisted brief headers), and a
  block left mid-`Producing` at the crash was never saved or acked, so `BlockWeaver`
  just **cuts a fresh block** under the current clock — a never-committed cut may
  legitimately differ, and only committed state must match ([§9](#9-failure-scenarios-to-specify--test)). The transient
  `pollResults` is seeded **by the `ReplayActor`**, which reads L1 directly from
  `CardanoBackend` and sends the first `PollResults` rather than waiting on
  `CardanoLiaison`'s poll cadence ([§5.5](#55-the-l1-boundary-is-re-sampled-live-not-replayed)) — so deposit decisions can proceed
  immediately on replay.
- **Inputs:** `UserRequestWithId`, `BlockBrief.Next`, `Block.SoftConfirmed`,
  `PollResults` (the first one from the `ReplayActor`'s direct `CardanoBackend`
  read; later ones live from `CardanoLiaison` — [§5.5](#55-the-l1-boundary-is-re-sampled-live-not-replayed)), `Wakeup`, finalization trigger.
- **Persists:** nothing — authors no journal; drives `JointLedger` via
  `StartBlock`/`CompleteBlock{Regular,Final}`, which authors the brief.
- **Coil peer:** **follower-only** — never enters Leader role, drives `JointLedger`
  from observed `BlockBrief.Next` + relayed requests + `PollResults` exactly as a
  head follower does (`coil-network.md` §5.2). Recovery is identical: it holds no
  persistent state and rebuilds mempool + `nextBlockNumber` from the replay tail
  off the `fastBlockMark + 1` ([§5.1](#51-the-markers--all-derived-none-stored)).

#### 6.2.2 `JointLedger`

Post-split, owns only the fast-side state; treasury moved to `StackComposer`.

- **State:** one of two shapes —
  - `Done(previousBlockHeader, deposits)` — between blocks;
  - `Producing(previousBlockHeader, deposits, l2LedgerState, startTime, userRequestState)` —
    mid-block.
- **Recover:** restore `Done(previousBlockHeader, deposits)` from two sources —
  load the **`deposits` map** from the snapshot bundled with the `fastBlockMark` ack
  ([§5.1](#51-the-markers--all-derived-none-stored), [§5.2](#52-state-recovery-the-base-snapshots)), and reload `previousBlockHeader` from `BlockSpine[fastBlockMark]` (its
  brief is already persisted there; no need to duplicate it in the snapshot).
  Those two are `JointLedger`'s whole passive state. It then **co-anchors the L2
  ledger**: reads `L2CommandNumber[fastBlockMark]` and calls `l2Ledger.restoreTo(it)` so
  the committed L2 state matches the `fastBlockMark` boundary (see the L2 co-anchoring
  note above). (The acked-but-unconfirmed
  band `[softConfirmed + 1, fastBlockMark]` is **not** `JointLedger` state: those
  blocks are already applied — `JointLedger` is at `Done(fastBlockMark)` — and their
  pending soft-confirmations complete in the **aggregator**, not here; [§5.2](#52-state-recovery-the-base-snapshots).)
  `JointLedger` is **not fed by the `ReplayActor`**; `BlockWeaver` replays its own
  tail and re-drives `JointLedger` via `StartBlock`/`CompleteBlock` + forwarded
  requests, reproducing the `[fastBlockMark + 1, head]` blocks. A mid-`Producing`
  block is discarded and re-produced (a never-committed cut is safely redone).
- **Inputs (all interior signals — none from the `ReplayActor`):** from
  **`BlockWeaver*`* — forwarded `UserRequestWithId` (deposits fold into `deposits`)
  and `StartBlock`/`CompleteBlock{Regular,Final}`; from the fast aggregator —
  `Block.SoftConfirmed.Next`. (`GetState.Sync` is a defined query with no current
  sender.)
- **Persists:** when `JointLedger` finalizes a block it writes a per-block bundle in
  one atomic `WriteBatch` (CR4/CR6/CR8). The **fast-side snapshot** part fires on
  **every** peer (head and coil), keyed by `blockNum`:
    - the per-block **`BlockResult` deltas** → **`BlockResult`** CF (the `brief` is
      **not** stored — it already lives in the `Block` journal and is rehydrated from
      there at recovery, so it is not duplicated across two journals; [§3.3](#33-what-gets-stored-and-how-recovery-treats-it)) — so
      `StackComposer` can rebuild its `pending` map from disk on restart by loading
      `(StackSpine[hardAcked].lastBlockNum, head]` directly, instead of relying on
      `JointLedger` to re-emit results below the fast anchor,
    - the current **deposits snapshot** → **`DepositMap`** CF (single keyed blob;
      overwrites the previous snapshot),
    - the cumulative **request high-water** → **`RequestHighWater`** CF (keyed by
      `blockNum`, a `Map[HeadPeerNumber, RequestNumber]`). Feeds the Request-journal
      recovery cursors ([§5.3](#53-the-indices-algorithm-deriving-the-2--3n--h-journal-cursors)) — recovery reads the entry at the fast anchor,
    - the **L2 command number** → **`L2CommandNumber`** CF (keyed by `blockNum`, a
      single `Long`): `l2Ledger.currentCommandNumber` read after this block's L2
      commit. On recover `JointLedger` reads the fast-anchor entry and calls
      `l2Ledger.restoreTo` to co-anchor the committed L2 state.

  A **head** peer additionally writes, in the same batch, its own `BlockBrief` →
  **`BlockSpine`** (leader only — the spine author) and its own **soft-ack** → its
  own-author **`SoftAck`** CF. A **coil** peer writes neither: it never leads, and it
  **authors no soft-ack** ([§2](#2-two-kinds-of-actor)).
- **Fast-side anchor — the `fastBlockMark`.** The fast side recovers from one
  mark on both peer types: `fastBlockMark = max(BlockResult.key)`
  (`Markers.recoverFastBlockMark`) — the highest block this peer durably finalized.
  `State.recover(persistence, l2Ledger, fastBlockMark)` rebuilds `Done` from `doneAt`:
  the header from the `Block` journal (present on every peer, leader-written or inbound
  via `persistInbound`) plus the `DepositMap`, then the L2 co-anchor via
  `L2CommandNumber[fastBlockMark]`. No marker key is written — the mark derives from a
  single-CF scan ([§5.1](#51-the-markers--all-derived-none-stored)). The persisted `BlockResult` holds only the deltas — its `brief`
  is rehydrated from the `Block` journal at recovery (`BlockResult.fromPersisted`), not
  stored twice. Soft-**confirmation** is *not* written here — that is
  `FastConsensusActor`'s job (below). The own soft-ack's equivocation guard is a
  `PeerLiaison*`-boundary fact per CR2; `JointLedger` computes the signature.
  - **Why both peer types anchor on `BlockResult`.** The `BlockResult` CF is written
    every block by every peer, so `fastBlockMark` is identical on head and coil: on a
    head peer `max(BlockResult)` equals `max(own SoftAck)` (both written in the same
    atomic per-block batch), and a coil peer authors no soft-ack. The snapshot writes
    are factored into a role-independent `snapshotBundleBatch` that fires on every peer;
    a head peer's `persistOwnAckBundle` adds its own `Block` (leader) + `SoftAck` lanes
    on top, a coil peer's bundle writes only the subset (the brief broadcast + own-ack →
    `FastConsensusActor` stay head-only).
  - *Why not a coil soft-ack:* we considered having a coil peer produce a real soft-ack
    as the anchor ("same shape" with a head peer). That needs the `SoftAck` wire type's
    author widened from `HeadPeerNumber` to `PeerId`, which ripples through
    `FastConsensusActor` / liaisons / codecs even though the coil ack is never
    disseminated or aggregated. Anchoring on `BlockResult` avoids the widening and
    keeps `SoftAck` head-only — the fast-side bundle is identical either way.
- **L2 co-anchoring (keyed by an L2 command number — *not* by any ack).**
  `Producing.l2LedgerState` is WIP; the committed L2 state lives in the `L2Ledger`
  black box (its own persistence). That black box **knows nothing of acks, blocks,
  stacks, or confirmations**, so its persistence is keyed by something intrinsic to
  *its own* operation, never by a consensus mark like `fastBlockMark`. The key is the
  L2's own **monotonic command number** — a commit counter it increments on each
  state-mutating command. (A content hash of the evacuation map was considered and
  dropped: no cheap whole-map hash exists — only a slow `kzgCommitment` BLS pairing
  and a private per-entry blake2b — and the EUTXO ledger holds `activeUtxos` + diffs,
  not an assembled map, so a content key would cost a projection + digest per commit.
  Recovery needs only a stable per-commit key, which the command number gives for free.) The
  L2 reconstructs from `(initial state, target command number)`; the interface is
  `restoreTo(commandNumber)`. The **consensus → L2 translation lives entirely on the
  `JointLedger` side**: `JointLedger` reads the L2's `currentCommandNumber` right after the
  block's commits and records it per own soft-ack as that block's command number (the
  per-block `Cf.L2CommandNumber`, written in the same atomic bundle as the soft-ack); on
  recover it restores `Done(fastBlockMark)` then calls `restoreTo(L2CommandNumber[fastBlockMark])`.
  The co-anchoring requirement is still exact — the two must agree on the same committed
  point — but the membrane is never crossed by an ack; only a command number passes.
  - **Command counting — how & why.** The command number is the **L2's own** counter: it
    keys the L2's log / snapshot and is the argument to `restoreTo`, so the L2 holds it
    internally no matter what. `JointLedger` learns each block's value by reading
    `l2Ledger.currentCommandNumber` **after** that block's L2 commits (inside
    `persistOwnAckBundle`, just before the atomic batch) — a clean **post-block** boundary value
    (block N's commands are done; N+1's haven't started). The read is a non-atomic query in
    general, but **safe here because `JointLedger` is the sole, single-message-at-a-time L2
    driver** — nothing commits between a block's last command and the read. *Why a query, not
    `JointLedger`-side counting:* JL could count the successful real commands itself (it issues
    them all and sees each result), but the L2 **already** owns the counter above, so counting
    on JL's side would *duplicate* it and force JL to mirror the L2's "which commands advance"
    rule — two definitions that can silently drift into a wrong `restoreTo` (corrupt
    recovery). The query keeps that rule single-sourced in the L2; `JointLedger` only reads. If
    the trait surface ever needs to shrink (so a remote L2 need not expose a query), the
    cleaner alternative is to **return the new command number with each commit** (or carry it
    on the consumer-side `L2LedgerState`) — same single source, no query, no JL counting.
    Either way the value is **persisted** per soft-ack and read back at recover; an in-memory
    count would not survive the crash.
  - **Cost / optimization.** Full L2 state is far larger than the deposits map, so
    snapshotting it at *every* commit may be too expensive. The `L2Ledger` may
    instead snapshot **less frequently** and restore-to-command-number by loading its nearest
    snapshot `≤ commandNumber` and replaying its own command log forward to the target — the
    usual snapshot-interval-vs-replay-length tradeoff, internal to the black box. The
    mechanism is delegated / out-of-scope here; the contract is the
    `restoreTo(commandNumber)` interface plus JL holding the soft-ack → command-number mapping.

#### 6.2.3 `FastConsensusActor`, `SlowConsensusActor`

- **Aggregators that write the confirmation record.** Each holds in-flight cells
  (per-block soft-ack cells / per-stack hard-ack cells); when a cell reaches
  threshold it writes its confirmation record in one atomic `WriteBatch` (CR4):
    - `FastConsensusActor`: write the **`SoftConfirmation`** record (block header +
      aggregated multisig over the soft-acks) to the **`SoftConfirmation`** CF
      (keyed by `blockNum`), and prune the now-redundant soft-acks for this
      block from `SoftAck`. No marker key is written — `softConfirmed` derives
      as `max(SoftConfirmation.key)` ([§5.1](#51-the-markers--all-derived-none-stored)).
    - `SlowConsensusActor`: write the **`HardConfirmation`** record (multisigned
      effects / SECs / fallbacks in full) to the **`HardConfirmation`** CF
      (keyed by `stackNum`), and prune the now-redundant hard-acks from
      `HardAck`. `CardanoLiaison` submits the effects from this record. No marker
      key written — `hardConfirmed` derives as `max(HardConfirmation.key)`.

  See [§3.3](#33-what-gets-stored-and-how-recovery-treats-it).
- **Recover (replay):** rebuild only the **in-flight cells** above the side's
  confirmed mark — `FastConsensusActor` for cells `> softConfirmed`,
  `SlowConsensusActor` for cells `> hardConfirmed` — from briefs + acks (fast side)
  / the persisted `Stack.Unsigned` + acks (slow side). Re-acquire the
  acked-but-unconfirmed band (`[softConfirmed + 1, fastBlockMark]` on the fast side;
  `[hardConfirmed + 1, hardAcked]` — at most one stack, single-flight — on the
  slow side, where the `ReplayActor` reconstructs a `StackHandoff` from that
  stack's `UnsignedStack` + the own acks, so SCA forms the cell via its existing
  path; `StackComposer` stays out) — signing nothing, the aggregator may
  re-aggregate freely. The aggregator does **not** reload past
  confirmation records: a cell is dropped once it confirms, so confirmations
  below the confirmed mark are not aggregator state — they were persisted for
  **downstream consumers** (`CardanoLiaison` folds over `HardConfirmation`,
  evacuation reads SECs), who load them. The confirmed mark alone gives the
  floor below which a late ack is ignored. The own-ack signatures belong to
  the signers (`JointLedger` / `StackComposer`), guarded at the `PeerLiaison*`
  boundary (CR2).
- **Head vs coil.** `FastConsensusActor` aggregates **head-peer** soft-acks into
  `SoftConfirmation` on both node types — a coil peer authors no soft-ack at all ([§6](#6-per-actor-recovery-contracts)
  `JointLedger`), so FCA on a coil peer has no own ack to add and recovers identically. `SlowConsensusActor` reaches the population quorum by
  aggregating all head-peer `HardAck`s **plus `coilQuorum` coil acks read from the
  `HubHardAck` journals**; on recovery those coil acks reconstitute as the replayed
  `HubHardAck` tail lands, exactly as head acks reconstitute from `HardAck`. The one
  wiring difference: a coil peer's own hard-ack is broadcast **up its uplink**
  (`PeerLiaisonCoilToHub`) rather than across the mesh — but `StackComposer` has
  already persisted it (next section) before the handoff, so the write-before-send
  barrier (CR4) holds on the uplink unchanged. `ReplayActor.route` has one
  `case k: JournalKey.HardAck` feeding the unified `PeerId`-keyed journal (→ SCA) and a
  `HubHardAck` case (→ SCA via its `.ack`); the single `ReplayActor.replay`
  scans the hubs' `HubHardAck` on **every** peer type (via the `hubs` param) and, on a
  coil peer, additionally its own `HardAck(PeerId.Coil)` journal (`ReplayCursors.ownCoilHardAck`) — so
  head and coil peers both re-feed the coil quorum and rebuild the in-flight slow-side
  cell on recovery.

#### 6.2.4 `StackComposer`

The slow-side mirror of `JointLedger`. It pairs each `BlockResult` (from `JointLedger`)
with its `Block.SoftConfirmed` (from `FastConsensusActor`) by `blockNum`, closes a
stack from the longest contiguous prefix of paired blocks (leader) or from exactly
the leader's announced range (follower), derives the `StackEffects` (settlement /
fallback / rollouts / SECs / post-dated refunds, threading the treasury and folding
the evacuation map for KZG), and **signs this peer's hard-acks** for the stack.

- **State** (`StackComposer.State`):
  - the pairing maps `pending` / `ready`;
  - `inboundLeaderBrief` — remote leader `StackBrief`s;
  - `lastClosedStackNum` / `lastClosedBlockNum`;
  - `previousStackHardConfirmed` — the single-flight gate (close `n+1` only after stack
    `n` hard-confirms);
  - `nextOwnHardAckNum`;
  - the two **cumulative, non-derivable** values — `treasury` (the
    `MultisigTreasuryUtxo` chain, rotated on every settlement / finalization) and
    `evacuationMap` (the cumulative L2 evacuation map; the KZG commitment is computed
    from it), the slow-side analogue of `JointLedger`'s deposits map.
- **Recover** (same order):
  - `pending` rebuilds **directly from disk** by scanning the **`BlockResult`** CF over
    `(lastClosedBlockNum, head]` (JL wrote each block's result at ack time, [§6](#6-per-actor-recovery-contracts)
    `JointLedger`); `ready` and the `Block.SoftConfirmed`s for that band come from FCA's
    in-flight replay;
  - `inboundLeaderBrief` comes from the replayed **StackSpine**;
  - `lastClosedStackNum` is the closing stack — its number unpacked from the last own
    `HardAck(own)` value; `lastClosedBlockNum =
    UnsignedStack[lastClosedStackNum].brief.lastBlockNum`. The `lastBlockNum` is read
    from `UnsignedStack` (not the `Stack`-lane brief) for **both** peer types: every
    peer writes `UnsignedStack` on every close, atomic with the hard-ack, whereas the
    `Stack`-lane brief is leader-gated — so reading `UnsignedStack` is also the
    follower-boot correctness fix;
  - `previousStackHardConfirmed = (hardAcked ≤ hardConfirmed)`, re-armed when that
    stack's `Stack.HardConfirmed` arrives / loads;
  - `nextOwnHardAckNum = max(own HardAck journal) + 1` — over the unified
    `HardAck(own)` journal (`HardAck(PeerId.Head)` on a head peer, `HardAck(PeerId.Coil)`
    on a coil peer), one read for both peer types (CR3);
  - `treasury` from the `Treasury` CF and `evacuationMap` from
    `EvacuationMap[lastClosedBlockNum]` — the cumulative non-derivable
    state as of `hardAcked` (that key is always present for a non-final `hardAcked`
    stack: its last block is always committed, [§5.2](#52-state-recovery-the-base-snapshots); older committed `EvacuationMap`
    entries stay on disk for the rule-based evacuation read, not loaded into SC state).
  - Then close stacks `> hardAcked` — never `≤ hardAcked` (no re-signing, CR2).
- **Bootstrap vs recovery (one seam, two seeds).** On a **cold** store `PreStart`
  runs `bootstrapInitialStack`: stack 0's init + fallback are derived from `config`
  (no `StackBrief` is broadcast — every peer derives it identically), seeding
  `treasury` / `evacuationMap` from `config.initializationTx.treasuryProduced` /
  `config.initialEvacuationMap`. On a **non-empty** store that genesis seed is
  replaced by the loaded snapshot, and stack 0 — long since hard-confirmed — is
  **not** recomposed. (Cold start is the degenerate case, [§5](#5-recovery-architecture).)
- **Inputs:** `BlockResult` (`JointLedger`), `Block.SoftConfirmed` (`FastConsensusActor`),
  `StackBrief` (remote leader, via `PeerLiaison*` — the StackSpine), `Stack.HardConfirmed`
  (`SlowConsensusActor` — arms the next close).
- **Persists:** at each stack close, one atomic `WriteBatch` (CR4/CR6/CR8),
  **before the handoff to `SlowConsensusActor*`* (whose own acks SCA immediately
  broadcasts — so they must be durable before crossing the peer boundary),
  spanning five CFs:
    - the closed **`Stack.Unsigned`** (brief + locally-derived effects) →
      **`UnsignedStack`** CF (keyed by `stackNum`; every close, leader or
      follower) — the source `SlowConsensusActor`'s recovery re-forms its in-flight
      cell from (a `HardAck` signs the effects, which the `StackBrief` alone does
      not carry),
    - the own `StackBrief` → **`Stack`** CF (leader only — StackSpine author; never
      on a coil peer, which never closes as leader),
    - this peer's `HardAck`s for the stack (round-1 + round-2, or the sole ack)
      → the unified **`HardAck`** journal, `put(JournalKey.HardAck(ack.peerId, ack.hardAckNum))`
      routing to the per-author CF — `HardAck(PeerId.Head)` on a head peer,
      `HardAck(PeerId.Coil)` on a coil peer — so this one path serves both peer types
      byte-for-byte (the head/coil distinction is the `PeerId` in the key, not a separate
      CF kind),
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
      back nothing on-chain, so the dispute can never need them (see [§5.2](#52-state-recovery-the-base-snapshots)).

  Bundling the two snapshots with the acks keeps them aligned with the
  `hardAcked` anchor — exactly as deposits ride `JointLedger`'s soft-ack write.
  The unsigned `StackEffects` **are** persisted (inside `Stack.Unsigned`, above)
  so the slow-side aggregator can re-form its in-flight cell on recovery; they
  remain deterministically re-derivable from the prefix + `treasury` +
  `evacuationMap` (`StackEffectsBuilder`) — which is how a follower builds them in
  the first place.
  Hard-**confirmation** is *not* written here — it is `SlowConsensusActor`'s
  output (above). Round-2 is signed at close but released by `SlowConsensusActor`
  only after local round-1 confirmation, so it must be durable **before** that
  release (CR4). The own hard-ack equivocation guard is a `PeerLiaison*`-boundary
  fact per CR2; `StackComposer` computes the signatures (`EffectSigner`,
  Ed25519-deterministic ⇒ re-derivation is not equivocation).

#### 6.2.5 `CoilAckSequencer`

A hub assigns each of its coil peers' hard-acks a per-hub sequence number and
publishes the **`HubHardAck`** journal the population reads to reach the coil quorum
(`coil-network.md` §5.3). It *sequences* and fans out but signs nothing. It is a
**consensus actor** not because it seeds a base snapshot at an ack mark (it has none),
but because the `ReplayActor` re-feeds its input tail on boot exactly as it re-feeds the
other consensus actors (step 4 of `replay`, hub only); what it restores *directly* is
just its sequence scalar and the per-coil-peer stamp floor that bounds that replay.

- **State:**
  - `nextSeq` — the next `HubHardAckNumber` to assign;
  - a per-coil-peer **stamped-high-water** `Map[CoilPeerNumber, HardAckNumber]` — the
    highest coil ack number already sequenced onto the spine, per coil peer. This is not an
    idempotency guard against retransmits (the hub's `PeerLiaisonHubToCoil` restores its
    coil-ack receive cursor on boot, so a coil never re-serves an ack already held —
    [§6.1.2](#612-peerliaison-three-shapes)); it is the **floor of the gap** the replay must still stamp.
- **Recover (direct restore):** `nextSeq = max(HubHardAck.key where hub == own) + 1` —
  derived from the `HubHardAck` CF, **no stored counter** (the same no-marker-CF principle
  as `RequestSequencer`'s `max(own Request) + 1`). The per-coil-peer marks are read from
  the **`CoilStampMark`** singleton blob (a cheap point read, not a `HubHardAck` scan).
  That is the sequencer's whole direct boot — it does **not** scan for the gap.
- **Recover (replay-fed gap):** the unstamped gap is **replayed by the `ReplayActor`**
  (step 4 of `replay`, hub only): using each coil's `CoilStampMark` as the floor, it scans
  the hub's `HardAck(PeerId.Coil)` receive tail above it — durably received (the liaison
  persisted it before advancing its receive cursor, CR8) but left unstamped by a crash
  between the cursor advance and the stamp — and re-feeds those acks here through the
  normal `HardAck` path. The coil will never re-serve them, so the spine rebuilds from the
  store, not from a re-delivery; this runs in the coordinated pre-barrier replay, like
  every other durable input journal.
- **Inputs:** coil peers' `HardAck`s — live via the hub's `PeerLiaisonHubToCoil`s
  (each **exactly once**: next-expected cursor + restored receive cursor), and at
  boot the `ReplayActor`-replayed gap. Both land on the same `HardAck` path.
- **Persists:** each newly-sequenced `HardAckWithId` → **`HubHardAck`**, in the
  **same atomic batch** as the bumped per-coil-peer **`CoilStampMark`**, **before** it
  fans out to the mesh + `CoilRelay` (CR4 write-before-send — a re-stamp would
  equivocate on the `HubHardAck` spine: two `HubHardAckNumber`s for one underlying
  coil ack; the co-written mark keeps "what's stamped" consistent with the spine
  across a crash). The raw inbound coil acks land in the hub's `HardAck(PeerId.Coil)`
  receive CF via the liaison (above), so the receive log is already durable when the
  sequencer runs.

> No idempotency index keyed by `(coil, hardAckNum)`: the restored receive cursor
> makes re-delivery impossible (a stale re-serve is rejected by the inbound lane's
> `verify`), so a scalar high-water per coil peer is all that is needed — both to bound
> the boot-time gap stamp and as the consistency anchor for the spine.

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
  `{brief, ack, BlockResult, DepositMap, RequestHighWater, L2CommandNumber}` lands together across
  six CFs).
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
      others. A prefix extractor (e.g. the `[peer:1]` byte on satellite CFs, [§7.1](#71-key-layout--journal-ids))
      further lets a scan skip SSTables that hold no entries for a given peer.
    - **Tag-byte savings** — with the CF as the journal discriminant, the
      encoded key drops the tag byte entirely ([§7.1](#71-key-layout--journal-ids)).
    - **Per-CF metrics + backup granularity** — observability and (later) backups
      are CF-level.

Our store opens a **config-derived set of CFs** — no marker CF (every marker
derives from a single-CF scan, [§5.1](#51-the-markers--all-derived-none-stored)). The set is **fixed-shape CFs** (the two
spines, the spine-indexed working/confirmation CFs, the reverse indices, the snapshots, `Meta`) **plus
one CF per satellite author**, the count derived from head config at store-open:
`2 + 3N + H` for a head peer (`+ C` receive `HardAck(PeerId.Coil)` CFs on a hub; `+`
its own `HardAck(PeerId.Coil)` CF on a coil peer), where **N** head peers, **C** coil
peers, **H** hubs
([§3.2](#32-satellites)). Membership changes by closing and re-opening a fresh head, so the set is
constant for a store's lifetime — RocksDB opens exactly the CFs that exist.

```
   ┌── one RocksDB instance (per-peer directory) ──────────────────────────┐
   │                                                                       │
   │   ┌─────────── shared WAL ───────────┐                                │
   │   └──────────────────────────────────┘                                │
   │                                                                       │
   │   each CF has its own MemTable + own SSTables                         │
   │                                                                       │
   │   Spines (one global each):    Per-author satellites (one CF/author): │
   │     ┌──────────┐ ┌──────────┐    Request:0…Request:N-1                │
   │     │BlockSpine│ │StackSpine│    SoftAck:0…SoftAck:N-1 (+ own, coil)  │
   │     └──────────┘ └──────────┘    HardAck:0…HardAck:N-1                │
   │                                  HardAck:<coil>  (coil own / hub recv)│
   │                                  HubHardAck:<hub> ×H                  │
   │                                                                       │
   │   Spine-indexed working data + aggregator outputs:                    │
   │     ┌────────────┐ ┌─────────────────┐ ┌─────────────────┐            │
   │     │BlockResult │ │SoftConfirmation │ │HardConfirmation │            │
   │     └────────────┘ └─────────────────┘ └─────────────────┘            │
   │     ┌────────────────┐ ┌───────────────┐ ┌─────────────┐              │
   │     │RequestHighWater│ │L2CommandNumber│ │UnsignedStack│              │
   │     └────────────────┘ └───────────────┘ └─────────────┘              │
   │                                                                       │
   │   Per-side snapshots:                                                 │
   │     ┌───────────┐ ┌────────┐ ┌─────────────┐                          │
   │     │DepositMap │ │Treasury│ │EvacuationMap│                          │
   │     └───────────┘ └────────┘ └─────────────┘                          │
   │                                                                       │
   │   Metadata:                                                           │
   │     ┌──────┐                                                          │
   │     │ Meta │                                                          │
   │     └──────┘                                                          │
   │                                                                       │
   │   (arrival stamps ride inline as a 12-byte prefix on each             │
   │    journal value — not a separate CF                                  │
   └───────────────────────────────────────────────────────────────────────┘
```

RocksDB's own crash recovery is mechanical: on open it replays the WAL from where
the most recent SSTable flush stopped, reconstructing the MemTable. *Our*
recovery ([§5](#5-recovery-architecture)–[§6](#6-per-actor-recovery-contracts)) builds on top of that — once RocksDB hands us a consistent view
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
  the **derived `*Acked` marks** ([§5.1](#51-the-markers--all-derived-none-stored) — from CF scans), never by a
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

- **Journals** = key `JournalKey` (a `JournalId` + the within-author index; [§7.1](#71-key-layout--journal-ids)) ⇒ replay
  is a **range scan from a cursor**, RocksDB's sweet spot; the access pattern is
  range-scan-by-journal, not ad-hoc SQL.
- **Durability barriers** = one atomic **`WriteBatch`** spanning every CF the
  step touches + WAL sync ⇒ clean CR4/CR8/CR6.
- **Confirmation-driven ack-pruning** = at confirmation, the aggregator writes
  its `SoftConfirmation` / `HardConfirmation` record and deletes the
  now-redundant per-ack keys in the same `WriteBatch`. Physical deletion is
  **bounded by the last hard-confirmed stack's last block** — never delete the
  confirmation records, the `BlockResult` records below that floor, or anything
  else that would breach the R10 evacuation floor ([§5.1](#51-the-markers--all-derived-none-stored) note). The storage
  engine's own **native (SST) compaction** runs underneath and is a separate,
  internal matter.
- **Column families.** The satellite *journals* (`Request`, `SoftAck`, `HardAck`
  — the last `PeerId`-keyed across head and coil authors — and `HubHardAck`) are
  **split one CF per author** ([§3.2](#32-satellites), [§7.1](#71-key-layout--journal-ids)); the two
  spines (`BlockSpine`, `StackSpine`) are one CF each; six spine-indexed
  working / confirmation CFs (`BlockResult`, `SoftConfirmation`, `HardConfirmation`,
  `RequestHighWater`, `L2CommandNumber`, `UnsignedStack` — the last the
  `Stack.Unsigned` `StackComposer` persists before each handoff, keyed by `stackNum`);
  three snapshot CFs (`DepositMap` + `Treasury` single keyed blobs; `EvacuationMap`
  keyed per committed block); one metadata (`Meta`, store version + the
  arrival-stamp generation counter, [§5.4](#54-total-order-of-the-replayed-streams)). The generic benefits of CF-per-concern
  (per-CF tuning, compaction isolation, scoped Bloom filters) are enumerated in the
  primer above; the per-author split adds **append-only-per-CF** write behavior
  ([§7.1](#71-key-layout--journal-ids)). **Atomicity across CFs is preserved**: `WriteBatch` covers puts in
  multiple CFs as one WAL record, so the per-soft-ack bundle `{own SoftAck,
  BlockResult, DepositMap, RequestHighWater, L2CommandNumber}` (+ `BlockSpine` when
  leading) lands as one transaction across **six CFs**.
- **Snapshots** = `DepositMap` / `Treasury` are single keyed blobs in their own
  CF, overwritten in place; `EvacuationMap` is keyed by `blockNum`, one entry per
  committed block ([§3.3](#33-what-gets-stored-and-how-recovery-treats-it)).

**Per-CF profile — what CF-per-concern buys us.** The CFs have very
different workload shapes, and the win of splitting them is that each one's
tuning knobs (block size, Bloom-filter bits, MemTable size, compaction style) can
match its shape — and the per-author satellite split makes each satellite CF a
clean append-only stream. Concretely:

| CF | Workload shape | Concrete payoff of having its own CF |
|---|---|---|
| `BlockSpine`, `StackSpine` | sparse spine — one small entry per block / stack, low write rate, heavily *read* (everyone consults briefs, replay scans them in order) | high bits/key Bloom is almost free (few keys); index + Bloom pinned in cache; range-scan-from-cursor on the spine is the native access pattern |
| `Request:<peer>` (one per head peer) | **high** write rate — up to 50–75k requests/s across the head — variable-sized payloads, scanned by `requestNum` for block assembly | **one author per CF ⇒ a single monotonic append stream**: non-overlapping L0 SSTables, RocksDB trivial-moves them down, near-zero compaction even at peak rate (the decisive reason for the per-author split, [§7.1](#71-key-layout--journal-ids)); larger MemTable absorbs bursts |
| `SoftAck:<author>`, `HardAck:<peer>` (one per author; the `HardAck` author is a `PeerId`, head or coil) | per block / stack-round, small fixed-size entries (Ed25519 sigs), pruned by confirmation-driven ack-pruning | append-only per author (no interleave); strong Bloom (small keys → cheap bits/key); the `*Acked` markers derive from the last key of the **own** author's CF |
| `HardAck:<coil>` (the coil author of the unified `HardAck` journal — one per coil peer author) | a coil peer's own slow-side acks (+ a hub's receive copy per hubbed coil), one entry per stack | append-only per coil peer author; the coil peer's own is its `hardAcked` source; the hub's copies are its receive log, stamped after a crash ([§6](#6-per-actor-recovery-contracts) `CoilAckSequencer`) |
| `HubHardAck:<hub>` (one per hub) | the re-sequenced disseminated coil-ack journal, one entry per sequenced coil ack | append-only; `SeekToLast` gives `CoilAckSequencer`'s `nextSeq`; read by every `SlowConsensusActor` for the coil quorum |
| `BlockResult` | one entry per block (JL's per-block output), keyed by `blockNum`, scanned at recovery to rebuild `StackComposer.pending`, low live read pressure | scan-optimized profile; values larger than acks but smaller than snapshots — independent compaction shape |
| `UnsignedStack` | one entry per stack close (`Stack.Unsigned` = brief + effects), keyed by `stackNum`, read at recovery to re-form SCA's in-flight cell | scan-optimized + sparse; medium-sized values; independent of the ack CFs |
| `SoftConfirmation` | one entry per soft-confirmed block (FCA aggregate output), keyed by `blockNum`, low write rate (one per confirmation event); `softConfirmed = max(key)` | a single `SeekToLast` derives the marker for free; small + sparse — cheap to keep |
| `HardConfirmation` | one entry per hard-confirmed stack (multisigned effects / SECs / fallbacks), keyed by `stackNum`, the R10 evacuation floor — physical deletion never descends here; folded sequentially at recovery (`CardanoLiaison` + rule-based regime, [§6](#6-per-actor-recovery-contracts), [§5.7](#57-the-recovery-priority-ladder-graceful-degradation)) | scan-optimized; `hardConfirmed = max(key)` from `SeekToLast`; **never compacted past — physical-retention floor** |
| `DepositMap` | one key (`Meta`-like — single keyed blob), medium-sized blob, **rewritten on every own soft ack** (high churn on a single key) | RocksDB-friendly single-key churn (large MemTable absorbs the overwrites; compaction collapses the chain quickly); isolated from ack-CF compaction |
| `RequestHighWater` | one entry per block keyed by `blockNum`, a small `Map[HeadPeerNumber, RequestNumber]` (`N` entries), written on every own soft ack (cumulative — each entry extends the previous) | scan-optimized + sparse like `BlockResult`; recovery reads the `fastBlockMark` entry to seed the `N` Request journal cursors ([§5.3](#53-the-indices-algorithm-deriving-the-2--3n--h-journal-cursors)); its own CF keeps the churn off the journal / ack compaction queues |
| `L2CommandNumber` | one entry per block keyed by `blockNum`, a single `Long` (the L2 ledger's commit counter reached after that block's L2 commits), written on every own soft ack | tiny scan-optimized + sparse CF; `JointLedger`'s own `recover` reads the `fastBlockMark` entry and calls `l2Ledger.restoreTo` to co-anchor the committed L2 state ([§6](#6-per-actor-recovery-contracts)); isolated from the journal / ack compaction queues |
| `Treasury` | one key, rewritten only at own hard-ack stack-close (slow cadence); small (UTXO ref) | low write rate ⇒ tiny compaction footprint; own CF makes it trivially backupable / restorable as part of the R10 floor |
| `EvacuationMap` | keyed by `blockNum`; one entry per **committed** block (each major + each last-of-partition SEC minor — the only maps that back an on-chain commitment), written in batches at own hard-ack stack-close (the closing stack's committed blocks in one `WriteBatch`); each map is cumulative L2 state | scan-optimized profile (range-read for evacuation reconstruction); pruning is bounded — anything strictly older than the last-hard-confirmed major can be dropped, since those minors can never be disputed against once the next major supersedes them |
| `Meta` | store-level keys: the schema version, the **arrival-stamp `generation`** — a per-process counter read+incremented+persisted once at store-open ([§5.4](#54-total-order-of-the-replayed-streams)), so this process's journal stamps sort after earlier ones across restarts — and the **per-generation zero-time anchor** (`generation → epoch-nanos at that generation's monotonic zero`, sampled once at open) that converts any `(generation, monotonic)` stamp to wall-clock time on read | no tuning needed; one obvious place for store-level metadata |

A few benefits worth calling out separately:

- **Compaction isolation pays here for real.** Request CFs can burst (a user
  request storm) and ack CFs churn (every confirmation prunes a batch). With
  one big keyspace those two would share a compaction queue and starve each
  other; per-CF queues let them be independent — and the per-author split makes
  each Request author its own append-only CF ([§7.1](#71-key-layout--journal-ids)).
- **The atomic `WriteBatch` across CFs is what lets the per-soft-ack bundle
  `{BlockSpine, own SoftAck, BlockResult, DepositMap, RequestHighWater, L2CommandNumber}`
  land as one transaction**, even though its pieces live in six CFs. Without
  CF-cross-atomicity we'd need a manual two-phase scheme; with it the durability
  barrier (CR4/CR6/CR8) is one batch. The SC per-hard-ack-close bundle
  `{UnsignedStack, StackSpine, own HardAck(own), Treasury, EvacuationMap}` is
  the slow-side mirror.
- **The R10 floor is a clean subset.** P2 (the first-priority milestone) writes
  `HardConfirmation` + `Treasury` + `EvacuationMap` and reads them on rule-based
  handover — no entanglement with the fast-side CFs. If we ever want to back up
  *just* the evacuation floor, it's three CFs, not a key-range carve-out.
- **Self-documenting layout.** "Where do block briefs live?" → the `BlockSpine` CF.
  "Where do `BlockResult`s live?" → `Cf.BlockResult`. The CF inventory is the
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
- The "schema" is now **key-layout design**; **journal values reuse the existing wire
  codecs** (`consensus/transport/Codecs.scala`) under a **12-byte `ArrivalStamp`
  prefix** ([§5.4](#54-total-order-of-the-replayed-streams), [§7.1](#71-key-layout--journal-ids)) — strip the prefix and you have the byte-identical wire
  form (one codec to test, byte-identical forward path). Non-journal CF values
  (`BlockResult` / `SoftConfirmation` / `HardConfirmation` / `DepositMap` /
  `Treasury` / `EvacuationMap` / `Meta`) need their own codecs (no wire form,
  no stamp prefix).
- **Interface:** a `Persistence[F[_]]` capability (tagless / cats-effect), injected
  like `CardanoBackend` — `HeadMultisigRegimeManager` already reserves a
  `Dependencies.Persistence` enum case and termination handler, so the seam exists.
- **Layout:** one store per head instance, keyed by head ID, path from `NodeConfig`.
- **Versioning** mechanism from day one; recovery refuses to load an incompatible version. The
  number is **held at 1** (`StoreVersion.current`, in `Cf.Meta`) and **not** bumped while the
  format still churns in development — a format change just rebuilds the store; we start tracking
  backward-incompatible bumps once the layout stabilizes. (The current layout unifies the head and
  coil own-hard-ack CFs into one `PeerId`-keyed `HardAck` journal — one CF per peer, a coil
  author's named `HardAck:<peerWireInt>` — but that rode in without a version bump.)

### 7.1 Key layout — journal IDs

Two types, kept distinct: a **`JournalId`** names a journal (the CF to scan); a
**`JournalKey`** is a full addressable entry (`JournalId` + the within-author index).
`JournalId` is the cursor/scan unit — [§5.3](#53-the-indices-algorithm-deriving-the-2--3n--h-journal-cursors) derives exactly one resume cursor per journal.

```scala
/** Identifies one append-only journal = the CF to scan.
  * Spines are head-global (no author); satellites are per author. */
enum JournalId:
    case BlockSpine                       // the one block spine
    case StackSpine                       // the one stack spine
    case Request(peer: HeadPeerNumber)    // → CF "Request:<peer>"
    case SoftAck(peer: HeadPeerNumber)    // → CF "SoftAck:<peer>" (head-only)
    case HardAck(peer: PeerId)            // → CF "HardAck:<peerWireInt>" (head or coil author)
    case HubHardAck(hub: HeadPeerNumber)  // → CF "HubHardAck:<hub>"

/** A full entry key = JournalId + within-author index. */
enum JournalKey:
    case Block      (num: BlockNumber)
    case Stack      (num: StackNumber)
    case Request    (peer: HeadPeerNumber, num: RequestNumber)
    case SoftAck    (peer: HeadPeerNumber, num: SoftAckNumber)
    case HardAck    (peer: PeerId, num: HardAckNumber)
    case HubHardAck (hub: HeadPeerNumber, num: HubHardAckNumber)

    def journalId: JournalId = this match
        case Block(_)          => JournalId.BlockSpine
        case Stack(_)          => JournalId.StackSpine
        case Request(p, _)     => JournalId.Request(p)
        case SoftAck(p, _)     => JournalId.SoftAck(p)
        case HardAck(p, _)     => JournalId.HardAck(p)
        case HubHardAck(h, _)  => JournalId.HubHardAck(h)
```

**Each journal is its own column family — one CF per spine, one CF per satellite
author** ([§3.2](#32-satellites)). The `JournalId` maps to a CF handle (the CF *name* encodes
kind + author; the handle map is built from head config at store-open), so the
**author is the CF, not part of the on-disk key**. The encoded byte key is just the
**within-author index**, big-endian fixed-width, so lexicographic byte order matches
numeric index order (what makes a range scan correct):

| CF | on-disk key bytes |
|---|---|
| `BlockSpine` | `[blockNum : 4]` |
| `StackSpine` | `[stackNum : 4]` |
| `Request:<peer>` | `[requestNum : 8]` |
| `SoftAck:<peer>` | `[softAckNum : 4]` |
| `HardAck:<peerWireInt>` (one per peer author — head, or a coil's own + a hub's receive copy, both `HardAck:<coilWireInt>`) | `[hardAckNum : 4]` |
| `HubHardAck:<hub>` | `[hubHardAckNum : 4]` |

Widths follow the value ranges: `Block` / `Stack` / `SoftAck` / `HardAck` /
`HubHardAckNumber` are non-negative `Int` (4 bytes); `RequestNumber < 2⁴⁰`, carried
as an 8-byte `Long`. **No author prefix in the key** — splitting per author into its
own CF makes each CF a single monotonic append stream (non-overlapping L0 → trivial
compaction), the decisive property at the request rate ([§7](#7-storage-design--rocksdb)); it also drops the
`[peer:1]` prefix the old multiplexed layout carried.

**Range scan / cursor.** Each scan is the whole of one author's CF in index order.
To replay a journal from its cursor, pick the CF (`JournalId` → handle) and position an
iterator at `[cursorBE]`. Each replay cursor ([§5.3](#53-the-indices-algorithm-deriving-the-2--3n--h-journal-cursors)) is a `JournalKey` — its `journalId`
picks the CF, the index gives the seek key.

**Values.** Journal values are framed as `[arrivalStamp : 12][wirePayload …]` — the
durable `ArrivalStamp` (`[generation : 4][monotonicNanos : 8]` big-endian; [§5.4](#54-total-order-of-the-replayed-streams)) is
a fixed prefix on the wire-codec payload. Stripping the prefix gives the bytes that
go on the wire — there is no separate `ArrivalStamps` CF. Non-journal CFs (`BlockResult`,
`SoftConfirmation`, `HardConfirmation`, `RequestHighWater`, `L2CommandNumber`,
`UnsignedStack`, `DepositMap`, `Treasury`, `EvacuationMap`, `Meta`) carry no stamp
prefix.

### 7.2 fsync policy — a per-CF gradient

Durability is **not** one global policy: `WriteOptions::sync` is chosen per `Write`
call, by which CF (and what protocol promise) the write backs.

- **Always `sync=true` (R10-critical):** `HardAck`, `HardConfirmation`, `Treasury`,
  `EvacuationMap`. Losing one to a power loss would let us forget a hard-ack or
  hard-confirmation, or roll back the slow-side snapshot anchor — violating the
  protocol's external-effect safety premise. Group-commit happens naturally (the
  slow-consensus pipeline batches these), so the fsync cost amortizes across the batch.
- **`sync=true`, group-committed (soft-side protocol promises):** `SoftAck`,
  `BlockResult`, `SoftConfirmation`, `DepositMap`. Soft acks are protocol promises too,
  but losing one to a power loss is **recoverable via re-replication** (no
  external-effect violation), so we accept the fsync cost but rely on group commit
  (concurrent soft-acks coalesce into one fsync) to amortize.
- **`sync=false` (page-cache durable):** `Request` and the request-ID assignment.
  Without fsync the WAL write still lands in the **OS page cache**, which survives a
  process crash (the kernel owns the bytes), so a Hydrozoa crash with the host still
  powered recovers fine — RocksDB replays the WAL on restart. The forfeit is power-loss
  / kernel-panic durability for exactly those records. ⚠ For request-ID this trades
  against **CR1** (a power loss after telling the user an id but before the page flushes
  could re-assign) — accept only if power-loss is outside the durability promise;
  **decide explicitly**.

(RocksJava is embedded / in-process, not a separate process — the `sync=false` path's
process-crash durability comes from the OS page cache being kernel-owned and outliving
the JVM, not from a separate writer staying up.)

---

## 8. Boot sequence

Executed in/around the regime manager's `preStartLocal`, before
`pendingConnections.complete` — `HeadMultisigRegimeManager` on a head peer,
`CoilMultisigRegimeManager` on a coil peer (both run the single `PeerId`-parameterized
replay seam; [§6](#6-per-actor-recovery-contracts), [§10](#10-open-questions)). **All actors start together**; the two recovery mechanisms
(replay / restore) run concurrently ([§5](#5-recovery-architecture)).

1. Open the store; verify version; **bump the arrival-stamp `generation`** — read
   the `Cf.Meta` counter, increment, persist (once per store-open), so every journal
   stamp this process writes sorts after every earlier process's ([§5.4](#54-total-order-of-the-replayed-streams)). Absent
   store → cold start (the degenerate replay: empty base, empty mailboxes), and the
   generation simply starts at 1.
2. **Derive the markers** by CF scan — the five `Markers` fields (derivation formulas in
   [§5.1](#51-the-markers--all-derived-none-stored)):
   - `softConfirmed = max(SoftConfirmation.key)`;
   - `hardConfirmed = max(HardConfirmation.key)`;
   - `hardAcked` from the last own `HardAck(own)` — `HardAck(PeerId.Head)` on a head
     peer, `HardAck(PeerId.Coil)` on a coil peer (`Markers.recoverHardAcked(backend, own)`);
   - `fastBlockMark = max(BlockResult.key)` — the fast-side anchor;
   - `nextRequestNumber = max(own Request.key) + 1` — the RequestSequencer counter
     (`RequestNumber(0)` cold or on a coil peer; [§6.1.1](#611-requestsequencer)).
3. **Load base snapshots** for the consensus actors — `DepositMap` +
   `RequestHighWater[fastBlockMark]` (JL; the latter feeds the Request journal cursors,
   [§5.3](#53-the-indices-algorithm-deriving-the-2--3n--h-journal-cursors)), `Treasury` + `EvacuationMap` (SC; the latter at
   `EvacuationMap[StackSpine[hardAcked].lastBlockNum]`, always present for a
   non-final stack — [§5.2](#52-state-recovery-the-base-snapshots)). Validate invariants (markers consistent with the
   snapshot blobs being aligned to their anchors; counter monotonicity). On
   violation → **fail safe**.
4. **`ReplayActor` seeds the consensus actors' mailboxes** with the total-ordered
   journal-entry tail, **using per-actor resume cursors** ([§5.1](#51-the-markers--all-derived-none-stored)):
   signers / ledger from the acked mark `+ 1` (fast: `fastBlockMark + 1`; slow:
   `hardAcked + 1` — no re-signing, CR2); aggregators from `*Confirmed + 1` (fast:
   `softConfirmed + 1`; slow: `hardConfirmed + 1`), so the acked-but-unconfirmed
   band is re-aggregated as peers' late acks arrive. `StackComposer` also reads
   `BlockResult` for `(StackSpine[hardAcked].lastBlockNum, head]` to rebuild
   `pending` ([§6](#6-per-actor-recovery-contracts) `StackComposer`). The `ReplayActor` also reads L1 **directly from
   `CardanoBackend`** (`utxosAt(treasuryAddress)`) and seeds the first
   `PollResults` into `BlockWeaver`, so deposit decisions proceed immediately rather
   than waiting on `CardanoLiaison`'s poll cadence ([§5.5](#55-the-l1-boundary-is-re-sampled-live-not-replayed)). A **suspend barrier** may
   be needed to hold the consensus actors until seeding completes ([§10](#10-open-questions) Q1).
5. **Open the start barrier:** every actor — consensus *and* boundary — runs.
   Consensus actors rebuild state + cascades from the seeded tail; their external
   re-emissions (acks, briefs, effects) reach the already-running boundaries.
6. **Boundaries restore + filter, concurrently.** `RequestSequencer`'s counter from
   the store, `PeerLiaison*`'s per-remote cursors, `CardanoLiaison`'s target — each
   boundary restores its own state, loaded **before** the boundary processes anything.
   A replay re-emission `≤ cursor` is dropped by the same cursor / equivocation filter
   that runs in steady state; a `> cursor` output is genuine forward progress and goes
   out normally ([§5](#5-recovery-architecture)).
7. **Reconcile L1:** `CardanoLiaison` polls `CardanoBackend` on its own cadence
   (the `ReplayActor` already supplied the first sample in step 4); the in-flight
   tail re-verifies against live L1 ([§5.5](#55-the-l1-boundary-is-re-sampled-live-not-replayed)) and may diverge; detect whether the
   head moved to the rule-based regime while we were down (→ hand off to the
   rule-based path, which reads this store once then runs off L1 ([§5.7](#57-the-recovery-priority-ladder-graceful-degradation)) —
   not a multisig resume).
8. Resume `GetMsgBatch` over the connected `PeerLiaison*`s from restored cursors;
   catch up to current head height (ordinary liveness, not backfill).
9. Resume normal participation.

If step 8 can't complete within the timing budget (CR7), abort and signal the
operator / trigger evacuation rather than rejoin stale.

**Coil peers and hubs run the same sequence.** A coil peer boots through
`CoilMultisigRegimeManager.preStartLocal` (the head path is
`HeadMultisigRegimeManager.preStartLocal`); the steps are identical with three coil
adaptations: step 2's fast anchor `fastBlockMark = max(BlockResult)` is the same on head and coil, and
`hardAcked` derives from the same `PeerId`-keyed `HardAck(own)` journal on both peer
types — `HardAck(PeerId.Coil)` on a coil peer, `HardAck(PeerId.Head)` on a head peer ([§5.1](#51-the-markers--all-derived-none-stored));
step 4's `ReplayActor` reconstructs the in-flight `StackHandoff` from `UnsignedStack` +
the own `HardAck(PeerId.Coil)` tail and admits the inbound `HubHardAck` journals so
`SlowConsensusActor` re-reaches the coil quorum ([§6](#6-per-actor-recovery-contracts)); the coil peer runs one
`PeerLiaisonCoilToHub` instead of the mesh. A **hub** additionally restores its
`CoilAckSequencer` (`nextSeq` + per-coil-peer stamped marks) and has the `ReplayActor`
re-feed it any unstamped `HardAck(PeerId.Coil)` receive tail ([§6](#6-per-actor-recovery-contracts)), and restores its
`PeerLiaisonHubToCoil` outboxes + coil-ack receive cursors. Both regime managers
(`HeadMultisigRegimeManager` and `CoilMultisigRegimeManager`) drive the single
`ReplayActor.replay(own: PeerId, …)` seam, the `CoilAckSequencer` recover, and all
three liaisons' outbox recover ([§6](#6-per-actor-recovery-contracts)).

---

## 9. Failure scenarios to specify & test

- Crash after assigning a request ID but before the user was told (CR1).
- Crash after signing a soft/hard ack but before it crossed the peer boundary
  (CR2/CR4) — re-derivation must reproduce the identical signature, not a conflict.
- Crash mid-stack: round-1 acks persisted, round-2 not yet released.
- Crash after L1 submission, before observing the tx — re-submission is a
  no-op (the L1 layer rejects the duplicate `txId`); `CardanoLiaison` observes
  the original landing on the next poll.
- Crash during a snapshot write (CR6: atomic; fall back to previous snapshot +
  longer replay).
- Crash **during recovery itself** (a second crash mid-replay): the next boot
  must converge. Recovery is re-entrant — it only advances markers/snapshots
  monotonically and atomically (CR6/CR8/CR3) and replay is deterministic
  re-derivation, so a partially-completed recovery is just a valid earlier
  state to recover from again ([§5](#5-recovery-architecture)). Test: inject crashes at successive points
  *within* the recovery window and assert the same final committed state.
- L1 moved while down: in-flight tail re-verifies against current truth and
  legitimately diverges/falls back ([§5.5](#55-the-l1-boundary-is-re-sampled-live-not-replayed)) — assert this is *not* treated as
  corruption.
- Long downtime → head went to rule-based regime while down → recovery must detect
  and not re-enter multisig (CR7, [§8](#8-boot-sequence) step 7 — post-barrier L1 reconciliation).
- Torn/corrupt record, or counter regression on load → refuse start.
- Head-wide restart: all peers crash and recover — does the protocol re-converge
  purely from persisted state + replay?

**Coil / hub scenarios.**

- **Coil peer crash mid-stack.** It had signed a hard-ack into its `HardAck(PeerId.Coil)`
  journal (persisted before the uplink send, CR4) but the stack had not hard-confirmed.
  On reboot it reconstructs the in-flight `StackHandoff` from `UnsignedStack` + its own
  `HardAck(PeerId.Coil)` tail and re-presents the ack to `SlowConsensusActor`;
  re-derivation must reproduce the identical signature, and the `HardAck(PeerId.Coil)`
  journal (keyed by `HardAckNumber`, not `StackNumber`) must not confuse
  `hardAcked = max(own HardAck(PeerId.Coil))`.
- **Coil peer fast-side anchor.** After reboot its `fastBlockMark = max(BlockResult)`
  anchor + the `BlockResult` scan must rebuild `StackComposer.pending` exactly as a
  head follower's would — a coil peer authors no soft-ack, so the test asserts the
  fast side recovers off `BlockResult` alone.
- **Hub crash with in-flight coil acks.** A hub had received coil acks (in its
  `HardAck(PeerId.Coil)` receive CFs) and sequenced some onto `HubHardAck`. On reboot
  `CoilAckSequencer` recovers `nextSeq = max(HubHardAck) + 1` + the per-coil-peer marks,
  and the **`ReplayActor` re-feeds the durably-received-but-unstamped `HardAck(PeerId.Coil)`
  receive tail** above each mark (the acks a crash between cursor-advance and stamping left
  behind) — assert every received ack ends up stamped exactly once and no
  `HubHardAckNumber` is reused
  for a different underlying coil ack.
- **Coil quorum completes from late acks post-recovery.** A recovered peer's
  in-flight slow-side cell completes as replayed/live `HubHardAck` entries arrive —
  assert hard-confirmation still reaches `AllOf(head) ∧ coilQuorum`.

**Testing strategy.** Extend the model-based integration suites (stage1 / stage4)
with a **crash-restart action** that kills and reconstructs a peer from its store
mid-run, asserting the consensus invariants still hold — for **head, coil, and hub**
peers. Single-actor restart tests the per-actor contract; **whole-node reboot**
tests cross-actor re-convergence; **crash-window fault injection** tests the CR4/CR8
barriers. Run the one-by-one replay ([§5.6](#56-the-replay-mechanism) mechanism 2) as the deterministic oracle
the concurrent run is checked against. Property: *for any crash point, recovered
committed state is observationally equivalent to the no-crash run.*

---

## 10. Open questions

_Resolved questions have been folded into the sections they belong to ([§3](#3-consensus-data-the-journals), [§5](#5-recovery-architecture), [§6](#6-per-actor-recovery-contracts),
[§7](#7-storage-design--rocksdb)); only genuinely-open items remain here._

1. **Additional barrier?** Does the `ReplayActor` need an **extra suspend barrier** to
   hold the consensus actors while it seeds their mailboxes, or does the existing
   `Connections` barrier (`pendingConnections`) already suffice? Each actor blocks on
   that barrier inside its `PreStart` handler, so replay sends queue *behind* `PreStart`
   and drain in order once it opens — which appears sufficient (then we add nothing).
   Confirm empirically on the real substrate before adding a second barrier.
2. **Coil marker / cursor derivation seam — resolved (P13), confirm:** the coil path
   took a **`PeerId`-parameterized unification** — one `ReplayActor.replay(own: PeerId, …)`
   and one `ReplayCursors` (with an `ownCoilHardAck: Option[JournalKey.HardAck]`
   field), the fast side anchoring on the `fastBlockMark = max(BlockResult)` on
   both peer types. The slow-side own-ack journal is now the one `PeerId`-keyed `HardAck`
   journal, so `own: PeerId` flows straight into `JournalKey.HardAck(own, n)` in `Markers`,
   `StackComposer.recover`, `LaneOutgoingBacking.hardAck`, and `ReplayActor.route` (one
   `case k: JournalKey.HardAck`) — no peer-type `own match` in the recovery machinery
   ([§5](#5-recovery-architecture), [§6](#6-per-actor-recovery-contracts) `JointLedger` / `StackComposer`). Remaining: confirm under load that
   `HardAck(PeerId.Coil)` `max + 1` + the in-flight-stack unpack behave as the head
   `HardAck(PeerId.Head)` scan does.

---

**Priority.** Sequence by **custody value, not by data flow.** After the storage
skeleton (P1), the first deliverable is **persisting what the rule-based regime
reads** — the hard-confirmed effects / SECs / fallbacks (the **R10 evacuation
floor**) plus the read path that loads them once on handover ([§5.7](#57-the-recovery-priority-ladder-graceful-degradation) top rung).
With that durable, a crashed peer can always fall back, vote, and evacuate even if
nothing else is restored. Everything after it is liveness / performance and may be
built **in any order**.

Status legend: ✅ done · 🚧 done, uncommitted · 📋 scoped, not started · ⬜ pending.
Steps below renumbered (the old letter scheme `Pa–Pg` is now `P3–P9`); `P1`/`P2` are
unchanged. `P10–P14` are the coil workstream ([§2](#2-two-kinds-of-actor), [§6](#6-per-actor-recovery-contracts) — the head plan threaded onto coil
peers).

| Step | Deliverable | Status |
|---|---|---|
| P1 | **Foundation (prerequisite for all below).** `Persistence[F]` + RocksDB backend skeleton; `JournalId`/`JournalKey` layout ([§7.1](#71-key-layout--journal-ids)); versioning; wired through `HeadMultisigRegimeManager.Dependencies.Persistence`. | ✅ |
| P2 | **First priority — the rule-based regime's read-set = the R10 floor.** `SlowConsensusActor` writes `HardConfirmation` records **in full** (multisigned effects / SECs / fallbacks) as it produces them (CR4); `StackComposer`'s `Treasury` + `EvacuationMap` snapshots ride alongside ([§6](#6-per-actor-recovery-contracts) `StackComposer`); the rule-based regime's **read path** loads `HardConfirmation` + `Treasury` + `EvacuationMap` once on handover and runs off live L1 ([§5.7](#57-the-recovery-priority-ladder-graceful-degradation)). Custody-safe in isolation — needs no replay, no fast-side state. | ✅ write side · **read path → Peter** |
| P3 | Boundary persistence: `RequestSequencer` write-before-tell-user (CR1/CR4); `PeerLiaison*` inbound write-before-advance + cursors (CR8), with the 12-byte `ArrivalStamp` prefix on each journal value ([§5.4](#54-total-order-of-the-replayed-streams)). | ✅ |
| P4 | Equivocation guard at the peer boundary (CR2) + counter recovery (CR3); unit tests. | ✅ |
| P5 | Fast-side per-block persistence (`JointLedger`): per-soft-ack `WriteBatch` over `Block` + `SoftAck` + `BlockResult` + `DepositMap` + `RequestHighWater` ([§6](#6-per-actor-recovery-contracts) `JointLedger`); plus `FastConsensusActor`'s confirmation write to `SoftConfirmation` + soft-ack pruning. The `BlockResult` CF is what lets `StackComposer` rebuild `pending` from disk on restart ([§5.2](#52-state-recovery-the-base-snapshots)); `RequestHighWater` feeds the Request journal recovery cursors ([§5.3](#53-the-indices-algorithm-deriving-the-2--3n--h-journal-cursors)). | ✅ — soft-ack pruning **deferred** (keep-all) |
| P6 | `ReplayActor` + total-order merge ([§5.4](#54-total-order-of-the-replayed-streams)) + indices algorithm ([§5.3](#53-the-indices-algorithm-deriving-the-2--3n--h-journal-cursors)); pre-populate-mailboxes mechanism + suspend barrier ([§5.6](#56-the-replay-mechanism)); seeds the first `PollResults` straight from `CardanoBackend` so deposit decisions don't wait on `CardanoLiaison`'s poll cadence ([§5.5](#55-the-l1-boundary-is-re-sampled-live-not-replayed)). | ✅ — suspend barrier reuses the `Connections` barrier (Q1) |
| P7 | L1 reconciliation + live re-sample in `CardanoLiaison` ([§5.5](#55-the-l1-boundary-is-re-sampled-live-not-replayed), [§8](#8-boot-sequence) step 7 — post-barrier). | ✅ |
| P8 | Boot sequence end-to-end ([§8](#8-boot-sequence)); fail-safe paths (CR6/CR7). | ✅ — CR7 catch-up-budget abort **deferred** |
| P9 | Crash-restart integration action + one-by-one oracle + observational-equivalence property ([§9](#9-failure-scenarios-to-specify--test)). | 📋 scoped, not started — deferred |
| — | Slow-side schema (over `StackComposer` types) — unblocked: the slow-consensus types and Bootstrap have landed ([§6](#6-per-actor-recovery-contracts) `StackComposer`). | ✅ |
| P10 | **Coil recovery:** `CoilAckSequencer` recovers `nextSeq = max(HubHardAck)+1` + per-coil-peer stamped marks (`CoilStampMark`); the `ReplayActor` re-feeds the unstamped `HardAck(PeerId.Coil)` receive gap to it ([§6.2.5](#625-coilacksequencer)); `PeerLiaison*` lazy outbox recovery + hub→coil receive-cursor restore ([§6.1.2](#612-peerliaison-three-shapes)). | ✅ |
| P11 | **Coil fast-side anchor:** un-gate `JointLedger`'s per-block snapshot bundle on coil; coil JL recovers off the `fastBlockMark = max(BlockResult)` ([§6](#6-per-actor-recovery-contracts) `JointLedger`). | ✅ |
| P12 | **Coil slow-side anchor:** `StackComposer.recover(own: PeerId)` off the unified `HardAck(own)` journal + the `UnsignedStack` brief (`lastBlockNum` from `UnsignedStack` for both peer types); `Markers.recoverHardAcked(backend, own)` / `recoverHardConfirmed` ([§6](#6-per-actor-recovery-contracts) `StackComposer`). | ✅ |
| P13 | **Coil boot replay (Q2):** unified `ReplayActor.replay(own: PeerId)` / `ReplayCursors` with `ownCoilHardAck: Option[JournalKey.HardAck]` (one `route` case `JournalKey.HardAck`→SCA over the `PeerId`-keyed journal, `HubHardAck`→SCA via `.ack`, fast anchor the `fastBlockMark`); `CoilMultisigRegimeManager` replay seam. | ✅ |
| P14 | **Head-peer `HubHardAck` replay** (follow-up to P13): `HubHardAck` added to the head `ReplayCursors` + `replay` (`hubs = config.hubHeadPeerNumbers`) so a head peer in a coil head re-feeds the coil quorum on recovery. | ✅ |

---

## References

- Whitepaper: `introduction/roadmap` (M5); `single-head-gummiworm-protocol/peer-network`
  (replicated log, outbox/cursor recovery note); `…/replicated-state-machine`
  (deterministic replay); `…/consensus/{fast,slow}-consensus`, `…/finality`;
  `future-work/user-experience/anti-censorship` (lying-peer proof).
- Code: `multisig/HeadMultisigRegimeManager.scala` (actor topology, `Persistence`
  dependency seam); `multisig/consensus/{FastConsensusActor,SlowConsensusActor,StackComposer,
  BlockWeaver,RequestSequencer,CardanoLiaison}.scala`,
  `multisig/consensus/liaison/PeerLiaison{HeadToHead,HubToCoil,CoilToHub}.scala`,
  `multisig/consensus/{CoilAckSequencer,CoilRelay}.scala`;
  `multisig/ledger/joint/JointLedger.scala`; `multisig/consensus/transport/Codecs.scala`;
  `rulebased/persistence/Persistence.scala` (stub).
- Bootstrap (stack-0 init + fallback): `StackComposer.bootstrapInitialStack` — the
  cold-start seed; recovery loads the snapshot instead ([§6](#6-per-actor-recovery-contracts) `StackComposer`).
