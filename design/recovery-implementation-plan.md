# Recovery (read-path) — implementation plan

Companion to `persistence-and-crash-recovery.md` (the design). That doc says
**what** recovery is and **why**; this one says **how** and **in what order** we
build the read/boot side. Section refs (§5.1 etc.) point at the design doc.

**Status:** Draft for review · **Branch:** `feature/recovery` · 2026-05-30
**Scope:** head-peer multisig recovery read-path only (coil, TDX, rule-based
payloads out of scope per design §1).

---

## 1. Where we are (audit, 2026-05-30)

**Write side — done.** Everything a peer must persist is wired:

| Producer | Persists | CFs |
|---|---|---|
| JointLedger | own brief, own soft-ack, per-block result, deposits snapshot | `Block`, `SoftAck`, `BlockResult`, `DepositMap` |
| StackComposer | own stack brief, own hard-acks, treasury, per-committed-block evac map | `Stack`, `HardAck`, `Treasury`, `EvacuationMap` |
| FastConsensusActor | soft-confirmation record (CR4, before fan-out) | `SoftConfirmation` |
| SlowConsensusActor | hard-confirmation record in full (R10 floor) | `HardConfirmation` |
| EventSequencer | assigned request (CR1) | `Request` |
| PeerLiaison | inbound remote lane entries, arrival-stamped (CR8) | all lane CFs |

**Foundation — done.** `BackendStore` (InMemory + RocksDb) with `cursor(cf,
fromInclusive)` range-scan, `lastKey`, `lastKeyWithPrefix`; typed `Persistence`;
12 `Cf`s; `LaneId`/`LaneKey`/`LaneValue`; `StoreKey` with every typed key
(incl. `HardConfirmation = StackEffects.HardConfirmed`, `EvacuationMap`,
`Treasury`, `DepositMap`, `BlockResult`, `SoftConfirmation`); all codecs;
`ArrivalStamp`; `StoreVersion`; `StoreDump`. `Markers.derive` is built.

**Read/boot side — not started.** This is the entire scope of this plan:

- `Markers.derive` is **never called** — no boot derivation.
- No per-actor base-state load: JointLedger / StackComposer still cold-init /
  `bootstrapInitialStack` unconditionally; the snapshot CFs are written but never
  read back into actor state.
- No indices algorithm (§5.3), no total-order merge (§5.4), no `ReplayActor`.
- No boot sequence (§8) in `MultisigRegimeManager.preStartLocal` (it spawns all
  actors and completes one `PendingConnections` barrier — nothing recovery-aware).
- No boundary restore (PeerLiaison cursors / EventSequencer counter /
  CardanoLiaison `HardConfirmation` fold + live L1 re-sample).
- `rulebased/persistence/Persistence` is a bare `object Persistence {}` — the
  R10 read path (design §10 Q6, priority P2) does not exist.
- **EUTXO L2 ledger has no persistence at all** (design gap, not just unwired):
  `EutxoL2Ledger` keeps `State` in an in-memory `Ref`, always seeded from
  `config.initialEvacuationMap`. The design assumed the `L2Ledger` black box owns
  its own store + a load-by-ack interface; neither exists. See R2b.
- PeerLiaison queues are not loaded from the store on boot (`state = State()`
  starts empty); see R2.
- No fail-safe validation (CR6/CR7), no crash-restart tests (§9).

**Drift to fix in passing:** `Cf.scala` docstring says the arrival stamp is
**8 bytes** and cites §5.5; design §5.4/§7.1 say **12 bytes**
`[generation:4][monotonicNanos:8]` and §5.4. `ArrivalStamp(generation: Int,
monotonic: Long)` is 12 bytes — the code comment is wrong. Correct it when R1
touches that area.

---

## 2. Decisions already settled (design §10) — encoded as constraints

- **Resume points (Q9 = a).** Signers/ledger replay from `*Acked + 1`
  (JointLedger `softAcked + 1`, StackComposer `hardAcked + 1`); aggregators
  (FCA/SCA) from `*Confirmed + 1`. Per-actor resume cursors, not one global one.
- **Start order (Q4).** All actors start **together**; the steady-state
  cursor/equivocation filter at each boundary suppresses replay re-emissions, so
  there is **no boundary-last phase**. This matches today's topology exactly.
- **Replay mechanism.** Pre-populate mailboxes (mechanism 1) for production;
  one-by-one + quiescence (mechanism 2) is the **test oracle** only.
- **L1.** Re-sampled live (§5.5); the `ReplayActor` seeds the **first**
  `PollResults` straight from `CardanoBackend.utxosAt(treasuryAddress)` so deposit
  decisions don't wait on CardanoLiaison's poll tick.
- **fsync gradient (Q5)** is a write-side concern; out of scope here except that
  request-ID's `sync=false`-vs-CR1 call must be decided before mainnet (note it).

---

## 3. Phasing

Bottom-up: pure logic first (testable without the actor system and resilient to
the flaky tooling), then per-actor seams, then orchestration, then end-to-end.

### R1 — Recovery read primitives (pure, property-tested). No actor/boot changes.

New package `multisig/persistence/recovery/`:

1. **`LaneScan`** — typed range-scan over a lane CF from a `LaneKey` cursor,
   yielding `(LaneKey, payload, ArrivalStamp)`. Thin layer over
   `BackendStore.cursor`; strips/exposes the 12-byte stamp prefix; reuses the wire
   codec for the payload. (Satellite scans bound by `[peer]` prefix; spine scans
   are whole-CF.)
2. **`ReplayCursors`** — the indices algorithm (§5.3). From `Markers` + N head
   peers derive the **2 + 3N** lane resume cursors:
   - BlockLane = `softAcked + 1`; StackLane = `hardAcked + 1`.
   - SoftAckLane[p] / HardAckLane[p] from the side's ack mark.
   - RequestLane[p] = (highest request from p included in any block `≤ softAcked`)
     `+ 1`. **Resolved (Ilia, 2026-05-30):** `BlockBrief` exposes the included
     `RequestId` high-water, and `RequestId` carries the peer id — so we derive
     each peer's cursor directly from the brief, no block-body scan needed.
3. **`ArrivalOrderedMerge`** — k-way merge (§5.4) of the lane tails by 12-byte
   big-endian `ArrivalStamp`, producing one ordered, lazy stream of
   `(LaneKey, payload)`. This is the order the `ReplayActor` (R3) and the oracle
   (R4) both consume.

Tests (ScalaCheck): cursor derivation vs a generated marker set; merge output is
exactly stamp order; round-trip — seed a store through the real write paths, merge
it back, assert the recovered interleaving equals the recorded one.

### R2 — Per-actor base-state recover seams (unit-tested).

For each consensus actor add a pure `recover(persistence, markers, cursors): IO[State]`
and change cold init to **"non-empty store → recovered; empty store → bootstrap"**
(cold start is the degenerate case, §5).

- **JointLedger** → `Done(previousBlockHeader, deposits)`:
  `deposits` from `DepositMap`; `previousBlockHeader` from `BlockLane[softAcked].brief`.
  L2 co-anchoring: load committed L2 state **as of `softAcked`** via the `L2Ledger`
  black box — see R2b below for the actual persistence work.

### R2b — EUTXO L2 ledger persistence (NEW — design gap, flagged by Ilia 2026-05-30).

The design (§6 JointLedger "L2 co-anchoring") treats the `L2Ledger` as a black box
that owns its own persistence and only fixes the *load-by-ack interface*. **That
persistence does not exist.** `EutxoL2Ledger` holds its entire state in an
in-memory `cats.effect.Ref[IO, State]` — `State(activeUtxos, pendingDeposits,
errors, confirmations, headId)` — and `EutxoL2Ledger.apply` always seeds it from
`config.initialEvacuationMap.toUtxos`. A crashed peer loses all L2 state.

This is its own work item because the `L2Ledger` is a separate component (the
black box behind the `L2Ledger[F]` trait), and SugarRush will have its own L2 with
its own store. Scope here = the **EUTXO reference ledger** only:

- **Persist** `EutxoL2Ledger.State` durably, anchored so it can restore **to a
  given block boundary** (`softAcked`), co-anchored with JointLedger's recovered
  `Done(softAcked)` — the §6 exact-co-anchoring requirement (they tear otherwise).
- **Interface:** add the **load-by-ack** entry point to `L2Ledger[F]` (restore L2
  state as of block `n`); `EutxoL2Ledger` implements it. The mechanism — full
  snapshot per block vs periodic snapshot + forward-replay of its own block ops —
  stays *inside* the EUTXO ledger (design §6 calls this the snapshot-interval-vs-
  replay tradeoff). Full L2 state ≫ the deposits map, so per-block snapshotting may
  be too costly; start simple (snapshot per committed block) and optimize later.
- **Store placement (decide in R2b):** its own RocksDB store/CFs vs a CF set in the
  shared store. Leaning separate — keeps the black-box boundary clean and mirrors
  how SugarRush's L2 will be wholly external. Flag for Ilia.
- **Out of scope:** SugarRush's L2 persistence (their component, their store) — we
  only fix the trait contract so they can implement against it.
- **StackComposer** → full `State`: `treasury` from `Treasury`; `evacuationMap`
  from `EvacuationMap[StackLane[hardAcked].lastBlockNum]` (load invariant §5.2);
  `pending` from a `BlockResult` scan over `(StackLane[hardAcked].lastBlockNum, head]`;
  counters derived (`lastClosedStackNum = hardAcked`, `lastClosedBlockNum`,
  `nextOwnHardAckNum = max(own HardAck) + 1`,
  `previousStackHardConfirmed = hardAcked ≤ hardConfirmed`). `inboundLeaderBrief`
  fills from replayed StackLane in R3. Keep `bootstrapInitialStack` for empty store.
- **Boundaries (restore, no replay):**
  - EventSequencer: `next = max(own Request) + 1` — must precede telling the user
    an id (CR1; design flags the current code completes `dResponse` before persist —
    fix the barrier here).
  - PeerLiaison: per-remote cursors = `max(persisted) + 1`. **Queue loading
    (Ilia 2026-05-30 — must do, not just "queues empty"):** the in/out queues in
    `PeerLiaison.State` are reconstructed from the store on boot, not left empty.
    The **outbox** is the DB-backed view of this peer's own-produced lane prefix
    `[remote's cursor, head]` (re-served on each `GetMsgBatch`, exactly the
    steady-state write-through-cache behavior — a cold cache *is* recovery). The
    **inbound** side restores from the persisted inbound lane entries (CR8) above
    each remote's cursor. Define how `State()` is seeded from `persistence` at
    `preStartLocal` — today it's `private val state = State()` (empty).
  - CardanoLiaison: fold the `HardConfirmation` CF to rebuild
    `effectInputs` / `happyPathEffects` / `fallbackEffects`; `targetState` from
    config until stack-0 hard-confirms; effects faulted in lazily. Submission
    progress comes from L1 itself (§5.5), no own marker.
- **Rule-based read path (P2 / R10):** implement `rulebased/persistence/Persistence`
  as a **read-only** view over the same store loading `HardConfirmation` +
  `Treasury` + `EvacuationMap` once on handover (design §10 Q6). Custody-safe in
  isolation; no replay, no fast-side state.

Tests: seed a store through the real write paths, run each `recover`, assert the
returned State equals the expected crash-time State. The `recover` functions are
pure `IO` over `Persistence` — no actor system needed.

### R3 — `ReplayActor` + boot sequence (§8) + fail-safe.

- **`ReplayActor`** (new consensus-side actor): on boot — load base snapshots,
  build `ReplayCursors`, stream `ArrivalOrderedMerge`, **seed each consensus
  actor's mailbox** with its lane tail at its own resume cursor; read L1 from
  `CardanoBackend` and send the first `PollResults` to BlockWeaver; hold the
  suspend window.
- **`MultisigRegimeManager.preStartLocal`** grows the §8 steps: derive markers →
  load snapshots → spawn actors (recovered or bootstrapped) → `ReplayActor` seeds
  mailboxes → open the start barrier (`pendingConnections.complete`) → boundaries
  restore + filter → reconcile L1 → resume `GetMsgBatch`.
- **Suspend barrier decision.** Today every actor blocks in `initializeConnections`
  on the single `PendingConnections` `Deferred` until the manager completes it.
  **Plan A (preferred):** seed mailboxes *before* `pendingConnections.complete`,
  reusing that `Deferred` as the suspend gate — sends queue in the mailbox while
  the actor is parked, then drain in order once the barrier opens. **Plan B:** add
  a second `Deferred` suspend barrier only if cats-actors does not accept
  pre-barrier mailbox sends cleanly. Verify empirically at the start of R3.
- **Fail-safe (CR6/CR7):** validate marker/snapshot consistency on load (anchors
  aligned, counters monotone) → refuse start on violation; abort + signal/evacuate
  if catch-up can't finish within the timing budget.

### R4 — Tests (design §9, priority Pg).

- **One-by-one oracle** (mechanism 2) under `TestControl` via the cede-settle
  trick (see `reference_testcontrol_cats_actors`): feed the ordered tail one entry
  at a time, settle, assert.
- **Crash-restart action** in the stage1 / stage4 model suites: kill a peer,
  reconstruct it from its store mid-run, assert consensus invariants hold.
- **Observational-equivalence property:** for any crash point, recovered committed
  state is observationally equivalent to the no-crash run; the concurrent replay
  (mechanism 1) must match the oracle (mechanism 2).
- **Adversarial scenarios:** crash *during* recovery (re-entrancy), L1 moved while
  down (legitimate divergence, not corruption), long downtime → head went
  rule-based (detect, don't re-enter multisig), torn record → refuse start.

---

## 4. Open questions to close during implementation

1. ~~**RequestLane cursor source (R1).**~~ **Resolved (Ilia 2026-05-30):** derive
   from the `BlockBrief`'s included-`RequestId` high-water (RequestId carries the
   peer id). No block-body scan.
2. **EUTXO L2 store placement (R2b).** Own RocksDB store/CFs vs CFs in the shared
   store (leaning separate — clean black-box boundary). Plus the load-by-ack
   signature on `L2Ledger[F]` and the snapshot-cadence choice inside `EutxoL2Ledger`.
3. **Suspend barrier (R3).** Reuse `PendingConnections` (Plan A) vs second
   `Deferred` (Plan B) — *Ilia: TBD; decide empirically by testing pre-barrier
   mailbox sends.*
4. **Land order (§5).** Straight R1→R4 vs pull the rule-based R10 read path to the
   front — *Ilia: TBD.*
5. **Request-ID fsync vs CR1 (write-side, pre-mainnet).** `sync=false` on `Request`
   trades against CR1 under power loss (§10 Q5) — decide explicitly; out of this
   plan's critical path but track it.

---

## 5. Suggested order of landing

R1 → R2 (incl. R2b) → R3 → R4, each its own PR. R1 and the R2 `recover` functions
are pure and independently testable, so they de-risk the architectural R3 step. The
rule-based read path (R2, P2) can land first within R2 if we want the R10 custody
floor demonstrably recoverable before the liveness machinery. R2b (EUTXO L2
persistence) is loosely coupled — it can proceed in parallel with the other R2
seams since it's a self-contained component with its own store.