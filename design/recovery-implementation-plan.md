# Recovery (read-path) — implementation plan

Companion to `persistence-and-crash-recovery.md` (the design). That doc says
**what** recovery is and **why**; this one says **how** and **in what order** we
build the read/boot side. Section refs (§5.1 etc.) point at the design doc.

**Status:** Draft for review · **Branch:** `feature/recovery` · 2026-05-31
**Scope:** head-peer multisig recovery read-path only (coil, TDX, rule-based
payloads out of scope per design §1).
**Landed:** R1 (recovery primitives) · R2b (EUTXO L2 persistence — `L2CommandNumber`,
`L2Store` InMemory + RocksDB, `restoreTo`/`currentCommandNumber`, codecs, tests) · R2-fast
recover seams (2026-06-02): `RequestHighWater` write; per-block `Cf.L2CommandNumber` (14th CF) +
its `persistOwnAckBundle` write; `JointLedger.State.recover`/`recoverState` (+ L2 co-anchor via
`restoreTo`); `StackComposer.State.recover`; `BlockResultScan` — all pure-over-store and
unit-tested, with the actor wiring deferred to R3. · R2-bnd (boundary restores, 2026-06-04):
`Markers.recoverNextRequestNumber` (EventSequencer counter), `CardanoLiaison.State.recover`
(`HardConfirmation` fold via `HardConfirmationScan`), `PeerLiaison.recover` → `OutboxSeed` — all
pure-over-store and unit-tested, wiring deferred to R3. · R3a (recover wiring + bootstrap skip,
2026-06-04): each snapshot/boundary actor's `PreStart` self-derives `Markers` and restores via its
R2 `recover`; StackComposer skips `bootstrapInitialStack` on a non-empty store; CardanoLiaison
gained a `persistence` param; `StackComposerRecoveryTest` boots the actor against a seeded store.
**Next:** R3b (`ReplayActor` + mailbox replay), then R3c (boot sequence + fail-safe).

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
- ~~No boundary restore~~ **Done (R2-bnd, 2026-06-04):** EventSequencer counter
  (`Markers.recoverNextRequestNumber`), CardanoLiaison `HardConfirmation` fold + live L1 re-sample
  (`CardanoLiaison.State.recover`), PeerLiaison own-produced outbox (`PeerLiaison.recover` →
  `OutboxSeed`). Pure-over-store; actor wiring is R3.
- `rulebased/persistence/Persistence` is a bare `object Persistence {}` — the
  R10 read path (design §10 Q6, priority P2) is **WIP by Peter, out of this plan**.
- ~~**EUTXO L2 ledger has no persistence at all**~~ **Done (R2b, 2026-05-31):**
  `EutxoL2Ledger` now owns a command-number-keyed `L2Store` (InMemory + RocksDB), advances an
  `L2CommandNumber` on each real mutator, snapshots every `SnapshotInterval`, and restores via
  `restoreTo(commandNumber)`. The JL-side co-anchoring (recording the per-soft-ack command number) is
  still R2-fast.
- ~~PeerLiaison queues are not loaded from the store on boot~~ **Restore landed
  (R2-bnd):** `PeerLiaison.recover` rebuilds the outbox as an `OutboxSeed`; `State` is still
  cold-init at construction — R3 seeds it from the recovered value.
- No fail-safe validation (CR6/CR7), no crash-restart tests (§9).

**Drift to fix in passing:** `Cf.scala` docstring says the arrival stamp is
**8 bytes** and cites §5.5; design §5.4/§7.1 say **12 bytes**
`[generation:4][monotonicNanos:8]` and §5.4. `ArrivalStamp(generation: Int,
monotonic: Long)` is 12 bytes — the code comment is wrong. Correct it when R1
touches that area.

**Deferred cleanup (StoreKey codec altitude, 2026-06-04):** `StoreKey.decodeValue` /
`encodeValue` + the `given codec` live on the **key instance**, but a codec is really a property of
the **(CF, value-type)**, not of a particular key. Point `get` / `put` always have a key so it reads
naturally; **value-only iteration** (`BlockResultScan` today; the FCA/SCA spine scans coming in R3)
has values but no single key, so it rebuilds a key from the cursor's key bytes purely to reach the
codec. Cleaner: lift the codec to the case **companion** (or a `Cf`-indexed codec) so value-only ops
decode without an instance, keeping the instance `decodeValue` as one-line sugar. Small, isolated —
do it when the R3 spine scans land (they want the same accessor).

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

**Sub-tracks.** R2 splits into loosely-coupled sub-tracks:

  1. **R2-fast — JointLedger + StackComposer recover + the `RequestHighWater` write.**
  2. **R2-bnd — boundary restores (EventSequencer / PeerLiaison / CardanoLiaison).**
  3. **R2b — EUTXO L2 persistence** (own section; parallelizable).

The **rule-based read path (R10 custody floor)** is **out of scope for this plan —
WIP by Peter**, tracked separately. It is fully self-contained (read-only, no replay,
no fast-side state) and shares nothing with the R2 recover seams beyond reading the
same CFs, so it proceeds independently. The only piece that stays here is the
panic→spawn *handover* wiring (R3/orchestration), which consumes that read model but
does not build it.

**Common shape.** Every recover path is a pure `IO` over `Persistence` /
`BackendStore` (+ `Markers` from R1's derivation) — no actor system, unit-tested by
seeding a store through the *real write paths* (the producers already exist) and
asserting the recovered value equals the crash-time value. The actor wiring is the
same one seam in each: today every actor builds its `state` cold via
`Ref.unsafe(State.initial(config))` at construction and resolves peers in
`initializeConnections` (inside `preStartLocal` / the `PreStart` receive). R2 adds,
right after connections resolve: *if the store is non-empty, `state.set(recovered)`;
else keep the cold/bootstrap value.* Markers being all-`None` **is** the empty-store
signal — no separate "is cold" flag.

#### R2-fast — JointLedger + StackComposer recover (+ RequestHighWater write) — LANDED 2026-06-02

The recover functions are **pure over the store** (`recover` / `recoverState` on each actor's
`State` companion), unit-tested by seeding a store and asserting the reconstructed value equals the
crash-time value; the **actor wiring** (calling them in `preStart`, skipping `bootstrapInitialStack`,
deriving markers) is deferred to R3. The spine briefs a recover reads are present on **every** peer —
own (leader) via the producer, inbound via `PeerLiaison.persistInbound` — so there is no follower
gap.

- **JointLedger** → restore `Done(previousBlockHeader, deposits)`. Today
  `State.initialize(config) = Done(config.initialBlock.blockBrief.header,
  DepositsMap.empty)` (cold), held in `Ref.unsafe`. Recover (when `softAcked` is
  `Some`): `deposits` from `Cf.DepositMap`; `previousBlockHeader` from
  `BlockLane[softAcked].brief` (the persisted own/leader brief). A mid-`Producing`
  block is **not** restored — it was never acked, so `Done(softAcked)` is the whole
  passive state; BlockWeaver re-drives the `[softAcked + 1, head]` tail in R3.
  - **L2 co-anchoring (keyed by an L2 command number, *not* by ack — Ilia
    2026-05-31).** Also restore the committed L2 state to match JL's `softAcked`. The
    L2 ledger is a **black box that knows nothing of acks / blocks / stacks /
    confirmations**, so it is addressed by its own **monotonic command number** (commit
    counter), and recovers from `(initial state, target command number)`. JL holds the
    translation: per own soft-ack `persistOwnAckBundle` reads `l2Ledger.currentCommandNumber`
    (after the block's L2 commit) and records it under the per-block `Cf.L2CommandNumber`
    (14th CF); on recover it reads `L2CommandNumber[softAcked]` and calls
    `l2Ledger.restoreTo(it)` — the consensus → L2 mapping stays entirely on JL; the
    membrane is never crossed by an ack. (R2b built `restoreTo` / `currentCommandNumber`;
    R2-fast added the per-soft-ack recording + the recover call.)
  - **Write-side addition (`RequestHighWater`) — LANDED 2026-06-01:**
    `persistOwnAckBundle` writes `{Block(leader), SoftAck, BlockResult, DepositMap}` in
    one `WriteBatch`; a fifth put `StoreKey.RequestHighWater(blockNum)` was added —
    the previous block's high-water with this block's included request ids folded in
    via `ReplayCursors.mergeHighWater`, keyed **per block** (not a singleton, so the
    high-water at any committed block is recoverable; the latest entry subsumes the
    old singleton design). Created the new `Cf.RequestHighWater` (13th CF, spine-
    indexed group) + `StoreKey.RequestHighWater(num: BlockNumber)` + its `Map` codec
    (`RequestHighWaterCodec`). Not read by JL's own `recover`; the ReplayActor reads
    `RequestHighWater[softAcked]` to seed the RequestLane
    cursors (§5.3 / R1 `ReplayCursors`).
- **StackComposer** → restore full `State`. Today `State.initial(config)` seeds
  `treasury = config.initializationTx.treasuryProduced`,
  `evacuationMap = config.initialEvacuationMap`, empty maps, and `PreStart` runs
  `bootstrapInitialStack` (compose stack 0 → hand to SlowConsensusActor). Recover
  (when `hardAcked` is `Some`): the marker `hardAcked` is a `HardAckNumber` (the
  satellite key), **not** a stack number, so the closed-stack number `hardAckedStack`
  is read from the **last own HardAck value** (`HardAck.stackNum` at
  `HardAck[own, hardAcked]`); the marker only gives `nextOwnHardAckNum = hardAcked + 1`.
  Then: `treasury` from `Cf.Treasury`; `lastClosedBlockNum` from
  `StackLane[hardAckedStack].lastBlockNum`; `evacuationMap` from
  `EvacuationMap[lastClosedBlockNum]` (load invariant §5.2); `pending` rebuilt from
  `BlockResultScan.scanFrom(lastClosedBlockNum)` — the BlockResults soft-acked since the
  last close, `(lastClosedBlockNum, softAcked]` — folded via `recordBlockResult` (the
  `Block.SoftConfirmed` halves arrive from FCA replay in R3, completing each
  `PendingBlock` then `tryPair → ready`); the remaining counters are
  `lastClosedStackNum = hardAckedStack` and
  `previousStackHardConfirmed = hardAckedStack ≤ hardConfirmed`. `ready` /
  `inboundLeaderBrief` start empty and fill from replay in R3. **Skip
  `bootstrapInitialStack` on a non-empty store** (R3 wiring) — stack 0 long since
  hard-confirmed; keep it only for the empty-store cold path.

#### R2-bnd — boundary restores (restore only, no replay) — LANDED 2026-06-04

- **EventSequencer:** `next = max(own Request) + 1` (its `nLedgerEvent`
  `Ref[RequestNumber]`, cold-init 0). **CR1 is already satisfied** — current code
  persists the assigned request to the `Request` lane *before*
  `dResponse.complete(id)` (verified: `persistence.write(...)` precedes
  `req.dResponse.complete`). So R2-bnd only adds the boot-time counter restore; the
  earlier "fix the barrier" note is stale and dropped.
- **PeerLiaison:** restore the **own-produced outbox only**, via `PeerLiaison.recover` →
  `OutboxSeed`. `State` holds five own-produced cursors
  (`nAck`/`nBlock`/`nRequest`/`nStackBrief`/`nHardAck` — the highest number this peer has produced
  per lane), the five outbox queues (`qAck`/`qBlock`/`qRequest`/`qStackBrief`/`qHardAck`), and
  `currentlyRequesting` (our outstanding `GetMsgBatch` to the remote). `buildNewMsgBatch` serves the
  remote from the **in-memory** queues — so a cold (empty) outbox cannot answer the remote's
  catch-up. recover therefore re-loads each queue from the store, not just the cursors. It returns a
  pure `OutboxSeed` (the five lanes' own entries, ascending): satellites
  (`SoftAck`/`HardAck`/`Request`) are own-keyed, so a per-peer `LaneScan` yields exactly our entries;
  spines (`Block`/`Stack`) carry every leader's brief, so recover keeps only the ones this peer
  **leads** (`isLeader`/`isSlowLeader`, since the spine key has no peer byte). R3 seeds `State` from
  the value (each cursor `nX` = its queue's last number; `currentlyRequesting` stays
  `GetMsgBatch.initial(remote)` so the remote re-polls). **No inbound restore:** `persistInbound`
  writes each received entry (CR8) and immediately **forwards** it — PeerLiaison holds no inbound
  queue, so inbound re-delivery is the `ReplayActor`'s job in R3 (it seeds each consensus actor's
  mailbox from the persisted lane tails). Restoring inbound here would double-deliver. (Supersedes the
  2026-05-30 "reconstruct in/out queues" note — there are no inbound queues to restore.)
- **CardanoLiaison:** `State(targetState, effectInputs, happyPathEffects,
  fallbackEffects)` (cold: `targetState = Active(config…treasuryProduced.utxoId)`,
  empty maps). Recover by **folding `Cf.HardConfirmation` in stack order** through
  the same handlers the live path uses (`handleInitialStackL1Effects` for
  `Initial`, `handleStackL1Effects` for `Regular`) so `effectInputs` /
  `happyPathEffects` / `fallbackEffects` / `targetState` rebuild identically to a
  live run. **Submission progress is *not* persisted** — it is re-sampled from L1
  (`runEffects` polls `utxosAt(treasuryAddress)`, §5.5), so recover only restores
  the effect *index*; what is already on-chain is observed, not remembered. (Reads
  `Cf.HardConfirmation` directly — the same CF the rule-based read path reads, but
  independently; no cross-dependency.)

**R2 testing.** Seed a store through the real write paths, run each `recover` /
read-model query, assert the returned value equals the expected crash-time value.
All pure `IO` over `Persistence` / `BackendStore` — no actor system needed.

### R2b — EUTXO L2 ledger persistence (do first; design gap flagged by Ilia 2026-05-30)

**Do this before R2-fast** (Ilia 2026-05-31): JointLedger's recover cannot be
finished without a real L2 restore seam to co-anchor against, so we land R2b first
rather than against a stub.

`EutxoL2Ledger` holds its entire state in an in-memory `cats.effect.Ref[IO, State]`
— `State(activeUtxos, pendingDeposits, errors, confirmations, headId)` — and
`EutxoL2Ledger.apply` always seeds it from `config.initialEvacuationMap.toUtxos`. A
crashed peer loses all L2 state.

This is its own work item because the `L2Ledger` is a separate component (the black
box behind the `L2Ledger[F]` trait), and SugarRush will have its own L2 with its own
store. Scope here = the **EUTXO reference ledger** only.

**The anchoring key is an L2-internal command number — not any consensus marker, and
not a content hash (Ilia 2026-05-31).** The L2 ledger knows nothing about acks,
blocks, stacks, or confirmations, so keying its snapshots by `softAcked` would leak
consensus vocabulary across the membrane. A *content* hash was considered (the
evac-map digest) and dropped: there is no cheap whole-map hash today — `EvacuationMap`
only offers `kzgCommitment` (a slow BLS12-381 pairing, unviable per-interaction) and
a *private per-entry* blake2b; worse, the EUTXO ledger doesn't even hold an
`EvacuationMap` (it holds `activeUtxos` + emits diffs), so a content key would force
both a `toEvacuationMap` projection and a new digest on every commit. Instead the L2
keeps its **own monotonic command number** — a commit counter it increments on each
state-mutating command. Cheap, deterministic, consensus-agnostic; recovery doesn't
need content-addressing, only a stable per-commit key, which the command number gives
for free.

**Recovery contract: the L2 reconstructs from `(initial state, target command number)`.**
The initial state it already has (`config.initialEvacuationMap`); the target command
number is the only recovery input it takes.

**Storage: own RocksDB store, two CFs, via a dedicated `L2Store` (Ilia 2026-05-31).**
The L2 owns its store (separate from the consensus store) — clean black-box boundary,
mirrors how SugarRush's L2 will be wholly external. **Not** the consensus
`BackendStore`: that trait is hardwired to the consensus `Cf` enum
(`get(cf: Cf, …)`, `open()` over `Cf.all`/`Cf.Meta`), so reusing it for a separate L2
CF set would force parameterizing `BackendStore[F, C]` + `RawWriteBatch[C]` over the
CF type — a refactor reaching committed R1 (`LaneScan`/`ReplayCursors`) +
`Persistence`/`Markers`/`StoreDump`. Rejected. Instead a **small purpose-built
`L2Store` trait** in the `eutxol2` package, with `InMemory` + `RocksDb` impls, exposing
exactly what the two command-number-keyed CFs need (append log entry, get snapshot ≤
command number, scan log range, put snapshot). `EutxoL2Ledger.apply(config, store: L2Store[IO])` takes
it as a param (caller provides; tests use the in-memory impl). Zero consensus-store /
R1 churn.

- **`L2Log` CF** — append-only command log, key = `commandNumber` (BE), value = the applied
  ledger-state command. The **source of truth.** For the EUTXO ledger the command
  *is* the diff (applying it via the deterministic `transit` reproduces the next
  `activeUtxos`), so an event-sourced log needs no separate diff type.
- **`L2Snapshot` CF** — restore accelerator, key = `commandNumber` (BE), value = full
  `State` at that command number. Written every **N** commits, where **`N` is a named
  constant — `SnapshotInterval = 100`** (Ilia 2026-05-31; tune later if needed, not
  config-driven for now). Genesis (`config.initialEvacuationMap`) is the implicit
  command-number-0 snapshot, so an empty CF is fine.

`restoreTo(S)`: `SeekForPrev` the greatest snapshot key `≤ S` (else genesis), load
it, then re-fold the `L2Log` entries `(snapCommandNumber, S]`. The log is authoritative; the
snapshot only bounds replay length.

**Which commands advance the command number + get logged: the ledger-state mutators only**
— `sendApplyTransaction`, `sendApplyDepositDecisions`, `sendRegisterDeposit` (they
mutate `activeUtxos` / `pendingDeposits`). The proxy commands
(`sendProxyBlockConfirmation`, `sendProxyRequestError`) write transient,
client-facing data (`confirmations` / `errors`) that recovery's co-anchoring does
not need — **excluded** from the command number + log (so `confirmations` / `errors` are *not*
restored; a client needing them post-restart is a separate concern).

**Factoring (so restore reuses production logic exactly).** Split each mutator into a
pure `applyMutation(state, cmd): Either[L2LedgerError, State]` (the existing `transit`
core, advancing `state.commandNumber`) and a public method = `applyMutation` + append to
`L2Log` + snapshot-every-N. Restore folds `applyMutation` over the logged commands —
**no re-logging, no re-snapshot, no command-number re-bump.** Relies on `transit` being
deterministic given `(state, command)` (true today; verify at the top of R2b).

- **Interface:** `restoreTo(commandNumber): EitherT[F, L2LedgerError, Unit]` on `L2Ledger[F]`
  (name TBD), implemented by `EutxoL2Ledger` from `(genesis, commandNumber)` via the
  snapshot + log above.
- **Command-number exposure: a `currentCommandNumber` query (Ilia 2026-05-31).** Add
  `currentCommandNumber: F[L2CommandNumber]` to `L2Ledger`; JointLedger reads it right after a
  commit. Safe despite "non-atomic in general" because JL is a single-message-at-a-
  time actor and the sole L2 driver in this regime, so nothing interleaves between
  commit and read. Chosen over return-on-commit because the existing mutators go
  through a Kleisli / `L2LedgerAction` indirection and return a *consumer-side*
  `L2LedgerState`; threading the command number out of that is far more invasive than a query.
  - *Alternative noted (Ilia): JL computes the command number itself* by counting the
    successful real-commands it issues (it sees each `Either` result, the L2 logs
    only successes, JL is the sole driver → the two counts stay in lockstep). That
    would drop the `currentCommandNumber` trait method entirely. **But the L2 still needs an
    internal command number to key its own log/snapshot**, so this only removes the *query*,
    not the counter. Deferred — `currentCommandNumber` for now; revisit if the trait surface
    matters.
- **Trait blast radius:** `L2Ledger[F]` has **two** implementors — `EutxoL2Ledger`
  (our target) and `RemoteL2Ledger` (the SugarRush-facing remote). New trait methods
  (`restoreTo`, `currentCommandNumber`) force both to compile; `RemoteL2Ledger` gets an
  **unsupported stub** (`L2LedgerError` / `raiseError`) — it's out of scope ("we only
  fix the trait contract so SugarRush implements against it"). Constructors to ripple:
  `MultisigRegimeManager`, `JointLedgerTest` (both build an `EutxoL2Ledger`).
- **Snapshot value: the recoverable subset only (Ilia 2026-05-31)** —
  `(commandNumber, activeUtxos, pendingDeposits)`. `errors` / `confirmations` are the
  transient proxy data excluded from recovery, so on restore they start empty. Keeps
  the "not restored" contract honest and avoids needing codecs for `Tx.Serialized`
  (the `confirmations` value type).
- **State + codecs:** add a `commandNumber` field to `EutxoL2Ledger.State`; needs a codec for
  the snapshot subset (`L2Snapshot`) and a codec for the logged command (`L2Log`).
  `L2CommandNumber` = a small `opaque type` over `Long` in the `eutxol2` package.
- **Co-anchoring wiring:** JointLedger holds the consensus → L2 mapping: per own
  soft-ack it records the L2 command number for that block (durable alongside its own
  snapshot), then on recover restores `Done(softAcked)` and calls
  `restoreTo(thatBlock'sCommandNumber)`. The command number means nothing to the L2 beyond "how many
  commits"; the ack never crosses the membrane.
- **Pruning — none in the first cut.** The target command number JL asks for is its
  `softAcked` boundary, which can lag the L2 head, so a target may fall *below* the
  newest snapshot — we must keep the log (and a snapshot ≤ any reachable target).
  First cut: **keep all logs + periodic snapshots** (literally "always restore"). A
  later optimization prunes below a **JL-supplied command-number floor** (a command number, not an
  ack — membrane stays clean); deferred.
- **Out of scope:** SugarRush's L2 persistence (their component, their store) — we
  only fix the `L2Ledger` trait contract so they can implement against it. (The
  JointLedger side — recording the per-soft-ack command number — is **R2-fast**, not R2b.
  R2b delivers only the L2's own command number + persistence + `restoreTo`.)

**Build order (R2b):**

1. `L2CommandNumber` opaque type (`Long`) + `commandNumber` field on `EutxoL2Ledger.State`
   (genesis = 0); bump it in the three real mutators. Pure, compiles, no persistence
   yet. Confirm `HydrozoaTransactionMutator.transit` is deterministic here.
2. `L2Ledger` trait: add `currentCommandNumber` + `restoreTo(commandNumber)`; stub both in
   `RemoteL2Ledger` (unsupported). `EutxoL2Ledger.currentCommandNumber` reads `state.commandNumber`.
3. The L2 store: own RocksDB store, two CFs (`L2Log`, `L2Snapshot`); BE-`Long` keys;
   `Codec[command]` + codec for the snapshot subset `(commandNumber, activeUtxos,
   pendingDeposits)`. `SnapshotInterval = 100`. **Codec correction (landed 2026-05-31):**
   the `L2LedgerCommand.Real` wire codecs the trait exports **cannot be reused** — they
   rename fields (`userVKey` → `userVk`) and encode `Destination` lossily (object out,
   CBOR-hex string in), so they do *not* round-trip (the FIXMEs in `L2LedgerCommand`
   confirm it). `L2StoreCodecs` instead re-derives the three subtypes symmetrically
   (`deriveCodec`) and routes the Borer-only leaves (`Destination`, `L2Genesis`) through
   a CBOR-hex string. Lives store-local; the wire codecs are untouched.
4. Factor each mutator into pure `applyMutation` + public wrapper that appends to
   `L2Log` and snapshots every `SnapshotInterval`. Implement `restoreTo`
   (SeekForPrev snapshot ≤ commandNumber, fold log tail via `applyMutation`).
5. Tests: command-number monotonicity; commit→`currentCommandNumber`; `restoreTo(S)` reproduces the
   live state for S on a snapshot boundary, mid-interval, below the newest snapshot,
   and at 0/genesis; codec round-trips.

### R3 — `ReplayActor` + boot sequence (§8) + fail-safe.

Split into three sub-phases (mirrors R2). **Settled by the R3 understand pass
(2026-06-04):** the suspend barrier is **Plan A** — the cats-actors mailbox is
`Deferred`-driven and FIFO, and each actor enqueues a `PreStart` message then blocks
on `pendingConnections.get` *inside* that handler, so anything sent before
`pendingConnections.complete` queues behind `PreStart` and drains in order; no second
barrier. `FastConsensusActor` / `SlowConsensusActor` / `BlockWeaver` hold **no base
state** (§5.1, §6) and are rebuilt purely from the ReplayActor's lane tail — only the
snapshot/boundary actors recover base state (all landed in R2). Bootstrap-skip test =
`Markers.hardAcked.isDefined`.

#### R3a — recover wiring + bootstrap skip — LANDED 2026-06-04

Each snapshot/boundary actor's `PreStart` self-derives `Markers` and restores via its
R2 `recover`, gated on a non-empty store; cold start is unchanged. StackComposer
`recoverOrBootstrap` skips `bootstrapInitialStack` when `recover` returns `Some`.
JointLedger recovers `Done(softAcked)` + co-anchors L2. CardanoLiaison gained a
`persistence` param (ctor/factory/MRM) and folds `HardConfirmation` on boot.
EventSequencer (`State.seedNextRequestNumber`) + PeerLiaison (`State.seedFromOutbox`)
got small seed seams. `StackComposerRecoveryTest` boots the actor against a seeded
store and asserts the observable: cold ⇒ a stack-0 `StackHandoff` reaches
`SlowConsensusActor`, non-empty ⇒ none.

#### R3b — slow-side effects persistence (LANDED 2026-06-04) + `ReplayActor`

**Write-side prerequisite — LANDED.** StackComposer persists the closed `Stack.Unsigned`
(brief + locally-derived effects) under a new `Cf.UnsignedStack` (keyed by `stackNum`)
**before** handing the stack to `SlowConsensusActor`, in both close paths
(`tryCloseAsLeader` / `tryCloseAsFollower`) — every close, leader or follower. This fills a
real gap: the design's "SCA rebuilds cells from briefs + acks" holds on the fast side (a
`SoftAck` signs the block *header*, carried by `BlockBrief`) but **not** the slow side — a
`HardAck` signs the *effects*, which a `StackBrief` (range only) does not carry — so the
effects must be durable for SCA's in-flight-cell recovery. It also fixes a latent CR4/CR8
ordering bug: the close paths sent the handoff (whose own acks SCA immediately broadcasts)
*before* persisting, so an own ack could cross the peer boundary unpersisted; the persist
now precedes the brief broadcast + the handoff. Codec = `UnsignedStackCodec` (mirrors
`StackEffectsCodec` / `PartitionEffectsCodec` over the BARE SEC, reusing the SEC-agnostic
`Final` partition codec). Pruning postponed (keep-all for now).

**`ReplayActor`** (new consensus-side actor): on boot — derive `Markers` + `ReplayCursors`,
`scanLanes`, `ArrivalOrderedMerge`, decode each `RawLaneEntry` and **route it into the
reading actor's mailbox**:
  - Request → BlockWeaver; SoftAck → FCA; HardAck → SCA.
  - Block spine → FCA (`softConfirmed+1`) + BlockWeaver (`softAcked+1`).
  - Stack spine (`StackBrief`) → StackComposer (`hardAckedStack+1`) as inbound leader briefs
    for the forward stacks it re-closes. (SCA does **not** consume `StackBrief`; its
    `Request` = `PreStart | StackHandoff | HardAck`.)
  - **In-flight acked-but-unconfirmed stack** (≤1, single-flight — the §9 "crash mid-stack"
    case): read its `UnsignedStack` + the own acks (HardAck lane) and send SCA a
    **reconstructed `StackHandoff`**, so SCA forms its cell via its existing path and
    re-aggregates the replayed + live remote acks. SC stays out of recovery for the acked
    band (Ilia: acked-but-unconfirmed stacks go to SCA, not SC). Stack 0 in-flight is the
    exception — re-bootstrapped (R3a), not reconstructed.
  - Read L1 (`CardanoBackend.utxosAt(treasuryAddress)`) → first `PollResults` → BlockWeaver.
  - Spawned by MRM **before** `pendingConnections.complete` (Plan A).
- No inbound restore from PeerLiaison — it forwards; the ReplayActor re-feeds.

#### R3c — boot sequence (§8) + fail-safe

- **`MultisigRegimeManager.preStartLocal`** grows the §8 steps: spawn actors →
  `ReplayActor` seeds mailboxes → open the start barrier → boundaries restore + filter
  → reconcile L1 → resume `GetMsgBatch`.
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

### Rule-based read path (R10 custody floor) — out of scope (WIP by Peter)

The read-only view the rule-based regime loads on handover (design §10 Q6, priority
P2) is a separate workstream, **WIP by Peter** — not phased here. It is fully
self-contained (read-only, no replay, no fast-side state, no actor changes), sharing
nothing with the R2 recover seams beyond reading the same already-written CFs
(`Cf.HardConfirmation`, `Cf.Treasury`, `Cf.EvacuationMap`), so it builds and lands on
its own timeline. The only related piece inside this plan is the panic→spawn
*handover* wiring — `MultisigRegimeManager` watching the consensus children and, on
panic, constructing `RuleBasedRegimeManager` from that read model — which is
boot/orchestration and belongs to R3.

---

## 4. Open questions to close during implementation

1. ~~**RequestLane cursor source (R1).**~~ **Resolved (Ilia 2026-05-31):** read a
   **persisted per-peer request high-water counter** at boot; the RequestLane cursor
   is that `+ 1`. *Supersedes* the 2026-05-30 "derive from `BlockBrief` high-water"
   answer — folding briefs can't determine the high-water for a peer not covered by
   the unpruned brief suffix (you'd need to scan back to block 0, and §5.1 prunes
   old briefs). Counter maintenance + write-side wiring is R2/R3; `maxRequestNumberPerPeer`
   is the fold. R3 open: where the counter lives (Meta key / snapshot CF).
2. ~~**EUTXO L2 store placement (R2b).**~~ **Resolved (Ilia 2026-05-31):** own
   RocksDB store, two CFs — `L2Log` (append-only command log, source of truth) +
   `L2Snapshot` (full-state every N commits, restore accelerator); `restoreTo(commandNumber)`
   = nearest snapshot `≤ commandNumber` + re-fold log tail. The command number advances on the
   ledger-state mutators only; first cut keeps all logs (no pruning). `N` =
   `SnapshotInterval = 100` (named constant). Remaining sub-decision for the top of
   R2b: confirm `transit` determinism for replay.
3. **Suspend barrier (R3).** Reuse `PendingConnections` (Plan A) vs second
   `Deferred` (Plan B) — *Ilia: TBD; decide empirically by testing pre-barrier
   mailbox sends.*
4. ~~**Land order (§5).**~~ **Resolved (Ilia 2026-05-31):** the rule-based read path
   is **not** pulled to the front — it is a separate workstream (WIP by Peter), out of
   this plan. Main line is R1 → R2 → R2b → R3 → R4.
5. **Request-ID fsync vs CR1 (write-side, pre-mainnet).** `sync=false` on `Request`
   trades against CR1 under power loss (§10 Q5) — decide explicitly; out of this
   plan's critical path but track it.

---

## 5. Suggested order of landing

R1 → R2 (R2-fast, R2-bnd) → R2b → R3 → R4, each its own PR. R1 and the R2 `recover`
functions are pure and independently testable, so they de-risk the architectural R3
step. R2b (EUTXO L2 persistence) is loosely coupled — it can proceed in parallel
with the other R2 sub-tracks since it's a self-contained component with its own
store.

**The rule-based read path** runs **off the main line** entirely — standalone,
read-only, depends on nothing but the already-written CFs, and is **WIP by Peter**
(tracked outside this plan). It can land at any point; the consensus children must
panic before anything *uses* it, and that handover wiring is R3.