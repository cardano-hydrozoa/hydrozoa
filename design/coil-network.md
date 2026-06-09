# Coil-Ready Peer (M5)

**Status:** Active M5 workstream — core decisions locked 2026-05-30; the
**relay/lane architecture reworked 2026-06-05** (see §8). Pc1–Pc3 (the identity +
slow-consensus + single-hub spine) are built on `feature/coil`. An initial Pc4
relay was also built — a hub multiplexed the population's acks + requests onto one
re-sequenced `relayedMsg` lane (`CoilLinkRelay`), tapping the core consensus
actors. **That design is being replaced** by the one this doc now specs:

- **Separate lanes, no multiplexing.** The coil link carries the same per-author
  lane structure a head peer keeps (blocks, stacks, and per-author request /
  soft-ack / head-hard-ack lanes, plus per-hub coil-hard-ack lanes), not one fat
  `relayedMsg` lane.
- **A dedicated `CoilRelay` fan-out actor** does the hub→coil distribution, so the
  core consensus actors are no longer relay taps (they only ever send their *own*
  production).
- **Composition, not inheritance, for the liaisons** — three independent actors
  over a shared per-lane protocol, with separate per-shape batch message types.

So the code currently reflects the multiplexed Pc4; this doc is the target.
Open items unchanged: coil persistence / crash-recovery (lands on
`feature/recovery`); the generative stage4 property (harness settling); the
app-level launch path (`Main` dispatch + a coil config-loader).

This spec describes the **coil-peer node type**: the process that joins a
Gummiworm head as a custodial slow-consensus follower. It is M5's "coil
consensus" deliverable.

The protocol-level design for coil consensus is already laid out in the
whitepaper (`peer-network`, `slow-consensus`, `initialization`); this spec
turns it into an implementable node-type spec.

Persistence + crash-recovery for coil peers is **out of scope here** — it
will be added as §11 of `persistence-and-crash-recovery.md` later, as a delta
on the head-peer recovery architecture (parallel M5 workstream).

The permissionless coil-peer **marketplace** — on-chain registry, head
onboarding bonds, rent, peer selection, dynamic membership — is **deferred to a
separate future-work spec**. It builds on this node-type but is not part of M5.

**Whitepaper sources:**
- `single-head-gummiworm-protocol/peer-network` — coil topology, replicated set, hard-ack dissemination, communication flow
- `single-head-gummiworm-protocol/consensus/slow-consensus` — coil quorum role; round-1 / round-2 signing
- `single-head-gummiworm-protocol/replicated-state-machine` — what state each peer maintains
- `blockchain-specifics/cardano/initialization` — bootstrap config (`coilPeers`, `coilQuorum`), multisig native script

---

## 1. Goal

Build the **coil-peer node type**: a process that joins a Gummiworm head as a
custodial peer, **signs hard acks** to provide the coil quorum that
hard-confirms stacks (slow-consensus follower role), **submits multisigned L1
effects** alongside head peers (R8/R9), and **drives the rule-based regime
independently** if consensus breaks down (R10). Whitepaper anchors —

> *"R8. Independent verification: Both head peers and coil peers should be
> able to independently verify effects and submit them to the L1 network…"*
> *"R9. Resilient effect submission: Any peer (head or coil) should be able
> to independently submit verified effects to the L1 network."*

(`peer-network`).

The protocol is fully designed in the whitepaper; the codebase stops at the
static config (`HeadConfig.coilPeers`, `HeadParameters.coilQuorum`,
`config.head.coil.CoilPeers`) and never spawns coil-side actors. M5 closes
that gap.

Out of scope here:
- **Persistence + crash-recovery for coil peers** — deferred to
  `persistence-and-crash-recovery.md` §11 (parallel M5 workstream).
- **Permissionless coil-peer marketplace mechanics** (registry, bonds, rent,
  dynamic membership) — separate future-work spec.

## 2. The one-sentence design

A coil peer is a head peer in **constant follower mode**: `BlockWeaver` never
enters Leader role, `StackComposer` never closes as leader, `JointLedger`
produces no `SoftAck`, no `RequestSequencer` user-request surface exists, and the
head-peer mesh collapses to a single uplink toward its hub (a
`PeerLiaisonCoilToHub`). It keeps the **same lane structure** as a head peer — so
it persists and crash-recovers with the same machinery — same replicated set,
same L1-submission and rule-based-regime paths.

The "constant follower" intuition holds with **two precise breaks**:

1. **Authors one lane only — own `HardAck`.** Whitepaper: *"Coil peers can
   produce only one type of log entries: Hard acknowledgements."*
   (`peer-network`).
2. **Own `HardAck` may be gappy.** Whitepaper: *"coil peers may skip
   acknowledging blocks entirely since not all are available at all times"*
   (`peer-network`). Head peers must sign every stack; a coil peer may decline.

`CardanoLiaison` runs **unchanged** on a coil peer — happy-path effects
(initialization, settlement, finalization, rollouts) and the fallback tx are
submitted the same way head peers submit them, per R8/R9 (§9). Evacuation is
not a `CardanoLiaison` concern on either peer type — it lives in the rule-based
regime (`/rulebased/EvacuationActor` + `DisputeActor`), which a coil peer drives
the same way a head peer does.

## 3. Actor topology

The head-peer **multisig-regime** actor set (`MultisigRegimeManager`) is the
reference. A coil peer's multisig regime is a strict subset (separate manager —
`CoilMultisigRegimeManager`); the **rule-based regime** is **shared** —
`RuleBasedRegimeManager` runs identically on head and coil peers (§10).

| Actor (head-peer reference) | Coil-peer treatment |
|---|---|
| `RequestSequencer` | **inert** — present to fill the shared `Connections` slot, but no HTTP surface routes user requests to it and a coil peer authors none |
| `BlockWeaver` | **follower-only** — drives `JointLedger` (`StartBlock` / `CompleteBlock`) from observed `BlockBrief.Next` + relayed requests + `PollResults`; never enters Leader role, no mempool drain |
| `JointLedger` | runs the same `Done → Producing → Done` cycle on every block whether leading or following. Two `handleBlock` steps are gated: the brief broadcast (leader-only, `canLeadFast` — false on a coil peer) and own soft-ack production (head-only — a coil peer authors none) |
| `FastConsensusActor` | aggregator-only — verifies + aggregates head-peer soft-acks into `SoftConfirmation`s; **signs nothing**. A coil peer simply has no own soft-ack to add |
| `StackComposer` | **follower-only** — pairs `BlockResult` × `Block.SoftConfirmed` × inbound `StackBrief` from the leader; never closes a stack as leader |
| `SlowConsensusActor` | same as head |
| liaisons | a coil peer runs **one** `PeerLiaisonCoilToHub` toward its hub (`coilPeers[me].hub`); a hub head peer runs one `PeerLiaisonHubToCoil` per coil peer it hubs, plus `CoilAckSequencer` and `CoilRelay` (§8) |
| `CoilRelay` | **hub-only** — fan-out actor that distributes the population stream to the hub's coil peers (§8). Absent on a coil peer and on non-hub head peers |
| `CoilAckSequencer` | **hub-only** — re-sequences this hub's coil peers' hard-acks onto its `HubHardAckLane` (§8) |
| `CardanoLiaison` | same as head |

Absent actors → no scaffolding spawned. Present actors → existing head-peer
implementations reused, with the shared `Connections` holding the unused slots
empty (the coil peer is a strict subset of a head peer's `Connections`).

**No rate limiters.** A coil peer spawns no `Limiter` actors — the
`blockWeaverLimiter` / `stackComposerLimiter` slots in `Connections` alias the
unthrottled `BlockWeaver` / `StackComposer` handles directly. A `Limiter` paces a
*leader*'s output against L1 timing; a coil peer never leads, so there is nothing
to pace.

## 4. What changes vs. head-peer code today

**Reused unchanged:**
- `BlockWeaver` follower path (`BlockBrief.Next` + `PollResults` →
  `StartBlock` / `CompleteBlock{Regular,Final}` driving JL).
- `StackComposer` follower path (`Block.SoftConfirmed` × `BlockResult` ×
  inbound `StackBrief` pairing; `StackEffectsBuilder`).
- `HardAckAggregator`, `HardAckSignatureVerifier`, `EffectSigner`.
- `CardanoLiaison` — same target-state / `happyPathEffects` / `fallbackEffects`
  maps and submission FSM (R8/R9), over an independent `CardanoBackend`.
- The head-mesh transport (`PeerWsTransport`, `Frame`, `Codecs`) — the link logic
  is unchanged, though server ownership was lifted out into `NodeWsServer` so a hub
  shares one server across the mesh and the coil link (§8.10).
- `L2Ledger` black box — a coil peer instantiates its own deterministic copy.

**New types / actors:**
- **`CoilPeerNumber`** opaque type — stable ordering for the multisig native
  script.
- **`CoilMultisigRegimeManager`** — coil-side counterpart to head's
  `MultisigRegimeManager`, spawning the present-actor subset with one uplink
  liaison. `RuleBasedRegimeManager` is **shared** with head (§10).
- **`CoilRelay`** + **`CoilAckSequencer`** — hub-side relay actors (§8).
- The three liaison shapes (§8): `PeerLiaisonHeadToHead`, `PeerLiaisonHubToCoil`,
  `PeerLiaisonCoilToHub`, over a shared per-lane protocol.
- Coil-side bootstrap entry point (`app/` layer), parallel to head's.

**Changed — slow-side identity (D-coil-1).** Coil-authored hard-acks travel the
same lanes as head-peer ones, so every slow-side identity slot changes from
`HeadPeerNumber` to the tagged `PeerId = Head(HeadPeerNumber) | Coil(CoilPeerNumber)`:

- `HardAckId` becomes `(PeerId, HardAckNumber)`. Wire form: peer number + a
  one-bit tag (`1` = head, `0` = coil), so the existing 2-int tuple grows by one
  bit rather than gaining a structural tag field.
- `SlowConsensusActor`'s quorum set and `HardAckAggregator`'s
  `vkeys: Map[PeerId, VerificationKey]` key on `PeerId`; the verifier rebuilds each
  `VKeyWitness` from the `PeerId → vkey` lookup, so a peer can only ever contribute
  a witness under its own key (head or coil).

This lands on **both** peer types — head peers aggregate coil-peer hard-acks to
reach hard-confirmation, so the widening is not coil-local.

**Changed — threshold native script (D-coil-1 cont.).** `HeadMultisigScript`
becomes a threshold composition:

```
AllOf(
  headPeerVKeys.map(Signature)                        // every head peer
    :+ MOf(coilQuorum, coilPeerVKeys.map(Signature))  // any coilQuorum coil peers
)
```

`MOf` is Scalus's k-of-n `Timelock` constructor (Blockfrost JSON
`atLeast`/`required`). The one script governs all four uses: minting the `HYDR` /
`HMRW` beacon tokens, spending the treasury and multisig-regime outputs, and the
head address. With **no coil peers configured** the `MOf` branch is omitted and the
script is **byte-identical** to today's head-only `AllOf(headSigs)` — same script
hash, address, and policy id — so coil-free heads are unaffected. `requiredSigners`
no longer flattens `AllOf → Signature`: the head keys stay **mandatory**; the
coil-peer contribution is a fixed **count** of `coilQuorum` slots (not all coil
peers, not identity-pinned) — the fee-sizing rule §6 leans on.

**Behavioral gates on reused actors** are read from the regime manager at
construction — **never** carried in a wire message:
- `BlockWeaver`: gate the Leader role; follower path unchanged.
- `JointLedger`: gate the leader-only brief broadcast (`canLeadFast`) and own
  soft-ack production (head-only). The `Producing` cycle itself is un-gated.
- `FastConsensusActor`: aggregator-only, no gate of its own.
- `StackComposer`: gate the leader-close path; full derivation otherwise.
- `SlowConsensusActor`: **no** role gate — aggregates head + coil hard-acks
  identically and signs its own. On a coil its own hard-ack is broadcast up the
  uplink rather than across the mesh, but that is a `Connections` wiring
  difference, not a behavioral gate. (Its skip-stack choice, §12, is a *runtime*
  decision, not a construction gate.)
- `CardanoLiaison`: **no** role gate — submits happy-path + fallback identically.

## 5. Configuration

**A coil peer reuses `HeadConfig` wholesale (D-coil-2 / D-coil-6)** —
`initialization`'s distribution phase delivers the *same* head config to head and
coil peers, and a coil peer needs nearly all of it (`coilPeers`, `coilQuorum`,
`headParams`, timing, `initTx`, `headId`). There is **no** slim `CoilConfig`, and a
separate `CoilMultisigRegimeManager` spawns the follower subset. The only thing
that differs between a head node and a coil node is **identity**, carried at the
`NodeConfig` private layer:

```
NodeConfig(headConfig: HeadConfig,        // shared verbatim, head & coil
           nodePrivateConfig)             // carries identity
```

A coil node swaps the `OwnHeadPeerPrivate` for an **`OwnCoilPeerPrivate`** — this
coil peer's signing key plus its derived `CoilPeerNumber`, located by matching its
own vkey against `coilPeerVKeys`. `HeadConfig` is untouched; `CoilMultisigRegimeManager`
reads the private side to know "I am coil peer N" and finds its hub via the
matching `coilPeers` entry (`coilPeerHub`).

**Coil peers are explicitly numbered** — `coilPeers` is a `CoilPeers` wrapper
(`SortedMap[CoilPeerNumber, CoilPeerData]`, `CoilPeerData = {verificationKey,
hubHeadPeerNumber}`) with a contiguity-from-0 (or empty) invariant, mirroring
`HeadPeers`. The number is authoritative — it orders the threshold native
script's coil branch and resolves hard-ack signer vkeys — and is **not** derived
from verification-key ordering. JSON is keyed by the number:
`"coilPeers": { "0": { "verificationKey": …, "hubHeadPeerNumber": 1 }, … }`.
`coilPeerVKeys` is the map's values in number order.

Everything else a coil peer reads from the shared `HeadConfig`: the head set,
`coilQuorum`, slow-consensus timing, finalization rules. L1 access is an
independent `CardanoBackend` connection (§9); the `L2Ledger` is constructed from
the same configuration head peers use (byte-deterministic across head and coil).

**Script accessor — coil context is type-level (done, Pc2).** `headMultisigScript`
/ `headMultisigAddress` live on the coil-aware `HeadConfig.Bootstrap.Section`
(which has `coilPeers` + `coilQuorum`); the head-only derivation on
`HeadPeers.Section` was deleted, so coil context is a *type* requirement — a bare
`HeadPeers` value can no longer silently drop the coil branch.

## 6. Slow-consensus participation

A coil peer's slow-consensus role is **identical to a head follower** except for
the identity of the signing key and the gappy allowance:

- **Pairing & derivation** — the coil peer's `StackComposer` follower path
  receives each leader's `StackBrief` via its hub, pairs it against `BlockResult`
  × `Block.SoftConfirmed` reconstructed locally, and derives `StackEffects`
  deterministically (byte-identical to every head peer's derivation).
- **Signing** — the coil peer's `EffectSigner` signs the same two-round (or sole)
  hard-ack: round-1 over everything except the unlock; round-2 over the unlock
  tx (`PartitionEffects.unlock` is the shared selector).
- **Aggregation** — the coil peer's `SlowConsensusActor` aggregates the whole
  population (head + coil) the same way head's does: all head-peer hard-acks +
  `coilQuorum` coil-peer hard-acks → write `HardConfirmation` → fan out.

The slow-side identity widening to `PeerId` and the threshold native script (both
prerequisites for coil hard-acks) are code-shape changes — see §4.

### Fixed-count coil-peer witnesses

Because every peer derives byte-identical tx bodies and the fee covers the
witness-set size, the witness *count* must be fixed: `nHeadPeers + coilQuorum`.

- **Saturate early.** A `SlowConsensusActor` cell is saturated the instant it
  holds all head-peer acks + *any* `coilQuorum` verified coil-peer acks — the
  whitepaper's "coil may skip" allowance: only *some* `coilQuorum` is needed.
- **Cap hard.** The aggregator attaches exactly `coilQuorum` coil-peer witnesses;
  a coil-peer ack arriving after saturation is dropped. `coilQuorum + 1` would
  exceed the fee-budgeted size and invalidate the tx.
- **Witness sets may differ across peers.** Each peer submits its *own* tx
  (R8/R9); peer A may attach coil peers `{1,2}` and peer B `{2,3}` — same count,
  both satisfy the on-chain `AtLeast`, and L1 duplicate-rejection decides which
  lands. No cross-peer witness agreement is required.
- **Two-phase stacks: each round picks its coil quorum independently.** A 2-phase
  stack signs in two rounds — round 1 over every effect except the unlock, round 2
  over the unlock/SEC tx. These are **different transactions**, each validated on
  its own against the threshold script (head peers via `AllOf(head)`, coil peers via
  `AtLeast(coilQuorum, …)`), so there is **no** requirement that the same coil peers
  sign both. Each round draws its own `coilQuorum` coil slots from the peers that
  signed *that* round (`SlowConsensusActor.selectSigners` → `quorumSigners`), and
  round 2 hard-confirms once the round-2 set is saturated by itself — not the
  intersection with round 1. The aggregator restricts each round's witnesses to its
  own quorum, so every tx still carries exactly `nHeadPeers + coilQuorum` witnesses
  even when the two coil subsets differ.

  Independent rounds are both **faster** (no waiting for the same coil peers to
  appear in both rounds) and **safer**: requiring the intersection would let a coil
  peer that signed round 1 withhold round 2 and block the stack — effective veto
  power — even though other coil peers could satisfy the round-2 quorum. Decoupling
  the rounds removes that leverage.

## 7. Fast-consensus participation — receive-only

A coil peer **does not** produce fast-side artifacts but **does** follow the
fast-side stream relayed by its hub (§8):

- Receives every block brief, every head-peer soft-ack, **and every head-peer
  user request** (the brief lists only request ids; the coil peer needs the
  request *content* to reproduce block bodies).
- Runs `BlockWeaver` follower-mode to drive `JointLedger` from observed
  `BlockBrief.Next` + the relayed requests + own `PollResults`, producing local
  `BlockResult` records that feed `StackComposer`'s pairing. The follower buffers
  a brief that arrives ahead of the requests it needs and replays it once the
  current block completes.
- Runs `FastConsensusActor` aggregator-mode to write `SoftConfirmation` records
  locally — also feeding `StackComposer`'s pairing.
- Emits **nothing** on the fast side — it only *follows*.

## 8. Hub topology and the coil link

### 8.1 Multi-hub partitioning (T1)

**Hubs** are the head peers that have at least one coil peer assigned. The
coil-peer set is **partitioned across hubs** by `coilPeers[i].hub`; let
`nHubs` = the number of distinct hub head peers (`nHubs ≤ nHeadPeers`).

A coil peer connects to **exactly one** hub — its partition's hub — for **both**
directions (T1):

- **Uplink** (coil → hub): the coil peer sends only its own hard-acks to its hub.
- **Downlink** (hub → coil): the hub relays the coil peer the **full** population
  stream. The hub already holds it all (it is a head peer in the mesh), so a coil
  peer needs only its one hub to see everything — every head peer's blocks /
  stacks / requests / soft-acks / hard-acks, and **every** hub's coil-hard-ack
  lane.

Multiple hubs exist across the coil-peer set; each hub serves its own partition.
The whitepaper anchors: `peer-network` §Coil-peer communication, §Disseminating
coil hard acks.

### 8.2 The lane model (one structure, every peer)

Every peer — head **or** coil — maintains the **same** set of lanes. This is also
the persisted lane structure, so coil-peer recovery reuses the head machinery
verbatim (recovery symmetry, now on the wire too — no de-mux step). Lane
families:

| Lane family | Multiplicity | Key | Shape |
|---|---|---|---|
| `BlockSpine` | 1 | `blockNum` | contiguous, head-global |
| `StackSpine` | 1 | `stackNum` | contiguous, head-global |
| `Request` | `nHeadPeers` | `(headPeer, requestNum)` | per author, contiguous |
| `SoftAck` | `nHeadPeers` | `(headPeer, softAckNum)` | per author, contiguous |
| `HardAck` (head) | `nHeadPeers` | `(headPeer, hardAckNum)` | per author, contiguous |
| `HubHardAck` (coil) | `nHubs` | `(hub, seqNum)` | per hub, contiguous |

- **Spines** are single contiguous lanes; the leader is recorded *inside* each
  item, not in the key. The mesh's sparse-per-leader brief dissemination is
  interleaved back into the contiguous spine before storage — nothing sparse
  reaches the store.
- **Per-author satellites** (`Request` / `SoftAck` / `HardAck`) carry one lane per
  head-peer author.
- **`HubHardAck`** carries one lane per hub. Each hub's `CoilAckSequencer`
  re-sequences its partition's coil-peer hard-acks onto its lane; the coil author
  is carried *inside* each `HardAck`. All `nHubs` lanes are disseminated to
  everyone. `SlowConsensusActor` reaches the coil quorum by reading these lanes
  and counting **distinct coil authors** (from the embedded `PeerId`), regardless
  of which hub re-sequenced them.

**Lanes stay separate — there is no `relayedMsg` multiplexing.** The coil link
carries every one of these lanes on its own cursor. This is the central change
from the earlier Pc4 design and is what makes the coil peer's stored structure
identical to a head peer's.

### 8.3 `CoilRelay` — the hub fan-out actor

**Why a separate actor.** Earlier the relay tapped the core consensus actors —
`FastConsensusActor` teed every soft-ack and `SlowConsensusActor` every hard-ack
to the relay. But a core actor's job ends earlier than the relay's: a
`SlowConsensusActor` cell saturates at *all head peers + `coilQuorum`* and can
ignore further hard-acks. As a tap it would have to keep accepting and forwarding
every *received* hard-ack past that point, purely to relay it — complicating the
actor we most want to keep simple. So relaying moves out of the core actors into a
dedicated **`CoilRelay`** (one per hub).

**What it does.** `CoilRelay` is a pure, **stateless** fan-out: it receives each
population lane item and forwards it to **every** `PeerLiaisonHubToCoil` the hub
runs. It holds no buffer and no cursors of its own — each
`PeerLiaisonHubToCoil` owns its **own** per-lane outbox queues and serves its coil
peer's `GetMsgBatch` from them (D-coil-5, pinned: per-liaison outboxes, not a
shared `CoilRelay` buffer). It aggregates **nothing** — the six lane families stay
separate end-to-end.

**What feeds it:**

- **Other head peers' production — via the mesh.** Each `PeerLiaisonHeadToHead`
  already routes its verified inbound to the local consensus actors; it
  additionally forwards it to `CoilRelay`. (A liaison is a routing actor, so an
  extra recipient is cheap and changes no consensus logic.) This carries every
  other head peer's blocks, stacks, requests, soft-acks, hard-acks, and its
  `HubHardAckLane`.
- **The hub's own production — via its producers.** `JointLedger` (own blocks),
  `StackComposer` (own stacks), `RequestSequencer` (own requests),
  `FastConsensusActor` (own soft-acks), `SlowConsensusActor` (own hard-acks), and
  `CoilAckSequencer` (the hub's own `HubHardAckLane`) each send their **own**
  output to `CoilRelay`. This is a single extra recipient for an artifact the
  actor already produces and broadcasts — *not* the received-traffic relay that
  was the problem. The core actors send their own; they never relay others'.

So *others' production* reaches `CoilRelay` through the mesh liaisons and *own
production* through the local producers — and no core consensus actor ever has to
process a message past its own completion point.

### 8.4 `CoilAckSequencer` — the `HubHardAckLane` (per hub, standalone)

A coil-peer hard-ack has `author = coil`, but the per-author `HardAck` satellites
are keyed by head-peer author. So coil-peer hard-acks ride their own lane family.
Each hub keeps a standalone **`CoilAckSequencer`** that:

1. Receives its partition's coil-peer hard-acks (via the hub's
   `PeerLiaisonHubToCoil`s) and persists them per coil peer — one CF
   `hardAckNumber → HardAck` per coil peer (durable write-ahead + idempotency
   guard against retransmits).
2. Assigns a hub-local sequence number to each newly-received ack and writes:
   - **`HubHardAckLane`**: `seqNum → HardAck` — the published, ordered lane.
   - **index**: `coilPeerId → hardAckNumber` — last-sequenced high-water per coil
     peer; the recovery key that prevents re-assigning a `seqNum` to an
     already-sequenced ack.

The lane is disseminated **both** ways: into the head mesh (one extra per-author
lane on each `PeerLiaisonHeadToHead`, so every head peer sees this hub's coil
acks) **and** into `CoilRelay` (so the hub's own coil peers hear each other).
Other hubs' `HubHardAckLane`s arrive at this hub over the mesh and likewise flow
into `CoilRelay`, so a coil peer hears **every** coil peer in the head via its one
hub forwarding all `nHubs` lanes.

`CoilAckSequencer` stays a separate actor from `CoilRelay`: it is the *producer*
of one lane family (re-sequencing coil acks), while `CoilRelay` is the *fan-out*
of all lane families to coil peers.

### 8.5 The three liaison shapes — composition, separate batch types

The peer↔peer link speaks a pull-based batch protocol (`GetMsgBatch` requests the
counterpart's outbox from a per-lane cursor; `NewMsgBatch` answers). There are
**three** liaison shapes, and they carry **genuinely different lane sets**, so
they are built by **composition over a shared per-lane protocol**, not a base
class:

- **`Lane[T]`** — the shared, reusable unit: one cursor + one outbox queue + the
  `append` / `build` / `verify` / `advance` logic for a single next-expected lane.
  A liaison's `GetMsgBatch` / `NewMsgBatch` is a **product of exactly the lanes it
  carries**; a thin driver composes them. Per-author lane families are a
  `Map[HeadPeerNumber, Lane[T]]` (or `Map[hub, Lane[T]]`), keyed by author.
- Each shape is its **own** `Actor` with its **own** `GetMsgBatch` / `NewMsgBatch`
  types (only the lanes that shape uses; no fat 8-field message, no inert lanes).

| Liaison | Outbound lanes | Inbound lanes |
|---|---|---|
| `PeerLiaisonHeadToHead` (head ↔ head) | this head peer's own production: `block` (sparse, own-led), `stack` (sparse), `request`, `softAck`, `hardAck`, and `HubHardAck` (if this head peer is a hub) | the remote head peer's same set |
| `PeerLiaisonHubToCoil` (hub → coil) | the **full** population: `block` (contiguous), `stack` (contiguous), `request ×nHeadPeers`, `softAck ×nHeadPeers`, head `hardAck ×nHeadPeers`, coil `HubHardAck ×nHubs` | the coil peer's own `hardAck` (one lane) |
| `PeerLiaisonCoilToHub` (coil → hub) | this coil peer's own `hardAck` (one lane) | the **full** population (mirror of `PeerLiaisonHubToCoil` outbound) |

There is no `(coil, coil)` shape — coil peers only ever link to their hub.

### 8.6 Relay is transport-only — verification stays end-to-end

On the hub → coil direction every artifact arrives with `author ≠ remote`
(authored across the population, merely relayed by the hub). The lanes carry the
**full signed** artifact, never a reference — the coil peer's `FastConsensusActor`
/ `SlowConsensusActor` verify each signature themselves and aggregate by the
embedded author. The hub is never trusted to vouch for an artifact; it only orders
and forwards. The coil link therefore needs no `author == remote` lane check (that
check belongs to the head mesh, where author *is* the remote).

### 8.7 Spawning

- A **hub** head peer's `MultisigRegimeManager` spawns, beyond its mesh
  `PeerLiaisonHeadToHead`s: one `PeerLiaisonHubToCoil` per coil peer it hubs
  (`coilPeers.filter(_.hub == ownHeadPeerNum)`), one `CoilAckSequencer`, and one
  `CoilRelay`.
- A **non-hub** head peer spawns neither `CoilRelay` nor `CoilAckSequencer`; it
  only carries the `nHubs` `HubHardAckLane`s on its existing mesh links.
- A **coil** peer's `CoilMultisigRegimeManager` spawns exactly one
  `PeerLiaisonCoilToHub`, toward `coilPeers[me].hub`.

### 8.8 The cost of being a hub

A hub buffers and forwards the **entire** population stream to each of its coil
peers, not just its own artifacts. Each `PeerLiaisonHubToCoil` outbox is therefore
sourced from the hub's *received-from-mesh* state plus its own production, and
bandwidth scales `O(coilPeers × stream)`. This is inherent to the hub model; we
accept it. (Partitioning coil peers across multiple hubs spreads this cost.)

### 8.9 Hub failure

Out-of-scope for M5. The whitepaper fixes hubs statically; head-side outbox
retransmission covers transient outages. A durable hub-down leaves its partition's
coil peers dark until the hub recovers or the head escalates to the rule-based
regime / evacuation. **D-coil-3** flags whether even a stub failover is M5-worth.

### 8.10 Transport — one shared WS server per peer

A coil peer is a separate process, so the hub↔coil link runs over a real
WebSocket transport, not only the in-process wiring used by tests. **Each peer
binds exactly one WS server** (`NodeWsServer`) — a hub does **not** run a second
server. That one server mounts two routes:

- **`/peer`** — the head mesh (`PeerWsTransport`, `Frame` envelope, `Mesh.Get` /
  `Mesh.New`). Topology: lower-numbered head peer dials higher.
- **`/coil`** — the hub→coil links (`CoilHubTransport`, `CoilFrame` envelope,
  `Population.Get/New` + `OwnHardAck.Get/New`). Mounted only on a hub.

The hub↔coil link is a **star**: each coil peer dials its single hub's `/coil`
(`CoilUplinkTransport`) and identifies itself with `CoilFrame.Hello(coilNum)`; the
hub binds that socket to the coil's `CoilPeerNumber`, routes inbound batches to
that coil's `PeerLiaisonHubToCoil`, and drains that coil's outbox for outbound
batches. A coil runs **no server** — only the uplink dialer.

The liaisons reach their counterparts through proxy actors that stand in for the
remote handle and forward over the transport: `RemoteHubProxy` (coil → hub) and
`RemoteCoilProxy` (hub → one coil), mirroring the mesh's `RemotePeerProxy`. The
wire carries the **full signed** artifacts (§8.6), so verification stays
end-to-end regardless of transport.

## 9. L1 access — independent (multisig regime)

A coil peer maintains its **own** Cardano backend connection — not mediated by
its hub. R8/R9 require it: a coil peer must independently verify and submit
multisigned L1 effects without depending on the head population.

> *"R8. Independent verification … R9. Resilient effect submission: Any peer
> (head or coil) should be able to independently submit verified effects to the
> L1 network."* (`peer-network`).

`CardanoLiaison` runs **unchanged** on a coil peer:

- Same `targetState` + `happyPathEffects` + `fallbackEffects` maps; same
  submission FSM.
- Same `PollResults` emission to `BlockWeaver` (so follower-mode can verify the
  leader's deposit decisions).
- A coil peer submits happy-path effects and the fallback tx the same way head
  peers do — R9's "resilient" wording is the point: any one peer drives
  submission and the others observe L1 duplicate-rejection.

Rule-based-regime L1 (vote / tally / resolve / evacuate) is covered in §10.

## 10. Rule-based regime — coil

A coil peer, like a head peer, operates in **both** regimes: the multisig regime
(§§3–9) and the rule-based regime. R10 *requires* a coil peer to be able to drive
the rule-based regime end-to-end so that custody is preserved even if every head
peer colludes:

> *"R10. … every peer (head or coil) should be able to independently … evacuate
> funds."* (`peer-network`).

**`RuleBasedRegimeManager` is shared** — the same regime manager + actors run on
head and coil peers. No coil-specific variant is needed.

**Transition** — a coil peer enters the rule-based regime the same way a head peer
does: the multisig regime submits a fallback tx (`CardanoLiaison` on any peer; §9)
and when it lands on L1 both head and coil peers tear down their multisig-regime
actors and bring up the rule-based ones.

**Rule-based actors (shared):**

- **`DisputeActor`** — vote, tally, resolve. A coil peer votes its own
  hard-confirmed evacuation commitment the same way head peers vote theirs.
- **`EvacuationActor`** — post-resolve `EvacuationTx`s authorized by KZG proof
  against the prevailing commitment. *"Evacuation transactions … can be drafted
  and submitted by anyone, without coordinating with the head or coil peers."*
  (`cardano-as-l1`).

**Evacuation is therefore not a `CardanoLiaison` concern** on either peer type —
`CardanoLiaison` is the **multisig**-regime L1 actor; the **rule-based**-regime L1
work lives in `/rulebased/`.

**The R10 floor on a coil peer** holds because: (a) it has its own L1 connection
(§9); (b) the rule-based actors are present and unchanged from head; (c) the R10
read-set (`HardConfirmation` + `Treasury` + `EvacuationMap`) is loaded once on
handover from local persistence — see `persistence-and-crash-recovery.md` §5.7.

## 11. Bootstrap

The "no brief broadcast" stack-0 bootstrap
(`StackComposer.bootstrapInitialStack`) already covers a coil peer: every peer —
head and coil — derives stack 0's init + fallback identically from the head
config, so a coil peer's `StackComposer` runs the same `PreStart` initialization a
head peer does. A coil peer's first observable output is signing the round-2
init-tx hard-ack, joining the head's stack-0 hard-confirmation.

`coilQuorum` bites at stack 0 in **both** signing rounds: round 1 (fallback) and
round 2 (init tx) each complete only when all head peers + at least `coilQuorum`
coil peers have signed. So even a 1-head / 1-coil bring-up with `coilQuorum = 1`
forces the coil peer through both `Round1Payload.Initial` and
`Round2Payload.Initial` — a full end-to-end exercise of coil slow consensus, not a
degenerate one. A coil peer's `Round2Payload.Initial.individualSig` is always
`None`: only head peers fund the init tx from individual addresses.

## 12. Skip-hard-ack policy

**Status: designed, not yet applied (Pc6).** Today a coil peer signs every stack;
the skip *triggers* (D-coil-4) are not plumbed. The lane protocol, however, already
accommodates skipping for free — see "Lane protocol" below — so turning it on is a
slow-consensus decision change, not a wire-format change.

A coil peer's freedom to *skip* is **scoped to its hard-ack signature output
only**. Every other piece of slow-side work is mandatory and runs on every block /
stack:

- **Block derivation runs every block.** BlockWeaver-follower drives JL through
  `StartBlock → Producing → CompleteBlock` on every received `BlockBrief.Next`,
  producing the local `BlockResult`.
- **Stack derivation runs every stack.** StackComposer pairs every block, closes
  every stack, derives `StackEffects` deterministically — keeping `treasury` +
  `evacuationMap` cumulative and the R10 floor honest.

So a coil peer **recreates every block + every stack** unconditionally; only its
own hard-ack signature is optional.

**Two distinct skip triggers:**

1. **Availability-driven skip (unavoidable, the whitepaper case).** The coil peer
   is offline, its hub is silent, or local derivation isn't complete by the
   stack's hard-ack deadline. *"coil peers may skip acknowledging blocks entirely
   since not all are available at all times"* (`peer-network`).
2. **Quorum-already-satisfied skip (optional optimization).** The coil peer
   observes that `coilQuorum` acks already exist for this stack without its own —
   its ack would be redundant.

Either way the **persisted shape is the same**: no entry in own `HardAck` lane for
that stack, but `BlockResult` and the `Treasury` / `EvacuationMap` snapshots
advance through the skipped stack on the normal cadence — cumulative slow-side
state is never gappy even when own hard-acks are.

**Lane protocol — no special support needed for gaps.** A `HardAck` carries its
`hardAckNum` and its `stackNum` as **separate** fields, and `hardAckNum` is a
**dense per-coil counter** (`StackComposer.nextOwnHardAckNum`) advanced only when
an ack is actually produced — a 2-phase stack consumes two numbers, a sole stack
one, a **skipped** stack zero. So a skip never consumes a `hardAckNum`: the own
`HardAck` lane (`Lane.contiguous`, successor `+1`) stays gap-free on the wire, and
so does the hub's re-sequenced `HubHardAckLane`. The gap lives **only** in the
`stackNum` dimension, carried *inside* each `HardAck`; the receiving
`SlowConsensusActor` places each ack by its embedded `stackNum` and simply never
sees a coil ack for a skipped stack (it reaches the quorum from other coil peers).
No sparse lane, skip marker, or out-of-band signal is required — the contiguous
next-expected protocol already supports skipping unchanged.

**D-coil-4** picks the concrete triggers + deadline: for (1), the deadline beyond
which a missing input forces skip; for (2), whether to enable it at all and how to
observe the current quorum count. A `lastConsideredStackNum` watermark records "I
considered stack N and chose not to ack" so a coil peer doesn't re-decide on
restart — a recovery concern for `persistence-and-crash-recovery.md` §11.

## 13. Implementation phasing

**Spine done (Pc1–Pc3).** Identity + threshold script (Pc1),
`CoilMultisigRegimeManager` + identity seam + role-gated wiring (Pc2), and the
single-hub liaison spine + `CoilAckSequencer` (Pc3) are built on `feature/coil`.

**Relay/lane rework done (Pc4).** The multiplexed `relayedMsg` relay is gone. The
coil link is the §8 architecture: separate per-author lanes (no multiplexing), the
stateless `CoilRelay` fan-out actor (core actors send only their own production),
and three composition liaisons (`PeerLiaison{HeadToHead, HubToCoil, CoilToHub}`)
with per-shape batch types. The hub↔coil WebSocket transport is built (§8.10).

| Step | Deliverable |
|---|---|
| Pc1 — **DONE** | `PeerId` tagged sum + `CoilPeerNumber` + one-bit wire tag through `HardAckId` / verifier / aggregator; threshold `HeadMultisigScript` with the mandatory-head / fixed-count-coil signer split (D-coil-1) |
| Pc2 — **DONE** | `CoilMultisigRegimeManager` + `OwnCoilPeerPrivate` identity seam (reuse `HeadConfig`); role-gated actor wiring; fixed-count aggregator; `headMultisigScript` moved to `Bootstrap.Section`; `CardanoLiaison` + `RuleBasedRegimeManager` shared (D-coil-2, D-coil-6) |
| Pc3 — **DONE** (spine) | The single-hub liaison spine + `CoilAckSequencer` / `HubHardAckLane`; 1h/1c stage4 reaches stack hard-confirmation at `coilQuorum = 1`. **Carve-out open:** the app-level coil bootstrap entry point (`Main` dispatch + config loader) |
| Pc4 — **DONE** | §8 relay/lane rework: `CoilRelay` fan-out + separate per-author lanes (no `relayedMsg`); three composition liaisons with per-shape `Mesh`/`Population`/`OwnHardAck` batches; reshaped `Connections` (contravariant `ActorRef` lets each producer broadcast its own artifact); hub↔coil WS transport (§8.10) |
| Pc5 | Stage-4 multi-peer model-based test with coil follower(s). **Partial:** Direct + WS 2h1c coil runs exist as opt-in props (coil follows + co-signs), but are not in the green set — blocked on harness settling, not correctness |
| Pc6 | Skip-stack policy plumbing (D-coil-4) |
| Pc7 | Coil peer submits happy-path + fallback alongside head (R8/R9) verified |

Persistence + crash-recovery for coil peers deferred to
`persistence-and-crash-recovery.md` §11. Rule-based-regime handover for a coil
peer (spawning `DisputeActor` + `EvacuationActor`) is a **separate task**.

### Pc4 — what shipped

The liaison layer lives in `consensus.liaison`, by **composition** (no base class):

- **Lane primitives** — `Lane[T, N]`, the per-lane next-expected unit (append /
  reply / verify / advanceTo; contiguous + sparse constructors); `BatchNumber`; and
  the per-shape batch types `BatchMessages.{Mesh, Population, OwnHardAck}.{Get, New}`.
- **Composition engines** — `Puller` (pull half) + `Server` (serve half, with the
  `Served` outcome in its companion). The pull/serve split covers the asymmetric
  hub↔coil link, where each half carries a different lane set.
- **Three liaisons** — `PeerLiaison{HeadToHead, HubToCoil, CoilToHub}`, sharing the
  `Request`/`Handle` aliases in `LiaisonProtocol`; plus the stateless `CoilRelay`
  fan-out (which lives next to `CoilAckSequencer`). `HardAckWithId` carries
  `hubPeer` so a re-sequenced coil ack self-describes its hub.

`Connections` is the wiring seam. `ActorRef` is **contravariant** in its message
type, so each producer broadcasts its **own** artifact to whichever liaison shape
accepts it: `headPeerLiaisons` (mesh, empty on a coil), `coilUplink` (Some on a
coil), `coilRelay` (Some on a hub). **Core actors never relay *received* traffic
to `CoilRelay`** — they send only their *own* production. So the spine briefs are
split by leadership: the hub's mesh `PeerLiaisonHeadToHead`s forward the
**remote-led** block/stack briefs (and that head's satellites), while the hub's
own `JointLedger` / `StackComposer` relay only the **own-led** briefs. Each brief
is therefore relayed exactly once.

`CoilRelay` needs **no reordering** for the contiguous block/stack lanes: the hub
is a head peer, and a leader can't produce artifact N+1 until N is confirmed by
**all** head peers (soft-confirm `acks.keySet == headPeerVKeys`; hard-confirm
`AllOf(head)`), so the hub always holds brief N before N+1 exists and relays them
in spine order. The contiguous `LaneOutbound.append` is the loud backstop if that
invariant is ever weakened. (See `CoilRelay`'s doc for the full proof + the three
load-bearing invariants.)

**Transport (§8.10):** one shared `NodeWsServer` per peer mounts `/peer` (head
mesh) and, on a hub, `/coil`; `CoilHubTransport` / `CoilUplinkTransport` /
`CoilFrame` carry the hub↔coil link, with `RemoteCoilProxy` / `RemoteHubProxy`
standing in for the remote handles. stage4 WebSocket mode wires coils over WS.

## Cross-cutting concerns

Concerns that cut across the phases above; each must be honored even though none
is owned by a single section:

- **Script-hash consistency (Pc1, load-bearing).** The threshold
  `HeadMultisigScript` must be the *same* script used to derive the stack-0
  treasury / beacon-token address at bootstrap. If any effect-tx builder
  re-derives the head address without the `MOf(coilQuorum, …)` branch, the script
  hash mismatches and stack-0 silently fails. Thread `coilQuorum` + coil keys into
  `HeadMultisigScript.apply` at the single derivation point.
- **`requiredSigners` downcast (Pc1).** The old `AllOf → Signature` flatten throws
  on the nested `MOf` node. Walk the head `Signature` leaves plus the `MOf`
  branch, emitting exactly `nHead + coilQuorum` `ExpectedSigner` placeholders
  (count-only — Scalus sizes fees by witness count). `checkSigners` moves from
  set-equality to *all-head ∧ coil ≥ coilQuorum*.
- **Persistence lane model (rework → recovery).** Coil-peer recovery now reuses
  the head lane model verbatim (§8.2) — no de-mux step. The persisted lane
  families are: spines (`BlockSpine`/`StackSpine`), per-author satellites
  (`Request`/`SoftAck`/`HardAck` keyed by `HeadPeerNumber`), and the per-hub
  `HubHardAck` lanes (`(hub, seqNum) → HardAck`, coil author inside). Folds into
  the deferred coil-persistence §11.
- **`CoilAckSequencer` CFs (rework → recovery).** Three CFs: the per-coil receive
  log (`hardAckNumber → HardAck`, one per coil peer), the published
  `HubHardAckLane` (`seqNum → HardAck`), and the index (`coilPeerId →
  hardAckNumber`, the recovery key). Payload duplicated for now; a later
  optimization stores it once with two index maps.
- **Logback sync.** Any new coil Tracer route (e.g. `CoilRelay`,
  `CoilMultisigRegimeManager`) must be added to **all three** `logback.xml`
  configs (root main + test, integration test), per the repo logging rule.
- **Coil `NodeConfig` validations (Pc2).** The shared `HeadConfig` validations
  (e.g. `pollingPeriod * 5 ≤ depositMaturityDuration`) must still run for a coil
  node, but head-private validations must be skipped. Enumerate which
  `NodeConfig.apply`-time checks are head-specific when building the coil path.

## 14. Open questions

- **D-coil-3 — hub failover.** Whether even a stub hub-failover is M5-worth
  (§8.9), or coil peers simply go dark on a durable hub-down until rule-based
  escalation.
- **D-coil-4 — skip triggers + deadline.** The concrete availability deadline and
  whether to enable the quorum-already-satisfied optimization (§12).

**D-coil-5 — `CoilRelay` outbox ownership — RESOLVED (per-liaison outboxes).**
`CoilRelay` is a stateless fan-out; each `PeerLiaisonHubToCoil` owns its own
per-lane outbox queues + cursors (§8.3). This keeps the liaison the single owner
of a link's send-state (uniform with `PeerLiaisonHeadToHead`) and recovery-aligned
per link, at the cost of buffering each lane item once per coil peer on the hub —
the accepted hub cost (§8.8). A shared `CoilRelay` buffer sliced per coil peer was
the rejected alternative.
