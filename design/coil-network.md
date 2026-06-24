# Coil-Ready Peer (M5)

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
2. **Own `HardAck` may be gappy (whitepaper allowance — deferred).** Whitepaper:
   *"coil peers may skip acknowledging blocks entirely since not all are available
   at all times"* (`peer-network`). Head peers must sign every stack; the whitepaper
   lets a coil peer decline. **The current implementation does not take this up** —
   a coil peer hard-acks **every** stack, so its `CoilHardAck` family is dense in
   `StackNumber`; the skip-hard-ack optimization is deferred (§6.1, GUM-152).

On both consensus sides it only **follows** — reconstructing the same local state
a head follower does, emitting no artifact of its own. **Fast side:** it consumes
the hub-relayed stream (block briefs, head-peer soft-acks, and the user-request
*content* the briefs reference) and runs `BlockWeaver` follower + `FastConsensusActor`
aggregator to rebuild `BlockResult` / `SoftConfirmation` locally (§4). **Slow side:**
its `StackComposer` pairs each leader `StackBrief` against those local inputs and
derives `StackEffects` byte-identically; `EffectSigner` signs the two-round (or sole)
hard-ack; `SlowConsensusActor` aggregates the whole population — all head-peer acks +
`coilQuorum` coil acks → `HardConfirmation`. The slow-side `PeerId` widening and
threshold script are the only code-shape changes (§7).

`CardanoLiaison` runs **unchanged** on a coil peer — happy-path effects
(initialization, settlement, finalization, rollouts) and the fallback tx are
submitted the same way head peers submit them, per R8/R9. A coil peer drives
evacuation — the rule-based regime (`/rulebased/EvacuationActor` + `DisputeActor`)
— the same way a head peer does.

## 3. Fixed-count coil-peer witnesses

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
  over the unlock (the settlement tx). These are **different transactions**, each validated on
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

## 4. Hub topology and the coil link

### 4.1 Multi-hub partitioning

**Hubs** are the head peers that have at least one coil peer assigned. The
coil-peer set is **partitioned across hubs** by `coilPeers[i].hub`; let
`nHubs` = the number of distinct hub head peers (`nHubs ≤ nHeadPeers`).

A coil peer connects to **exactly one** hub — its partition's hub — for **both**
directions:

- **Uplink** (coil → hub): the coil peer sends only its own hard-acks to its hub.
- **Downlink** (hub → coil): the hub relays the coil peer the **full** population
  stream. The hub already holds it all (it is a head peer in the mesh), so a coil
  peer needs only its one hub to see everything — every head peer's blocks /
  stacks / requests / soft-acks / hard-acks, and **every** hub's coil-hard-ack
  lane.

Multiple hubs exist across the coil-peer set; each hub serves its own partition.
The whitepaper anchors: `peer-network` §Coil-peer communication, §Disseminating
coil hard acks.

A hub buffers and forwards the **entire** population stream to each of its coil
peers, not just its own artifacts. Each `PeerLiaisonHubToCoil` outbox is therefore
sourced from the hub's *received-from-mesh* state plus its own production, and
bandwidth scales `O(coilPeers × stream)`. This is inherent to the hub model; we
accept it. (Partitioning coil peers across multiple hubs spreads this cost.)

### 4.2 The lane model

A **lane** is an append-only, ordered sequence of a single artifact kind, addressed
by a monotonic per-lane number; a reader keeps a cursor and pulls only what lies
past it. Every peer — head **or** coil — maintains the **same** set of lanes. Lane
families:

| Lane family | Multiplicity | Key | Shape |
|---|---|---|---|
| `BlockSpine` | 1 | `blockNum` | head-global, sparse per leader |
| `StackSpine` | 1 | `stackNum` | head-global, sparse per leader |
| `Request` | `nHeadPeers` | `(headPeer, requestNum)` | per author, contiguous |
| `SoftAck` | `nHeadPeers` | `(headPeer, softAckNum)` | per author, contiguous |
| `HardAck` (head) | `nHeadPeers` | `(headPeer, hardAckNum)` | per author, contiguous |
| `HubHardAck` (coil) | `nHubs` | `(hub, seqNum)` | per hub, contiguous |

- **Spines** are single contiguous lanes; the leader is recorded *inside* each
  item, not in the key. The mesh's sparse-per-leader brief dissemination is
  interleaved back into the contiguous spine.
- **Per-author satellites** (`Request` / `SoftAck` / `HardAck`) carry one lane per
  head-peer author.
- **`HubHardAck`** carries one lane per hub. Each hub's `CoilAckSequencer`
  re-sequences its partition's coil-peer hard-acks onto its lane; the coil author
  is carried *inside* each `HardAck`. All `nHubs` lanes are disseminated to
  everyone. `SlowConsensusActor` reaches the coil quorum by reading these lanes
  and counting **distinct coil authors** (from the embedded `PeerId`), regardless
  of which hub re-sequenced them.

**The relay is transport-only — verification stays end-to-end.** Lanes carry the
**full signed** artifact, never a reference, so on the hub → coil direction (every
item authored across the population, `author ≠ remote`) the coil peer's
`FastConsensusActor` / `SlowConsensusActor` verify each signature themselves and
aggregate by the embedded author. The hub only orders and forwards — never trusted
to vouch — so the coil link needs no `author == remote` lane check (that belongs to
the head mesh, where author *is* the remote).

### 4.3 Transport — one shared WS server per peer

A coil peer is a separate process, so the hub↔coil link runs over a real
WebSocket transport, not only the in-process wiring used by tests. **Each peer
binds exactly one WS server** (`NodeWsServer`) — a hub does **not** run a second
server. That one server mounts two routes:

- **`/head`** — the head mesh (`PeerWsTransport`, `HeadFrame` envelope, `Mesh.Get` /
  `Mesh.New`). Topology: lower-numbered head peer dials higher.
- **`/hub`** — the hub→coil links (`HubWsTransport`, `CoilFrame` envelope,
  `Population.Get/New` + `OwnHardAck.Get/New`). Mounted only on a hub.

The hub↔coil link is a **star**: each coil peer dials its single hub's `/hub`
(`CoilPeerWsTransport`) and identifies itself with `CoilFrame.Hello(coilNum)`; the
hub binds that socket to the coil's `CoilPeerNumber`, routes inbound batches to
that coil's `PeerLiaisonHubToCoil`, and drains that coil's outbox for outbound
batches. A coil runs **no server** — only the uplink dialer.

The liaisons reach their counterparts through proxy actors that stand in for the
remote handle and forward over the transport: `RemoteHubProxy` (coil → hub) and
`RemoteCoilProxy` (hub → one coil), mirroring the mesh's `RemotePeerProxy`. The
wire carries the **full signed** artifacts (§4.2), so verification stays
end-to-end regardless of transport.


## 5. Detailed design

### 5.1 Configuration

**A coil peer reuses `HeadConfig` wholesale** —
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
independent `CardanoBackend` connection; the `L2Ledger` is constructed from
the same configuration head peers use (byte-deterministic across head and coil).

**Script accessor — coil context is type-level (done, Pc2).** `headMultisigScript`
/ `headMultisigAddress` live on the coil-aware `HeadConfig.Bootstrap.Section`
(which has `coilPeers` + `coilQuorum`); the head-only derivation on
`HeadPeers.Section` was deleted, so coil context is a *type* requirement — a bare
`HeadPeers` value can no longer silently drop the coil branch.

### 5.2 Actor topology

The head-peer **multisig-regime** actor set (`HeadMultisigRegimeManager`) is the
reference. A coil peer's multisig regime is a strict subset (separate manager —
`CoilMultisigRegimeManager`); the **rule-based regime** is **shared** —
`RuleBasedRegimeManager` runs identically on head and coil peers.

| Actor (head-peer reference) | Coil-peer treatment |
|---|---|
| `RequestSequencer` | **inert** — present to fill the shared `Connections` slot, but no HTTP surface routes user requests to it and a coil peer authors none |
| `BlockWeaver` | **follower-only** — drives `JointLedger` (`StartBlock` / `CompleteBlock`) from observed `BlockBrief.Next` + relayed requests + `PollResults`; never enters Leader role, no mempool drain |
| `JointLedger` | runs the same `Done → Producing → Done` cycle on every block whether leading or following. Two `handleBlock` steps are gated: the brief broadcast (leader-only, `canLeadFast` — false on a coil peer) and own soft-ack production (head-only — a coil peer authors none) |
| `FastConsensusActor` | aggregator-only — verifies + aggregates head-peer soft-acks into `SoftConfirmation`s; **signs nothing**. A coil peer simply has no own soft-ack to add |
| `StackComposer` | **follower-only** — pairs `BlockResult` × `Block.SoftConfirmed` × inbound `StackBrief` from the leader; never closes a stack as leader |
| `SlowConsensusActor` | same as head |
| liaisons | a coil peer runs **one** `PeerLiaisonCoilToHub` toward its hub (`coilPeers[me].hub`); a hub head peer runs one `PeerLiaisonHubToCoil` per coil peer it hubs, plus `CoilAckSequencer` and `CoilRelay` (§5.3, §5.4) |
| `CoilRelay` | **hub-only** — fan-out actor that distributes the population stream to the hub's coil peers (§5.4). Absent on a coil peer and on non-hub head peers |
| `CoilAckSequencer` | **hub-only** — re-sequences this hub's coil peers' hard-acks onto its `HubHardAckLane` (§5.3) |
| `CardanoLiaison` | same as head |

Absent actors → no scaffolding spawned. Present actors → existing head-peer
implementations reused, with the shared `Connections` holding the unused slots
empty (the coil peer is a strict subset of a head peer's `Connections`).

**No rate limiters.** A coil peer spawns no `Limiter` actors — the
`blockWeaverLimiter` / `stackComposerLimiter` slots in `Connections` alias the
unthrottled `BlockWeaver` / `StackComposer` handles directly. A `Limiter` paces a
*leader*'s output against L1 timing; a coil peer never leads, so there is nothing
to pace.

### 5.3 New actor `CoilAckSequencer` — the `HubHardAckLane` (per hub, standalone)

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
hub forwarding all `nHubs` lanes. The re-sequenced ack is a `HardAckWithId` whose
`hubPeer` field self-describes which hub sequenced it.

`CoilAckSequencer` stays a separate actor from `CoilRelay`: it is the *producer*
of one lane family (re-sequencing coil acks), while `CoilRelay` is the *fan-out*
of all lane families to coil peers.

### 5.4 New actor `CoilRelay` — the hub fan-out actor

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
peer's `GetMsgBatch` from them (per-liaison outboxes, not a
shared `CoilRelay` buffer sliced per coil peer — this keeps each liaison the single
owner of its link's send-state, uniform with `PeerLiaisonHeadToHead` and
recovery-aligned per link, at the accepted cost of buffering each lane item once
per coil peer on the hub, §4.1). It aggregates **nothing** — the six lane families
stay separate end-to-end.

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

**No reordering needed.** `CoilRelay` never reorders the contiguous block / stack
lanes — not because it sorts, but because two guarantees compose. **A consensus
invariant** spaces the briefs in time: the hub is a head peer, and a leader cannot
produce artifact N+1 until N is confirmed by **all** head peers (a block seals only
on `Block.SoftConfirmed(N-1)`, and soft-confirm needs every head peer's ack
`acks.keySet == headPeerVKeys`; a stack closes only once the prior is hard-confirmed,
`AllOf(head)` ∧ a coil quorum). Being in every all-head quorum, the hub receives
brief N before N+1 can exist anywhere. **The actor model** then preserves that order
for free: messages from one sender to one receiver are delivered in send order, so a
feeder that forwards to `CoilRelay` **the moment it receives** carries the arrival
order intact into the outbox — `CoilRelay`'s FIFO mailbox never has to sort. The
contiguous `LaneOutbound.append` is the loud backstop — a violation raises
`AppendOutOfOrder` rather than corrupting silently.

Three invariants keep this true — **don't break them**:

1. **Confirmation is all-head** (soft: `acks.keySet == headPeerVKeys`; hard:
   `AllOf(head)`). A `< nHeadPeers` threshold would let the hub be skipped and
   receive N+1 before N, forcing a reorder buffer here.
2. **A leader gates the next artifact on the prior's confirmation** — no pipelining
   past one.
3. **Every brief fed here is one the hub itself received or produced** (mesh liaison
   for remote-led, own `JointLedger` / `StackComposer` for own-led) — don't add a
   feeder that forwards a brief the hub hasn't itself seen.

### 5.5 The three liaison shapes

The peer↔peer link speaks a pull-based batch protocol (`GetMsgBatch` requests the
counterpart's outbox from a per-lane cursor; `NewMsgBatch` answers). There are
**three** liaison shapes, and they carry **genuinely different lane sets**, so
they are built by **composition over a shared per-lane protocol**, not a base
class:

- **Lane primitives** — the next-expected lane, split by direction:
  `LaneOutbound` (`append` / `reply`) and `LaneInbound` (`cursor` / `verify` /
  `advanceTo`), each with contiguous + sparse constructors, plus `LaneBidirectional`
  pairing the two for the symmetric mesh. A per-author lane family is a
  `Map[HeadPeerNumber, …]` (or `Map[hub, …]`), keyed by author.
- **Batch messages** — `BatchMessages.{Mesh, Population, OwnHardAck}.{Get, New}`,
  one `Get` / `New` product per link, carrying **exactly** that link's lanes (no
  fat message, no inert lanes) and tagged with a `BatchNumber`. A liaison's
  `GetMsgBatch` / `NewMsgBatch` is therefore a product of just the lanes it carries.
- **Composition engines** — `Puller` (pull half) and `Server` (serve half, with the
  `Served` outcome in its companion) compose the per-lane units into a liaison. The
  pull/serve split is what lets the asymmetric hub↔coil link give each half a
  different lane set.
- **Three liaison actors** — `PeerLiaison{HeadToHead, HubToCoil, CoilToHub}`, each
  its own `Actor`, sharing the `Request` / `Handle` aliases hoisted into
  `LiaisonProtocol` (which breaks the compile cycle between the two hub↔coil shapes).

| Liaison | Outbound lanes | Inbound lanes |
|---|---|---|
| `PeerLiaisonHeadToHead` (head ↔ head) | this head peer's own production: `block` (sparse, own-led), `stack` (sparse), `request`, `softAck`, `hardAck`, and `HubHardAck` (if this head peer is a hub) | the remote head peer's same set |
| `PeerLiaisonHubToCoil` (hub → coil) | the **full** population: `block` (contiguous), `stack` (contiguous), `request ×nHeadPeers`, `softAck ×nHeadPeers`, head `hardAck ×nHeadPeers`, coil `HubHardAck ×nHubs` | the coil peer's own `hardAck` (one lane) |
| `PeerLiaisonCoilToHub` (coil → hub) | this coil peer's own `hardAck` (one lane) | the **full** population (mirror of `PeerLiaisonHubToCoil` outbound) |

There is no `(coil, coil)` shape — coil peers only ever link to their hub.


### 5.6 Spawning actors

- A **hub** head peer's `HeadMultisigRegimeManager` spawns, beyond its mesh
  `PeerLiaisonHeadToHead`s: one `PeerLiaisonHubToCoil` per coil peer it hubs
  (`coilPeers.filter(_.hub == ownHeadPeerNum)`), one `CoilAckSequencer`, and one
  `CoilRelay`.
- A **non-hub** head peer spawns neither `CoilRelay` nor `CoilAckSequencer`; it
  only carries the `nHubs` `HubHardAckLane`s on its existing mesh links.
- A **coil** peer's `CoilMultisigRegimeManager` spawns exactly one
  `PeerLiaisonCoilToHub`, toward `coilPeers[me].hub`.

### 5.7 Bootstrap zero stack

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


## 6. Scope and phasing

### 6.1 Deferred work

Coil-peer follow-ups outside the implemented spine, each tracked separately:

- **Skip-hard-ack policy.** A coil peer may skip producing its own hard-ack when
  the stack has already reached `coilQuorum` without it — designed, not yet
  plumbed. [GUM-152](https://linear.app/gummiworm-labs/issue/GUM-152/coil-peer-skip-own-hard-ack-production-when-quorum-already-satisfied)
- **Persistence + crash-recovery.** Lands as §11 of
  `persistence-and-crash-recovery.md`, a delta on the head-peer recovery
  architecture. [GUM-153](https://linear.app/gummiworm-labs/issue/GUM-153/persistence-recovery-update-for-the-coil-peers)
- **Rule-based-regime handover.** Spawning `DisputeActor` + `EvacuationActor` for a
  coil peer. [GUM-150](https://linear.app/gummiworm-labs/issue/GUM-150/switching-to-rule-based-regime-for-coil-peers)
- **R8/R9 effect submission.** Verify a coil peer submits happy-path + fallback
  effects alongside the head peers. _(link: TBD)_

### 6.2 Implementation phasing

| Step | Deliverable |
|---|---|
| Pc1 — **DONE** | `PeerId` tagged sum + `CoilPeerNumber` + one-bit wire tag through `HardAckId` / verifier / aggregator; threshold `HeadMultisigScript` with the mandatory-head / fixed-count-coil signer split |
| Pc2 — **DONE** | `CoilMultisigRegimeManager` + `OwnCoilPeerPrivate` identity seam (reuse `HeadConfig`); role-gated actor wiring; fixed-count aggregator; `headMultisigScript` moved to `Bootstrap.Section`; `CardanoLiaison` + `RuleBasedRegimeManager` shared |
| Pc3 — **DONE** (spine) | The single-hub liaison spine + `CoilAckSequencer` / `HubHardAckLane`; 1h/1c stage4 reaches stack hard-confirmation at `coilQuorum = 1`. **Carve-out open:** the app-level coil bootstrap entry point (`Main` dispatch + config loader) |
| Pc4 — **DONE** | §4 relay/lane rework: `CoilRelay` fan-out + separate per-author lanes (no `relayedMsg`); three composition liaisons, each with its own `Mesh`/`Population`/`OwnHardAck` batches; reshaped `Connections` (contravariant `ActorRef` lets each producer broadcast its own artifact); hub↔coil WS transport (§4.3) |
| Pc5 — **DONE** | Stage-4 multi-peer model-based test with coil follower(s): Direct + WS 2h1c runs where the coil peer follows every leader's blocks and co-signs every stack |

## 7. What changes vs. a head peer

**Reused unchanged:**
- `BlockWeaver` follower path (`BlockBrief.Next` + `PollResults` →
  `StartBlock` / `CompleteBlock{Regular,Final}` driving JL).
- `StackComposer` follower path (`Block.SoftConfirmed` × `BlockResult` ×
  inbound `StackBrief` pairing; `StackEffectsBuilder`).
- `HardAckAggregator`, `HardAckSignatureVerifier`, `EffectSigner`.
- `CardanoLiaison` — same target-state / `happyPathEffects` / `fallbackEffects`
  maps and submission FSM (R8/R9), over an independent `CardanoBackend`.
- The head-mesh transport (`PeerWsTransport`, `HeadFrame`, `Codecs`) — the link
  logic is unchanged; server ownership lives in `NodeWsServer` so a hub shares one
  server across the mesh and the coil link (§4.3).
- `L2Ledger` black box — a coil peer instantiates its own deterministic copy.
- `RuleBasedRegimeManager` and its actors — **shared**, run identically on a coil
  peer (the coil handover wiring is deferred, §6).

**Added:**
- **`CoilPeerNumber`** opaque type — stable ordering for the multisig native
  script.
- **`CoilMultisigRegimeManager`** — coil-side counterpart to head's
  `HeadMultisigRegimeManager`, spawning the present-actor subset with one uplink
  liaison.
- **`CoilRelay`** + **`CoilAckSequencer`** — hub-side relay actors (§5.3, §5.4).
- The three liaison shapes (§5.5): `PeerLiaisonHeadToHead`, `PeerLiaisonHubToCoil`,
  `PeerLiaisonCoilToHub`, over a shared per-lane protocol.

The coil bootstrap entry point (`app/` layer, parallel to head's) is the one piece
still unwired — the Pc3 carve-out (§6).

**Changed — slow-side identity.** Coil-authored hard-acks travel the same lanes as
head-peer ones, so every slow-side identity slot is the tagged
`PeerId = Head(HeadPeerNumber) | Coil(CoilPeerNumber)`:

- `HardAckId` is `(PeerId, HardAckNumber)`. Wire form: peer number + a one-bit tag
  (`1` = head, `0` = coil), so the 2-int tuple grows by one bit rather than gaining
  a structural tag field.
- `SlowConsensusActor`'s quorum set and `HardAckAggregator`'s
  `vkeys: Map[PeerId, VerificationKey]` key on `PeerId`; the verifier rebuilds each
  `VKeyWitness` from the `PeerId → vkey` lookup, so a peer can only ever contribute
  a witness under its own key (head or coil).

This lands on **both** peer types — head peers aggregate coil-peer hard-acks to
reach hard-confirmation, so the widening is not coil-local.

**Changed — threshold native script.** `HeadMultisigScript` is a threshold
composition:

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
peers, not identity-pinned) — the fee-sizing rule §3 leans on.

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
  difference, not a behavioral gate. (The skip-hard-ack optimization, §6.1, is
  deferred — a coil currently hard-acks every stack.)
- `CardanoLiaison`: **no** role gate — submits happy-path + fallback identically.
