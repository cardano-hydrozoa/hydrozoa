# Coil-Ready Peer (M5)

**Status:** Active M5 workstream — core decisions locked 2026-05-30 (see
**Resolved decisions** below). **Pc1–Pc4 implemented on `feature/coil`:** the
identity spine + threshold script (Pc1), the `CoilMultisigRegimeManager` +
identity seam + role-gated actor wiring (Pc2), the two coil liaison shapes +
`CoilAckSequencer` + the 1h/1c spine (Pc3), and the **multi-head relay** (Pc4) —
contiguous brief relay + a re-sequenced `relayedMsg` lane (`CoilLinkRelay`)
carrying head soft-acks, head + coil hard-acks, and head user requests, de-muxed
by author at the coil, plus follower-`BlockWeaver` brief buffering.

A coil is now a **full consensus participant (fast + slow)**: a manual stage4 run
at 2 heads / `coilQuorum`=1 hard-confirms ~11 stacks on both heads *and* the coil.
Open items: (a) the generative stage4 `Two-heads-one-coil` property isn't green —
blocker is **harness settling** (the coil's extra liaison + relay round-trips
keep the post-run drain from idling), not the relay; (b) **coil persistence /
crash-recovery** is designed-for (recovery symmetry drove the relay design) but
unbuilt here — it lands on `feature/recovery` (the satellite `LaneId` key widens
`HeadPeerNumber → PeerId`); (c) the **app-level launch path** (`Main` dispatch +
a coil config-*loader*) is untouched — only the actor-level `CoilMultisigRegimeManager`
is wired. Later phases Pc5–Pc7 (§13) remain.

This spec describes the **coil-peer node type**: the process that joins a
Gummiworm head as a custodial slow-consensus follower. It is M5's "coil
consensus" deliverable.

The protocol-level design for coil consensus is already laid out in the
whitepaper (`peer-network`, `slow-consensus`, `initialization`); this spec
turns it into an implementable node-type spec.

Persistence + crash-recovery for coil peers is **out of scope here** — it
will be added as §11 of `persistence-and-crash-recovery.md` later, as a delta
on the head-peer recovery architecture (parallel M5 workstream).

The permissionless coil **marketplace** — on-chain registry, head onboarding
bonds, rent, peer selection, dynamic membership — is **deferred to a separate
future-work spec**. It builds on this node-type but is not part of M5.

**Whitepaper sources:**
- `single-head-gummiworm-protocol/peer-network` — coil topology, replicated set, hard-ack dissemination, communication flow
- `single-head-gummiworm-protocol/consensus/slow-consensus` — coil quorum role; round-1 / round-2 signing
- `single-head-gummiworm-protocol/replicated-state-machine` — what state each peer maintains
- `blockchain-specifics/cardano/initialization` — bootstrap config (`coilPeers`, `coilQuorum`), multisig native script

---

## Resolved decisions (2026-05-30)

Decided after reviewing `peer-network`, `slow-consensus`, and
`initialization`.

- **D-coil-1 — peer identity is a tagged sum.**
  `PeerId = Head(HeadPeerNumber) | Coil(CoilPeerNumber)`, where
  `CoilPeerNumber` is the coil's index in `coilPeers` sorted canonically **by
  verification-key bytes** (independent of the config's list order — every peer
  must derive the same index→vkey map AND the same `MOf` branch order, else the
  script hash diverges).
  Wire form: the peer number plus a one-bit tag (`1` = head, `0` = coil), so
  the existing 2-int `HardAckId` tuple grows by one bit rather than gaining a
  structural tag field. `HardAckId`, the slow-consensus quorum set, the
  aggregator's vkey map, the verifier, and the native-script signer ordering
  all carry it. **Both** peer types thread it — head peers aggregate coil
  hard-acks to hard-confirm, so this is not coil-only. (§6.)
- **Threshold native script.** The head's native script becomes
  `AllOf(headSigs) ∧ AtLeast(coilQuorum, coilSigs)`. It governs beacon-token
  minting, the treasury / multisig-regime spend, and the head address — one
  script, all uses. Confirmed by `initialization`: the init tx "must be signed
  by all head peers and a quorum of coil peers." (§6.)
- **Fixed-count coil witnesses.** Every effect tx is built and fee-sized for
  exactly `nHeadPeers + coilQuorum` witnesses. The aggregator considers a cell
  saturated as soon as it holds all head acks + any `coilQuorum` verified coil
  acks, attaches exactly that many coil witnesses (never more — a larger set
  underpays the fee), and proceeds. Which coil keys fill the slots may differ
  across peers; only the count is fixed, and any `coilQuorum` satisfies the
  on-chain `AtLeast`. (§6.)
- **D-coil-2 / D-coil-6 — reuse `HeadConfig`, parallel manager.** Coil reuses
  `HeadConfig` wholesale (distribution delivers the same head config to head
  and coil peers); the only difference is identity, carried at the
  `NodeConfig` private layer as an `OwnCoilPeerPrivate` (signing key + derived
  `CoilPeerNumber`) in place of `OwnHeadPeerPrivate`. A separate
  `CoilMultisigRegimeManager` spawns the follower actor subset. No slim
  `CoilConfig`. (§5.)

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
`config.head.coil.CoilPeer`) and never spawns coil-side actors. M5 closes
that gap.

Out of scope here:
- **Persistence + crash-recovery for coil peers** — deferred to
  `persistence-and-crash-recovery.md` §11 (parallel M5 workstream).
- **Permissionless coil marketplace mechanics** (registry, bonds, rent,
  dynamic membership) — separate future-work spec.

## 2. The one-sentence design

A coil peer is a head peer in **constant follower mode**: `BlockWeaver` never
enters Leader role, `StackComposer` never closes as leader, `JointLedger`
produces no `SoftAck`, no `RequestSequencer` exists, and the head-mesh
`PeerLiaisonHeadToHead` collapses to a single hub. Same lanes, same replicated set,
same L1-submission and rule-based-regime paths.

The "constant follower" intuition holds with **two precise breaks**:

1. **Authors one lane only — own `HardAck`.** Whitepaper: *"Coil peers can
   produce only one type of log entries: Hard acknowledgements."*
   (`peer-network`).
2. **Own `HardAck` may be gappy.** Whitepaper: *"coil peers may skip
   acknowledging blocks entirely since not all are available at all times"*
   (`peer-network`). Head peers must sign every stack; coil may decline.

`CardanoLiaison` runs **unchanged** on coil — happy-path effects
(initialization, settlement, finalization, rollouts) and the fallback tx are
submitted by coil the same way head peers submit them, per R8/R9 (§9).
Evacuation is not a `CardanoLiaison` concern on either peer type — it lives
in the rule-based regime (`/rulebased/EvacuationActor` + `DisputeActor`),
which coil drives the same way head does.

## 3. Actor topology

The head-peer **multisig-regime** actor set (`MultisigRegimeManager`) is the
reference. Coil's multisig regime is a strict subset (separate manager —
`CoilMultisigRegimeManager`); the **rule-based regime** is **shared** —
`RuleBasedRegimeManager` runs identically on head and coil (§10).

| Actor (head-peer reference) | Coil-peer treatment |
|---|---|
| `EventSequencer` (a.k.a. `RequestSequencer`) | **absent** — no user-request surface |
| `BlockWeaver` | **follower-only** — drives `JointLedger` (`StartBlock` / `CompleteBlock`) from observed `BlockBrief.Next` + `PollResults`; never enters Leader role, no mempool drain |
| `JointLedger` | runs the same `Done → Producing → Done` cycle on every block whether leading or following (`Producing` is the block-application state, entered by both — not a leader path). Two steps in `handleBlock` are gated: the brief broadcast (leader-only, `canLeadFast` — false on coil) and own soft-ack production (head-only — coil authors none) |
| `FastConsensusActor` | aggregator-only — verifies + aggregates head soft-acks into `SoftConfirmation`s; **signs nothing** (soft-acks are authored by `JointLedger`). On coil it still aggregates the head soft-acks relayed via the hub; a coil simply has no own soft-ack to add |
| `StackComposer` | **follower-only** — pairs `BlockResult` × `Block.SoftConfirmed` × inbound `StackBrief` from the leader, never closes a stack as leader |
| `SlowConsensusActor` | same as head |
| `PeerLiaisonHeadToHead` | **one** `PeerLiaisonCoilToHead` toward the hub (`coilPeers[me].hub`) — sends only own hard-acks, receives the full relayed population stream (contiguous briefs + the re-sequenced `relayedMsg` lane, de-muxed by author). The hub head runs the counterpart `PeerLiaisonHeadToCoil` + the two relay sequencers `CoilAckSequencer` (coil acks → head mesh) and `CoilLinkRelay` (the whole population's soft-acks / hard-acks / requests → its coils) (§8) |
| `CardanoLiaison` | same as head |

Absent actors → no scaffolding spawned. Present actors → existing head-peer
implementations reused, with the `Connections` barrier holding the absent
slots empty (or the barrier shape itself slimmed for coil — D-coil-6).

## 4. What changes vs. head-peer code today

**Reused unchanged:**
- `BlockWeaver` follower path (`BlockBrief.Next` + `PollResults` →
  `StartBlock` / `CompleteBlock{Regular,Final}` driving JL).
- `StackComposer` follower path (`Block.SoftConfirmed` × `BlockResult` ×
  inbound `StackBrief` pairing; `StackEffectsBuilder.deriveRegular` /
  `deriveInitial`).
- `HardAckAggregator`, `HardAckVerifier`, `EffectSigner`.
- `CardanoLiaison` — same target-state / `happyPathEffects` /
  `fallbackEffects` maps and submission FSM (R8/R9). Driven by an
  independent `CardanoBackend` connection.
- Transport (`PeerWsTransport`, `Frame`, `Codecs`).
- `L2Ledger` black box — coil instantiates its own deterministic copy.

**New types / actors:**
- **`CoilPeerNumber`** opaque type — stable ordering for the multisig native
  script (the open question already flagged in
  `config/head/coil/CoilPeer.scala`).
- **`CoilMultisigRegimeManager`** — coil-side counterpart to head's
  `MultisigRegimeManager` (multisig regime only), spawning the present-actor
  subset (no `EventSequencer`, no leader role for `BlockWeaver` /
  `JointLedger` / `StackComposer`) with one PeerLiaisonHeadToHead-to-hub. The
  rule-based regime's `RuleBasedRegimeManager` is **shared** with head — no
  coil-specific variant needed (§10).
- Coil-side bootstrap entry point (`app/` layer), parallel to head's.

**Behavioral gates on reused actors:**
- `BlockWeaver`: gate the Leader role on role; follower path (drive JL from
  `BlockBrief.Next` + `PollResults`) unchanged.
- `JointLedger`: the `Done → Producing → Done` cycle is unchanged — entered on
  every block in both modes (`Producing` is the block-application state, not a
  leader path). Two `handleBlock` steps are role-gated: the leader-only brief
  broadcast (`canLeadFast`) and own soft-ack production (head-only; coil emits
  none).
- `FastConsensusActor`: aggregator-only, no gate of its own — on coil it simply
  never receives an own soft-ack to schedule, because JointLedger emits none.
- `StackComposer`: gate the leader-close path on role; coil keeps full
  derivation (every block, every stack) but the **own-hard-ack production**
  is the only coil-only optionality (§12 skip policy).
- `PeerLiaisonHeadToHead`: single-hub case is structurally the existing per-remote
  `PeerLiaisonHeadToHead` with `nRemotes == 1`.

`CardanoLiaison` carries **no role gate** — it submits happy-path + fallback the
same way on coil as on head (§9). JointLedger's `Producing` cycle is likewise
un-gated (every block, both modes); only the two `handleBlock` steps above
differ between head and coil.

The role parameter lives in the regime manager and is read by each actor at
construction; it is **not** carried in any wire message.

## 5. Configuration

**Coil reuses `HeadConfig` wholesale** — `initialization`'s distribution phase
delivers the *same* head config to head and coil peers, and coil needs nearly
all of it (`coilPeers`, `coilQuorum`, `headParams`, timing, `initTx`,
`headId`). The only thing that differs between a head node and a coil node is
**identity**, carried at the `NodeConfig` private layer:

```
NodeConfig(headConfig: HeadConfig,        // shared verbatim, head & coil
           nodePrivateConfig)             // carries identity
```

Today `nodePrivateConfig` holds an `OwnHeadPeerPrivate`. A coil node swaps in
an **`OwnCoilPeerPrivate`** — this coil's signing key plus its derived
`CoilPeerNumber` (its index in `coilPeers` sorted canonically by
verification-key bytes, located by matching its own vkey). `HeadConfig` is untouched; `CoilMultisigRegimeManager` reads the private
side to know "I am coil N" and finds its hub via the matching `coilPeers`
entry.

Everything else a coil needs it reads from the shared `HeadConfig`: the head
set, `coilQuorum`, slow-consensus timing, finalization rules. L1 access is an
independent `CardanoBackend` connection (§9); the `L2Ledger` is constructed
from the same configuration head peers use (byte-deterministic across head and
coil). This resolves **D-coil-2** toward reuse.

**Script accessor — coil context is type-level (done, Pc2).** `headMultisigScript`
/ `headMultisigAddress` live on the coil-aware `HeadConfig.Bootstrap.Section`
(which has `coilPeers` + `coilQuorum`); the head-only derivation on
`HeadPeers.Section` was deleted, so coil context is a *type* requirement, not a
runtime invariant — a bare `HeadPeers` value can no longer silently drop the coil
branch. The ~12 tx-builder `Config` aliases (Initialization / Settlement / Refund
/ Deposit / Finalization / Rollout / MultisigRegimeUtxo + rulebased Vote / Tally /
Resolution / Deinit / RuleBasedTreasury) carry `Bootstrap.Section` accordingly.

## 6. Slow-consensus participation

Coil's slow-consensus role is **identical to a head follower** except for the
identity of the signing key and the gappy allowance:

- **Pairing & derivation** — coil's `StackComposer` follower path receives
  the leader's `StackBrief` via hub, pairs it against `BlockResult` ×
  `Block.SoftConfirmed` reconstructed locally, derives `StackEffects`
  deterministically (byte-identical to every head peer's derivation).
- **Signing** — coil's `EffectSigner` signs the same two-round (or sole)
  hard-ack: round-1 over everything except the unlock; round-2 over the
  unlock tx. (`PartitionEffects.unlock` is the shared selector.)
- **Aggregation** — coil's `SlowConsensusActor` aggregates the whole
  population (head + coil) the same way head's does. Threshold met (all head
  hard-acks + `coilQuorum` coil hard-acks) → write `HardConfirmation` → fan
  out.

### Peer identity — `PeerId`

Coil-authored hard-acks travel the same lanes as head ones, so every slow-side
identity slot widens from `HeadPeerNumber` to the tagged
`PeerId = Head(HeadPeerNumber) | Coil(CoilPeerNumber)`:

- `HardAckId` becomes `(PeerId, HardAckNumber)`. Wire form: peer number +
  a one-bit tag (`1` = head, `0` = coil), so the existing 2-int tuple grows by
  one bit rather than gaining a structural tag field.
- `SlowConsensusActor`'s quorum set and `HardAckAggregator`'s
  `vkeys: Map[PeerId, VerificationKey]` both key on `PeerId`.
- The verifier rebuilds each `VKeyWitness` from the `PeerId` → vkey lookup,
  so a peer can only ever contribute a witness under its own key (head or coil).

This lands on **both** peer types: head peers aggregate coil hard-acks to reach
hard confirmation, so the widening is not coil-local.

### Threshold native script

The head's native script (`HeadMultisigScript`) becomes a threshold
composition:

```
AllOf(
  headPeerVKeys.map(Signature)                        // every head peer
    :+ MOf(coilQuorum, coilPeerVKeys.map(Signature))  // any coilQuorum coils
)
```

`MOf` is Scalus's k-of-n `Timelock` constructor — `MOf(m: Int, scripts:
IndexedSeq[Timelock])`, Blockfrost JSON `atLeast`/`required` (verified against
Scalus 0.15.1 `Timelock.scala`). It governs all four uses of the script:
minting the `HYDR` / `HMRW` beacon tokens, spending the treasury and
multisig-regime outputs, and the head address. `initialization` makes the
requirement explicit — the init tx "must be signed by all head peers and a
quorum of coil peers."

With **no coil peers configured** (`coilPeers` empty) the `MOf` branch is
omitted entirely and the script is **byte-identical** to today's head-only
`AllOf(headSigs)` — same script hash, address, and policy id. Coil-free heads
are therefore unaffected, which is what makes the change safe to land ahead of
the coil node type.

`HeadMultisigScript.requiredSigners` currently flattens `AllOf → Signature` and
feeds every key to the tx builder as a required signer; that flat cast no
longer holds. The head keys stay **mandatory**; the coil contribution is a
fixed **count** of `coilQuorum` slots — not all coils, and not identity-pinned
for fee purposes.

### Fixed-count coil witnesses

Because every peer derives byte-identical tx bodies and the fee covers the
witness-set size, the witness *count* must be fixed. Every effect tx is built
and fee-sized for exactly `nHeadPeers + coilQuorum` witnesses:

- **Saturate early.** A `SlowConsensusActor` cell is saturated the instant it
  holds all head acks + *any* `coilQuorum` verified coil acks — no waiting for
  stragglers. This is exactly the whitepaper's "coil may skip" allowance: only
  *some* `coilQuorum` is ever needed.
- **Cap hard.** The aggregator attaches exactly `coilQuorum` coil witnesses and
  no more; a coil ack arriving after saturation is dropped (same path as the
  existing round-1-already-saturated drop). Attaching `coilQuorum + 1` would
  exceed the fee-budgeted size and invalidate the tx.
- **Witness sets may differ across peers.** Each peer aggregates and submits
  its *own* tx (R8/R9, §9); peer A may attach coils `{1,2}` and peer B
  `{2,3}` — same count, both satisfy the on-chain `AtLeast(coilQuorum, …)`, and
  L1 duplicate-rejection decides which lands. No cross-peer witness agreement
  is required.
- **Two-phase stacks: the chosen coil peers sign *both* rounds.** A 2-phase
  stack signs in two rounds — round 1 over every effect except the unlock,
  round 2 over the unlock/SEC tx (§6). One peer attaches a *single* fixed signer
  set (`nHeadPeers + coilQuorum`) to *all* of its effect txs, and those txs span
  both rounds, so each chosen signer must have a signature on every tx. Head
  peers sign both rounds anyway (`AllOf(head)` binds every tx); for coil peers
  this means the `coilQuorum` slots are drawn from the **intersection** of the
  round-1 and round-2 signers (`SlowConsensusActor.chooseSigners`), and the cell
  only hard-confirms once that intersection is saturated. This is the
  uniform-fixed-set choice, not an on-chain requirement — each tx is validated
  independently, so a relaxed scheme could let different coil peers satisfy the
  quorum per round, at the cost of a non-uniform witness set and more aggregation
  bookkeeping. The uniform set is preferred for deterministic tx size and simple
  aggregation; its cost is a liveness delay if coil peers churn between the two
  rounds (the cell waits for `coilQuorum` *both-round* coil peers rather than
  failing).

## 7. Fast-consensus participation — receive-only

Coil **does not** produce fast-side artifacts but **does** follow the
fast-side stream:

- Receives every block brief, head soft-ack, **and head user request** from its
  hub (the brief only lists request ids; the coil needs the request *content* to
  reproduce block bodies). Same `GetMsgBatch` / `NewMsgBatch` shape head peers use.
- Runs `BlockWeaver` follower-mode to drive `JointLedger` from observed
  `BlockBrief.Next` + the relayed requests + own `PollResults`, producing local
  `BlockResult` records — these feed `StackComposer`'s pairing. The follower
  buffers a brief that arrives ahead of the requests it needs (the hub relays
  briefs faster than requests) and replays it once the current block completes.
- Runs `FastConsensusActor` aggregator-mode to write `SoftConfirmation`
  records locally — also feed `StackComposer`'s pairing.
- Emits **nothing** on the fast side (no soft-ack, no block brief, no user
  request) — it only *follows*.

## 8. Hub topology

A coil connects to exactly **one** head peer — its hub (`coilPeers[me].hub`).
The hub fans the whole population's traffic down to the coil and fans the coil's
hard-acks back up into the head + coil population. Whitepaper anchors:
`peer-network` §Coil peer communication, §Disseminating coil hard acks.

This makes the head↔head `PeerLiaisonHeadToHead` one of **three** liaison shapes. Each is
the same `GetMsgBatch` / `NewMsgBatch` cursor protocol with a different **lane
policy** — which lanes it sends, which it accepts, and whether briefs are full or
sparse:

| Liaison | `(own, remote)` | Sends | Receives |
|---|---|---|---|
| `PeerLiaisonHeadToHead` (existing) | (head, head) | own artifacts; briefs / stack-briefs **sparse** (own-led only); own hard-acks; own `HubHardAckLane` | the remote head's artifacts + its `HubHardAckLane` |
| `PeerLiaisonHeadToCoil` (new) | (head, coil) | **everything the hub holds** — briefs / stack-briefs **full** (all leaders, relayed), all soft-acks, all head hard-acks, **all** `HubHardAckLane`s | only that coil's own hard-acks |
| `PeerLiaisonCoilToHead` (new) | (coil, head) | **only** this coil's own hard-acks | the full relayed population stream from the hub |

`(coil, coil)` does not exist — coils only ever link to their hub. The three are
selected by the `(own, remote)` kinds, so they share the cursor protocol and
differ only in lane policy (one batch-protocol core + three policies, not three
hand-rolled actors).

> **`RemotePeer` — kept (revisited Pc3/Pc4).** Pc2 added
> `RemotePeer = Head(HeadPeerId) | Coil(CoilPeerNumber)` so a liaison can name a
> head or coil remote. With the liaison shapes now implemented as three concrete
> actors over one `PeerLiaisonBatchProtocol` core, `RemotePeer` is what each one
> passes for its remote (it drives the brief-lane successor + the initial cursor),
> so it earns its place and is retained. It could still fold into a richer
> `(own, remote)` kind later, but there's no pressure to remove it now.

### Disseminating coil hard-acks — the `HubHardAckLane`

A coil's hard-ack has `author = coil`, but the head mesh's `hardAck` lane is
keyed per-author with `author == remote`. A coil ack relayed by its hub onto a
head↔head link would violate that invariant. So each head peer that hubs coils
**re-publishes** its coils' acks on a new per-head lane, the **`HubHardAckLane`**,
keyed `(head peer, sequence number) → HardAck`. It behaves exactly like the
per-peer **request lane** — contiguous, sequenced, published to every head link,
one per head peer. The main head↔head `PeerLiaisonHeadToHead` therefore just gains **N**
more request-shaped lanes (`N = nHeadPeers`, one per head, each carried inbound
on that head's link).

The lane is produced by a new actor, **`CoilAckSequencer`**, analogous to
`RequestSequencer`:

1. On receiving a coil's hard-acks (via the `PeerLiaisonHeadToCoil`), persist
   them per coil — one CF `hardAckNumber → HardAck` per coil peer (durable
   write-ahead and the idempotency guard against retransmits).
2. The sequencer assigns a hub-local sequence number to each newly-received ack
   and writes two CFs:
   - **`HubHardAckLane`**: `seqNum → HardAck` — the published, ordered stream.
   - **index**: `coilPeerId → hardAckNumber` — last-sequenced high-water per
     coil; lets the sequencer recover its position and never re-assign a seqNum
     to an ack it already sequenced.

   (The ack payload is stored twice for now — per-coil CF and `HubHardAckLane`.
   Collapsing to one payload store + two index maps is a later optimization; the
   duplication is acceptable.)

**Cross-hub dissemination falls out for free.** H1 sequences its coils into H1's
`HubHardAckLane`; the head mesh propagates it to H2 (one more inbound lane on the
H1↔H2 link); H2 relays it — plus its own — down to *its* coils. A coil thus
hears **every** coil in the head, via its hub forwarding **all** `HubHardAckLane`s.

### Relay is transport-only — verification stays end-to-end

On the hub→coil direction, briefs / soft-acks / head hard-acks / coil hard-acks
all arrive with `author ≠ remote` (authored across the population, merely relayed
by the hub). The lane carries the **full signed** artifact, never a reference —
the coil's `FastConsensusActor` / `SlowConsensusActor` verify each signature
themselves. The hub is never trusted to vouch for an artifact; it only orders and
forwards.

#### Hub→coil link lane encoding (resolved 2026-06-04)

**Driver: recovery symmetry.** A coil must persist and crash-recover with the
*same* machinery as a head (`persistence-and-crash-recovery.md`): the same column
families, the same `LaneId`s, the same recover seams. So the binding requirement
is not on the wire — it is that the coil's **internal, persisted lane structure
equals a head's**. The wire is free to multiplex however is efficient, as long as
the coil **de-multiplexes back into the head's lane structure** before anything is
stored.

A head's store has two lane shapes (`persistence/Cf.scala`, `LaneId.scala`):

- **Spines — contiguous, head-global.** `BlockSpine` / `StackSpine`: `JointLedger`
  / `StackComposer` write block 1, 2, 3, … and stack 0, 1, 2, … one after another,
  one lane each for the whole head. The leader is recorded *inside* each block, not
  in the lane key. The mesh's *sparse-per-leader brief lanes* are a wire/transport
  detail of dissemination only — the receiving `JointLedger` interleaves the N
  streams back into the contiguous spine before writing. Nothing sparse reaches
  the store.
- **Satellites — per-author.** `SoftAck` / `HardAck` / `Request`: one lane per
  author, keyed today by `HeadPeerNumber`, indexed within by that author's
  sequence number.

This maps onto the coil link directly:

- **Briefs (block + stack): relayed contiguously.** Since the spine is contiguous
  regardless of how briefs arrived, the hub hands the coil one already-interleaved
  contiguous brief stream; the coil's `JointLedger` / `StackComposer` build the
  identical contiguous spine. Cursor is plain `num.increment`; brief verify
  (number-match, no author check) is unchanged.
- **Soft-acks + hard-acks: multiplexed on the wire, de-muxed by author.** The hub
  may re-sequence them onto a relay lane (the `HubHardAckLane` shape — a sequenced
  wrapper over the signed ack, **no author check on the wire**) for cheap
  single-link transport. On receipt the coil's `FastConsensusActor` /
  `SlowConsensusActor` verify each embedded signature and **aggregate by the
  embedded author**, which *is* the de-mux into per-author satellite lanes. So the
  persisted satellite structure is per-author exactly as on a head.
- **User requests: same re-sequenced lane.** The third per-author satellite,
  `Request`, rides the same relay lane (a `RelayedMsg.Req` wrapper alongside
  `Soft` / `Hard`): the coil needs each request's content, not just the ids the
  brief carries, to reproduce block bodies. The coil de-muxes it to its
  `BlockWeaver`, which keys requests by author exactly as a head does.

**The single persistence change for coils:** widen the satellite `LaneId` key from
`HeadPeerNumber` to `PeerId` (Head | Coil), so a coil's own and other coils'
hard-acks land in `HardAck(PeerId)` lanes. That is the persistence side of Pc1's
`PeerId` tagging (already flagged as a Pc1→recovery cross-cutting concern). With
that, the entire write/recover path is shared.

Consequences:

- The hub **does not** strip a coil's own hard-acks from the relay lane (filtering
  would gap a contiguous seqNum lane); the coil's `SlowConsensusActor` **dedups the
  echo of its own ack** (`handleRemoteHardAck`, already landed). A coil emits no
  soft-acks (§7), so there is no own-soft-ack echo to dedup.
- Per-author monotonicity is preserved *within* the multiplexed stream (each
  author's acks keep their relative order); cross-author interleaving is free.

#### Implementation plan (Pc4) — status

1. **Brief relay — contiguous (DONE).** `PeerLiaisonBatchProtocol` got an overridable
   brief-lane shape; the coil liaisons select contiguous. `JointLedger` fans every
   (re)produced block brief and `StackComposer` every stack brief (own + received)
   to the hub's coil-ward liaisons.
2. **Ack + request relay — multiplexed, author-preserving (DONE).** The
   `relayedMsg` lane carries every head peer soft-ack, head peer + coil peer
   hard-ack, **and head peer user request** to the coil peer, re-sequenced by
   `CoilLinkRelay` (the sibling of `CoilAckSequencer`); its three feeding taps are
   in the "Hub feed taps" table below. The coil peer de-muxes by type + embedded
   author: `Soft`→FCA, `Hard`→SCA, `Req`→BlockWeaver. Plus the follower-BlockWeaver
   brief buffer (above).
3. **Persistence (when recovery merges):** satellite `LaneId` key `HeadPeerNumber`
   → `PeerId`; spines + recover seams otherwise unchanged.
4. **Result + the open blocker.** With the relay in place the coil is a **full
   participant** at ≥2 heads / `coilQuorum`=1: a manual stage4 run hard-confirms
   ~11 stacks on both heads *and* the coil. But the generative `Two-heads-one-coil`
   property is **not yet green** — and contrary to the earlier guess, the blocker
   is **harness settling**, not leadership pacing: the coil's extra liaison link +
   relay round-trips make the post-run `waitForIdle` drain take very long to (or
   never) reach a stable idle (a 4× drain-budget bump ran past the wall-clock
   limit). Correctness is proven; greening the property is an open stage4
   drain/settling problem (why the coil-augmented mesh won't idle — relay/resend
   timer density? continuous relay activity?), to be solved separately.

### The cost of being a hub

A hub buffers and forwards the **entire** fast-side stream to each of its coils,
not just its own artifacts (today a head outboxes only what it produces; briefs
are sparse = own-led only). The `PeerLiaisonHeadToCoil` is therefore sourced from
the hub's *received-from-mesh* state, not its own outbox, and bandwidth scales
`O(coils × stream)`. This is inherent to the hub model; we accept it.

### Head-side spawning

`MultisigRegimeManager` (a head peer), in addition to its head-peer-mesh
`PeerLiaisonHeadToHead`s, spawns one `PeerLiaisonHeadToCoil` per coil peer it hubs
(`coilPeers.filter(_.hub == ownHeadPeerNum)`) plus the two relay sequencers
`CoilAckSequencer` and `CoilLinkRelay`. The shared `Connections` separates the
head-peer mesh (`headPeerLiaisons`) from the coil-ward liaisons
(`coilPeerLiaisons`).

#### Hub feed taps

The coil-ward outbound stream is sourced by **five taps** in the hub's own
actors, in two mechanisms. Briefs ride their own dedicated contiguous lanes, so
their producers fan straight to `coilPeerLiaisons`; requests + acks are
multiplexed onto the single re-sequenced `relayedMsg` lane, so their producers
tee into `CoilLinkRelay` (which stamps the `RelayedMsgNumber` and fans the
wrapped `RelayedMsg`).

| Tap (hub actor) | Artifact | Destination |
| --- | --- | --- |
| `JointLedger` | block briefs | → `coilPeerLiaisons` (dedicated `blockBrief` lane) |
| `StackComposer` | stack briefs | → `coilPeerLiaisons` (dedicated `stackBrief` lane) |
| `BlockWeaver` | user requests (own + received) | → `CoilLinkRelay` → `relayedMsg` lane |
| `FastConsensusActor` | soft-acks (own + received) | → `CoilLinkRelay` → `relayedMsg` lane |
| `SlowConsensusActor` | hard-acks (own + received) | → `CoilLinkRelay` → `relayedMsg` lane |

The reverse direction (coil peer → head-peer mesh) is one tap:
`PeerLiaisonHeadToCoil` hands each inbound coil peer hard-ack to
`CoilAckSequencer`, which re-publishes it on the `HubHardAckLane` (§8
"Disseminating coil hard-acks"). Each producer fans **everything it sees** (own
*and* received-from-mesh), not just its own artifacts — see "The cost of being a
hub".

`CoilMultisigRegimeManager` (a coil peer) spawns exactly one
`PeerLiaisonCoilToHead`, toward `coilPeers[me].hub`. Non-hub head peers are
unaffected beyond consuming the extra `HubHardAckLane`s on their existing
head-peer↔head-peer links.

### Hub failure

Out-of-scope for M5. The whitepaper fixes one hub statically; head-side outbox
retransmission covers transient outages. A durable hub-down leaves the coil dark
until the hub recovers or the head escalates to the rule-based regime /
evacuation. **D-coil-3** flags whether even a stub fallover is M5-worth.

## 9. L1 access — independent (multisig regime)

Coil maintains its **own** Cardano backend connection — not mediated by the
hub. R8/R9 require this: coil must independently verify and submit
multisigned L1 effects without depending on the head population.
Whitepaper:

> *"R8. Independent verification: Both head peers and coil peers should be
> able to independently verify effects and submit them to the L1 network…"*
> *"R9. Resilient effect submission: Any peer (head or coil) should be able
> to independently submit verified effects to the L1 network."*

(`peer-network`).

`CardanoLiaison` runs **unchanged** on coil:

- Same `targetState` + `happyPathEffects` + `fallbackEffects` maps; same
  submission FSM head peers use.
- Same `PollResults` emission to BlockWeaver (so BlockWeaver follower-mode
  can verify deposit decisions in the leader's brief).
- Coil submits happy-path effects (init / settlement / finalization /
  rollouts) and the fallback tx the same way head peers do — R9's
  "resilient" wording is the point: any one peer drives submission and the
  others observe L1 duplicate-rejection.

Rule-based-regime L1 (vote / tally / resolve / evacuate) is covered
separately in §10.

## 10. Rule-based regime — coil

Coil, like head, operates in **both** regimes: the multisig regime (§§3–9)
and the rule-based regime. R10 *requires* coil to be able to drive the
rule-based regime end-to-end (vote, tally, resolve, evacuate) so that custody
is preserved even if every head peer colludes:

> *"R10. … every peer (head or coil) should be able to independently …
> evacuate funds."* (`peer-network`).

**`RuleBasedRegimeManager` is shared** — the same regime manager + actors
run on head and coil. No coil-specific variant is needed: rule-based
behavior is identical on both peer types.

**Transition** — coil enters the rule-based regime the same way head does:
the multisig regime submits a fallback tx (`CardanoLiaison` on any peer, per
R8/R9; §9) and when it lands on L1 the head transitions; both head and coil
peers tear down their multisig-regime actors and bring up the rule-based
ones.

**Rule-based actors (shared):**

- **`DisputeActor`** — vote, tally, resolve. Coil votes its own
  hard-confirmed evacuation commitment the same way head peers vote theirs;
  the prevailing commitment is selected by the tally.
- **`EvacuationActor`** — post-resolve `EvacuationTx`s authorized by KZG
  proof against the prevailing evacuation commitment. Whitepaper:
  *"Evacuation transactions … can be drafted and submitted by anyone,
  without coordinating with the head or coil peers."* (`cardano-as-l1`).
  So coil drafts + submits `EvacuationTx`s the same way head does.

**Evacuation is therefore not a `CardanoLiaison` concern** on either peer
type — `CardanoLiaison` is the **multisig**-regime L1 actor (happy-path +
fallback submission); the **rule-based**-regime L1 work lives in
`/rulebased/` (`DisputeActor` + `EvacuationActor`).

**The R10 floor on coil** — coil's custody guarantee holds because:
(a) coil has its own L1 connection (§9); (b) the rule-based actors are
present and unchanged from head; (c) the R10 read-set
(`HardConfirmation` + `Treasury` + `EvacuationMap`) is loaded once on
handover from local persistence — see `persistence-and-crash-recovery.md`
§5.7's recovery-priority ladder (top rung — the custody floor).

## 11. Bootstrap

The "no brief broadcast" stack-0 bootstrap
(`StackComposer.bootstrapInitialStack`) already covers coil: every peer —
head and coil — derives stack 0's init + fallback identically from the head
config, so coil's `StackComposer` runs the same `PreStart` initialization a
head peer does. Coil's first observable output is signing the round-2
init-tx hard-ack, joining the head's stack-0 hard confirmation. Whitepaper
anchor: `slow-consensus` (stack-0 hard-confirm arms stack 1).

`coilQuorum` bites at stack 0 in **both** signing rounds: round 1 (fallback)
and round 2 (init tx) each complete only when all head peers + at least
`coilQuorum` coil peers have signed (`initialization` §Signing initialization
block effects). So even a 1-head / 1-coil bring-up with `coilQuorum = 1` forces
the coil through both `HardAck.Round1Payload.Initial` and
`Round2Payload.Initial` — the thin slice is a full end-to-end exercise of coil
slow consensus, not a degenerate one. Coil's
`Round2Payload.Initial.individualSig` is always `None`: only head peers fund
the init tx from individual addresses.

## 12. Skip-hard-ack policy

A coil peer's freedom to *skip* is **scoped to its hard-ack signature
output only**. Every other piece of slow-side work is mandatory and runs on
every block / stack:

- **Block derivation runs every block.** BlockWeaver-follower drives JL
  through `StartBlock → Producing → CompleteBlock` on every received
  `BlockBrief.Next`, producing the local `BlockResult`. Skipping here would
  break deposit-map progression and starve StackComposer's pairing.
- **Stack derivation runs every stack.** StackComposer pairs every block,
  closes every stack, derives `StackEffects` deterministically — keeping
  `treasury` + `evacuationMap` cumulative and the R10 floor honest.

So coil **recreates every block + every stack** unconditionally; only the
production of its own hard-ack signature is optional.

**Two distinct skip triggers:**

1. **Availability-driven skip (unavoidable, the whitepaper case).** Coil is
   offline, its hub is silent, or local derivation is not complete by the
   stack's hard-ack deadline. Whitepaper anchor: *"coil peers may skip
   acknowledging blocks entirely since not all are available at all times"*
   (`peer-network`).
2. **Quorum-already-satisfied skip (optional optimization).** Coil observes
   that the coil quorum already has `coilQuorum` acks for this stack without
   its own signature — its ack would be redundant. Coil may deliberately
   skip rather than spend compute / bandwidth.

Either way the **persisted shape is the same**: no entry in own `HardAckLane`
for that stack, but `BlockResult` and the `Treasury` / `EvacuationMap`
snapshots advance through the skipped stack on the normal cadence —
**cumulative slow-side state is never gappy** even when own hard-acks are.

**D-coil-4** picks the concrete triggers + deadline:
- For (1), the deadline beyond which a missing input forces skip (typically
  a multiple of the slow-consensus timing window).
- For (2), whether to enable the optimization at all (saves work but loses
  liveness margin if other peers go offline mid-stack), and how to observe
  the current quorum count without polling every peer.

A `lastConsideredStackNum` watermark (or equivalent) records "I considered
stack N and chose not to ack" so coil doesn't re-decide it on restart — this
is a recovery concern picked up later in
`persistence-and-crash-recovery.md` §11.

## 13. Implementation phasing

**First PR = Pc1–Pc3** (the spine), all done. `coilQuorum` is a parameter, so the
thin slice already builds the general mechanism. Pc4 then carries it to a full
multi-head population — which turned out to be substantial (the relay lane +
`CoilLinkRelay` + the hub feed + follower brief buffering), not the "little new
code" first guessed, because a coil following >1 head needs the whole population
stream relayed and de-muxed.

| Step | Deliverable |
|---|---|
| Pc1 — **DONE** | `PeerId` tagged sum (Head / Coil) + `CoilPeerNumber` + one-bit wire tag through `HardAckId` / verifier / aggregator vkey map; threshold `HeadMultisigScript` (`AllOf(head) ∧ AtLeast(coilQuorum, coil)`) with the mandatory-head / fixed-count-coil signer split (resolves D-coil-1) |
| Pc2 — **DONE** | `CoilMultisigRegimeManager` + `OwnCoilPeerPrivate` identity seam (reuse `HeadConfig`); actor wiring with role gates on BW / JL / FCA / SC / SCA / PL (single-hub); fixed-count aggregator (saturate at `coilQuorum`, cap hard); head `MultisigRegimeManager` hub spawns coil-ward liaisons; coil-typed `RemotePeer` discriminator; `headMultisigScript` / `headMultisigAddress` moved to the coil-aware `Bootstrap.Section`, head-only base deleted (§5); `CardanoLiaison` reused unchanged; `RuleBasedRegimeManager` shared (resolves D-coil-2, D-coil-6) |
| Pc3 — **DONE** (consensus spine) | The two new liaison shapes over one `PeerLiaisonBatchProtocol` core (§8) — `PeerLiaisonHeadToCoil` + `PeerLiaisonCoilToHead`; `HubHardAckLane` + `CoilAckSequencer` were **pulled forward** to here; demonstrative 1h/1c stage4 run reaches stack hard-confirmation with `coilQuorum = 1`. **Carve-out still open:** the *app-level* "coil-side bootstrap entry point" (`Main` dispatch + a coil config-loader) — only the actor-level `CoilMultisigRegimeManager` is wired |
| Pc4 | **Multi-head relay (DONE) so a coil follows the whole population through its hub** (§8 "Hub→coil link lane encoding"): contiguous brief relay + a re-sequenced `relayedMsg` lane carrying head soft-acks, head + coil hard-acks, and head user requests, de-muxed by author at the coil (`CoilLinkRelay` + hub feed taps in JL / StackComposer / FCA / SCA / BlockWeaver), plus follower-BlockWeaver brief buffering. The coil becomes a full participant — a manual stage4 2h/coilQuorum=1 run hard-confirms ~11 stacks on both heads and the coil. OPEN: the generative `Two-heads-one-coil` property isn't green yet — blocker is **stage4 harness settling** (post-run `waitForIdle` drain never idles with the coil), not the relay. `HubHardAckLane` + `CoilAckSequencer` were pulled forward into Pc3. Recovery symmetry is the design driver; satellite `LaneId` key widens `HeadPeerNumber`→`PeerId` when `feature/recovery` merges |
| Pc5 | Stage-4 multi-peer model-based test with coil follower(s) |
| Pc6 | Skip-stack policy plumbing (resolves D-coil-4) |
| Pc7 | Coil submits happy-path + fallback alongside head (R8/R9) verified |

Persistence + crash-recovery for coil deferred to
`persistence-and-crash-recovery.md` §11.

Rule-based-regime handover for a coil (spawning `DisputeActor` + `EvacuationActor`
the same as on a head) is a **separate task**, out of this consensus phasing.

## Cross-cutting concerns (Pc1 readiness audit, 2026-05-31)

A pre-implementation audit surfaced concerns that cut across the phases above;
each must be honored even though none is owned by a single section:

- **Script-hash consistency (Pc1, load-bearing).** The threshold
  `HeadMultisigScript` must be the *same* script used to derive the stack-0
  treasury / beacon-token address at bootstrap. If any effect-tx builder
  re-derives the head address without the `MOf(coilQuorum, …)` branch, the
  script hash mismatches and stack-0 silently fails even with correct witness
  counts. Thread `coilQuorum` + coil keys into `HeadMultisigScript.apply` at
  the single derivation point.
- **`requiredSigners` downcast (Pc1).** The current
  `script.asInstanceOf[Timelock.AllOf].scripts.map(_.asInstanceOf[Signature])`
  flatten **throws** on the nested `MOf` node. Rewrite it to walk the head
  `Signature` leaves plus the `MOf` branch, emitting exactly
  `nHead + coilQuorum` `ExpectedSigner` placeholders — count-only, since Scalus
  sizes fees by witness count (dummy 32-byte vkey + 64-byte sig, added then
  removed during balancing). `checkSigners` likewise moves from set-equality to
  *all-head ∧ coil ≥ coilQuorum*. Confirm every coil-relevant effect-tx builder
  stays on `Tx.Validators.nonSigningValidators` (signature-satisfaction rules
  commented out) so placeholder coil keys don't fail the build.
- **Persistence codec shape (Pc1 → recovery).** Widening `HardAckId` to carry
  the `PeerId` tag changes the on-disk ack codec. The
  `persistence-and-crash-recovery.md` store keys for ack records must track the
  new shape; fold into the deferred coil-persistence §11.
- **`CoilAckSequencer` CFs (Pc4 → recovery).** The relay path (§8) adds three
  new column families to the deferred coil-persistence §11: the per-coil
  receive log (`hardAckNumber → HardAck`, one per coil), the published
  `HubHardAckLane` (`seqNum → HardAck`), and the sequencer index
  (`coilPeerId → hardAckNumber`). The index is the recovery key — it lets
  `CoilAckSequencer` resume without re-assigning seqNums to already-sequenced
  acks. (Payload is duplicated across the receive log and the lane for now; a
  later optimization stores it once with two index maps.)
- **Logback sync (Pc2).** Any new coil Tracer route (e.g. a
  `CoilMultisigRegimeManager` route) must be added to **all three**
  `logback.xml` configs (root main + test, integration test), per the repo
  logging rule.
- **Coil `NodeConfig` validations (Pc2).** The shared `HeadConfig` validations
  (e.g. `pollingPeriod * 5 ≤ depositMaturityDuration`) must still run for a coil
  node, but head-private validations must be skipped / replaced. Enumerate which
  `NodeConfig.apply`-time checks are head-specific when building the coil apply
  path.
