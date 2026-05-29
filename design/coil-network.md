# Coil-Ready Peer (M5)

**Status:** Draft outline — **active M5 workstream**.

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
enters Leader role, `StackComposer` never closes as leader, `FastConsensusActor`
signs no `SoftAck`, no `RequestSequencer` exists, and the head-mesh
`PeerLiaison` collapses to a single hub. Same lanes, same replicated set,
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
| `JointLedger` | **follower-only** — never enters the `Producing` leader path |
| `FastConsensusActor` | aggregator-only — verifies + aggregates head soft-acks, signs none (coil has no `SoftAckLane`) |
| `StackComposer` | **follower-only** — pairs `BlockResult` × `Block.SoftConfirmed` × inbound `StackBrief` from the leader, never closes a stack as leader |
| `SlowConsensusActor` | aggregator + own hard-ack signer; aggregates head + coil hard-acks the same way |
| `PeerLiaison` | **one** — connects to the hub head peer only |
| `CardanoLiaison` | **same as head** — independent L1, verifies + submits happy-path + fallback (R8/R9) |

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
  `JointLedger` / `StackComposer`) with one PeerLiaison-to-hub. The
  rule-based regime's `RuleBasedRegimeManager` is **shared** with head — no
  coil-specific variant needed (§10).
- Coil-side bootstrap entry point (`app/` layer), parallel to head's.

**Behavioral gates on reused actors:**
- `BlockWeaver`: gate the Leader role on role; follower path (drive JL from
  `BlockBrief.Next` + `PollResults`) unchanged.
- `FastConsensusActor`: gate the own-soft-ack emission on role; aggregator
  path unchanged.
- `StackComposer`: gate the leader-close path on role; coil keeps full
  derivation (every block, every stack) but the **own-hard-ack production**
  is the only coil-only optionality (§12 skip policy).
- `PeerLiaison`: single-hub case is structurally the existing per-remote
  `PeerLiaison` with `nRemotes == 1`.

`JointLedger` and `CardanoLiaison` carry **no role gate** — JL runs the same
`Done → Producing → Done` cycle on every block whether leading or following
(Producing is the block-application state, not leader-only; leadership is
gated upstream in BlockWeaver). CardanoLiaison submits happy-path + fallback
the same way on coil as on head (§9).

The role parameter lives in the regime manager and is read by each actor at
construction; it is **not** carried in any wire message.

## 5. Configuration

The head's `HeadConfig.Bootstrap` already carries
`coilPeers: List[CoilPeer(vkey, hub: HeadPeerNumber)]` and `coilQuorum: Int` —
every head peer consumes these for slow-consensus quorum logic. A coil peer
needs its **own** config record:

- **Identity** — this coil peer's vkey (matched against an entry in
  `coilPeers`), with the derived `CoilPeerNumber` (position in
  canonical-sorted coil list).
- **Head set** — the full `headPeers` list (vkeys + network addresses), same
  view every head peer has.
- **Hub** — read from the matching `coilPeers` entry; not re-specified.
- **Head parameters** — `coilQuorum`, head quorum, slow-consensus timing,
  finalization rules — the same `HeadParameters` head peers use.
- **L1 access** — independent `CardanoBackend` connection.
- **L2Ledger** — same configuration used to construct head's L2Ledger
  (byte-deterministic across head and coil).

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

## 7. Fast-consensus participation — receive-only

Coil **does not** produce fast-side artifacts but **does** follow the
fast-side stream:

- Receives every block brief and head soft-ack from its hub (same
  `GetMsgBatch` / `NewMsgBatch` head peers use).
- Runs `BlockWeaver` follower-mode to drive `JointLedger` from observed
  `BlockBrief.Next` + own `PollResults`, producing local `BlockResult`
  records — these feed `StackComposer`'s pairing.
- Runs `FastConsensusActor` aggregator-mode to write `SoftConfirmation`
  records locally — also feed `StackComposer`'s pairing.
- Emits **nothing** on the fast side (no soft-ack, no block brief, no user
  request).

## 8. Hub topology

Coil's `PeerLiaison` connects to exactly **one** head peer — the hub
specified in `coilPeers[me].hub`. The hub fans every other peer's traffic
into coil and fans coil's own hard-acks back out to the rest of the
head + coil population. Whitepaper anchors: `peer-network` §Coil peer
communication, §Disseminating coil hard acks.

Concrete:
- **Inbound** — coil's hub-PeerLiaison receives the same five lane-types
  every head follower receives: block briefs, soft-acks, stack briefs, head
  hard-acks, other-coils' hard-acks.
- **Outbound** — coil's hub-PeerLiaison emits **only** its own `HardAck`
  entries on the reverse flow of `NewMsgBatch`.
- **Hub failure** — out-of-scope for M5. Whitepaper fixes one hub statically;
  head-side outbox retransmission covers transient outages. Durable hub-down
  → coil is dark until hub recovers or head escalates to rule-based regime /
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

| Step | Deliverable |
|---|---|
| Pc1 | `CoilPeerNumber` opaque type + wire codec disambiguation (resolves D-coil-1) |
| Pc2 | `CoilMultisigRegimeManager` + actor wiring with role gates on BW / JL / FCA / SC / SCA / PL (single-hub); `CardanoLiaison` reused unchanged; `RuleBasedRegimeManager` shared with head |
| Pc3 | Coil-side bootstrap entry point + integration test: 1 head + 1 coil reach `Stack(0).HardConfirmed` |
| Pc4 | Multi-coil quorum: `coilQuorum > 0` slow-consensus path through stage1 |
| Pc5 | Stage-4 multi-peer model-based test with coil follower(s) |
| Pc6 | Skip-stack policy plumbing (resolves D-coil-4) |
| Pc7 | Coil submits happy-path + fallback alongside head (R8/R9) verified; rule-based-regime handover spawns `DisputeActor` + `EvacuationActor` on coil the same as on head |

Persistence + crash-recovery for coil deferred to
`persistence-and-crash-recovery.md` §11.

## 14. Open questions

- **D-coil-1.** `CoilPeerNumber` representation (§5) — opaque `Int`
  mirroring `HeadPeerNumber`, or a wider tagged identifier? Needed by the
  multisig native script AND the `HardAck` wire codec to disambiguate
  head- vs coil-author IDs.
- **D-coil-2.** Slim `CoilConfig` record vs reusing `HeadConfig` and
  ignoring head-only fields (§5).
- **D-coil-3.** Hub-fallover semantics for M5 (§8) — strictly out of
  scope, or worth a stub?
- **D-coil-4.** Concrete skip-stack triggers (§12).
- **D-coil-5.** Coil's `L2Ledger` — same restore-by-block-boundary
  interface as head's, or simpler forward-only? Confirm byte-determinism
  across head/coil divide.
- **D-coil-6.** `CoilMultisigRegimeManager` shape — fully parallel to head's
  `MultisigRegimeManager`, or extract a shared `MultisigRegimeManager` base
  and parametrise on role? (`RuleBasedRegimeManager` stays shared either
  way.)
