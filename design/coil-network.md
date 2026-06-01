# Coil-Ready Peer (M5)

**Status:** Active M5 workstream — core decisions locked 2026-05-30 (see
**Resolved decisions** below).

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
`initialization`. These supersede the matching entries in §14.

- **D-coil-1 — peer identity is a tagged sum.**
  `PeerId = Head(HeadPeerNumber) | Coil(CoilPeerNumber)`, where
  `CoilPeerNumber` is the coil's index in the canonically-sorted `coilPeers`.
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
`CoilPeerNumber` (its index in the canonically-sorted `coilPeers`, matched by
vkey). `HeadConfig` is untouched; `CoilMultisigRegimeManager` reads the private
side to know "I am coil N" and finds its hub via the matching `coilPeers`
entry.

Everything else a coil needs it reads from the shared `HeadConfig`: the head
set, `coilQuorum`, slow-consensus timing, finalization rules. L1 access is an
independent `CardanoBackend` connection (§9); the `L2Ledger` is constructed
from the same configuration head peers use (byte-deterministic across head and
coil). This resolves **D-coil-2** toward reuse.

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

### Head-side wiring — the hub spawns liaisons toward its coils

The hub relationship is **two-sided**, and the head side does not exist yet.
`MultisigRegimeManager` today spawns one `PeerLiaison` per *head* peer
(`headPeerIds.filterNot(_ == ownHeadPeerId)`) and has no notion of coils — so a
hub head peer currently has no way to deliver fast-side traffic to its coils or
to receive their hard-acks. **Pc2 therefore also touches the head
`MultisigRegimeManager`**, not only the new coil manager: a head peer that is a
hub (`coilPeers.filter(_.hub == ownHeadPeerNum)`) must additionally spawn a
`PeerLiaison` toward each such coil, whose remote identity is coil-typed
(`PeerId.Coil`) — so the per-remote `PeerLiaison` construction widens its
`remotePeerId` to accept a coil remote. Non-hub head peers are unaffected: a
coil's hard-ack reaches them relayed through the normal head mesh, fanned out by
the hub's existing outbox.

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

**First PR = Pc1–Pc3** (the whole spine). `coilQuorum` is a parameter, so the
thin slice already builds the general mechanism; Pc4 then validates it at
higher quorum with little new production code.

| Step | Deliverable |
|---|---|
| Pc1 | `PeerId` tagged sum (Head / Coil) + `CoilPeerNumber` + one-bit wire tag through `HardAckId` / verifier / aggregator vkey map; threshold `HeadMultisigScript` (`AllOf(head) ∧ AtLeast(coilQuorum, coil)`) with the mandatory-head / fixed-count-coil signer split (resolves D-coil-1) |
| Pc2 | `CoilMultisigRegimeManager` + `OwnCoilPeerPrivate` identity seam (reuse `HeadConfig`); actor wiring with role gates on BW / JL / FCA / SC / SCA / PL (single-hub); fixed-count aggregator (saturate at `coilQuorum`, cap hard); **head `MultisigRegimeManager` hub spawns coil-ward `PeerLiaison`(s)** (§8); `CardanoLiaison` reused unchanged; `RuleBasedRegimeManager` shared (resolves D-coil-2, D-coil-6) |
| Pc3 | Coil-side bootstrap entry point + integration test: 1 head + 1 coil reach `Stack(0).HardConfirmed` with `coilQuorum = 1` |
| Pc4 | Multi-coil quorum: `coilQuorum > 1`, multiple coil peers through stage1 (validates the spine; minimal new production code) |
| Pc5 | Stage-4 multi-peer model-based test with coil follower(s) |
| Pc6 | Skip-stack policy plumbing (resolves D-coil-4) |
| Pc7 | Coil submits happy-path + fallback alongside head (R8/R9) verified; rule-based-regime handover spawns `DisputeActor` + `EvacuationActor` on coil the same as on head |

Persistence + crash-recovery for coil deferred to
`persistence-and-crash-recovery.md` §11.

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
- **Logback sync (Pc2).** Any new coil Tracer route (e.g. a
  `CoilMultisigRegimeManager` route) must be added to **all three**
  `logback.xml` configs (root main + test, integration test), per the repo
  logging rule.
- **Coil `NodeConfig` validations (Pc2).** The shared `HeadConfig` validations
  (e.g. `pollingPeriod * 5 ≤ depositMaturityDuration`) must still run for a coil
  node, but head-private validations must be skipped / replaced. Enumerate which
  `NodeConfig.apply`-time checks are head-specific when building the coil apply
  path.

## 14. Open questions

- **D-coil-1 — RESOLVED.** Tagged sum `PeerId = Head(HeadPeerNumber) |
  Coil(CoilPeerNumber)`; wire = peer number + one-bit tag (`1` head, `0`
  coil). See Resolved decisions and §6.
- **D-coil-2 — RESOLVED.** Reuse `HeadConfig` wholesale; identity differs via
  `OwnCoilPeerPrivate` at the `NodeConfig` private layer. No slim
  `CoilConfig`. See §5.
- **D-coil-3.** Hub-fallover semantics for M5 (§8) — strictly out of
  scope, or worth a stub?
- **D-coil-4.** Concrete skip-stack triggers (§12).
- **D-coil-5.** Coil's `L2Ledger` — same restore-by-block-boundary
  interface as head's, or simpler forward-only? Confirm byte-determinism
  across head/coil divide.
- **D-coil-6 — RESOLVED.** Fully parallel `CoilMultisigRegimeManager`
  (`RuleBasedRegimeManager` stays shared). See §3, §5.
