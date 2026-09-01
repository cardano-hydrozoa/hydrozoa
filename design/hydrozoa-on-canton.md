# Hydrozoa / Sugar Rush on Canton — Concept Mapping & PoC Framing

**Status:** internal design note, pre-PoC. Draft for circulation.

**Thesis.** Canton natively provides most of what a Hydrozoa "head" was built to provide on
Cardano — threshold-hosted parties, BFT ordering, deterministic finality — so a naive port is
mostly redundant. The *one* thing Canton cannot do is enforce that an untrusted operator ran the
L2 correctly. That gap is exactly what a **TEE-bundled L2 (Sugar Rush)** fills. So on Canton,
Hydrozoa contracts down to a **signature-gathering + event-replication kernel**, and the TEE
becomes the load-bearing differentiator rather than one of two trust levers.

**Confidence legend:** ✓ verified against primary Canton/DAML docs · ◐ our design conclusion
(sound, not externally verified) · ⚠ must verify before relying on it.

---

## 1. Base-layer concept mapping: Cardano L1 ↔ Canton

| Concept | Cardano | Canton | Notes |
|---|---|---|---|
| **Ledger model** | eUTxO: datum + validator | DAML active contract: template + signatories/observers/choices | ✓ Strong analogy. Consuming/creating UTxOs ≈ archive/create contracts. Both are "contract instances as first-class state." |
| **Authorization** | Native scripts, multisig, validator logic | Signatories (must authorize create/archive), controllers (exercise choices), observers (read) | ✓ Multi-party auth maps well. DAML authority is *contract-scoped* (signatory delegation), not tx-scoped. |
| **Global ordering** | Ouroboros — one global total order | Per-**synchronizer** sequencer: payload-blind, authenticated total-order multicast | ✓ Deepest divergence: Cardano has ONE global chain; Canton has many domains, ordering is per-synchronizer, no single global order across synchronizers. |
| **Global commitment / consensus root** | Ouroboros (open, economic) | Synchronizer = sequencer + mediator (permissioned, BFT). Commits to **per-tx root-hash + verdict in total order**, NOT a global ACS hash | ✓ "No global state" = no global *replica* + no neutral *authorizer*, but there IS a global *committer*. Single-operator sequencer can equivocate order → anchor on a BFT synchronizer. Deployment safety = which synchronizer you pick. |
| **Finality** | Probabilistic (settle over multiple blocks, ~20s+) | Deterministic — final on confirmation via mediator two-phase commit | ✓ This is why we can **drop most of Hydrozoa's timing machinery**. |
| **On-chain crypto** | Plutus BLS12-381 (Chang/CIP-381), MSM (CIP-133), targets KZG-PLONK / Halo2 | **None** of that. Only SHA-256/Keccak-256 + secp256k1 ECDSA verify (DAML 3.4, Early Access) | ✓ Confirmed asymmetry. Architectural reason: Canton has no public re-executing verifier to feed a validity proof to. |
| **Neutral enforcer** | Validator script releases funds **regardless of operator liveness** | **None** — funds move only with confirmations from the signatory's hosting participants | ✓ Structural. The single biggest consequence for custody/dispute design (see §4). |
| **State visibility** | Global, public ledger | Per-participant; sub-transaction privacy (each party sees only entitled views; sequencer sees only encrypted blobs + routing metadata) | ✓ Contract state stored only on stakeholders' participants, never globally replicated. |
| **Storage** | Append-only, immutable, global | Prunable DB; ACS held per-participant | ✓ Enables our O(1)/reclaimable space story (§3). |
| **Throughput bottleneck** | Global block-space contention (~7 TPS practical) | Hot-contract (UTxO-style) contention; scales horizontally by adding synchronizers | ✓ Live Canton is demand-limited, not ceiling-limited. Measured burst ~30 TPS on MainNet; "thousands/synchronizer" claimed but unverified. |
| **Decentralization / trust** | Anonymous BFT validators, economic security | Accountability + non-repudiation; permissioned BFT ordering (CantonBFT), threshold-hosted parties | ✓ Prevention-vs-attribution. BFT ordering is now off-the-shelf (Global Synchronizer, ~2/3 honest; adoption numbers reported but not fact-checked). |

**One-line takeaway of §1:** the ledger/authorization/finality rows *help* us (clean analogies,
free finality); the crypto and neutral-enforcer rows are what force the redesign.

---

## 2. Hydrozoa / SRL concept mapping

*Terminology reconciled to the Gummiworm spec — see §9. (Protocol = **Gummiworm**; **Hydrozoa** = the
Scala implementation. "SEC" = **Standalone Evacuation Commitment**, not "signed event certificate".)*

| Gummiworm / SRL concept (spec term) | On Cardano (Gummiworm today) | On Canton (our design) | Confidence |
|---|---|---|---|
| **Head peers** — *fast consensus*, unanimous | Produce **block briefs**, soft-confirmed by all-peer header sigs; own head equity; only entry points | Stay **off-Canton in the TEE** (fast path: SR ledger + block-brief production) | ✓ spec |
| **Coil peers** — `CoilQuorum` M-of-N, fund custodians | Hard-confirm **block-stack effects** in *slow consensus*; "wider safety net even if all head peers collude" | **≈ Canton decentralized-party `confirmationThreshold`** — the coil quorum *is* the Canton confirming-participant set | ◐ **the key mapping** |
| **Slow-consensus effect → L1** | Multisigned effects (head unanimity + coil quorum) executed on L1 | Threshold-confirmed commit of a custody DAML contract | ◐ |
| **Treasury** (all L2 funds + equity) | L1 UTxO under the head native script | Custody contract balance (native, threshold-confirmed) | ◐ Canton owns "quantity" for free |
| **Evacuation map** (who-exits-what; our "*quality*") | KZG **set accumulator** in treasury datum `evacuationActive`, updated in **minor blocks** | Merkle root + nullifier (no pairings on Canton) — recovers correctness, **loses** the accumulator's free-complement/batch proof | ◐ not "zero cost" |
| **SEC** = **Standalone Evacuation Commitment** | KZG over the evacuation map + head/coil multisigs; domain-sep by `headId` + `versionMajor` | Same role; signed bundle `(headId, versionMajor, versionMinor, root, treasury)` | ◐ our anti-replay = spec's |
| **Major block / settlement** (our "*quantity*" move) | Absorb deposits, remit payouts, supersede fallback | Healthy-regime checkpoint: rebind funds to low-threshold settlement party + sync watchtowers | ◐ |
| **Rules-based regime** (Fallback→Vote→Tally→Resolve→Evacuate→Deinit) | On-chain ballot-box; ratchet to strictly-higher-`versionMinor` SEC under pinned `versionMajor` | Same monotonic ratchet, DAML-enforced: advance-to-freshest → Resolve → permissionless Evacuate | ◐ structural match |
| **Fallback effect** (dead-man's-switch) | Deferred effect; fires on head-peer unavailability → rules-based regime | The **exit timeout we keep** (Canton finalizes on sign but ≠ operator liveness) | ✓ validates our design |
| **Permissionless Evacuate** | Anyone drains the resolved map via KZG membership proofs | **Cold watchtower** drives constrained exit at threshold-1 (Merkle proof + conservation) | ◐ |
| **Event / replication** | request → block → effect (no "event log"); consensus over blocks | Borrow the **Canton sequencer** as ordered, signed bus for SEC/checkpoint commits | ◐ |
| **Trust minimization** | KZG validity (evac) **+** TDX (order privacy, single-enclave) | **TEE-attested confirmers** turn Byzantine→omission — *extends* the spec's single-enclave TDX | ◐ the delta |
| **Throughput motive** | — | Relieve **hot-contract contention** (batch on/off-ramp); a DEX is the ideal case | ✓ load-test-confirmed |

---

## 3. The custody / settlement contract (design sketch)

*Vocabulary note: below, `MRH` is our Canton stand-in for the spec's `evacuationActive` commitment
(a KZG accumulator on Cardano, a Merkle root on Canton); "quantity/quality" is our analogy for
treasury vs. evacuation map (major- vs. minor-block updates). These are our terms, not the spec's — see §9.*

**Two regimes on one custody contract:**

- **Multisig regime (normal):** the m-of-n threshold party advances the bundle
  `(instanceID, epoch, seqNo, MRH, quantity)`, enclave-signed; acceptance monotonic on `(epoch, seqNo)`.
  Each field is anti-replay: seqNo→backward replay, instanceID→cross-instance, epoch→post-reset,
  quantity-bound-to-MRH→splice. You can't forge "newer," only replay "older" (inert under
  monotonicity). Real threat = *equivocation* (two MRHs at one rank), killed structurally by the
  enclave's monotonic counter. = Canton decentralized-party confirmation. ◐
- **Rules-based regime (dispute):** a **monotonic challenge game** — anyone posts a candidate MRH; the
  contract accepts iff (valid enclave secp256k1 signature) ∧ (seqNo > current). Ordering:
  **advance-to-freshest → lock canonical MRH → open withdrawals.** That ordering *is* the
  anti-stale-settlement guard. ◐

**Conservation guard (native DAML, enclave-independent):** custody enforces `Σ(quality) ≤ quantity`.
So even a **fully compromised enclave** can only misallocate *within* custody, never withdraw
*beyond* it. This is what makes resting all quality-validity on the TEE tolerable. ◐

**Authority split (the precise DAML mechanic):** separate *who triggers* (choice `controller` =
broad: a depositor or a public party) from *whose authority is wielded* (`custodian`, via
**signatory delegation** — a choice on a custodian-signed contract can mint custodian-signed
children with no fresh key signature). So permissionless withdrawal likely needs **no well-known
key**. ⚠ **Must verify:** does Canton honor signatory delegation for an *external* party without its
external key at exercise time? The whole permissionless-exit path hinges on this. Fallback if not: a
quarantined public `settlementAgent` co-signer, pre-authorized in the healthy regime, that can *only*
mint Merkle-proven payouts.

**Space-bounding (double-spend prevention):**
- ❌ Naive nullifier list → O(N) live + O(N²) churn. Don't.
- ✅ **Archival-as-nullifier**: represent the *unclaimed* set as (lazily-split) subtree contracts;
  "spent" = archived (absence), not a growing field. Live state = O(unclaimed), shrinks to ~0 after
  full exit; prunable. **Happy path = O(1)** (just the root). ◐
- Canton relief vs. an L1: state lives **only on stakeholders' participants** (not globally
  replicated) and is **prunable** (Canton is a DB, not an append-only log). ✓

---

## 4. Perma-lock & the watchtower backstop (the crux)

**The structural problem.** No global state ⇒ **no neutral enforcer.** On Cardano a validator script
releases funds with zero operator liveness. On Canton, moving custodian-held funds *always* requires
confirmations from the custodian's hosting participants — the very parties who've gone dark in a
dispute. Any action on a custodian-signed contract (even non-consuming) routes to them. ✓

**Irreducible tradeoff.** O(1) happy-path space (materialize-at-exit) **XOR** custodian-independent
exit (O(N) pre-provisioned claim tickets) — *not both*, because materialization touches the
custodian-signed contract. ◐

**Mitigation 1 — threshold-1 constrained exit.** Because the exit is fully constrained (Merkle vs.
the canonical enclave-signed MRH + conservation), a confirmer *cannot* produce a wrong release — so
it's safe to require only **1-of-N** for the exit path. Host `custodian` on a wide, diverse N.
Wrinkle: Canton thresholds are per-party, not per-choice → split into `custodian` (high threshold,
guards quantity/MRH updates) + `settlementAgent` (threshold-1, guards only the Merkle-constrained
exit). ⚠ verify `PartyToParticipant` threshold composition.

**Mitigation 2 — cold watchtower (synthesized neutral enforcer).** A **cold, constraint-bound,
DA-carrying party**, pre-installed at a major-block checkpoint (via topology) as a threshold-1
confirming slot in `settlementAgent`. It sits out the fast path but ingests every checkpoint (full
MRH preimage = data availability). On dispute it wakes, reads the freshest on-ledger MRH, materializes
payouts from its replica, and confirms the constrained exit — **even if all hot custodians are dark.**
◐

- **Why it needs no trust:** the enclave-signed MRH + conservation make it *incapable* of anything but
  correct release. Neutrality comes from the **constraint**, not impartiality. So run *many*
  independent watchtowers; you only need **one** awake. Perma-lock collapses to "all custodians **and**
  all watchtowers dark."
- **Residual limit = data availability only:** you can exit to the freshest MRH whose *preimage* is
  available; checkpoint cadence bounds the worst-case haircut = `(latest signed − last synced)`. This
  is the concrete form of "dispute is a DA problem, not an adjudication problem."

---

## 5. What the synchronizer commits to — journal, not balance sheet

Canton commits to a **journal, not a balance sheet.** The atomic unit of global commitment is
`(transaction-root-hash, verdict)` in a total order — *not* a hash of the ACS. There is no global
state hash by default; each participant *derives* state by folding the ordered, finalized transaction
log onto its own local ACS projection.

- **Sequencer** commits an ordered log of encrypted envelopes (payload-blind; never hashes state). ✓
- **Mediator** commits a verdict (commit/reject) for a transaction identified by its Merkle root hash. ✓
- **Finality** = the instant the mediator's positive verdict is sequenced. Deterministic, one
  round-trip, no probabilistic settling. ✓

**MRH = an application-level ACS commitment.** Canton won't hash the ACS for you — its native *ACS
commitments* are pairwise, periodic, and for divergence-detection/audit, **not** settlement (⚠ verify
granularity). But you get exactly the committed state-hash you want by holding the **MRH in a DAML
contract**: committing the `UpdateState`/`ChallengeMrh` transaction globally commits `MRH=X @ seqNo=N`,
provable from the non-repudiable sequenced verdict. You supply a 32-byte hash of your off-Canton
balance sheet; the synchronizer commits it as one journal entry. ◐

**Throughput — the synchronizer is the ceiling, but it binds on transaction *count*, not L2 activity.**
Every tx costs a sequencer slot + mediator verdict + confirmation round-trip (BFT-bounded). But:
- *Quality-only activity (DEX trades):* millions collapse into one 32-byte MRH, committed once per
  checkpoint — **O(1) on the synchronizer regardless of batch size.** L2 trade throughput is fully
  decoupled from synchronizer throughput. ◐
- *Quantity-changing activity (on/off-ramps):* needs a synchronizer commit per event/batch (real
  custody moves, threshold-confirmed). **This — not the trade rate — is the actual Canton-throughput
  consumer.** ◐

So the bottleneck binds on **checkpoint cadence + ramp rate**, never on trade volume. The cadence dial:

| Commit more often | Commit less often (bigger batches) |
|---|---|
| Fresher on-chain MRH → smaller DA haircut in dispute; snappier ramp finality | Cheaper on the synchronizer; more headroom |
| Consumes more synchronizer throughput | Staler MRH → larger worst-case haircut (quality changes uncommitted until next checkpoint) |

**Crux:** the MRH is an application-level journal entry that summarizes an entire off-Canton balance
sheet in 32 bytes — which is why the architecture is cheap exactly where it needs to be. The DEX can
run arbitrarily fast off-Canton; only ramp rate and checkpoint cadence touch the ceiling.

---

## 6. What we drop, keep, add

- **Drop:** most timing/finality machinery (Canton finalizes on sign); dependence on KZG/BLS
  primitives; building a BFT ordering layer (use CantonBFT off-the-shelf).
- **Keep:** the signature-gathering + event-replication kernel; **dispute/exit timeouts** (the only
  clock left — Canton finality ≠ operator liveness).
- **Add:** **TEE bundling of the L2 (Sugar Rush)** — the whole differentiator. Each confirmer runs the
  same attested binary, so "honest confirmer" stops being an operator assumption and becomes an
  enclave-enforced property. This is what stock Canton m-of-n *cannot* do.

---

## 7. Open questions to resolve before/into the PoC

1. ⚠ **External-party signatory delegation** without the external key at exercise time — *the*
   load-bearing assumption for permissionless exit. **Test this first.**
2. ⚠ **Per-party vs. per-choice thresholds** — can `custodian` (high) and `settlementAgent` (1) compose
   without contaminating each other's confirmation requirements?
3. ◐ **DAML engine determinism across enclaves** — bit-identical contract IDs / view ordering under
   replicated hosting (submission seed agreement).
4. ✓/❓ **Real per-synchronizer throughput** — DA benchmarks unreproduced; measure on our own workload.
5. ◐ **Enclave-as-signer** — key provisioning, attestation flow, and whether the in-enclave key signs
   `(MRH, quantity, seqNo)` as one bundle.
6. ◐ **Checkpoint cadence vs. DA haircut** — how often major blocks fire, and watchtower sync policy.
7. ◐ **Signer-key governance** — enclaveKey / signer-set changes must be threshold-authorized *and*
   epoch-bumped, so a rotated/swapped key can't retroactively validate stale or forged states.

---

## 8. Suggested minimal PoC

**Goal:** de-risk the two ⚠ assumptions and demonstrate the untrusted-operator story end to end.

1. **DAML custody contract**: fields `(custodian, quantity, mrh, seqNo, enclaveKey)`; choices
   `UpdateState` (threshold), `ChallengeMrh` (monotonic + enclave-sig verify), `ClaimPayout` (Merkle
   inclusion + conservation + delegated mint), archival-as-nullifier.
2. **Kill-the-assumption test**: exercise `ClaimPayout` with `custodian` as a multi-hosted external
   party while its threshold key is withheld — does delegation carry the mint? (Resolves OQ-1.)
3. **Toy enclave**: a stub that holds a signing key, runs a trivial SR-like state machine, and signs
   `(MRH, quantity, seqNo)`. Attestation can be mocked initially.
4. **Watchtower exit**: bring every custodian offline; have a cold watchtower wake, post the freshest
   MRH, and drive a full constrained exit. Demonstrates the synthesized neutral enforcer.

If (2) passes, the architecture stands as written. If it fails, fall back to the quarantined public
`settlementAgent` and re-test.

**Status:** step 1 is scaffolded in [`../canton-poc/`](../canton-poc/README.md) — a Daml package with
the `Custody`/`Payout` templates, a SHA-256 Merkle module, and Daml Script tests covering exit,
conservation, the double-claim nullifier, and the model-level delegation. The in-memory script service
can't produce secp256k1 signatures, so the positive signature path and OQ-1's *runtime* answer wait on
the Canton deployment (step 2).

---

## 9. Reconciliation with the Gummiworm spec

Checked against the published whitepaper + on-chain spec (`gummiwormlabs.github.io/gummiworm-writing-room`)
and the local `rbr-dispute-evacuation-attack-checklist`. Corrections (⚑) and validations (✓✓):

**⚑ Naming.** The protocol is **Gummiworm**; **Hydrozoa** is the Scala state-channels *implementation*.
`SEC` is an established spec abbreviation but expands to **Standalone Evacuation Commitment** (a KZG
hash over an evacuation map carrying head+coil multisignatures) — **not** "signed event certificate."
There is **no "snapshot"** (unit is the block / block brief / block stack) and **no "quantity/quality"**
vocabulary in the spec (those are our analogy).

**⚑ Peer structure — the correction that most improves the mapping.** There are **two** peer classes:
- **Head peers** — small, **unanimous** multisig, own the equity, only entry points, expected
  continuously available. Run **fast consensus**: leader/follower schedule producing **block briefs**,
  *soft-confirmed* by all-peer header signatures. Censorship requires *all* head peers to collude.
- **Coil peers** — large, **M-of-N quorum (`CoilQuorum`)**, fund custodians, *not* always-available,
  follower-only. Run **slow consensus** with the head: derive + *hard-confirm* the **effects** of a
  **block stack**, which are what execute on L1.

  → **`CoilQuorum` M-of-N ≈ Canton decentralized-party `confirmationThreshold`.** The natural mapping:
  **the coil network becomes the Canton confirming-participant set; head-peer fast consensus stays
  off-Canton in the TEE.** Slow-consensus hard-confirmation ≈ Canton's threshold-confirmed commit.
  Cleaner and more faithful than the single "N-peer multisig" earlier drafts assumed.

**⚑ KZG is narrower than implied, and the Canton port is NOT "zero cost."** KZG is used *specifically*
for the **evacuation map**: a vanishing-polynomial **set accumulator** (48-byte BLS12-381 G₁) in the
treasury datum field **`evacuationActive`**, updated in **minor blocks**. Its killer property — a
**membership proof for a subset doubles as a commitment over the complement** (the residual still to
evacuate), `W(S;T)=Acc(T∖S)` — gives batch evacuation as one 48-byte residual, and **binds each payout
output to the map** (a redirected payout fails membership). On Canton (no pairings) you replace this
with **Merkle + explicit nullifier/residual tracking** (our archival-as-nullifier): correctness is
recovered but you **lose the accumulator's free-complement/batch proof**, and the Merkle leaf must
commit `(recipient, amount)` to preserve the payout-binding property.

**✓✓ Rules-based regime — our dispute design converged on the real mechanism.** Spec flow:
**Fallback → Vote → Tally → Resolve → Evacuate → Deinit**. The **fallback effect** is a
**dead-man's-switch** firing on head-peer unavailability; it mints vote tokens + a beacon NFT + a ring
of ballot boxes; peers **Vote** a multisigned SEC; anyone **ratchets to a strictly-higher-`versionMinor`
SEC under the pinned `versionMajor`**; **Tally** keeps `maxVote`; **Resolve** commits the winning
`evacuationActive`; **Evacuate** is **permissionless** (anyone drains via KZG membership proofs).
Mappings, all confirmed:
- Our monotonic "freshest-commitment challenge game" = the **Vote/ratchet phase**, and our anti-replay
  bundle **matches the spec exactly**: the SEC is domain-separated by **`headId`** + **`versionMajor`**,
  monotonic on **`versionMinor`** — i.e. our `(instanceID, epoch, seqNo)` *is* `(headId, versionMajor,
  versionMinor)` (attack-checklist §Vote). Independent convergence.
- Our **cold watchtower** = **permissionless Evacuate**.
- Our "keep exit timeouts (Canton finality ≠ operator liveness)" = the **fallback effect** — the spec's
  own dead-man's-switch is exactly this timeout.
- Double-drain defense: spec uses **value-conservation + accumulator-monotonicity**; ours uses
  **conservation guard + archival-as-nullifier** — the same two invariants.

**⚑ TDX is already in Sugar Rush — but for a different purpose (scope caveat).** The spec's **Sugar
Rush TDX architecture** (Intel **TDX**, not SGX; `sugar-rush-boot`; attestation over firmware/VM/
session-key/user-data) targets **order-flow privacy + front-running resistance** today, operating
**single-enclave**. Our proposal — **TEE-attested confirmers replicated across operators, converting
Byzantine→omission** — is an **extension** (cross-operator replication + coil-TEE integration are
explicitly future work). Frame it to colleagues as *repurposing an enclave the design already has*,
not inventing the TEE from scratch.

**Where the spec is silent / to verify:** cross-operator TEE replication (future work); coil-network
detailed spec ("still needs to happen"); the attack checklist is in the repo's `docs/spec/misc/` but
**excluded from the published site** — pull it from the repo for the security appendix.

---

## 10. Component-level translation (from the Hydrozoa codebase)

Mapped from `~/hydrozoa` (Scala 3 / Scalus, ~336 files). **Disposition:** 🟢 stays off-Canton (in the
TEE / fast path) · 🔵 replaced by a Canton-native mechanism · 🟣 becomes a DAML contract/choice · ⚫ dropped.

**Headline:** the L2 engine and fast path are *preserved*; a large fraction of the codebase — the whole
coil-network + ack-sequencing + liaison-lane stack, the bespoke hard-ack aggregation, the Blockfrost
backend, the L1 tx-builders, and the KZG machinery — is *replaced or deleted* by Canton's sequencer +
mediator + Ledger API. You keep the differentiated part (DEX ledger + fast consensus) and delete the
plumbing Canton gives you for free.

### Fast consensus & L2 ledger — 🟢 stays off-Canton (this *is* the TEE payload)
| Component (path under `src/main/scala/hydrozoa/`) | Role | Canton disposition |
|---|---|---|
| `JointLedger`, `L2Ledger`/`LocalL2Ledger` | eUTXO L2 engine; block-brief production | 🟢 the enclave payload — unchanged |
| `FastConsensusActor` (FCA), `BlockWeaver`, `SoftConfirmedHighWater`, `PollResults` | soft-confirm briefs; leader/follower schedule; Ed25519 header sigs | 🟢 off-Canton fast path; Canton has/needs no equivalent |
| `Block`/`BlockHeader`/`BlockBrief`/`Stack` | consensus data; `versionMajor`/`versionMinor` | 🟢 internal — header's `(versionMajor, versionMinor)` becomes the on-Canton commitment's `(epoch, seqNo)` |
| `EvacuationMap` | who-owns-what compartment map (our "quality") | 🟢 lives in the enclave; its **Merkle root** is the on-Canton commitment |
| `RemoteL2Ledger` (WebSocket) | out-of-process ledger contract | 🟢 ◐ reuse as the host↔TEE enclave boundary |
| `HydrozoaServer`/`Routes`, `EutxoL2LedgerReader`, `PeerMetrics`, `NodeStatus` | user L2 query/submit API, health, metrics | 🟢 portable as-is; `NodeStatus.HandedOffToRuleBased` ↔ Canton dispute state |

### Coil network & peer transport — 🔵 dissolves into the synchronizer (the biggest deletion)
| Component | Role | Canton disposition |
|---|---|---|
| `CoilRelay` | fan-out briefs/acks to coil peers | 🔵 **sequencer multicast** |
| `CoilAckSequencer` (+ `HubHardAckNumber`) | stamp + total-order coil hard-acks | 🔵 **sequencer total order + mediator** |
| `HardAckAggregator`, `HardAckSignatureVerifier` | collect + verify coil-quorum hard-acks | 🔵 **mediator confirmation aggregation** (CoilQuorum = `confirmationThreshold`) |
| `PeerLiaisonHubToCoil`/`CoilToHub`, `Lane*`, `Puller`, `Server`, `*WsTransport`, `Remote*Proxy` | bespoke ordered lanes/journals over WebSocket | 🔵 **participant↔synchronizer connectivity** — deleted |
| `PeerLiaisonHeadToHead` (mesh), `Mempool` | head-peer fast-path p2p; leader request queue | 🟢 stays off-Canton (fast consensus is off-Canton) |
| `PeerId` (`Head`/`Coil`) | peer identity | 🔵 Canton party / participant IDs |

*This is the "Canton already provides the head" thesis at the code level: the coil-quorum consensus
machinery — a big slice of the 336 files — is exactly what the sequencer+mediator do natively.*

### Slow consensus & L1 effects — 🔵/🟣 become Canton commits + DAML choices
| Component | Role | Canton disposition |
|---|---|---|
| `SlowConsensusActor` (SCA) | aggregate hard-acks → hard-confirmed effects | 🔵 Canton two-phase commit (submit → confirm → mediate) |
| `StackComposer`, `StackEffectsBuilder`, `Limiter` | batch blocks → stack; derive effects; throttle | 🟢/🟣 batching/cadence stays off-Canton; effect emission → a DAML checkpoint choice |
| `SettlementTx`/`FallbackTx`/`FinalizationTx`/`RefundTx`/`RolloutTx` | L1 effect txs | 🟣 DAML choices on the custody contract |
| `EnrichedTx` (sign/addSignatures/validate, Ed25519) | manual multisig assembly | 🔵 Canton participant signing + mediator — deleted |
| `CardanoLiaison` | poll L1, decide due effects, resubmit, trigger handoff | 🔵 mostly deleted: Canton gives synchronous finality; fallback timing → a DAML time-choice |
| `CardanoBackend`/`CardanoBackendBlockfrost` (Bloxbean) | L1 query/submit | 🔵 **Canton Ledger API client** |

### On-chain (Scalus/Plutus) → 🟣 DAML
| Component | Role | Canton disposition |
|---|---|---|
| `HeadMultisigScript` (multisig treasury) | native-script custody, multisig regime | 🟣 custody DAML contract + decentralized-party topology (CoilQuorum threshold) |
| `RuleBasedTreasuryValidator` (`Resolve`/`Evacuate`/`Deinit`) | rules-based custody | 🟣 rules-based custody DAML contract (choices) |
| `DisputeResolutionValidator` (`Vote`/`Tally`/`Resolve`/`Abstain`) | ballot-box dispute | 🟣 dispute DAML contract; `BallotBox` → a `Ballot` template |
| KZG on-chain code, `StandaloneEvacuationCommitmentOnchain`, `SetupLadder` | BLS pairing membership check + SRS | ⚫/🟣 Merkle-proof verify in DAML (native SHA256); **trusted setup dropped**; batch/complement proofs lost |
| `HydrozoaBlueprint`, `ScriptReferenceUtxos`, beacon minting, `DisputeCollateralUtxo` | script hashes/addresses; ref UTxOs; beacons; script collateral | 🔵/⚫ the **DAR package** + topology; no ref UTxOs; no beacons; script collateral ⚫ (n/a on Canton) |

### Rule-based driver · crypto · persistence · bootstrap
| Component | Role | Canton disposition |
|---|---|---|
| `RuleBasedActor`, `RuleBasedRegimeManager` | drive Vote/Tally/Resolve/Evacuate | 🟢 stays as an off-Canton driver, but exercises **DAML choices** via Ledger API |
| `VoteTx`/`RatchetVoteTx`/`TallyTx`/`ResolutionTx`/`EvacuationTx`/`DeinitTx` | rules-based txs | 🟣 DAML choices |
| `KzgCommitment`, `TrustedSetup`, `Membership`, `Scalar` (BLS12-381/Blst) | evacuation proof system | ⚫ Merkle+nullifier — **drop the trusted setup**, lose O(1) residual/batch proofs |
| `Ed25519`/`VKey` signing | header/tx signing | 🔵 Canton participant keys (Ed25519/ECDSA native); aggregation → mediator |
| `Persistence`/`BackendStore`/RocksDB `Cf.*`/`Journal*`/`Markers`/`ReplayActor` | consensus + ledger durability, recovery | 🟢/🔵 **consensus/journal CFs stay** (node still persists fast-consensus state + recovery); **Ledger/Stack mirroring → the Canton participant's ACS store** |
| `Bootstrap`, `InitializationTxBuilder`, `InitializationFunding`, `HeadConfig` | keygen, init tx, funding, head config | 🔵/⚫ → Canton **topology setup + DAR upload + custody-contract create**; wallet-funding & beacon-minting ⚫; `HeadConfig` (parties/threshold/synchronizer) stays |

### Load-bearing for the PoC (build order)
1. **`RuleBasedTreasuryValidator` → custody DAML contract** (§3) — carries the ⚠ external-party delegation question (**OQ-1**). *Build first.*
2. **`HeadMultisigScript` → decentralized-party topology**, `CoilQuorum` = `confirmationThreshold` — the coil mapping; carries the per-party-vs-per-choice threshold question (**OQ-2**).
3. **`DisputeResolutionValidator` → dispute DAML contract** — Vote/Tally/Resolve/Evacuate; the monotonic-ratchet + permissionless-Evacuate validated in §9.
4. **`KzgCommitment`/`Membership` → Merkle+nullifier** — the evacuation-map port; the one place you lose something.
5. **`JointLedger` + `FastConsensusActor`** — unchanged, wrapped in the TEE; `RemoteL2Ledger`'s WebSocket already gives the enclave boundary.

**Deleted outright:** the coil transport/liaison/lane stack, `CoilAckSequencer`/`HardAck*`, `EnrichedTx`
signature assembly, most of `CardanoLiaison`, the Blockfrost backend, and KZG/`TrustedSetup`/`SetupLadder`
— all subsumed by Canton's sequencer, mediator, Ledger API, and synchronous finality.

---

## 11. DAML / Canton tech stack & operational topology

### Developer stack — language → bytecode → engine → node → synchronizer → integration
- **Daml** — functional, statically-typed contract language (templates + signatories/observers/choices);
  compiles to **Daml-LF**, the versioned, deterministic on-ledger bytecode.
- **Daml engine** — interprets Daml-LF deterministically inside each participant node (the determinism
  our replicated-hosting story leans on — OQ-3).
- **SDK / tooling** — `daml` CLI, Daml Studio (VS Code), **Daml Script** (tests + ledger init +
  automation), REPL, in-memory **Sandbox**; **Daml Triggers** for event-driven automation.
- **Integration — the Ledger API (gRPC)**: command submit, tx/event streaming, ACS queries.
  **Java/Scala bindings** (JVM — direct continuity for our Scala driver), TS/JS codegen
  (`@daml/ledger`, `@daml/react`), an HTTP JSON API wrapper, and **PQS** (Participant Query Store →
  PostgreSQL read model).
- **Runtime — Canton** (Scala/JVM, PostgreSQL-backed); **Canton Console** (Scala REPL) for admin/topology.

**For our port:** custody + dispute contracts are **Daml templates**; the SR DEX engine stays
**non-Daml, in the TEE**. The ex-`CardanoLiaison`/`RuleBasedActor` becomes a **Ledger API gRPC client
via the Java bindings** — same JVM/Scala language, swap Bloxbean/Blockfrost → Ledger API.

### Operational topology — who runs what
| Role | Does | Who runs it |
|---|---|---|
| **Participant node** | hosts parties, holds the ACS, runs the Daml engine, validates + confirms, serves the Ledger API | **us** (+ each coil operator runs their own) |
| **Sequencer** | ordered, payload-blind multicast | the **synchronizer** operator |
| **Mediator** | aggregates confirmations → verdict (finality) | the **synchronizer** operator |
| topology / identity mgr | party / participant / key topology | the synchronizer operator |

**Do we own the mediator? It depends on the synchronizer** (this is the "deployment safety = which
synchronizer you pick" point, made concrete):
- **Private / application synchronizer** (likely for the PoC): **we run the sequencer + mediator — we
  own them.** Full control of finality / ordering / censorship-resistance; no Canton Coin; no external
  dependency. Cost: a single-operator sequencer *can* equivocate the order → run it BFT (CantonBFT) or
  accept the trust for a PoC.
- **Global Synchronizer**: sequencer + mediator are run by the **Super Validator BFT network**; **we
  connect, we don't own.** Trust-minimized (~2/3 BFT) but SV-dependent, Canton-Coin-metered, and routed
  through the shared BFT bottleneck.

**Privacy nuance:** the mediator is **payload-blind** — it sees only the confirmation-tree structure +
verdicts, never contract data. So *not* owning it (Global Synchronizer) leaks nothing about our L2
state; owning it buys **liveness/ordering control, not privacy.**

**Role mapping:** coil peers → independent participant-node operators co-hosting the custody party
(`CoilQuorum` = `confirmationThreshold`) — and the **mediator does the coil-quorum ack-collection** we
deleted `CoilAckSequencer`/`HardAckAggregator` for. Head peers → a participant node to *submit*
checkpoints (fast-consensus engine in the TEE alongside). Users → external parties hosted on the head's
participant, touching Canton only at on/off-ramp.

**PoC topology:** stand up a **private synchronizer we own** (sequencer + mediator, co-located for dev)
+ a few participant nodes (head + coil) to exercise a real `CoilQuorum` threshold. Productionization
decision later: keep our own (BFT) synchronizer vs. anchor settlement to the Global Synchronizer for
external trust-minimization + interop — a **sequencer-trust tradeoff, not a privacy one.**
