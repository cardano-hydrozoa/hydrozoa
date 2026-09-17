# Hydrozoa / Sugar Rush on Canton

**Status:** internal design note, pre-PoC.

**Thesis.** Two layers do almost all of Hydrozoa's work:

1. **Canton** provides threshold-hosted parties, BFT ordering, and deterministic finality — so a
   head is a Canton party authorizing settlements, and the consensus/transport plumbing (coil
   network, ack-sequencing, timing machinery) is subsumed by the sequencer + mediator.
2. **CIP-0112 (the token standard)** provides holdings, allocations, atomic multi-leg settlement, and
   the on/off-ramp lifecycle as a standard — so custody, the on/off-ramp, settlement, and value
   conservation are standard primitives, consumed through a reference implementation (§8).

What remains genuinely Hydrozoa is an **off-Canton TEE L2 (Sugar Rush)** running high-frequency
activity between checkpoints, and a **checkpoint-settlement driver** that settles its net position on
Canton via the token standard. The system is: *the TEE payload + a thin settlement driver.*

**Confidence legend:** ✓ verified against primary Canton/DAML docs or our own live tests · ◐ a
design conclusion (sound, not externally verified) · ⚠ must verify before relying on it.

---

## 1. Cardano L1 ↔ Canton

The ledger/authorization/finality rows are what make the port natural; the crypto and
neutral-enforcer rows are the ones the token standard absorbs (§3, §5).

| Concept | Cardano | Canton | Notes |
|---|---|---|---|
| **Ledger model** | eUTxO: datum + validator | DAML active contract: template + signatories/observers/choices | ✓ Consuming/creating UTxOs ≈ archive/create contracts. |
| **Authorization** | Native scripts, multisig, validator logic | Signatories, controllers, observers | ✓ DAML authority is *contract-scoped* (signatory delegation), not tx-scoped. |
| **Global ordering** | Ouroboros — one global total order | Per-**synchronizer** sequencer: payload-blind authenticated total-order multicast | ✓ Deepest divergence: many synchronizers, no single global order across them. |
| **Finality** | Probabilistic (~20s+) | Deterministic — final on mediator two-phase commit | ✓ No Hydrozoa timing machinery needed. |
| **On-chain crypto** | Plutus BLS12-381, KZG/PLONK | SHA-256/Keccak + secp256k1 ECDSA only | ✓ No pairings; the pool-quality commitment uses Merkle/SHA-256 (§3). |
| **Neutral enforcer** | Validator releases funds regardless of operator liveness | **None** natively — funds move only with the signatory's participants' confirmations | ✓ The consequence for custody; the token standard's allocation-withdraw + deadlines recover most of it (§5). |
| **State visibility** | Global public ledger | Per-participant; sub-transaction privacy | ✓ Contract state lives on stakeholders' participants only. |
| **Storage** | Append-only, global | Prunable DB; ACS per-participant | ✓ |
| **Throughput** | ~7 TPS global | Hot-contract contention; scales by adding synchronizers | ✓ Binds on settlement/ramp count, not L2 trade rate (§6). |
| **Trust** | Anonymous BFT, economic | Permissioned BFT ordering (CantonBFT), threshold-hosted parties | ✓ Off-the-shelf. |

---

## 2. Concept realization

*Terminology per the Gummiworm spec (§9): protocol = **Gummiworm**; **Hydrozoa** = the Scala
implementation; **SEC** = Standalone Evacuation Commitment; head/coil peers; fast/slow consensus;
`versionMajor`/`versionMinor`.*

| Gummiworm concept | Realization |
|---|---|
| **Head peers** (fast consensus, own equity) | 🟢 The TEE payload — off-Canton |
| **Coil peers** (`CoilQuorum` M-of-N custodians) | The Canton confirming-participant set; the treasury/registry party is threshold-hosted on it (`confirmationThreshold`) |
| **Treasury** (all L2 funds + equity) | A committed, iterated pooled `Allocation` over real `HoldingV2` — the `TreasuryFlow` shape |
| **On-ramp** (deposit) | `AllocationFactory_Allocate` locking a holder's funds toward the treasury settlement |
| **Off-ramp / payout** | `SettlementFactory_SettleBatch` — atomic, registry-enforced |
| **Settlement (major block)** | A `SettleBatch` checkpoint over the L2 net position |
| **DEX cross-asset settlement** | Multi-registry atomic DvP (`CrossRegistrySwapFlow`) |
| **Conservation** (`Σquality ≤ quantity`) | Registry-enforced by the standard in `SettleBatch` |
| **Evacuation map** (who-exits-what, "quality") | The pool's internal per-user distribution, committed by an enclave-signed MRH (§3) |
| **Exit timeout / fallback** | Allocation `executeBefore`/`settlementDeadline` + `Allocation_Withdraw` (§5) |
| **Permissionless Evacuate** | Self-withdraw of unsettled allocations; pooled-fund exit is the residual case (§5) |

---

## 3. On-Canton state: quantity in contracts, quality in the pool MRH

**Quantity is concrete.** The treasury holds real `HoldingV2` contracts; deposits and withdrawals are
real `Allocation`s settled by `SettleBatch`. Canton owns the aggregate balance (the **quantity**) as
first-class contracts, and the registry enforces value conservation in every `SettleBatch` — so no
custody contract, no conservation guard, and no on-chain commitment of the balance sheet are ours to
write.

**Quality is committed, not materialized.** The treasury is a *pool*: it holds the aggregate, but who
owns which share (the **quality**) is invisible on Canton — an internal DEX trade leaves the pool
unchanged and shifts only off-Canton ownership. A pool by construction hides its quality, so the
per-user distribution stays **off-Canton, committed by a small enclave-signed MRH** and materialized
**lazily — driven by the MRH only at dispute/exit.** Steady state is cheap: checkpoints move only
deposits/withdrawals (O(deposits+withdrawals), not O(active users)); the cost is carrying that
commitment plus a DA/watchtower story scoped to the pool's quality (§5), and the compliance cost that
the pool hides its quality from the registry (§4).

The head contract (`HydrozoaTreasury`) therefore holds: a reference to the pooled treasury
`Allocation`, and an enclave-signed `(instanceId, epoch, seqNo, mrh)` ratchet over the quality —
`instanceId` (cross-head replay), `epoch` (post key-rotation), `seqNo` (backward replay), `mrh` bound
to the settlement (splice). Acceptance is monotonic on `(epoch, seqNo)`: you can forge only "older",
which monotonicity makes inert. Merkle membership proofs exist **only for exit from the pool**; the
settled deposit/withdrawal layer is concrete and proof-free.

### Finalization is atomic

A deposit is a live, registry-governed `Allocation`, not an inert UTxO. Absorbing it (`SettleBatch`)
is itself registry-adjudicated and can fail — pending, expired, blacklisted-at-settle, a race. So an
existing allocation is a *request*, not a commitment; only the settle commits.

> **Finalization invariant.** The L2 reflects a value movement (a deposit credited, a withdrawal
> paid) **iff** its Canton settlement commits. Absorption and the checkpoint advance are **one Canton
> transaction** — `Treasury_SettleMajorBlock` = the `SettleBatch`(es) for this block's
> deposit/payout legs, plus the MRH ratchet. **Never credit the L2 on the request.**

Canton makes this atomicity a single DAML transaction; on Cardano the L1 spend and the L2 commitment
cannot be bound under the registry's rules, which is why a Cardano deposit can be an inert UTxO
absorbed lazily and here it cannot.

- **Deposits are push.** The holder creates their own sender-side `AllocationFactory_Allocate`; the
  registry adjudicates it at **absorb** (§4), so the head only ever pools KYC-clear deposits. No
  head-issued `AllocationRequest` or provider-approving two-step is required — head-membership policy
  (capacity, instruments) is a separate, optional concern.
- **Withdrawals are request → authorize → settle.** The treasury holds the funds, so a withdrawer
  cannot push; they signal a request, the head authorizes the treasury's payout allocation, and it
  settles. The registry adjudicates whether the payee can receive.

### The proposer's pre-flight

Because the atomic block commits only if every absorbed leg settles, "will it settle" is decided
**in block-building**, not after agreement. A candidate major block is **proposable** iff, for its
chosen deposit/withdrawal set:

1. **Registry context assembles for every leg** — `getSettlementFactory` (and `getAllocationFactory`
   for allocations the head itself creates, e.g. payouts) returns a valid choice context +
   disclosures. A blacklisted party or a pending/expired allocation fails here, off-ledger,
   pre-submission. The RegistryApi client is a **consensus input**.
2. **Each deposit allocation is present, registry-valid, and not past `executeBefore`**; each
   withdrawal payee can receive (has any required account config / transfer pre-approval).
3. **Conservation holds** for the batch (the registry enforces it on-ledger; checking it in the
   proposal avoids a guaranteed-fail submission).
4. **The proposed MRH / L2 delta matches the on-Canton legs** — the settlement legs are exactly the
   deposits/withdrawals the checkpoint accounts for, no L2 credit without a corresponding settling
   leg and none the other way.

Peers soft-confirm a block **already known to settle**. A leg that races past the pre-flight and is
refused at settle (a blacklist landing between assembly and commit) is **deferred, not re-proposed**:
because an allocation is a durable, self-custodied object, the checkpoint advances over the legs that
did settle and the refused allocation stands for a later checkpoint or self-withdraws (§4). The
invariant holds trivially — a leg is credited only when *its* settle commits, so a deferred leg is
simply absent from this checkpoint's delta. **Granularity rule:** since a combined `SettleBatch` is
all-or-nothing, keep the absorbed set small — per-party settles under one checkpoint — so a refused
leg isolates to itself rather than failing the whole block.

---

## 4. KYC across the quantity/quality boundary

The registry enforces its KYC/sanctions policy only where it can see an owner. The quantity/quality
split (§3) draws that line: the registry sees the **quantity** layer — the pool party and every party
on a settlement leg — and is blind to the **quality** layer — who owns which share inside the pool. So
KYC is enforceable at three points and unreachable at a fourth.

**The three enforcement points** (each is where a party actually touches Canton):

1. **On-ramp, at absorb.** Deposits are bare **push** allocations — the holder creates their own
   `AllocationFactory_Allocate`, no two-step. KYC bites one step later: the absorbing `SettleBatch`
   creates a holding, and the registry refuses to create it if either end of the leg is blacklisted —
   here the depositor as **sender**. A blacklisted depositor's allocation is created freely and then
   bounces at absorb; her funds never leave her self-custodied allocation (no L2 credit, per the
   finalization invariant) and she self-withdraws. Enforcing at absorb rather than at allocate is
   **fresher** (checked at the moment of absorption) and folds the deposit-KYC gate and the
   deposit-race handling into one mechanism.
2. **On-ledger settle.** Every checkpoint leg runs through the registry's `SettlementFactory`, which
   enforces conservation and the sender/receiver eligibility of each leg. This *is* the registry's
   authorization of the checkpoint — no separate admin confirmation on `Treasury_SettleMajorBlock` is
   needed, and one would add no reach.
3. **Per-owner exit.** Dispute/exit expands the MRH into per-holder payout legs (§5), each a settle
   leg the registry adjudicates — so a blacklisted holder's own exit leg (treasury→holder) is refused
   and she cannot exit **as herself**.

**Fail-after-allocation, not fail-the-block.** The allocation and the recipient holding it settles
into are separate events, and only the second is adjudicated. So a settle refusal never destroys the
allocation — it leaves it standing, self-custodied, recoverable by withdraw. That is what makes the
race (§3) benign and deferral safe.

**The blind spot: laundering through the pool.** The one place KYC is *not* enforceable on Canton is a
blacklisted party moving value to a clean party **inside** the pool. Alice and Eve both deposit (both
clear at absorb); the registry blacklists Eve; Eve **trades** her share to Alice in the off-Canton L2
— ordinary DEX activity, the pool aggregate unchanged, only the quality shifting; Alice withdraws
clean. Every Canton leg the registry sees has clean endpoints (the withdrawal is treasury→Alice), and
Eve appears nowhere, because her ownership change never produced a Canton transaction. Holding-refusal
(points 1–3) cannot reach it — there is no blacklisted sender or receiver to refuse. Trading, not an
explicit transfer, is the channel, so it is indistinguishable from legitimate activity.

**Closing it needs the enclave.** The only component that sees the quality *while it mutates* is the
TEE, so per-holder KYC on **pooled** funds is delegated to the attested enclave, fed the registry's
blacklist. The enclave consults the current blacklist before admitting a deposit or executing any
trade touching a party, and on blacklist **freezes** that party's share — rejecting her orders and her
withdrawal — so the Eve→Alice trade never executes. A frozen share can go neither to Eve (blacklisted)
nor, since the enclave blocked the trade, to Alice; it is **quarantined** in a residual pool pending
registry resolution (seizure, or the party clearing). This stays within the trust boundary Sugar Rush
already assumes — the enclave is attested for L2 correctness — but adds two hard requirements: a
**registry → enclave blacklist feed** with a freshness SLA (the launder window is bounded by feed
latency), and **attestation coverage of the KYC-enforcement path**.

**The trilemma.** Pooling efficiency, registry-native per-holder KYC, and quality privacy cannot all
hold at once. On-Canton per-user allocations give the registry native KYC but abandon pooling
(O(users) settle legs — wrong for a DEX); a pool is efficient and private but its per-holder KYC must
be delegated to the TEE. A compliant DEX that needs pooling takes the second — so enclave-delegated
KYC is a **precondition** of the pooled model being sanctions-usable, not optional hardening.

---

## 5. Dispute & exit

Canton has no neutral enforcer — a signatory's funds move only with its participants' confirmations —
so an operator going dark is the perma-lock risk. The token standard reduces this to a corner case:

- **Unsettled deposits are self-custodied.** A depositor holds their own locked `Allocation`. If the
  operator never settles by `settlementDeadline`/`executeBefore`, the depositor exercises
  **`Allocation_Withdraw`** and reclaims — a party-controlled exit independent of operator liveness,
  with no watchtower and no Merkle proofs. ◐ (⚠ confirm the target registry honors withdraw at the
  authorizer's sole authority post-deadline.)
- **Pooled/settled funds** (swept into the treasury `Allocation`) remain operator-dependent — the
  only residual perma-lock surface. It is covered by a threshold-1 `settlementAgent` + a cold
  watchtower, scoped to the pool rather than the whole treasury. The watchtower carries the pool's
  quality MRH preimage as DA so it can materialize pool payouts at dispute.

**Dispute regime.** Because the L2 is deterministic and enclave-signed, honest operators cannot
disagree, so dispute is not vote-tallying but a monotonic "freshest enclave-signed MRH wins" ratchet:
any party may post a candidate `(seqNo, mrh)`, accepted iff the enclave signature verifies and
`seqNo` strictly increases. The ordering is advance-to-freshest → lock the canonical MRH → open pool
exits — and that ordering is itself the anti-stale-settlement guard. Once the MRH is fixed, any
single peer expands it, deterministically, into the exit legs and settles them out of the pool; the
enclave signature + monotonicity + conservation constrain the driver, so its identity grants no
power. Each exit leg is registry-adjudicated, so a blacklisted holder's own leg is refused (§4). The
residual limit is **data availability**: exit is to the freshest MRH whose preimage is available, and
checkpoint cadence bounds the recovery window.

---

## 6. Throughput

The synchronizer binds on **transaction count**, not L2 activity:

- *L2 trades between checkpoints* run entirely off-Canton in the TEE — zero synchronizer cost.
- *Checkpoints* are `SettleBatch` commits: one atomic transaction per checkpoint regardless of how
  many L2 trades it nets. `SettlementFactory_SettleBatch` is structured for net debits/credits with
  per-account privacy.
- *On/off-ramps* are `Allocate`/`SettleBatch` per event or batch — the real synchronizer consumer.

So the bottleneck is **checkpoint cadence + ramp rate**, never trade volume. The cadence dial also
sets the DA freshness of the pool-quality MRH (§3): commit more often → a shorter dispute-recovery
window, more synchronizer load.

---

## 7. What the system is

- **The TEE L2 (Sugar Rush)** — the off-Canton high-frequency engine and fast consensus. The
  differentiator, and the delegated KYC enforcer for pooled funds (§4).
- **The checkpoint-settlement driver** — an off-Canton client that takes the TEE L2's net position
  and drives token-standard `Allocate`/`SettleBatch` against a registry over the Ledger API. Thin: a
  client of the token standard, built on the reference library (§8).
- **The head contract (`HydrozoaTreasury`)** — the pool `Allocation` reference + the enclave-signed
  MRH ratchet over the pool's quality + the dispute regime (§3, §5). The only bespoke DAML.

Everything else — timing/finality, BFT ordering, custody, conservation, settlement, the coil
transport/ack-sequencing stack — is provided by Canton (sequencer + mediator + threshold parties) and
the token standard.

---

## 8. Reference implementation

**`canton-reference-registry`** (local `~/daml-scratch`, GitHub
`cardano-hydrozoa/canton-reference-registry`) is a Scala 3 implementation of CIP-0112 that provides
the substrate to build and test the settlement driver **without hand-writing token DAML and without
booting Canton**. Hydrozoa consumes it via sbt `ProjectRef` (both are sbt 2 / Scala 3.3.7 — see that
repo's `CONSUMING.md`). Modules:

| Module | Provides |
|---|---|
| `api` | The traits the driver programs against: `RegistryApi` (off-ledger registry surface), `LedgerClient` (submit + ACS reads), `Submission` (atomic multi-command). |
| `impl` | Reference impls: pure context-assembly, an http4s registry server/client, and the live-Canton adapters `LedgerClientCanton` / `AcsSourceCanton`. |
| `engine` | Fast reference tier: `EngineLedger`/`EngineRegistry` — the real Daml interpreter in-process (bundled DARs, no container). Hydrozoa flows run against it in milliseconds. |
| `testkit-it` | Live-Canton harness: boot a container, allocate parties, mint. |

Demonstrated in `hydrozoa/canton-reference-poc` (consuming the `engine` tier):

- `TreasuryFlow` — deposits into an iterated pooled allocation, settle, payout: the treasury on the
  token standard.
- `CrossRegistrySwapFlow` — atomic cross-registry DvP: the DEX cross-asset case.
- `AllocationRequestAcceptPoc` — the two-step request+accept deposit lifecycle (an alternative to
  bare push where the account config requires provider approval).

The checkpoint-settlement driver is a consumer of `RegistryApi` + `LedgerClient`, testable against
`EngineLedger` (fast) and the live-Canton tier (integration).

---

## 9. Gummiworm spec alignment

- The **evacuation map / SEC / permissionless-Evacuate** cluster maps to token-standard allocations +
  settlement + withdraw (§3, §5). The spec's `(headId, versionMajor, versionMinor)` anti-replay bundle
  is the MRH ratchet that commits the pool's quality; Merkle membership serves pool exit only.
- The spec's **conservation + accumulator-monotonicity** double-drain defense is the token standard's
  own settlement conservation.
- The **TEE extension** (attested confirmers, Byzantine→omission) is the delta beyond the spec's
  single-enclave order-privacy TDX design — and the enclave also enforces the registry's per-holder
  KYC on pooled funds (§4).

---

## 10. Component realization

🟢 off-Canton (TEE / fast path) · 🔵 Canton-native mechanism · 🟠 token-standard flow · ⚫ not present.

**Off-Canton (🟢):** `JointLedger`/`L2Ledger`, `FastConsensusActor`/`BlockWeaver`, block/brief/stack
types, `EvacuationMap` (the enclave's compartment map = the pool's quality, committed by the MRH,
§3), `RemoteL2Ledger` (host↔TEE boundary), the L2 query/submit API + health/metrics.

**Canton-native (🔵):** the coil transport / ack-sequencing / liaison-lane stack (`CoilRelay`,
`CoilAckSequencer`, `HardAck*`, `PeerLiaison*`, `Lane*`, `*WsTransport`), `EnrichedTx` signature
assembly, `CardanoLiaison`, the Blockfrost/Bloxbean backend → sequencer + mediator + Ledger API.

**Token-standard flow (🟠):**

| Concern | Realization |
|---|---|
| Treasury custody | Pooled `Allocation`; threshold party topology |
| Settlement / rollout | `SettlementFactory_SettleBatch` via `RegistryApi`/`LedgerClient` |
| Deposit / refund | `AllocationFactory_Allocate` (+ `Allocation_Withdraw` for refund) |
| Deposit KYC | Registry refuses the absorb leg for a blacklisted sender (§4) |
| Dispute / evacuation | `Allocation_Withdraw` (deposits) + the MRH-ratchet dispute regime + scoped watchtower exit (pool, §5) |
| Membership / commitment | ⚫ KZG/trusted-setup/BLS absent; ◐ Merkle membership scoped to pool exit (the MRH's quality, §3) |
| Ledger backend | `LedgerClient` (`LedgerClientCanton`, raw gRPC) |

**The checkpoint-settlement driver (🟢, the one addition):** batches L2 net positions and drives
`RegistryApi`/`LedgerClient`, built and tested against the reference `engine` tier.

**PoC build order:**

1. **Checkpoint-settlement driver over `TreasuryFlow`-shaped flows** against the `engine` tier —
   `canton-reference-poc` in spirit; wire it to the actual TEE L2 net-position output.
2. **Deposit/withdraw lifecycle** (`Allocate` + `Allocation_Withdraw`) — validate the self-custodied
   exit (§5), the load-bearing neutral-enforcer claim, and the absorb-refusal deposit-KYC gate (§4).
3. **Pool quality MRH + exit Merkle** — the enclave-signed ratchet over the pool distribution and the
   membership proofs that drive pool exit (§3, §5).
4. **Pooled-fund watchtower exit** — the residual corner from §5.
5. **Enclave KYC enforcement** — the registry→enclave blacklist feed + share-freeze/quarantine (§4).
6. **TEE bundling** — the Sugar Rush enclave around the L2 engine.

---

## 11. DAML / Canton stack & operational topology

Custody/settlement/dispute for value are the token standard's DARs, consumed via the reference
registry or a real registry; the head contract (`HydrozoaTreasury`) is the only bespoke DAML. The
settlement driver is a Ledger API gRPC client — the reference `LedgerClient` (`LedgerClientCanton`,
raw gRPC). The SR DEX engine is non-DAML, in the TEE.

**Operational topology:** coil peers → participant-node operators co-hosting the treasury/registry
party (`CoilQuorum` = `confirmationThreshold`); the mediator collects the coil-quorum acks. The
**registry admin** is a Canton party — for the reference registry it is ours; in production the
counterpart could be Amulet/Canton Coin's registry (Scan) or DA Registry, and is also the source of
the enclave's blacklist feed (§4). Head peers → a participant node to submit checkpoints, with TEE
fast-consensus alongside. Users → external parties, touching Canton only at on/off-ramp (deposit
`Allocate` / payout `SettleBatch`).

**PoC topology:** a private synchronizer we own (sequencer + mediator) + head/coil participant nodes;
or, for fast iteration, the reference `engine` tier with no synchronizer at all. Productionization
decision later: own (BFT) synchronizer vs. anchor to the Global Synchronizer.

---

## Open decisions

1. ⚠ **Allocation-withdraw semantics** — does the target registry let a depositor reclaim an
   unsettled allocation at their sole authority after the deadline? (The self-custody exit, §5 — the
   load-bearing neutral-enforcer claim.)
2. ◐ **Pooled-fund exit** — the threshold-1 `settlementAgent` + watchtower scoped to the pool: signer
   composition (per-party `confirmationThreshold`) and the DA cadence that bounds the recovery window.
3. ◐ **TEE ↔ token-standard boundary** — the enclave signs the checkpoint's net position; the
   settlement driver submits it as the atomic `Treasury_SettleMajorBlock`. The remaining
   genuinely-Hydrozoa integration piece.
4. ◐ **Settlement granularity** — one combined `SettleBatch` per major block vs. per-party settles
   under one MRH ratchet (the granularity rule, §3): the dial between throughput and per-leg deferral.
5. ⚠ **Enclave KYC feed** — the registry→enclave blacklist feed and its freshness SLA (the launder
   window is bounded by feed latency); attestation must cover the KYC-enforcement path (§4).
6. ◐ **Quarantined-share resolution** — how a frozen/blacklisted share leaves the residual pool at
   dispute: seizure to the registry, or held pending the holder clearing (§4).
7. ✓ **Reference substrate** — resolved: `canton-reference-registry` (`engine` tier for fast tests,
   live-Canton tier for integration), consumed via `ProjectRef`.
