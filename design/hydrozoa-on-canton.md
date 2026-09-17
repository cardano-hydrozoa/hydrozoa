# Hydrozoa / Sugar Rush on Canton — Gameplan (CIP-0112 edition)

**Status:** internal design note, pre-PoC. Supersedes the pre-CIP-0112 draft — the custody /
settlement / dispute layer this note used to build by hand is now the **Canton Network Token
Standard (CIP-0112)**, so the plan simplifies considerably.

**Thesis.** Two layers do almost all the work Hydrozoa was built to do:

1. **Canton** gives threshold-hosted parties, BFT ordering, and deterministic finality — so the
   consensus/transport plumbing (coil network, ack-sequencing, timing machinery) is redundant.
2. **CIP-0112 (the token standard)** gives holdings, allocations, atomic multi-leg settlement, and
   the on/off-ramp lifecycle as a *standard* — so the bespoke DAML custody contract, the MRH
   commitment, the Merkle+nullifier evacuation map, the KZG port, and most of the dispute ballot-box
   are **deleted**, replaced by standard primitives (and a reference implementation we already have —
   see §7).

What's left is the genuine differentiator §6 always named: an **off-Canton TEE L2 (Sugar Rush)**
running high-frequency activity between checkpoints, and periodic **token-standard settlement** of
its net position on Canton. Hydrozoa contracts down to *the TEE payload + a checkpoint-settlement
driver against the token standard.*

**Confidence legend:** ✓ verified against primary Canton/DAML docs or our own live tests · ◐ our
design conclusion (sound, not externally verified) · ⚠ must verify before relying on it.

---

## 1. Base-layer concept mapping: Cardano L1 ↔ Canton

(Unchanged from the prior analysis — still the foundation. The ledger/authorization/finality rows
*help* us; the crypto and neutral-enforcer rows are what used to force a redesign, and are exactly
what the token standard now absorbs.)

| Concept | Cardano | Canton | Notes |
|---|---|---|---|
| **Ledger model** | eUTxO: datum + validator | DAML active contract: template + signatories/observers/choices | ✓ Consuming/creating UTxOs ≈ archive/create contracts. |
| **Authorization** | Native scripts, multisig, validator logic | Signatories, controllers, observers | ✓ DAML authority is *contract-scoped* (signatory delegation), not tx-scoped. |
| **Global ordering** | Ouroboros — one global total order | Per-**synchronizer** sequencer: payload-blind authenticated total-order multicast | ✓ Deepest divergence: many synchronizers, no single global order across them. |
| **Finality** | Probabilistic (~20s+) | Deterministic — final on mediator two-phase commit | ✓ Lets us **drop Hydrozoa's timing machinery**. |
| **On-chain crypto** | Plutus BLS12-381, KZG/PLONK | SHA-256/Keccak + secp256k1 ECDSA only | ✓ Confirmed asymmetry — but with the token standard we no longer need pairings *or* Merkle proofs (§3). |
| **Neutral enforcer** | Validator releases funds regardless of operator liveness | **None** natively — funds move only with the signatory's participants' confirmations | ✓ Was the biggest consequence for custody. **The token standard's allocation-withdraw + deadlines now recover most of this** (§4). |
| **State visibility** | Global public ledger | Per-participant; sub-transaction privacy | ✓ Contract state on stakeholders' participants only. |
| **Storage** | Append-only, global | Prunable DB; ACS per-participant | ✓ |
| **Throughput** | ~7 TPS global | Hot-contract contention; scales by adding synchronizers | ✓ Binds on settlement/ramp count, not L2 trade rate (§5). |
| **Trust** | Anonymous BFT, economic | Permissioned BFT ordering (CantonBFT), threshold-hosted parties | ✓ Off-the-shelf. |

---

## 2. Hydrozoa / Gummiworm concepts on Canton (via the token standard)

*Terminology per the Gummiworm spec (§8): protocol = **Gummiworm**; **Hydrozoa** = the Scala
implementation; **SEC** = Standalone Evacuation Commitment; head/coil peers; fast/slow consensus;
`versionMajor`/`versionMinor`.*

The prior draft mapped each Gummiworm concept onto bespoke Canton DAML. The insight now: **almost
every one maps onto a CIP-0112 primitive instead.**

| Gummiworm concept | Prior bespoke Canton design | CIP-0112 mapping |
|---|---|---|
| **Head peers** (fast consensus, own equity) | Off-Canton in the TEE | 🟢 Unchanged — the TEE payload |
| **Coil peers** (`CoilQuorum` M-of-N custodians) | Canton decentralized-party `confirmationThreshold` | ◐ Unchanged — the confirming-participant set; the registry-admin / treasury party is hosted on it |
| **Treasury** (all L2 funds + equity) | Bespoke custody contract holding "quantity" + MRH | **Real `HoldingV2` + a committed/iterated pooled `Allocation`** — literally `TreasuryFlow` |
| **On-ramp** (deposit) | Custody `UpdateState` | **`AllocationFactory_Allocate`** — lock holdings into the treasury (optionally the two-step request+accept) |
| **Off-ramp / payout** | Merkle-proven `ClaimPayout` | **`SettlementFactory_SettleBatch`** — atomic, registry-enforced |
| **Settlement (major block, "quantity")** | MRH bundle advance | **A `SettleBatch` checkpoint** over the L2 net position |
| **DEX cross-asset settlement** | — | **Multi-registry atomic DvP** (`CrossRegistrySwapFlow`) |
| **Conservation** (`Σquality ≤ quantity`) | Native DAML guard we wrote | **Registry-enforced by the standard** — deleted from our code |
| **Evacuation map** (who-exits-what, "quality") | KZG accumulator → Merkle+nullifier | **The settled state is real allocations/holdings** — no commitment or membership proof needed for the settled layer (see §3) |
| **SEC** (evacuation commitment) | Signed `(headId, versionMajor, versionMinor, MRH, treasury)` | Reframed — see the **MRH decision, §3** |
| **Exit timeout / fallback (dead-man's-switch)** | Bespoke timeout + watchtower | **Allocation `executeBefore`/`settlementDeadline` + `Allocation_Withdraw`** — the standard's own timeouts (§4) |
| **Permissionless Evacuate** | KZG/Merkle membership drain | **Self-withdraw of unsettled allocations; pooled-fund exit is the residual hard case (§4)** |

---

## 3. On-Canton state: real allocations, not a hash — and the MRH decision

**The pivot.** The prior design committed a **32-byte MRH** over an off-Canton balance sheet and
reconstructed payouts at exit from the MRH preimage via Merkle membership proofs (with a KZG
accumulator on Cardano). With the token standard, **the on-Canton state is concrete**: the treasury
holds real `HoldingV2` contracts, deposits are real `Allocation`s, and each checkpoint settles a
real `SettleBatch`. Canton owns *quantity and quality* as first-class contracts.

Deletes, directly:
- ❌ the bespoke custody contract and its `(instanceID, epoch, seqNo, MRH, quantity)` bundle;
- ❌ the conservation guard (the registry enforces value conservation in `SettleBatch`);
- ❌ the Merkle+nullifier evacuation map and archival-as-nullifier (the settled set *is* the live
  allocation/holding contracts — Canton already prunes them);
- ❌ the KZG port, trusted setup, and BLS machinery — **not even Merkle proofs remain** for the
  settled layer;
- ❌ most of the dispute ballot-box (Vote/Tally/Resolve/Evacuate) — replaced by allocation
  lifecycle + settlement (§4).

**What still needs a commitment (the one open decision).** Between checkpoints, the TEE L2 has
uncommitted activity (DEX trades). On dispute, how much of that delta is recoverable?

- **Option A — drop the MRH; last-settled-checkpoint is the dispute floor.** On-Canton state = the
  last `SettleBatch`. A dispute unwinds to that; the uncommitted delta since the last checkpoint is
  lost (a bounded haircut = one checkpoint interval). Deletes the *most* machinery — no commitment,
  no preimage/DA story at all. Checkpoint cadence directly bounds the worst-case haircut. ◐
- **Option B — keep a small MRH for the intra-checkpoint delta only.** The token standard holds the
  settled state; a minimal enclave-signed commitment covers *only* the uncommitted L2 activity since
  the last checkpoint. Preserves zero-haircut recovery, but retains a slice of the old machinery (a
  commitment contract + the DA/watchtower story, scoped to the delta rather than the whole balance
  sheet). ◐

**This is now the central design decision of the plan** — it replaces the old OQ-1…OQ-7. Both are
laid out here rather than pre-decided. A pragmatic lean: start with **A** (simplest, and a tight
checkpoint cadence makes the haircut small), add **B** only if the residual haircut proves
unacceptable for the target workload.

---

## 4. Dispute & exit — the perma-lock problem, reduced

The prior §4 crux was: *no neutral enforcer* on Canton → funds move only with the operator's
participants' confirmations → **perma-lock** if the operator goes dark, requiring a bespoke
threshold-1 exit + cold watchtower + Merkle-constrained release. The token standard **shrinks this
to a corner case**:

- **Unsettled deposits are self-custodied.** A depositor holds their own `Allocation`, locked. If
  the operator never settles by `settlementDeadline`/`executeBefore`, the depositor exercises
  **`Allocation_Withdraw`** and reclaims — a **party-controlled exit that does not depend on operator
  liveness.** This is the neutral-enforcer gap closed by the standard, with no watchtower and no
  Merkle proofs. ◐ (⚠ confirm TestTokenV2 / the target registry actually honor withdraw at the
  authorizer's sole authority post-deadline.)
- **Pooled/settled funds** (swept into the treasury `Allocation` owned by the Hydrozoa/coil party)
  remain operator-dependent — this is the *only* residual perma-lock surface. The old §4 mitigations
  (threshold-1 `settlementAgent`, cold watchtower) may still apply here, but scoped to the pool, not
  the whole treasury — a much smaller, better-contained problem. And under Option A (§3) the
  watchtower's job is just "drive the last-agreed settlement," with no MRH preimage/DA to carry.

Net: the dispute design goes from "reimplement Gummiworm's Fallback→Vote→Tally→Resolve→Evacuate on
Canton" to "**lean on allocation-withdraw for deposits; keep a small watchtower-backed exit for the
pool.**"

---

## 5. Throughput — unchanged conclusion, cleaner cause

The synchronizer binds on **transaction count**, not L2 activity. With the token standard:
- *L2 trades between checkpoints* run entirely off-Canton in the TEE — **zero synchronizer cost**.
- *Checkpoints* are `SettleBatch` commits: one atomic transaction per checkpoint regardless of how
  many L2 trades it nets. **The batch is the standard's own primitive** (`SettlementFactory_SettleBatch`
  is explicitly structured for net debits/credits with per-account privacy — CIP-0112).
- *On/off-ramps* are `Allocate`/`SettleBatch` per event or batch — the real synchronizer consumer.

So the bottleneck binds on **checkpoint cadence + ramp rate**, never trade volume — same as before,
but now the batching is the token standard's job, not ours. The cadence dial also sets the Option-A
haircut (§3): commit more often → smaller haircut, more synchronizer load.

---

## 6. What we drop / keep / add

- **Drop:** timing/finality machinery (Canton finalizes on sign); KZG/BLS **and** the Merkle+nullifier
  port (the token standard holds real state); the bespoke custody + conservation + evacuation-map +
  ballot-box DAML; the coil transport/ack-sequencing stack (Canton's sequencer+mediator); building BFT
  ordering (CantonBFT).
- **Keep:** the off-Canton **TEE L2 (Sugar Rush)** and fast consensus; **exit/settlement deadlines**
  (now the token standard's own allocation timeouts); the small watchtower **only** for the pooled-fund
  corner (§4).
- **Add:** a **checkpoint-settlement driver** — an off-Canton component that takes the TEE L2's net
  position and drives token-standard `Allocate`/`SettleBatch` against a registry via the Ledger API.
  This is thin: it's a client of the token standard, and we already have the client library (§7).

---

## 7. The reference implementation already exists

A separate repo — **`canton-reference-registry`** (local `~/daml-scratch`, GitHub
`cardano-hydrozoa/canton-reference-registry`) — is a Scala 3 port of CIP-0112 that gives us the
substrate to build and test all of the above **without hand-writing token DAML and without booting
Canton**. Hydrozoa consumes it via sbt `ProjectRef` (both are sbt 2 / Scala 3.3.7 — see that repo's
`CONSUMING.md`). Modules:

| Module | Provides |
|---|---|
| `api` | The traits Hydrozoa's checkpoint driver programs against: `RegistryApi` (the off-ledger registry surface), `LedgerClient` (submit + ACS reads), `Submission` (atomic multi-command). |
| `impl` | Reference impls: the pure context-assembly, an http4s registry server/client, and the live-Canton adapters `LedgerClientCanton` / `AcsSourceCanton`. |
| `engine` | **Fast reference tier**: `EngineLedger`/`EngineRegistry` — the real Daml interpreter in-process (bundled DARs, no container). Run Hydrozoa flows against it in milliseconds. |
| `testkit-it` | Live-Canton harness: boot a container, allocate parties, mint. |

**Already demonstrated** (`hydrozoa/canton-reference-poc`, consuming the `engine` tier):
- `TreasuryFlow` — deposits into an iterated pooled allocation, settle, payout: **the Hydrozoa
  treasury, on the token standard.**
- `CrossRegistrySwapFlow` — atomic cross-registry DvP: **the DEX cross-asset case.**
- `AllocationRequestAcceptPoc` — the two-step **request+accept** deposit lifecycle.

So the checkpoint-settlement driver is not greenfield: it's a consumer of `RegistryApi` +
`LedgerClient`, testable against `EngineLedger` (fast) and the live-Canton tier (integration).

---

## 8. Reconciliation with the Gummiworm spec

(Retained from the prior note; the ⚑ corrections still hold — head/coil peer structure, KZG's
narrow role, the rules-based regime flow, TDX already in Sugar Rush. What changes under CIP-0112:)

- The **evacuation map / SEC / permissionless-Evacuate** cluster, which the spec builds on a KZG
  accumulator, maps on Canton to **real token-standard allocations + settlement + withdraw** (§3–§4),
  not a Merkle port. The spec's `(headId, versionMajor, versionMinor)` anti-replay bundle is retained
  *only if* we keep an intra-checkpoint MRH (Option B, §3); under Option A the "commitment" is just
  the last `SettleBatch`.
- The spec's **conservation + accumulator-monotonicity** double-drain defense becomes the token
  standard's **own settlement conservation** — we no longer implement it.
- The **TEE extension** (attested confirmers, Byzantine→omission) is unchanged and still the delta.

---

## 9. Component-level translation (updated dispositions)

🟢 stays off-Canton (TEE / fast path) · 🔵 replaced by Canton-native mechanism · 🟠 **replaced by a
CIP-0112 token-standard flow** · ⚫ dropped.

**Preserved (🟢):** `JointLedger`/`L2Ledger`, `FastConsensusActor`/`BlockWeaver`, block/brief/stack
types, `EvacuationMap` (now purely the enclave's compartment map — no on-chain commitment under
Option A), `RemoteL2Ledger` (host↔TEE boundary), the L2 query/submit API + health/metrics.

**Dissolves into Canton (🔵):** the entire coil transport / ack-sequencing / liaison-lane stack
(`CoilRelay`, `CoilAckSequencer`, `HardAck*`, `PeerLiaison*`, `Lane*`, `*WsTransport`), `EnrichedTx`
signature assembly, most of `CardanoLiaison`, the Blockfrost/Bloxbean backend → **sequencer +
mediator + Ledger API**.

**Replaced by a token-standard flow (🟠) — the new bucket, and the big simplification:**

| Component | Prior plan (bespoke DAML) | Now (CIP-0112) |
|---|---|---|
| `HeadMultisigScript` (treasury custody) | custody DAML contract + threshold party | treasury = pooled `Allocation`; party topology unchanged |
| `SettlementTx`/`FinalizationTx`/`RolloutTx` | choices on the custody contract | `SettlementFactory_SettleBatch` via `RegistryApi`/`LedgerClient` |
| deposit / `RefundTx` | custody `UpdateState` | `AllocationFactory_Allocate` (+ `Allocation_Withdraw` for refund) |
| `RuleBasedTreasuryValidator`, `DisputeResolutionValidator`, `BallotBox`, Vote/Tally/Resolve/Evacuate | rules-based custody + dispute DAML | mostly ⚫ — replaced by allocation-withdraw (deposits) + a scoped watchtower exit (pool, §4) |
| `KzgCommitment`/`TrustedSetup`/`Membership`, `StandaloneEvacuationCommitmentOnchain` | Merkle+nullifier DAML port | ⚫ **deleted entirely** — no membership proofs; settled state is real contracts |
| `CardanoBackend`/Blockfrost | Canton Ledger API client | the reference registry's `LedgerClient` (`LedgerClientCanton`) |

**The checkpoint-settlement driver (🟢 off-Canton, the one addition):** the ex-`SlowConsensusActor`
/ `StackComposer` / `CardanoLiaison` role becomes a thin client that batches L2 net positions and
drives `RegistryApi`/`LedgerClient` — built and tested against the reference `engine` tier.

**Build order for the PoC (revised):**
1. **Checkpoint-settlement driver over `TreasuryFlow`-shaped flows** against the `engine` tier — done in spirit by `canton-reference-poc`; wire it to the actual TEE L2 net-position output.
2. **Deposit/withdraw lifecycle** (`Allocate` + `Allocation_Withdraw`) — validate the self-custodied exit (§4), the load-bearing neutral-enforcer claim.
3. **Decide §3 (MRH) on measured haircut**, then either ship Option A or add the delta commitment.
4. **Pooled-fund watchtower exit** — only the residual corner from §4.
5. **TEE bundling** — the Sugar Rush enclave around the L2 engine (unchanged differentiator).

---

## 10. DAML / Canton stack & operational topology

(Unchanged from the prior note — still accurate.) Custody/dispute contracts are **no longer ours to
write**; we consume the token standard's DARs (via the reference registry or a real registry). The
ex-`CardanoLiaison`/`RuleBasedActor` becomes a **Ledger API gRPC client** — which is exactly the
reference `LedgerClient` (`LedgerClientCanton`, raw gRPC). The SR DEX engine stays **non-DAML, in the
TEE**.

**Operational topology:** coil peers → participant-node operators co-hosting the treasury/registry
party (`CoilQuorum` = `confirmationThreshold`); the mediator does the coil-quorum ack-collection.
The **registry admin** is a Canton party — for the reference registry it's ours; in production the
counterpart could be Amulet/Canton Coin's registry (Scan) or DA Registry. Head peers → a participant
node to submit checkpoints (TEE fast-consensus alongside). Users → external parties, touching Canton
only at on/off-ramp (deposit `Allocate` / payout `SettleBatch`).

**PoC topology:** unchanged — a private synchronizer we own (sequencer + mediator) + head/coil
participant nodes; or, for fast iteration, the reference `engine` tier (no synchronizer at all).
Productionization decision later: own (BFT) synchronizer vs. anchor to the Global Synchronizer.

---

## Open decisions (replacing the old OQ list)

1. **§3 — MRH or not** (Option A vs B): the central call, drives how much of the old machinery
   survives. Decide on measured intra-checkpoint haircut.
2. ⚠ **Allocation-withdraw semantics**: does the target registry let a depositor reclaim an
   unsettled allocation at their sole authority after the deadline? (The self-custody exit, §4.)
3. ◐ **Pooled-fund exit**: threshold-1 `settlementAgent` + watchtower scoped to the pool — still
   needed, or does a short checkpoint cadence + Option A make it moot?
4. ◐ **TEE ↔ token-standard boundary**: does the enclave sign the checkpoint's net position, and does
   the settlement driver submit it as one `SettleBatch`? (The remaining genuinely-Hydrozoa piece.)
5. ✓ **Reference substrate**: resolved — `canton-reference-registry` (`engine` tier for fast tests,
   live-Canton tier for integration), consumed via `ProjectRef`.
