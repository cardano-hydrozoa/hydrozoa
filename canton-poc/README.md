# canton-poc — custody contract (PoC step 1)

A minimal Daml package exploring the Hydrozoa-on-Canton port. It is **step 1** of
the PoC in [`design/hydrozoa-on-canton.md`](../design/hydrozoa-on-canton.md) §8:
the custody / settlement contract of §3, built first because it carries the
load-bearing open question **OQ-1** (external-party signatory delegation, §7).

This is exploratory design code, not part of the Scala build. It compiles to
Daml-LF 2.1 with the SDK pinned in the repo flake (`daml` 3.4.11) and targets the
Canton 3.5.15 runtime — see design §11 for why compiler and runtime versions
differ.

## Layout

| File | Role |
|---|---|
| `daml/Custody.daml` | `Custody` template (two regimes) + `Payout` child |
| `daml/Merkle.daml` | SHA-256 Merkle inclusion proofs — the evacuation-map stand-in for KZG |
| `daml/CustodyTest.daml` | Daml Script tests |

## What the contract models

Both regimes of design §3 on one `Custody` template:

- **Multisig regime** (`UpdateState`, controller `custodian`): the Canton
  decentralized/threshold party advances the enclave-signed bundle
  `(instanceId, epoch, seqNo, mrh, quantity)`. Canton supplies the m-of-n
  confirmation; the choice adds monotonicity + a `secp256k1` signature check.
- **Rules-based regime** (`ChallengeMrh` → `ClaimPayout`): a monotonic ratchet to
  the freshest enclave-signed MRH, then a constrained exit. `ClaimPayout` checks
  Merkle inclusion of `(recipient, amount)`, a conservation bound
  (`amount ≤ quantity`), and a leaf-digest nullifier against double-claims, then
  mints a custodian-signed `Payout` — **without a custodian key at exercise
  time** (the delegation under test, OQ-1).

The exit is driven by a `settlementAgent` observer, mirroring the threshold-1
constrained-exit / watchtower party of §4. Its identity grants no power; the
enclave signature, monotonicity, and conservation are the only constraints.

## Build & test

From the repo root, inside the dev shell (`nix develop`):

```bash
cd canton-poc
daml build   # → .daml/dist/custody-0.1.0.dar
daml test    # runs daml/CustodyTest.daml
```

All five scripts pass: `testExitFlow` (happy path + nullifier replay +
conservation drain), `testWrongProofRejected`, `testConservation`,
`testBadSignatureRejected`, and the `setup` fixture.

## What these tests do and don't settle

The in-memory script service **cannot produce secp256k1 signatures** (Daml offers
verify, not sign), so:

- ✅ **Settled here:** Merkle inclusion binding, conservation, the double-claim
  nullifier, and the *model-level* custodian-authority delegation into `Payout`
  (the exit runs with no enclave key and mints a custodian-signed child). The
  signed-transition paths are covered on their **rejection** side (a bad
  signature is rejected on the signature, not on visibility — the settlement
  agent is an observer).
- ⏳ **Not settled here — needs the Canton deployment (PoC step 2):**
  1. **OQ-1 runtime:** does Canton honour signatory delegation for an *external,
     multi-hosted* custodian without its external key at exercise time? The Daml
     model always honours it; only a real participant topology answers this.
  2. **OQ-2:** composing a high-threshold `custodian` with a threshold-1
     `settlementAgent` via `PartyToParticipant`.
  3. The **positive** `secp256k1` path, driven by the toy enclave of §8 step 3
     signing `(instanceId, epoch, seqNo, mrh, quantity)` — note the message is
     hex-encoded (`Merkle.toHex`) before signing/verifying.

## Step 2 — live Canton (OQ-1 verified)

`canton/` runs the contract on a real single-process Canton (3.5.15): a BFT
synchronizer (sequencer + mediator) plus three participants, in-memory storage.

| File | Role |
|---|---|
| `canton/topology.conf` | synchronizer + 3 participants (ports 50xx) |
| `canton/smoke.canton` | boot + connect + cross-participant ping |
| `canton/oq-test.canton` | upload DAR, set up parties, create Custody, drive ClaimPayout |

Run headless from the repo root inside the dev shell (build the DAR first):

```bash
cd canton-poc && daml build && cd ..
canton run canton-poc/canton/oq-test.canton -c canton-poc/canton/topology.conf --no-tty </dev/null
```

**Result — OQ-1 ✓.** The `settlementAgent` (a *different* party from `custodian`)
exercises `ClaimPayout` and a **custodian-signed `Payout` is minted via signatory
delegation, with no custodian key at exercise time**. The Merkle single-leaf
proof, conservation bound, and nullifier all pass on the live Daml engine — and
the leaf hash computed in Scala matches Daml's `sha256`, confirming the hashing
convention across the boundary.

**Open — OQ-2 ⚠.** The three console `propose` calls that should promote
`custodian` to a 2-of-3 decentralized party leave the mapping at
`serial=1/threshold=1` (they don't aggregate). So today `custodian`, `agent`, and
`alice` are all hosted on participant1 and OQ-1 holds at *single-participant*
strength. Proper decentralized-party onboarding — proposals targeted at the
shared synchronizer store, or the interactive-topology flow of Canton's
`08-interactive-submission` example — is the next increment, and is also the
sharpest form of OQ-1 (custodian as an *external* party with its key withheld).

## Simplifications vs. the design

- **Nullifier as an explicit list** (`claimed : [Text]`) — O(N). Production uses
  **archival-as-nullifier** (design §3) for O(1) happy-path space.
- **Merkle over SHA-256** replaces the KZG set accumulator; it recovers
  correctness but loses the accumulator's free-complement / batch proof (§9).
- Deposits, on/off-ramp batching, and the checkpoint/major-block cadence are out
  of scope for step 1.
