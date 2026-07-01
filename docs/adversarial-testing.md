# Adversarial Testing

Working document for Lantr onboarding developers to write adversarial tests against Hydrozoa.


## Deployment / harness scope

The specified tests are to be run using a (mostly) black-box test harness in the following sense:

- The test should be run against a multi-peer hydrozoa + sugar rush deployment setup using as-close-to-production
  configurations as is practical.
- Tests should primarily try to exploit API surface boundaries.
- Tests should simulate MITM, bad-faith actors with or without access to private keys.

Thus, we need at least three proxies capable of intercepting and mutating requests:

- **Request / tx mutation proxy**: HTTP interceptor between the client and the peer's REST API — 
  mutates request bodies, headers, and Ed25519 signatures mid-flight. 
  Additionally requires a Cardano-tx mutation harness that builds valid txs against current chain
  state and lets the tester tamper with inputs / outputs / datums / redeemers / validity ranges
  before submission.
- **Attacker-controlled peer**: WS interceptor that tester can use to mutate, delay, drop, or forge WS frames — 
  capable of equivocated against/lying to other peers (including byzantine faults). 
- **Configurable `CardanoBackend`** : Backend proxy that lies about UTxOs, tx
  submission, or timing.
- **Plutus script harness** (script-level probes only): scalus evaluator for
  negative unit tests on `DisputeResolutionValidator` / `RuleBasedTreasuryValidator`.

The general classes of vulnerabilities we are looking for include:

- Unhandled exceptions on malformed data
- Invalid state transitions
- DOS manipulation, amplification attacks, or spam vulnerabilities; including resource leaks
- Missing signature checks leading to invalid state transitions
- Consensus divergence (equivocation) without triggering fallback
- Insolvency bugs or other risks to user funds.


Per-peer observability required: L1 and L2 ground truth, consensus internals (ack
accumulation, block progress) at `FastConsensusActor` / `SlowConsensusActor`,
`RuleBasedActor.Tick` output.

## Threat model

We assume that the blue team is an honest, update-to-date node running the hydrozoa software with sanctioned 
configuration parameters.

The red team comes in three flavors:

| # | Boundary                      | Adversary posture                                                                              | Central questions                                                                                                                                                                                                |
|---|-------------------------------|------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1 | User requests (unified API)   | Unauthenticated client                                                                         | What if the request is [ forged \| malformed \| spammed ] AND such a request is [ signed \| unsigned ]?                                                                                                          |
| 2 | Peer-to-peer messages         | Authenticated but dishonest peer; MITM with malware-access to signing key                      | What if the peer [ signs \| does not sign ] a [ well \| mal ]-formed but [ honest \| dishonest ] message? Can we trigger automated signing of bad data? (equivocation, staleness, selective disclosure, silence). |
| 3 | L1 observation (Cardano node) | Third-party on-chain actor + implicit trust in local node; Plutus-level script vulnerabilities | What if a third party publishes on-chain state that fools our classifier? What if there is a plutus script vulnerability that can be exploitable with a hand-crafted transaction?                               |

---


---

## Boundary 1 — User requests (unified API)

Entry: `src/main/scala/hydrozoa/multisig/server/HydrozoaRoutes.scala`.

| Endpoint                     | Body                                                  | Auth       |
|------------------------------|-------------------------------------------------------|------------|
| `POST /api/l2/submit`        | `TransactionRequest` — Ed25519 over blake2b_256(body) | none       |
| `POST /api/deposit/register` | `DepositRequest` — Ed25519 over blake2b_256(body)     | none       |
| `POST /api/admin/finalize`   | flag flip                                             | HTTP Basic |
| `GET /api/head-info`         | —                                                     | none       |
| `GET /health`                | —                                                     | none       |

### Probes

- **Signature checks are effective.** Unsigned / wrong-key / sig-over-different-body / replayed /
  truncated sig — all must be rejected with no L2 side effect.
- **HTTP Basic on `/api/admin/finalize` enforced.** Missing header, wrong password, malformed
  base64 — must not flip the finalization flag.
- **Malformed bodies → 4xx, not 5xx.** Non-JSON, truncated JSON, missing fields, oversize
  (10 MB / 100 MB), slowloris.
- **Amplification Attack: possible DOS risk.** Each peer mints a fresh `RequestId` for the same
  body; N-peer submit → N× L2 work per peer. No body-hash dedup. Probes: submit-to-all-peers
  (confirm behavior), burst (10 k req/s against one peer, peer survives), slow-drip
  (N clients × 1 req/s × 1 h, memory / connection-pool growth bounded). 

---

## Boundary 2 — Peer messages (batch protocol over WebSocket)

Transport frames: `HeadFrame` (head ↔ head), `CoilFrame` (hub ↔ coil).

Batch protocol payloads:

- `Mesh.Get` / `Mesh.New` — head ↔ head
- `Population.Get` / `Population.New` — hub → coil
- `OwnHardAck.Get` / `OwnHardAck.New` — coil → hub

Inline artifacts carried in batches: `BlockBrief.Next`, `StackBrief`, `UserRequestWithId`,
`SoftAck`, `HardAck`, `HardAckWithId`. Sig-bearing types (`SoftAck`, `HardAck`, `HardAckWithId`)
get Ed25519-verified against the expected signer at receipt.

Test approach: run an attacker-controlled peer in a multi-node deployment; send mutated / forged
/ equivocating payloads to honest peers.

### Probes

- **Malformed frames don't crash.** Garbage bytes, truncated frames, oversize frames, unknown-tag
  CBOR, deep nesting — parse failure at transport, connection dropped, honest peer stays live.
- **Signed artifacts verified.** `SoftAck` / `HardAck` / `HardAckWithId` with missing / wrong-key
  / mis-attached / zero-valued / stale signatures — rejected, not accumulated, no crash.
- **Equivocation.** Attacker peer signs different content at the same batch cursor for different
  recipients — is the inconsistency detected downstream?
- **Cursor stall / replay.** Never advance past cursor N; resend old cursors — honest peers must
  not hang, must not buffer unboundedly.
- **Selective disclosure / silence.** Send full batch to P1, subset to P2. Or drop all outbound.
  Verify honest peers still make progress (via fallback).
- **Automated signing.** No handler currently auto-signs an L1 tx on peer-message receipt.
  Probes must confirm this holds: a forged quorum of `SoftAck`s cannot induce `StackComposer` to
  sign an unintended stack closure; a replayed `StackHandoff` cannot double-release the round-1
  ack.

---

## Boundary 3 — L1 observation (Cardano node)

Two sub-surfaces:

- **3a — Query-layer**: the peer trusts whatever its `CardanoBackend` returns. Test by pointing
  the peer at a MITM'd Blockfrost or a hostile local Cardano node.
- **3b — On-chain state**: what a third party can publish on-chain. Test by having an attacker
  publish crafted on-chain state and observing peer behavior. Plutus scripts are the on-chain
  last line of defense.

### 3a — Query-layer probes

> Note: A _genuine_ "fake" beacon token is out of scope. We assume that if the actual ground truth of the L1 
> indicates more than one beacon token, then an earlier invariant has been broken.

- **Lying about UTxOs at the treasury address**: if one or more (or all) peers a fed incorrect data by a malicious 
  blockfrost endpoint, then
  - Empty return → does the peer loop-resubmit initialization indefinitely? 
  - Fake treasury (matching beacon token, fake datum) → parse failure recoverable, not a crash? 
  - Beacon-token collision at a different address → classifier scoped to address, not just token?
  - Phantom competing UTxOs at the correct address → classifier disambiguates by beacon token, not first-hit?
- **Lying about tx submission.** 
  - False success (returned OK, tx never lands; also simulates rollback resiliency) → self-corrects on next poll? 
  - False failure (returned error, tx landed) → duplicate submit handled? 
  - Wrong error type (Timeout returned for InvalidTx) → uniform error handling?
- **Adversarial delay.** 
  - All calls hang → peer signals, silently halts, or misfires the fallback?
  - Slow drip (each call 10 s) → soft-confirmation or dispute deadlines slip?
- **Corrupted `Persistence`.** 
  - Tampered stack effects on `CardanoLiaison.State.recover` — detected or silently replayed? 
  - Corrupted `HardAck` signature bytes on disk — verify-on-load?

### 3b — On-chain-state probes

Auto-signing surface: **`RuleBasedActor.Tick`** polls treasury + dispute UTxOs, classifies them,
and signs+submits one of `fallback | vote | tally | resolve | evacuate`. This is the only actor
that auto-signs L1 txs; feeding it a misclassified UTxO set is the primary on-chain attack angle.

- **Malicious L1 UTxOs.** At treasury / dispute addresses: mis-tagged tokens, ambiguous datums,
  unexpected extra inputs on continuing txs, near-boundary validity windows, hostile rollbacks.
  Does `RuleBasedActor.Tick`'s classifier mis-classify?
- **Two competing ballot boxes for the same dispute.** Classifier must pick by matching vote
  token, not first-hit.
- **Negative Plutus tests — `DisputeResolutionValidator`**
  (`cardano-onchain/src/main/scala/hydrozoa/rulebased/ledger/l1/script/plutus/DisputeResolutionScript.scala:28`).
  Script must reject: non-monotonic `versionMinor`, wrong beacon token, `Vote` past treasury
  deadline, `Tally` that breaks linked-list preservation, `Resolve` without treasury beacon,
  reserved-phase `Vote` without SEC signature, any action without required coil quorum.
- **Negative Plutus tests — `RuleBasedTreasuryValidator`**
  (`cardano-onchain/src/main/scala/hydrozoa/rulebased/ledger/l1/script/plutus/RuleBasedTreasuryScript.scala:31`).
  Script must reject: invalid KZG membership proof, failed accumulator update, total-value
  invariant violation, `Deinit` with unevacuated entries, `Resolve` without voted ballot box,
  dropped beacon or CIP-67 prefix on continuing output.


