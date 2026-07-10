# L2 ledger backends, isomorphism & deployment — design note

Status: the working design note for the **L2-isomorphism refactoring** (branch `feature/isomorphic-l2`)
— a living document; expect churn as the work lands. Scope spans isomorphism, the two selectable L2
backends, screening/submission, deployment, and GUM-104; kept in one file for now.

This note now covers four intertwined threads:

1. **Isomorphism** — make the built-in **EUTXO** ledger drivable by standard, unmodified Cardano txs.
2. **Two selectable L2 backends** — the built-in EUTXO ledger and a **remote** ledger (SugarRush),
   chosen per head — including how each is **configured at initialization** (GUM-104, §6.1).
3. **Two-phase checks** (screening → submission) — the model that *replaces* the earlier
   "introspector" idea.
4. **Deployment** — run a fully-isomorphic L2 head with one command (container composition).

**EUTXO is the target and the main motivator for isomorphism.** The whole point of L1-isomorphism is
to drive the EUTXO ledger with standard, unmodified Cardano txs — no CIP-8 envelope, no extra request
fields. It is underused today precisely because of that friction. For the remote SugarRush ledger the
mechanical pack/unpack refactor still applies (its payload is not a native tx), but the isomorphism
semantics below are EUTXO-only.

## 1. Isomorphism goal (EUTXO)

Make the EUTXO L2 ledger *isomorphic* to L1, at two strengths:

1. **Format isomorphism (always):** the head speaks the native Cardano tx format, so you can build
   L2 transactions with any standard Cardano library. The tx may still carry a head-pin annotation.
2. **Identity isomorphism (opt-in, off by default):** the *exact* transaction an application already
   uses on L1 runs on L2 unchanged — no head-specific tailoring. Depositing is separate — it's the
   prerequisite for working on L2, not an L2 tx.

## 2. Why identity isomorphism is optional: cross-head replay

A MITM can forward your L2 tx to another Gummiworm head (or to L1) hoping it validates there. By
default Gummiworm therefore requires each L2 tx to carry **metadata that pins it to this head**
(`headId`). The pin is authenticated for free: metadata is covered by the tx's `auxiliaryDataHash`,
which is in the signed body, so it can't be stripped or swapped without breaking the witness.

That pin is compatible with **format isomorphism** (it's just annotation on an otherwise standard tx)
but not with **identity isomorphism** — a vanilla L1 app tx carries no such pin. So identity
isomorphism means dropping the pin, which reopens cross-head replay; that is exactly why it is opt-in
and off by default. (L1 replay is *already* mostly prevented structurally — an L2 tx spends L2 utxo
references that don't exist on L1 — so the pin mainly defends **cross-head** replay and adds defense
in depth.)

## 3. What a request carries today (from the code)

`UserRequest` = `header` + `body` + `userVk`, and the **body already carries the payload fields** —
the payload split is NOT what's new:

- `DepositRequestBody(l1Payload, l2Payload)` — `l1Payload` is the cbor-encoded (unsigned) deposit tx;
  `l2Payload` is an opaque byte array passed unmodified to the L2.
- `TransactionRequestBody(l2Payload)` — just the opaque `l2Payload`.

The header:

- **`UserRequestHeader` = `{ headId, validityStart, validityEnd, bodyHash }`. That's all of it.**
  - `headId` — blake2b_224 of the seed utxo ++ CIP-67 `HYDR` prefix (= the treasury-token asset name).
  - `validityStart` / `validityEnd` — epoch seconds; the block-creation start must fall in the window.
  - `bodyHash` — blake2b_256 of the body; binds header↔body (checked at decode).

The deposit's `l1Payload` also (in the target design) carries a hash of `l2Payload`, binding the L1
deposit to its intended L2 effect on-chain. (Today the binding is at the request level: `bodyHash`
covers both payloads — `blake2b_256(hash(l1Payload) ++ hash(l2Payload))` — and the `signature` covers
`bodyHash`.)

### 3.1 `userVk` — auth-only on the EUTXO path

`userVk` is the user's `VerificationKey`, a **sibling of `header`/`body`, not part of the header**. On
the EUTXO path it is **auth-only** (verified against code): it verifies the header `signature` at
parse (`signature` verified-then-dropped; the parsed request keeps `userVk`). `JointLedger` threads it
into `RegisterDeposit.userVKey` / `ApplyTransaction.userVKey` and it rides the wire, **but the
built-in EUTXO ledger ignores it in both `applyMutation` branches** — deposits read recipients from
the `GenesisObligation` addresses in `l2Payload`; tx application (`HydrozoaTransactionMutator.transit`)
takes `config`/`time`/`activeUtxos`/`l2Tx`, never `userVKey`. It's kept on the wire only because the
remote ledger may consume it. **Consequence:** native-tx witnesses subsume auth, so `userVk` can drop
out of the EUTXO request entirely.

### 3.2 Key observation: every check on these fields is ledger-specific

Every check Hydrozoa runs on the header — that the block-creation time falls within `[validityStart,
validityEnd]`, that the `signature` verifies against `userVk`, that outputs are well-formed, that
`bodyHash` matches the body, **and that `headId` matches this head** — is an assumption borrowed from
one ledger's semantics. It is easy to imagine a ledger that doesn't care whether the block-creation
time falls in a validity range — or that has no validity range at all. Baking these checks into
Hydrozoa hardwires the EUTXO ledger's notion of validity into the request layer.

Even the `headId` match is the ledger's. The ledger is **configured with this head's `headId` from day
one**, so it can check the pin itself — and *whether* it does is the ledger's call (that toggle is
identity isomorphism, §5.2). Gummiworm no longer extracts or matches the `headId`.

**This is the taking-off point for the change:** the header and its (ledger-specific) checks don't
belong in Hydrozoa; they belong to the ledger. That is what §4 delegates — Hydrozoa stops interpreting
these fields entirely and asks the ledger to screen (stateless) and submit (stateful).

## 4. Two-phase checks (replaces the earlier "introspector" idea)

The radical reframe: don't have Gummiworm *extract data from the tx and run checks itself* (the old
"deep inspector"). Instead **ask the ledger to run the checks**, passing whatever reference
information Gummiworm holds. There is no introspection on the Gummiworm side, so the old
"placement A vs B" question is gone. Checks run in two phases:

### 4.1 Screening — stateless, pre-request-id

Only what's checkable from the request's payload alone, with no ledger state:

- **Transaction (EUTXO):** validate signatures, some output checks.
- **Deposit:** check the `l1Payload`↔`l2Payload` hash binding and that the deposit tx is well-formed.
- **`headId` pin:** the ledger knows this head's `headId` (configured from day one), so it checks the
  pin here itself — *if* it's set to (that's the identity-isomorphism toggle, §5.2).

It *cannot* check anything that needs state: the current time (validity range), the utxo set (resolve
inputs / balance), or — for a deposit — that the L1 funds exist.

**Screening returns a plain `No | Yes`.** The ledger owns the `headId` check now, so nothing about the
head identity is handed back to Gummiworm. Passing screening **assigns the `RequestId`** and admits
the request — the gate before Gummiworm spends any resources on it. (So `RequestId` is assigned here,
not carried in the request.) For a **remote** ledger, screening is an endpoint on `remoteLedgerUri`
that takes the `l2Payload` and returns yes/no.

### 4.2 Submission — stateful

The full checks: the tx/deposit is valid-or-not **against a particular ledger state** — where validity
range and balance/input resolution happen.

Gummiworm passes **reference data** to the ledger alongside the `l2Payload`, so the ledger can run the
checks Gummiworm did before this change. For now that's a single **timestamp** — the current
block-creation time — which the ledger needs to test the validity range (it has no clock of its own).
The set is deliberately extensible: more reference fields get added as further Gummiworm-side checks
move to the ledger.

After Gummiworm-side validation, Gummiworm signals the final valid/invalid verdict back to the ledger.

## 5. Deriving what's needed from a native tx — EUTXO only

For the **EUTXO** ledger the `l2Payload` IS a Cardano tx, so the checks read everything from it:

| needed | source in the native tx |
|---|---|
| `headId` | tx metadata (auxiliary data) — the ledger checks it against its own head (default; §5.2) |
| `validityStart` / `End` | tx validity interval (slots → L2 time — needs a convention) |
| auth | the tx's own vkey witnesses — native tx signing, no COSE envelope (a screening check) |
| `bodyHash` | internal/derived; the witness signs the whole tx incl. payload, so the header→body→sig chain collapses into native signing |
| `userVk` | **not needed by the EUTXO ledger** (auth-only; see above) — extract a distinguished `userVk` only if the remote ledger needs one |

### 5.1 The semantic changes are EUTXO-only; the check-delegation applies to both

(a) auth moving from a COSE envelope into the tx's own witnesses, and (b) validity moving from explicit
fields to the tx's slot interval (forcing a slot↔L2-time mapping the explicit fields were hiding) —
**both are EUTXO-only.** SugarRush's `l2Payload` is not a native Cardano tx, so it keeps its own auth
*semantics* (a `signature` over the header, verified against `userVk`) and explicit validity fields —
no witness-based auth, no slot-interval validity there.

But SugarRush is **not** unchanged: the §4 **check-delegation** applies to both backends. The checks
Gummiworm used to run — signature, validity range, output structure, `bodyHash`, **and the `headId`
match** — now run inside the ledger's **screening / submission endpoints**, so SugarRush must
implement them (with its own semantics). Gummiworm keeps no request checks of its own. The *semantics*
stay SugarRush's own; the *responsibility* for running the checks moves to it.

### 5.2 The lever: identity isomorphism is the ledger's headId toggle

Identity isomorphism and cross-head replay protection are the **same switch** — whether the ledger
enforces the `headId` pin (§4.1). Since the ledger holds this head's `headId` from day one, the toggle
lives **in the ledger** (a ledger setting), not in Gummiworm:

- **Enforce (default, pinned):** the ledger requires the payload to carry this head's `headId` →
  replay-safe, but the tx carries Gummiworm metadata, so it's **format isomorphism + an annotation** —
  a notch below identity.
- **Don't enforce:** the ledger ignores the pin, so the tx needn't carry one → it can be
  byte-identical to a vanilla L1 app tx (true **identity isomorphism**) → but loses cross-head
  protection.

Either way, screening's return stays a plain `No | Yes` — the head identity never leaves the ledger.

So identity isomorphism *is* the footgun. Default the ledger to enforce; make "don't enforce" an
explicit, loud ledger setting. It is only safe when the app already bakes a headId-like domain
separator into its own tx.

### 5.3 Deposits: derive `validityEnd` from the deposit tx's TTL; keep `submissionDuration`

A deposit's `validityEnd` is its **accept-by deadline** — the head must *learn* (observe/accept) the
deposit before it (absorption happens later, on a schedule derived from it) — and it is mandatory for a
real reason: at
registration the head builds three L1-timing values from it —

- the deposit tx's **TTL** = `validityEnd + submissionDuration`,
- the **absorption start** = `validityEnd + absorptionStartOffset` (when the head begins absorbing),
- the **post-dated refund** validity-start = `validityEnd + refundStartOffset` (when the user reclaims
  if the head never absorbs).

The refund is the deposit's safety net ("unabsorbed by the deadline → I get my funds back"); it is
pre-signed at registration with a start derived from `validityEnd`. No deadline → no refund → a
deposit could be stuck forever. That is why it's mandatory — safety, not bureaucracy. (Uses in code:
`DepositTx.scala` takes `requestValidityEndTime`; `TxTiming.refundValidityStart(validityEnd)`; TTL
relation at `TxTiming.scala:39`.)

**In the isomorphic paradigm the field goes away; the deadline does not.** The deposit request is
`l1Payload` (the deposit tx) + `l2Payload`, and the deposit tx is a native Cardano tx that already
carries a **TTL**. Since `deposit_tx.ttl = validityEnd + submissionDuration`, the head **derives**
`validityEnd = deposit_tx.ttl − submissionDuration` (`submissionDuration` is head config). So the
separate `validityEnd` field is dropped (read from the tx, per §5) and *"mandatory `validityEnd`"*
becomes *"mandatory deposit-tx TTL"* — the idiomatic Cardano home for a deadline. Note the deposit
case is **not** `validityEnd == ttl` (as it is for an L2 transaction): a deposit's `validityEnd` sits
`submissionDuration` *below* the TTL, a deliberate grace window so the tx stays submittable past the
user's deadline.

**`submissionDuration` stays — it is the accept-by margin.** The head must *learn* a deposit before
`ttl − submissionDuration` (= `validityEnd`), so that a deposit it accepts still has at least
`submissionDuration` of slack to be submitted and confirmed on L1 before the deposit tx's TTL expires.
This is enforced by an explicit **accept-by check**: when the head would include a deposit in a block it
must verify `block_creation_start ≤ validityEnd` — i.e. `now ≤ ttl − submissionDuration`. So
`validityEnd` is a genuine check reading `ttl − submissionDuration` directly (not merely a schedule
anchor), which makes `submissionDuration` load-bearing. It remains head config and stays exposed via
`/head-info`.

*Why it can look removable:* in the absorption/refund **schedule** alone, `submissionDuration` cancels —
`absorption_start = validityEnd + absorptionStartOffset = depositSubmissionDeadline + maturityDuration =
ttl + maturityDuration`, and likewise for refund. So if `validityEnd` were *only* a schedule anchor it
would fold away. But it is also the accept-by deadline, so it does not. (This is also why the stage1
generator's `reservedSubmissionDuration` — which reserves exactly this margin — stays as is.)

## 6. L2 backend selection & config model

- **Head config gains an `l2ledger` field: `cardano-eutxo` | `any-remote`.** It fixes the ledger
  *type*, which all peers must agree on (an EUTXO ledger and a remote black box are very different
  trust models), so it belongs with the agreed head parameters — committed via `l2ParamsHash`
  (already "a black-box, L2-specific hash of the L2 params peers agree on," hashed into the treasury
  datum). `headParamsHash` is still `= ???` in code; the discriminator lands as part of finishing it.
- **`remoteLedgerUri`** (rename of `sugarRushUri`): **optional**, per-node **private** config, used
  only by `any-remote`. **Every node runs its own ledger** (sidecar: node-i ↔ its own remote ledger),
  so the endpoint differs per node and must NOT live in the shared/committed head config. Precedent:
  `headPeerAddresses` sit in the head-config JSON but are operational topology, not hashed params.
- **`Main` selects the implementation** from `l2ledger`: `EutxoL2Ledger` (in-process) for
  `cardano-eutxo`, `RemoteL2Ledger(remoteLedgerUri)` for `any-remote`. Today Main always wires the
  remote ledger and passes `None` for the L2-query reader (see "Landed" below).
- **Determinism is a flat requirement on ANY backend** (part of the `L2Ledger` contract), not an
  EUTXO advantage: consensus feeds every peer's replica the same ordered commands, so a
  non-deterministic backend diverges and breaks consensus. EUTXO gets it from native tx validation;
  SugarRush provides it by design.

### 6.1 Ledger configuration at initialization (GUM-104)

The whitepaper's initialization / negotiation-bootstrap section under-specifies the **ledger** config
— it just says "inject specific parts of the config." Today the head's init config
(`InitializationParameters`) bakes in **`initialEvacuationMap`** ("the utxos with which the head's L2
ledger should be populated upon initialization"). That is **EUTXO-specific state** (an evacuation map
of Cardano utxos) sitting in the *shared, agreed* init config, which:

- couples bootstrap to the built-in EUTXO ledger — a producer of that map must run at bootstrap time;
- doesn't fit a **remote** ledger, whose initial state has a different shape;
- is consumed *beyond* the ledger — the rule-based/**fallback** regime's KZG commitment
  (`RuleBasedActor`), `StackComposer`/`StackEffectsBuilder`, and the `InitializationTx` all read it —
  so it isn't purely ledger-internal state.

**Direction (aligns with §6).** Keep the shared init/bootstrap config **ledger-agnostic**: carry only
what every backend needs and all peers must agree on — the `l2ledger` type, the `l2ParamsHash`, and
the opening funds (`initialEquityContributions`, `seedUtxo`, funding utxos). **Each ledger derives its
own initial state** instead of having it injected (the EUTXO ledger derives its initial utxo set /
evacuation map; SugarRush derives its own). This is GUM-104's "the initial evacuation map can always
be derived, so postpone / remove it from the bootstrap config."

**What it's derived from — to pin down.** The initial L2 state is the head's opening distribution of
funds; its *value* is already fixed (`initialL2Value = Σ(map utxo values)`), so what's missing is the
*distribution* (which L2 address holds what). Candidates: agreed opening params + a stated initial
allocation; the `InitializationTx` outputs; or **empty + funded by deposits** (head opens with no L2
utxos, everything arrives via the deposit path — the simplest, and the most ledger-agnostic).

**The commitment angle.** Because the fallback regime commits to the initial L2 state (a KZG
commitment) and all peers must agree on it, "derive not store" doesn't mean "forget it" — the shared
config keeps a **commitment** to the initial state (folded into `l2ParamsHash`, or the fallback KZG
root), while the *full* state is derived by whoever needs it. So the agreed artifact shrinks from a
whole EUTXO evacuation map to a ledger-agnostic hash/commitment.

**Open questions.** (a) Derivable purely from agreed opening params, or does init need a ledger
round-trip (the ledger produces its own genesis, Gummiworm records only the commitment)? (b) Can the
fallback regime derive the map too, or does removing it from the config just move the coupling into
the fallback path? (c) If "empty + deposits" is chosen, does anything require a non-empty opening L2
state (e.g. peer equity that must appear on L2 at open rather than as treasury)?

## 7. Deployment / one-command

- **Goal:** run a fully-isomorphic L2 head with **one command**.
- **Runtime = container composition.** Extend the existing SugarRush compose with the Hydrozoa node
  fleet; use compose **profiles** so one topology covers both backends (`any-remote` brings up N node
  + N remote-ledger sidecars; `cardano-eutxo` runs the node services only).
- **L1 = public testnet (Preview) first, Yaci devnet later.** Honest scope: on Preview the single
  command is the **run phase** (`compose up` over a prebuilt `head-config.json`). The **provisioning
  phase** (keygen-fleet → deploy-reference-scripts → build-head-config) needs a funded peer-0 + a
  Blockfrost key + a one-time ref-script deployment, so it's a prerequisite. So today: "**one command
  to run**"; "**one command from nothing**" waits for Yaci (auto-fund folds provisioning in).

## 8. Landed so far

On branch `ilia/multi-peer-deployment` (PR open, merged up to date with `main`): the L2 query
endpoints are now **EUTXO-only and optional**, a provisional step toward backend selection.
- `L2LedgerReader` → **`EutxoL2LedgerReader`**, decoupled from `L2Ledger` (only `EutxoL2Ledger`
  implements it; `RemoteL2Ledger` dropped its empty stubs).
- `HydrozoaRoutes`/`HydrozoaServer` take `Option[EutxoL2LedgerReader]`; the two L2 endpoints are split
  into tapir definitions (feed the schema) + serverLogic (mounted only when the reader is present).
- Two OpenAPI docs: `docs/openapi.yaml` (core, always) + `docs/openapi-eutxo-l2.yaml` (the L2
  endpoints), each pinned by a golden test.
- `Main` passes `None` provisionally — `// TODO(l2ledger)`: pass `Some(reader)` once the head-config
  `l2ledger` field selects `cardano-eutxo`. So a remote-ledger node serves no L2 endpoints (404).

## 9. Work items

1. **`l2ledger` head-config field** (`cardano-eutxo` | `any-remote`) + `sugarRushUri` →
   `remoteLedgerUri` rename + `Main` wiring `EutxoL2Ledger` (and `Some(reader)`) when `cardano-eutxo`.
2. **Separate runnable targets** to submit deposits and transactions when the `cardano-eutxo` ledger
   is used.
3. **Screening endpoint** on the remote ledger (takes `l2Payload` → yes/no) + the final verdict
   signal-back after Gummiworm-side validation.
4. **Native-tx isomorphism (EUTXO):** strip header + `userVk` + `signature` from the request; screening
   does signature validation + output checks; submission does the stateful checks. Drop `userVk`.
5. **slot↔L2-time convention** for interpreting a tx's validity interval on L2.
6. **Deposits (§5.3):** drop the `validityEnd` field — derive it from the deposit tx's TTL
   (`ttl − submissionDuration`); require the deposit tx to carry a TTL. **Keep `submissionDuration`**
   (the accept-by margin) and add/enforce the accept-by check `block_creation_start ≤ validityEnd`
   (`now ≤ ttl − submissionDuration`).
7. **Naming:** replace working terms (`l2Payload`/`l1Payload`) with project-consistent names before code.
8. **GUM-104 — ledger config at init (§6.1):** specify it in the whitepaper's initialization section;
   make the shared bootstrap config ledger-agnostic; derive the initial L2 state (drop
   `initialEvacuationMap`, keep only a commitment / `l2ParamsHash`); confirm the fallback regime +
   `InitializationTx` can derive it too.

## 10. Open questions

- Finishing `headParamsHash` (`= ???`) and where exactly `l2ledger` + the L2 params hash in.
- Whether the remote ledger needs a distinguished `userVk` (decides whether it can be dropped from
  the wire entirely, not just the EUTXO path).
