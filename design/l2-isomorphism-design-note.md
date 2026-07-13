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

## Changes in this PR (#531)

A grouped overview of what branch `feature/isomorphic-l2` changes; the sections below give the rationale.

- **L2 isomorphism — strip the request wrapper (§5.4).** The EUTXO ledger is now drivable by native
  Cardano txs: the `headId` pin moves into a tx metadatum the ledger checks; tx validity comes from the
  tx's own slot interval; a stateless `L2Ledger` screening step gates `RequestSequencer` and
  authenticates the tx's own witnesses; and the COSE envelope, `userVk`, and the whole
  `UserRequestHeader` are gone — a transaction request is just its body. Deposit accept-by is derived
  from the deposit tx's TTL (§5.3). Isomorphism covers L2 *transactions* only — deposits keep an
  explicit signature (§5.5).
- **L2 backend selection & config.** `HeadParameters.l2Ledger` (`cardano-eutxo` | `any-remote`) +
  `identityIsomorphism`; `sugarRushUri` → `remoteLedgerUri` (optional); L2 query endpoints gated
  EUTXO-only.
- **Bootstrap config — `peers.json` → `bootstrap.json` (§6.1).** A spec-shaped `Bootstrap.BootstrapConfig`
  carrying `cardanoNetwork`, `scriptReferenceUtxos`, and the opening `initialL2State` — folding the
  network, script refs, and opening state out of hard-code / side-files into one agreed artifact,
  assembled by `BuildBootstrapConfig` and read by `BuildHeadConfig`.
- **Deposit authentication — COSE over `hash(l2Payload)` (§5.5).** Deposits aren't L2 txs and can't
  self-authenticate, so the depositor COSE-signs `hash(l2Payload)` and carries the key+signature in the
  deposit metadata; Hydrozoa verifies it while parsing the deposit tx (before screening). Screening
  splits into `sendScreenTx` / `sendScreenDeposit`. The CIP-30 `signData` primitives the strip had
  retired are restored (re-pointed at the deposit, not the old request header).
- **Proxy removal.** Drop the L2-ledger proxy commands (`ProxyBlockConfirmation` / `ProxyRequestError`)
  and everything feeding them — the write-only `confirmations` / `errors` ledger state and the
  `FastConsensusActor` → `JointLedger` soft-confirm fan-out.
- **Merged from `origin/main`.** fund14-proj69 (`/ready` + `NodeStatus`) and #505 / GUM-129 (init-tx
  parsing; `Bootstrap` moved to `hydrozoa.bootstrap`).
- **Docs.** This design note (§5.4 roadmap, §5.3 deposits, §6.1 evacuation-map correction +
  bootstrap-config revision).

## 1. Isomorphism goal (EUTXO)

Make the EUTXO L2 ledger *isomorphic* to L1, at two strengths:

1. **Format isomorphism (always):** the head speaks the native Cardano tx format, so you can build
   L2 transactions with any standard Cardano library. The tx may still carry a head-pin annotation.
2. **Identity isomorphism (opt-in, off by default):** the *exact* transaction an application already
   uses on L1 runs on L2 unchanged — no head-specific tailoring.

**Isomorphism is over L2 *transactions*, not the boundary crossings.** Deposits (funds entering L2)
and withdrawals (funds leaving) have no L1 counterpart — they are precisely what stops an L2 from being
just another L1 — so they can't be isomorphic and keep their own machinery (a deposit carries an
explicit signature, §5.5; a withdrawal is a specially-marked L2 output settled out on L1). Both are
avoidable to a degree: a head can open with an initial utxo set instead of taking deposits, and can
return funds via finalization/evacuation instead of per-tx withdrawals. What's left — moving value
*within* L2 — is exactly the L2 transactions that isomorphism targets.

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

## 3. What a request carried before the strip (the baseline)

> **Landed (§5.4):** all of this is now gone. The `header`, `userVk`, and the COSE `signature` were
> removed; a `UserRequest` is now just its `body` (a native, self-authenticating tx). This section is
> the pre-strip baseline that motivated the change — read §5.4 for the end state.

`UserRequest` was `header` + `body` + `userVk`, and the **body already carried the payload fields** —
the payload split is NOT what was new:

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
- **Deposit:** the ledger's value checks — the `l2Payload` is well-formed and `depositL2Value` covers
  its outputs (§5.5). The deposit tx's own well-formedness and the COSE signature over `hash(l2Payload)`
  are verified *before* screening, while Hydrozoa parses the deposit tx — not here (and there is no
  `l1Payload`↔`l2Payload` "hash binding" step anymore, §5.5).
- **`headId` pin:** the ledger knows this head's `headId` (configured from day one), so it checks the
  pin here itself — *if* it's set to (that's the identity-isomorphism toggle, §5.2).

It *cannot* check anything that needs state: the current time (validity range), the utxo set (resolve
inputs / balance), or — for a deposit — that the L1 funds exist.

**Screening returns a plain `No | Yes`.** The ledger owns the `headId` check now, so nothing about the
head identity is handed back to Gummiworm. Passing screening **assigns the `RequestId`** and admits
the request — the gate before Gummiworm spends any resources on it. (So `RequestId` is assigned here,
not carried in the request.) For a **remote** ledger, screening is an endpoint on `remoteLedgerUri`
that takes the `l2Payload` and returns yes/no.

### 4.2 Submission (applying) — stateful

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
must verify `block_creation_start < validityEnd` — i.e. `now < ttl − submissionDuration`. So
`validityEnd` is a genuine check reading `ttl − submissionDuration` directly (not merely a schedule
anchor), which makes `submissionDuration` load-bearing. It remains head config and stays exposed via
`/head-info`.

The check reuses the existing shared `TxTiming.checkRequestValidityInterval` predicate, which is a
**strict** `<` (not `≤`): it also gates L2 transaction requests, where `validityEnd == ttl` and Cardano's
TTL is exclusive (`invalid_hereafter`), so a strict bound is the correct one there; for deposits the
strict bound is a hair more conservative (it guarantees *strictly more* than `submissionDuration` of
slack), which is safe. Implementation: the check is deposit-specific in that it reads the *derived*
`validityEnd` (= `ttl − submissionDuration` from the parsed deposit tx) rather than a request-header
field, and it runs *after* the deposit tx is parsed (the derived deadline is only knowable post-parse).

*Why it can look removable:* in the absorption/refund **schedule** alone, `submissionDuration` cancels —
`absorption_start = validityEnd + absorptionStartOffset = depositSubmissionDeadline + maturityDuration =
ttl + maturityDuration`, and likewise for refund. So if `validityEnd` were *only* a schedule anchor it
would fold away. But it is also the accept-by deadline, so it does not. (This is also why the stage1
generator's `reservedSubmissionDuration` — which reserves exactly this margin — stays as is.)

### 5.4 Field-removal roadmap (stripping the request wrapper)

A user request today is a Hydrozoa-specific **wrapper** around the payload:
`UserRequestHeader { headId, validityStart, validityEnd, bodyHash }` + `userVk` + a COSE signature over
the header JSON. Under §4 (delegate every check to the ledger — which per §5.1 applies to **both**
backends), the whole wrapper is redundant for **any** ledger, not just EUTXO: every ledger's payload is a
self-describing, self-authenticating transaction (its own validity interval and signatures, and it can
carry the `headId` pin in its metadata), and every ledger runs its own screening — so the checks the
wrapper enabled move into the ledger and the fields fall away. The teardown is sequenced **EUTXO-first**
only for a *practical* reason: the in-process EUTXO ledger is here now, while the remote screening endpoint
(§9 item 3) that lets a remote ledger self-source is later work — *not* because a remote ledger needs the
wrapper. The work is a
**staged teardown**, each phase an independently-landable, green slice that keeps the wire working — the
model is §5.3's deposit slice (move the check into the ledger, *keep* the shared field, delete nothing on
the wire until a later structural phase).

> **Key finding (corrects §3.2):** there is **no `headId` equality check anywhere in the code today**
> (grep-verified). `headId` is carried and bound by the COSE signature but is never compared to *this*
> head's own id. So the pin work **adds** a check; a regression here fails silently (reopens cross-head
> replay with everything still green), which is why Phase 1's enforcement toggle must default to *enforce*.

**Phases** (ordered; each independently landable):

1. **`headId` aux-data pin** — *EUTXO, additive, wire-neutral.* Extract a **new dedicated** `headId`
   metadatum from the tx's auxiliary data and compare it to `config.headId` (both already exist:
   `InitializationParameters.headId`; `L2Tx.utxoPartition` already parses tx aux metadata). It is a
   *separate* key from the existing CIP-67 head-tag metadatum, which carries the per-output L1/L2
   designation list (`Metadatum.List` of `Int(1)`/`Int(2)`, L2Tx.scala:85-106) — do not overload it.
   Gate enforcement behind the ledger's identity-isomorphism toggle (§5.2), **default enforce**. No
   header/COSE/wire change.
   > **Consequence — metadata becomes mandatory on every L2 tx.** Today a *regular* L2 tx carries no
   > auxiliary data; only a withdrawal-bearing tx carries the CIP-67 head-tag designation metadatum. With
   > the pin enforced, **every** L2 tx must now carry the `headId` metadatum, so a plain Cardano tx from a
   > standard library is no longer valid unrewritten — the head's submission/SDK path must inject the
   > metadatum. This mandatory-ness *is* the identity-isomorphism switch: enforce the pin ⟺ require the
   > metadatum; turn it off ⟺ the exact L1 tx runs with no added metadata (true identity isomorphism, at the
   > cost of cross-head-replay protection, §5.2). "Switch in the future" = surfacing this toggle.
2. **Tx validity from the tx's own slot interval** — *EUTXO semantics.* Stop trusting `header.validityEnd`
   on the transaction path; the ledger's validity-interval validator (the tx's `[invalidBefore, ttl)` in the
   slot domain, checked against block-creation-start) is authoritative. `validityStart` is already dead
   code. *Keep the header field* (mirrors the deposit slice — no field deleted).
3. **Ledger screening entry point + invert id-assignment** — *both backends; the structural pivot.* Add
   `screen(payload): No | Yes` to the shared `L2Ledger` trait; a *Yes* is what assigns the `RequestId`
   (gating the sequencer). EUTXO screening = the stateless subset of the tx mutator's validators (signature
   verification, output/conformance/size checks) + the Phase-1 pin; the remote gets a new screening endpoint
   on `remoteLedgerUri`. Submission stays the current stateful apply path. This is the prerequisite for
   moving auth off COSE.
4. **Drop the COSE envelope + `bodyHash` — *all backends*** *(corrected from EUTXO-only).* **DONE** (auth
   moved into `EutxoL2Ledger.screen`, reusing `VerifiedSignaturesInWitnessesValidator`; shared
   `UserRequestDecoder` no longer recovers a key from a COSE envelope). COSE lives at the
   **shared** HTTP front door (`UserRequestDecoder`) with **zero backend-awareness**, and is not on the peer
   wire; making it EUTXO-only would mean *adding* a backend branch to keep it for remote nodes — backwards.
   Under §4 the ledger owns *every* check including signature validation, and both backends implement
   `screen()`, so each ledger authenticates its own payload (EUTXO = the native tx's vkey witnesses; remote =
   SugarRush's self-authenticating payload). So the Hydrozoa COSE wrapper is redundant for both — delete the
   shared decoder outright. **Depends on Phase 3** (the remote screening endpoint must authenticate the remote
   payload first). **Assumes** the remote payload self-authenticates (true of any ledger's tx format; confirm
   against the SugarRush contract). Regenerate/delete the `JsonCodecsTest` COSE golden.
5. **Delete the redundant header fields from the request type** — **DONE** (the whole `UserRequestHeader`
   is gone, not just the fields: all four were redundant after Phases 1-2, so the request collapsed to its
   body). `headId` was pinned in the tx metadatum (Phase 1); `validityStart` was already `@unused`
   everywhere; `validityEnd` on the deposit path is derived from the deposit tx TTL (the model now sources
   it from `DepositUtxo.requestValidityEndTime` too); `bodyHash` bound a header that no longer exists. The
   **AnyRemote** tx-path header-validity check was dropped — a remote ledger enforces validity in its own
   screening (Ilia chose collapse-now over deferring to that slice). Wire form drops the `header` envelope
   on both the user-facing API (still `stringJsonBody`, so no OpenAPI change) and the peer transport.
6. **Remove `userVk` from the ledger calls** — **DONE** (done together with Phase 4: dropped from
   `UserRequest`/`UserRequestHeader`, `L2LedgerCommand.{RegisterDeposit,ApplyTransaction}`, the peer-wire
   `Codecs`, and `JointLedger` construction; zero `.userVKey` reads remained). — `userVk` was used **only** for COSE validation, so once
   Phase 4 removes COSE it has no remaining purpose: drop it from the request, from `L2LedgerCommand.userVKey`
   (the host→ledger command wire), and from the persisted command log. The ledger reads any key it needs from
   the `l2Payload` itself (EUTXO: the native tx's own vkey witnesses; remote: SugarRush's payload — *if* it
   needs one). **Tightly coupled to Phase 4:** `userVk`'s only *source* is the COSE-key recovery
   (`JsonCodecs.scala:199-206`), not an explicit field, so removing COSE strips `userVk` from the request at
   Phase 4 and the command + persistence must follow. Weight: an on-disk persistence-log migration
   (`L2StoreCodecs`, R2b write-before-advance) — no longer an open/contract-gated question.

**Decision forks** (recommendation in **bold**):

- *Screening before removing COSE, or rely on submission-time auth?* → **Screening first.** Removing COSE
  without screening lets an unauthenticated request get a durable `RequestId` + peer fan-out *before*
  rejection (a DoS/resource-waste regression). The order-inversion is the substantive §4 value.
- *When can the wrapper be stripped for the remote?* → The end state (§5.1: Gummiworm keeps **no** checks
  of its own) is that the wrapper is stripped for **both** backends — for the remote, SugarRush runs every
  check on its own payload. So the fields are droppable-for-all; the only gating factor is a *sequencing*
  one: until the SugarRush screening endpoint + self-sourcing contract lands (Phase 3 / §9 item 3), the
  remote path still leans on the Gummiworm-side fields, so **keep them on the shared wire transitionally**
  (the deposit pattern). This is a temporary crutch, **not** a permanent "the remote needs the wrapper"
  asymmetry.
- *Where does the `headId` pin live in tx metadata?* → **A new dedicated metadatum** (`Metadatum.Text(headId.toHex)`,
  mirroring the L1 convention), not an overload of the existing CIP-67 head-tag list.
- *Tx validity: derive from `ttl`, or rely on the ledger?* → **Rely on the ledger** for EUTXO. A tx's
  `validityEnd == ttl` (no offset, unlike a deposit), and the ledger already enforces the interval — the
  Gummiworm-side check is a redundant second source that can disagree.
- *§4.2 positive valid/invalid verdict signal-back — now or later?* → **Defer.** The in-process EUTXO ledger
  makes the verdict implicit; it only matters for the remote submission phase.

**Prerequisites** (mostly already present):

- *slot↔L2-time convention* is the existing per-head `SlotConfig` with block-creation-start as "now"
  (already wired through the tx mutator) — it needs *naming/formalizing* and the assertion `validityEnd ==
  tx.ttl` for L2 txs, not a new clock.
- *Native-witness auth as a screening check* requires partitioning the tx mutator's validator stack into a
  stateless subset (witness verification, conformance, size, output checks) versus a stateful subset
  (input-set membership, value conservation, required-signers-present, script execution). "Are all required
  signers present?" needs resolved inputs → stateful; only "do these witnesses verify over the tx id?" is
  stateless. This is a real refactor, not a rename.
- *`config.headId` + tx aux-data parsing* already exist.

**Biggest risks:** the COSE golden (must be re-signed, never hand-edited); a **silent** cross-head-replay
regression (Phase 1 toggle default must be *enforce*); remote-ledger wire breakage (three distinct wires —
client HTTP, peer transport, host→ledger command — carry different subsets; keep the shared wire until the
remote can self-source); the peer-transport golden + on-disk persistence-log migration; and the
`RequestSequencer` order-inversion, which must preserve head-local sequential ids and the persist-before-
observable invariant.

### 5.5 Deposit authentication — a signature over `hash(l2Payload)` in the deposit metadata

**Landed by the strip (§5.4), then reconsidered (2026-07-13, post-George-sync).** Dropping COSE with the
rest of the request wrapper was misleading for *deposits*: isomorphism is a **transaction** property (a
native L2 tx self-authenticates through its own vkey witnesses), but a deposit is **not** an L2 tx —
it's the on-ramp, and its `l2Payload` (the `GenesisObligation`s spawned on absorption) can't
self-authenticate. So deposits must keep an explicit signature. The old COSE was worthless as auth
anyway: its key was **self-declared** in the request, checked against nothing (§3.1 / the removed
`JsonCodecs.validateCoseSignature`) — an attacker could self-sign a genuine-deposit + arbitrary-`l2Payload`
pair.

**The binding this builds on.** Before this change the deposit tx metadata carried `l2PayloadHash` (a
`Hash32` metadatum), and `DepositTx.Parse` enforced `blake2b_256(l2Payload) == l2PayloadHash`. Because the
deposit tx's own L1 witnesses (the fund controllers) sign the body — which commits to the metadata — a
deposit signer already endorses `hash(l2Payload)` *once the deposit is on-chain*. The gap is purely
**screening-time**: the request's `l1Payload` is the *unsigned* deposit tx, so at screening there are no
witnesses to check.

**Design.** Have the depositor — one of the deposit tx's signers — **COSE-sign `hash(l2Payload)`** and put
the **COSE key + signature into the deposit tx metadata** (replacing the bare `l2PayloadHash`). The COSE
sig is self-contained, so it is verifiable at screening on the unsigned tx, and its key ties the
endorsement to a real deposit party rather than a self-declared one. Screening a deposit has two stages:

- **Deposit pre-screening — Hydrozoa's stage, a dedicated `DepositPreScreening` module.** Runs *before*
  the ledger sees the deposit. It (a) **authenticates** the deposit by verifying the COSE signature over
  `hash(l2Payload)` (the key must belong to a deposit-tx signer — see the open point on *when* that tie is
  checked, given the request-time tx is unsigned), and (b) runs the **validity_end (accept-by) check**:
  `block_creation_start < validityEnd`, with `validityEnd` derived from the deposit tx's TTL (§5.3).
  Deposit-tx well-formedness is established here too — reaching the metadatum + TTL requires parsing the
  tx.
- **The ledger's stage (`sendScreenDeposit`):** ledger-specific value / well-formedness — the `l2Payload`
  matches `depositL2Value` / `depositFee`. For the **EUTXO** ledger: `depositL2Value` covers the
  `l2Payload` outputs (Σ output values ≤ `depositL2Value`), plus min-ada / parseable `GenesisObligation`s.
  (Open: the exact value relation — `≤`, `==`, or `== depositL2Value − depositFee` — and whatever else the
  ledger should assert.)

**Two screening entrypoints** on `L2Ledger`, replacing the single `screen(l2Payload, Option[l1Payload])`
(the `Option` encoded "deposit-or-not" in a nullable arg):

- `sendScreenTx(l2Payload)` — native-tx path (parse + headId pin + witness sigs, §4.1/§5.4). A
  transaction has **no pre-screening stage**: it self-authenticates through its own witnesses, so this
  ledger stage is the whole of its screening.
- `sendScreenDeposit(depositId, depositFee, depositL2Value, refundDestination, l2Payload)` — deposit path
  (a `ScreenDeposit` type = `RegisterDeposit` minus the consensus-assigned `{requestId, blockNumber,
  blockCreationStartTime}`), preceded by deposit pre-screening (above).

**Where pre-screening lives.** The `DepositPreScreening` module parses the deposit tx (reaching the COSE
key+sig metadatum and the TTL), verifies the COSE signature, and runs the validity_end check — all
Hydrozoa-side, before `sendScreenDeposit`. (The underlying deposit-tx parse does far more than parse — it
validates and verifies — so `DepositTx.Parse` wants a better name.)

**Restore the COSE primitives.** The strip deleted the sign/verify crypto (`WalletModule.signCoseCip30`,
`Cip30SignedData`, the `CIP30DataSigner` verify helper). Resurrect them and re-point at `hash(l2Payload)`
+ the deposit metadata — *not* the old request-header COSE flow, which this replaces.

**Landed (2026-07-13).** The whole §5.5 pipeline is implemented: the COSE primitives
(`WalletModule.signCoseCip30` / `PeerWallet.signCoseCip30` / the keyless `Cip30Verify.verify`); the
deposit metadata carries the COSE pair as ≤64-byte chunked byte lists (`Metadata.Deposit(depositIx,
depositFee, coseKey, coseSignature)` — the bare `l2PayloadHash` metadatum is gone), supplied to
`DepositTx.Build` as `l2PayloadCose` and verified in `DepositTx.Parse`
(sig valid + signed payload == `blake2b_256(l2Payload)`); `DepositPreScreening.preScreen` (parse/COSE +
the wall-clock accept-by gate) gates `RequestSequencer`'s deposit path before
`sendScreenDeposit`; and the ledger split `sendScreenTx` / `sendScreenDeposit(ScreenDeposit)` landed
with the EUTXO covers-check (`depositL2Value` ≥ Σ `l2Payload` outputs, componentwise). Still open: the
`coseKey ∈ deposit-tx signers` tie (below) and the remote screening endpoint (§9 item 3).

**Open point.** `coseKey` must be a deposit-tx signer to be meaningful (not self-declared, the old flaw).
But the request-time `l1Payload` is unsigned, so at screening only the COSE sig's internal validity is
checkable; the `coseKey ∈ deposit-tx witnesses` tie lands when the deposit is observed on L1. Decide
whether screening should additionally pin `coseKey` to something stateless (the `refundDestination` /
deposit-input credential) so the screening step is not itself self-asserted.

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
(`InitializationParameters`) bakes in **`initialEvacuationMap`** — a concrete evacuation map for the
head's opening L2 state.

The evacuation map is **not** EUTXO-specific. It is the projection of *any* L2 ledger's state into its
**L1-compatible (Cardano-utxo) representation** — the utxos that would be materialized on L1 if the
head is evacuated/settled — so *every* backend must define it, precisely because L1 is Cardano. The
code already treats it this way: `EvacuationMap` (`= TreeMap[EvacuationKey, Payout.Obligation]`) lives
in the ledger-agnostic `multisig.ledger.joint` package, the `L2Ledger` contract *requires* every
command to return `EvacuationDiff`s, and both `EutxoL2Ledger` and `RemoteL2Ledger` produce them. For
EUTXO the L2 state already *is* Cardano utxos, so the projection is near-identity; a remote ledger
projects its own internal state into the same map. So the map type is universal — the coupling is
narrower:

- **A concrete *initial* map is injected into the *shared, agreed* config**, so a producer of that map
  must run at bootstrap time — coupling bootstrap to whoever computes the opening state (today, an
  EUTXO utxo set). A remote ledger would have to synthesize a Cardano-utxo initial map just to satisfy
  this field.
- It is consumed *beyond* the ledger — the rule-based/**fallback** regime's KZG commitment
  (`RuleBasedActor`), `StackEffectsBuilder`, and the `InitializationTx` all read it — so removing it
  from the shared config touches the fallback path too, not just the ledger.

**Direction — revised (landed 2026-07-12): keep the opening L2 state in the bootstrap config.** GUM-104's
"derive not store, keep only a commitment" does **not** work here. The initialization tx *commits to the
opening state on-chain*: the multisig-treasury value is `initialL2Value = Σ(output values)`, and the
treasury datum carries the evacuation map's KZG commitment. GUM-129's parse side recomputes that datum
from the config and verifies it, and the EUTXO ledger seeds its opening state from the same map. You
cannot "derive and forget" a value the on-chain tx commits to and every node verifies — so the opening
state stays an explicit, agreed field of the bootstrap config. It is carried in a human-readable form —
`initialL2State`, a list of CIP-0116 outputs (address + value) — which `BuildHeadConfig` keys into the
`initialEvacuationMap` once the seed utxo is resolved. Each output's input reference is
`(L2Genesis.mkGenesisId(seedInput), i)`: a synthetic tx id that hashes the *whole* seed input (tx id +
index), the same convention the EUTXO ledger uses for deposit-created utxos — reusing the seed's raw tx
id would let `(seedTxId, i)` collide with a real on-chain output of the seed's own transaction. (Its
*content* can still be as simple as empty, the current default.)

**Divergence from the spec: `initialL2State`, not the spec's `initialEvacuationMap`, in the bootstrap
config.** The whitepaper's bootstrap-config section (`.../cardano/initialization`) specifies the opening
state as `initialEvacuationMap` — a *keyed* map `{ cbor(input): cbor(destination) }`. That keyed map
**cannot be authored in the bootstrap config**: its keys are CBOR-encoded `TransactionInput`s derived
from the seed utxo, which `BuildHeadConfig` only resolves at build time (the demo auto-selects it from
head peer 0's wallet). So the bootstrap config carries the unkeyed pre-image `initialL2State` (a list of
outputs), and the keyed `initialEvacuationMap` is what `BuildHeadConfig` derives into the head config
(`InitializationParameters`) — which *does* match the spec's shape. The spec text is left unchanged; this
is a deliberate representation divergence, not a spec change.

**Landed: a spec-shaped bootstrap config, `peers.json` → `bootstrap.json`.** The whitepaper's
head-initialization section defines a **bootstrap config** (human-authored) that the tooling turns into
the head config (adding `headId` + the built `initTx`). The tooling now matches that shape:
`Bootstrap.BootstrapConfig` (`config/demo/bootstrap.json`, read by `BuildHeadConfig`) carries
`cardanoNetwork`, the peer topology (`headPeers`/`coilPeers`/`coilQuorum`), `scriptReferenceUtxos`, and
the opening `initialL2State`. It is assembled from four operator-facing files by the pure
`BuildBootstrapConfig` tool: the `roster.json` peer topology (`GenerateKeyPair`), a
`bootstrap-defaults.json` (network + coil quorum) and an `l2-state.json` template (one funded output per
head peer) both emitted by `InitBootstrapFiles`, and the `script-refs.json` from
`deploy-reference-scripts`. This folded three things out of the code / side-files into one agreed
artifact: the network (was hard-coded `Preview`), the script references (was a separate
`--script-refs script-refs.json`), and the opening state (was hard-coded in `Bootstrap.scala`).

**Landed 2026-07-12: `headParams` + equity + timing surfaced into the config.** The values
`mkSharedHeadConfig` used to hard-code are now agreed bootstrap-config fields, seeded with demo
defaults by `InitBootstrapFiles` into `bootstrap-defaults.json`: the full `headParams` (`txTiming`,
`fallbackContingency`, `disputeResolutionConfig`, `settlementConfig`, `coilQuorum` — folding the
standalone `coilQuorum` into `headParams`, per the spec shape) and the per-peer
`initialEquityContributions` (replacing `BuildHeadConfig`'s `--equity` CLI). Block-zero timing
(`blockZeroStartTime` / `blockZeroEndTime`) is **optional**: omit it and `BuildHeadConfig` anchors the
initial block to wall-clock at build (kept relaxed — the head config stays ironclad-concrete either
way, since it is built right before bring-up).

**Still simplified / not yet in the config** (the demo's head-0-funds-everything model): only
`seedUtxo` + `additionalFundingUtxos` remain out — `build-bootstrap` stays pure, so `build-head-config`
resolves the seed + funding from head peer 0's L1 address at build. Folding those in (which would let
`build-bootstrap` talk to L1 and carry the *keyed* `initialEvacuationMap`) is the remaining GUM-104
work.

**Open questions.** (a) Whether the pure `BuildBootstrapConfig` merge step earns its place at all, or
the roster + defaults + l2-state should be assembled another way (e.g. `GenerateKeyPair` writing
`bootstrap.json` directly). Kept for now. (b) Whether the fallback KZG commitment should fold into
`l2ParamsHash` rather than being recomputed from the config's evacuation map at build/parse.

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

**On branch `feature/isomorphic-l2` (tip `1773d3d8`): all six §5.4 field-removal phases + the CIP-30
retirement have landed.** The request wrapper is gone — no header, no `userVk`, no COSE envelope; a
request is just its body. `EutxoL2Ledger.screen` authenticates the native tx's own vkey witnesses
statelessly (pre-`RequestId`, reusing `VerifiedSignaturesInWitnessesValidator`); the headId pin lives
in a dedicated tx metadatum (label 4936) checked by the ledger; deposit `validityEnd` is derived from
the deposit tx TTL. The **AnyRemote** tx-path validity check was dropped in favor of the remote
ledger's own screening (item 3). Verified `-Werror`-clean across main/test/integration + 9/9 stage
properties (Stage1-Mock + Stage4 incl. 20-peer/WS).

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

> **Ordering note:** backend selection (item 1) is **landed**. GUM-104 (item 8) and the §5.4 field-removal
> phases are **orthogonal** — item 8 reworks the ledger *init* config; §5.4 reworks the *request* path.
> Neither blocks the other, so the §5.4 phases proceed independently. The one shared touchpoint is Phase 1's
> identity-isomorphism toggle, a small agreed `HeadParameters` field (like `l2Ledger`).

1. **`l2ledger` head-config field** (`cardano-eutxo` | `any-remote`) + `sugarRushUri` →
   `remoteLedgerUri` rename + `Main` wiring `EutxoL2Ledger` (and `Some(reader)`) when `cardano-eutxo`.
2. **Separate runnable targets** to submit deposits and transactions when the `cardano-eutxo` ledger
   is used.
3. **Screening endpoint** on the remote ledger (takes `l2Payload` → yes/no) + the final verdict
   signal-back after Gummiworm-side validation. **Now load-bearing:** §5.4 Phase 5 removed the
   Gummiworm-side validity/headId check on the AnyRemote path, so this endpoint is the *only* thing
   screening a remote payload. `RemoteL2Ledger.screen` is a passthrough stub until it lands — so
   AnyRemote is not yet a safe production backend (EUTXO is).
4. **Native-tx isomorphism — strip the request wrapper (header + COSE `signature` + `userVk`).** ✅
   **DONE** — all of **§5.4** landed (headId pin → tx-validity → ledger screening → drop COSE *for all
   backends* → delete the header entirely → drop `userVk`). Screening does signature/output checks;
   submission does the stateful checks.
5. **slot↔L2-time convention** for interpreting a tx's validity interval on L2 — see §5.4 prerequisites
   (it is the existing per-head `SlotConfig`, to be formalized, not built).
6. **Deposits (§5.3):** ✅ **DONE (behavioral, deposit-only).** The deposit path now derives
   `validityEnd = ttl − submissionDuration` from the parsed deposit tx's mandatory TTL (a missing TTL
   fails the parse) instead of reading a request-header field; the accept-by check
   `block_creation_start < validityEnd` runs post-parse against that derived value; `submissionDuration`
   stays (head config, exposed via `/head-info`). The `UserRequestHeader.validityEnd` field — and the
   whole `UserRequestHeader` — was **deleted in §5.4 Phase 5**: the transaction path now relies on the
   tx's own slot interval, and the stage model reads the deposit's `validityEnd` from
   `DepositUtxo.requestValidityEndTime` (matching the ledger) rather than a header field.
7. **Naming:** replace working terms (`l2Payload`/`l1Payload`) with project-consistent names before code.
8. **GUM-104 — ledger config at init (§6.1):** ⚠️ **partly landed.** The opening state stays in the
   bootstrap config (as `initialL2State`, not a bare commitment — the init tx commits to it on-chain and
   every node verifies it, see §6.1). Remaining: fold the still-simplified fields into the config
   (`initialEquityContributions`, `seedUtxo` + `additionalFundingUtxos`, block-zero timing, `headParams`)
   and specify the bootstrap config in the whitepaper's initialization section.

## 10. Open questions

- Finishing `headParamsHash` (`= ???`) and where exactly `l2ledger` + the L2 params hash in.
- `userVk` was **dropped from the wire entirely** (§5.4 Phase 6), not just the EUTXO path. If a remote
  ledger needs a key, it must source it from its own `l2Payload` — resolved as part of the remote
  screening endpoint (item 3), not a distinguished request field.
