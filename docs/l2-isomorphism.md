# L2 isomorphism: native transactions, screening, and the two ledger backends

The built-in EUTXO L2 ledger is drivable by standard Cardano transactions: build an L2 tx with
any Cardano library, sign it with ordinary vkey witnesses, and submit it — no request envelope, no
extra signature. The only head-specific content is two metadata entries
([What an L2 transaction must carry](#what-an-l2-transaction-must-carry)). This doc explains what
"isomorphic" covers and does not cover, the metadata an L2 tx must carry, the two-phase checks
every request passes through, how a deposit is pinned to its L2 payload (deposits are not
isomorphic), and how a head selects its L2 backend.

## Terminology

- **Format isomorphism** (always on): the head speaks the native Cardano tx format. The tx must
  carry head-specific *metadata*: the designation list and, by default, the headId pin (below).
- **Identity isomorphism** (`HeadParameters.identityIsomorphism`, opt-in, default off): the goal
  of running the *exact* tx an application already uses on L1, byte-identical. The toggle
  disables only the headId-pin check (see [Limitations](#limitations)).
- **Screening**: the stateless ledger check a request must pass *before* the head assigns it a
  `RequestId` — the gate before the head spends any resources on it.
- **Submission (applying)**: the stateful ledger check at block production — validity interval,
  input resolution, value conservation.
- **Deposit L1 screening**: the Hydrozoa-side stage that checks a deposit's l2Payload pin and
  accept-by deadline before the ledger screens it (deposits only; transactions have no L1
  screening stage).

## Scope: transactions, not boundary crossings

Isomorphism is a property of L2 *transactions* — moving value within L2. Deposits (funds entering
L2) and withdrawals (funds leaving) have no L1 counterpart — the boundary crossings are precisely
what stops an L2 from being just another L1 — so they cannot be isomorphic. Each keeps its own
machinery: a deposit pins its L2 payload in tx metadata
([Deposits](#deposits-pinned-to-their-l2-payload)); a withdrawal is an L2 output marked L1-bound in
the designation list, and a subsequent settlement tx pays it out on L1.

Both crossings are avoidable to a degree: a head can open with an initial utxo set instead of
taking deposits ([Opening state](#opening-state)), and can return funds via
finalization/evacuation instead of per-tx withdrawals.

## The headId pin and cross-head replay

A man-in-the-middle can forward an L2 tx to another head (or to L1) hoping it validates there. By
default the EUTXO ledger therefore requires each L2 tx to carry a **headId pin**: a dedicated
metadatum, label **4936**, value `Metadatum.Text(headId.toHex)`
(`multisig/ledger/eutxol2/HeadIdPin.scala`). `HeadIdPinValidator` rejects at screening any tx
whose pin is missing or names another head. The pin is authenticated for free: metadata is covered
by the tx's `auxiliaryDataHash`, which is in the signed body, so it cannot be stripped or swapped
without breaking the witnesses.

Replay onto L1 is already mostly prevented structurally — an L2 tx spends L2 utxo references that
do not exist on L1 — so the pin mainly defends against **cross-head** replay.

`identityIsomorphism = true` skips the pin check, so a tx need not carry the pin. That is the
step toward running an unmodified L1 tx, and it reopens cross-head replay: it is only safe when
the application already bakes a headId-like domain separator into its own txs. Like `l2Ledger`
([below](#backend-selection-and-configuration)), `identityIsomorphism` is a peer-agreed
`HeadParameters` field.

## What an L2 transaction must carry

| Element | Requirement | Checked by |
|---|---|---|
| Tx format | native Cardano tx (Babbage outputs), CBOR bytes as the request payload | `L2Tx.parse` |
| Designation list | metadatum label **4937** (`CIP67.Tags.head`): `Metadatum.List` with one `Int` per output, in output order — `1` = L1-bound (withdrawal), `2` = stays on L2. Mandatory on every tx | `L2Tx.utxoPartition` (parse fails without it) |
| headId pin | metadatum label **4936**, `Metadatum.Text(headId.toHex)` — mandatory unless `identityIsomorphism` | `HeadIdPinValidator` at screening |
| Authentication | the tx's own vkey witnesses; no other signature exists | screening (stateless), against the tx id |
| Validity | the tx's own slot interval, interpreted with the head's `SlotConfig`; block-creation start must fall inside it | submission (stateful) |
| Fee | the fee field must be `0`, so inputs must exactly balance outputs — the L2 ledger has no fee pot (Scalus builders: `withZeroFees` + a prebalanced diff handler) | `L2ConformanceValidator` (`fee == 0`) at submission |

`examples/src/main/scala/hydrozoa/examples/demo/SubmitL2Transaction.scala` builds exactly this
shape end-to-end and is the reference client.

What a client needs from the head: `GET /head/info` serves the `headId` (for the pin) and the
deposit timing offsets (`submissionDurationSeconds`, `absorptionStartOffsetSeconds`,
`refundStartOffsetSeconds`); slot↔time interpretation uses the network's standard slot config.

## Two-phase checks: screening, then submission

Hydrozoa does not interpret request payloads. Each check on a request — signature, validity
window, head identity — is an assumption borrowed from one ledger's semantics, so the ledger runs
them all. Hydrozoa hands over the payload plus the reference data the ledger lacks: the ledger has
no clock, so at apply it receives the block-creation time. What Hydrozoa keeps is the
*sequencing*: a request is screened first, admitted second.

```
user ──POST /head/requests──► RequestSequencer
                                   │  sendScreenTx(l2Payload)            (stateless)
                                   ▼
                              L2Ledger screening ── fail ──► rejected, no RequestId
                                   │ pass
                                   ▼
                              RequestId assigned, request enters consensus
                                   │  … block production …
                                   ▼
                              L2Ledger apply                             (stateful)
                                   │ fail: event marked Invalid (block still completes)
                                   ▼
                              L2 state updated
```

**Screening** (`L2Ledger.sendScreenTx` / `sendScreenDeposit`,
`multisig/ledger/l2/L2Ledger.scala`) checks only what the payload alone supports — no ledger
state, no clock. For the EUTXO ledger (`EutxoL2Ledger.sendScreenTx`): parse (including the
designation list), the headId pin, and the witness signatures (each witness must verify over the
tx id). Screening cannot resolve inputs, check balance, or test the validity window — those need
state. It returns pass/fail with an error reason; Hydrozoa itself never inspects or compares head
identities — the pin check lives entirely inside the ledger. Passing is what assigns the
`RequestId`, so an unparseable or unauthenticated payload is dropped before it consumes a durable
id or any peer traffic. On the wire, `POST /head/requests` returns the assigned `RequestId` on
acceptance and the rejection reason otherwise (`docs/openapi.yaml`).

**Submission** is the stateful apply path at block production: the validity interval against the
block-creation time, input resolution against the active utxo set, value conservation, the zero
fee, and that the witnesses cover the resolved inputs' payment credentials. A request that screens
clean but fails here (say, its inputs were spent by an earlier tx in the block) is marked
`Invalid` by `JointLedger`; the block completes regardless. An `Invalid` request is terminal — the
head does not retry it, and it never appears in `GET /l2/cardano-eutxo/transactions` — so the client
resubmits a corrected tx.

## Deposits: pinned to their L2 payload

A deposit request is two payloads: `l1Payload`, the CBOR of the **unsigned** deposit tx, and
`l2Payload`, the serialized `GenesisObligation`s — the L2 outputs the deposit spawns on
absorption. The `l2Payload` is not a transaction, so nothing intrinsic pairs it with the deposit
tx; an unpinned pairing would let anyone attach an arbitrary `l2Payload` to someone else's
deposit. So the deposit tx's metadata (`Metadata.Deposit`) carries **`blake2b_256(l2Payload)`**,
pinning the payload to the tx. At request time the tx is unsigned and the pin is a plain
consistency check; once the depositor signs and submits the tx, its L1 witnesses commit to the
pin via the signed body's auxiliary-data hash — swapping the `l2Payload` would change the tx body
and void the signatures.

The deposit path, in order (client steps marked):

1. **Client:** build the deposit tx (`DepositTx.Build` puts `blake2b_256(l2Payload)` in its
   metadata) and register both payloads with `POST /head/requests`.
2. **Deposit L1 screening** — Hydrozoa's stage, before the ledger sees the deposit
   (`multisig/ledger/l1/tx/DepositL1Screening.scala`, called by `RequestSequencer`).
   `screen(l1Payload, l2Payload, now)`:
   - parses the deposit tx (`DepositTx.Parse`), which checks the metadata's `l2PayloadHash`
     equals `blake2b_256(l2Payload)`;
   - gates on the accept-by deadline: `now < validityEnd` (below);
   - on success builds the `ScreenDeposit` command — `depositId`, `depositFee`, `depositL2Value`,
     `refundDestination`, `l2Payload` — for the ledger.
3. **Ledger screening** — `sendScreenDeposit(ScreenDeposit)`. The EUTXO ledger decodes the
   `GenesisObligation`s and checks that `depositL2Value` covers the `l2Payload` outputs
   componentwise (the value difference has no negative coin or asset quantity).
4. **Client:** registration returns the `RequestId`; the depositor signs the deposit tx and
   submits it to L1 — the head does not submit it.
5. **Absorption** — the head observes the deposit on L1 and, once matured, absorbs it at block
   production: the obligations spawn as L2 utxos, keyed by a synthetic genesis id
   ([Opening state](#opening-state) uses the same convention).

`examples/src/main/scala/hydrozoa/examples/demo/SubmitDeposit.scala` (`just deposit`,
DEPLOYMENT.md §5) is the reference deposit client.

### Deposit timing: the head derives the accept-by deadline from the tx TTL

A deposit tx must carry a TTL (a missing TTL fails the parse). From it the head derives

```
validityEnd (accept-by)   = ttl − depositSubmissionDuration
absorption start          = validityEnd + depositSubmissionDuration + depositMaturityDuration
absorption end            = absorption start + depositAbsorptionDuration
refund validity start     = absorption end + silenceDuration
```

(`config/head/multisig/timing/TxTiming.scala`). `validityEnd` is the deadline by which the head
must *learn* the deposit; the `depositSubmissionDuration` gap below the TTL guarantees an accepted
deposit still has that much slack to be submitted and confirmed on L1 before the deposit tx
expires. The deadline is mandatory because the post-dated **refund tx** — the depositor's safety
net if the head never absorbs — anchors on it; no deadline, no refund, and a deposit could be
stuck forever. The head pre-signs the refund as a block effect when it registers the deposit and
submits it once due; the refund pays the deposit back to the `refundDestination` given at
registration. Note the whole schedule anchors on the TTL, not on when the deposit lands on L1 — a
generously late (far-future) TTL directly delays absorption.

## Backend selection and configuration

`HeadParameters.l2Ledger` selects the backend per head: `cardano-eutxo` | `any-remote`
(`config/head/parameters/L2LedgerKind.scala`). It is a peer-agreed head parameter: an in-process
EUTXO ledger and a remote black box are different trust models, so all peers must agree. `Main`
wires the implementation from it — `EutxoL2Ledger` in-process, or `RemoteL2Ledger` driving an
external ledger over `remoteLedgerUri`. The URI is per-node private config (optional): every node
runs its own remote-ledger sidecar, so the endpoint is not shared head config.

Whatever the backend, **determinism is part of the `L2Ledger` contract**: consensus feeds every
peer's replica the same ordered commands, so a non-deterministic backend diverges and breaks
consensus. The EUTXO ledger gets determinism from native tx validation; a remote ledger must
provide it by design.

The read side follows the backend: the L2 query endpoints (`GET /l2/cardano-eutxo/utxos/{address}`,
`GET /l2/cardano-eutxo/transactions`) are mounted only when the node runs the EUTXO ledger — see
[l2-query-endpoints.md](l2-query-endpoints.md).

### Opening state

A head can open with a non-empty L2 ledger instead of depositing after startup. The bootstrap
config carries `initialL2State` — a human-readable list of CIP-0116 outputs (address + value) —
and `BuildHeadConfig` keys it into the `initialEvacuationMap` of the head config once the seed
utxo is resolved. (The evacuation map is the L1 projection of the L2 state: the utxos that would
materialize on L1 if the head settles or evacuates.) Each opening output's input reference is
`(L2Genesis.mkGenesisId(seedInput), i)`: a synthetic tx id hashing the *whole* seed input (tx id +
index), the same convention the ledger uses for deposit-created utxos. Reusing the raw seed tx id
would let `(seedTxId, i)` collide with a real output of the seed's own transaction.

The opening state is an explicit, agreed config field rather than a derived commitment. The
initialization tx commits to it on-chain: the treasury output — the head's L1 utxo holding the
locked funds — carries the opening outputs' total on top of the equity contributions, and its
datum carries the evacuation map's KZG commitment, which every node verifies when it parses the
init tx.

The whitepaper's bootstrap-config section specifies this field as `initialEvacuationMap`, a map
keyed by CBOR-encoded `TransactionInput`s. Those keys derive from the seed utxo, which is only
resolved at build time, so the keyed map cannot be human-authored. The bootstrap config therefore
carries the unkeyed pre-image (`initialL2State`); the *head* config's `InitializationParameters`
carry the derived keyed map, which does match the spec's shape. A deliberate representation
divergence, not a spec change.

## Limitations

- **Identity isomorphism is not yet byte-identical.** The toggle skips the pin check, but
  `L2Tx.utxoPartition` still requires the designation list (label 4937) on every tx, so an
  unmodified L1 tx does not parse. Full identity isomorphism needs a designation default for
  pin-less txs.
- **`any-remote` screening is a passthrough stub.** `RemoteL2Ledger.sendScreenTx` /
  `sendScreenDeposit` accept everything; the screening endpoint on `remoteLedgerUri` is not
  implemented, so nothing screens a remote payload. `any-remote` is not a safe backend until it
  lands.
- **`l2Ledger` and `identityIsomorphism` are pinned by the bootstrap tooling** to `cardano-eutxo`
  and `false` (`bootstrap/Bootstrap.scala`, `mkSharedHeadConfig`); surfacing them as bootstrap
  config fields is pending.
