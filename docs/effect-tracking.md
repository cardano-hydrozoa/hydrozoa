# Effect tracking: linking blocks and requests to their L1 effects

The Head API answers two effect questions:

- **per block** — `GET /head/blocks/{n}/effects` (and per-kind sub-resources), plus the reverse
  lookup `GET /head/effects/{l1TxId}`;
- **per request** — the `relatedEffects` field on `GET /head/requests/{id}`: the L1 effect tx(s) a
  request became.

L1 effects are produced on the **slow side**, per *stack* and per *partition* (see
[`slow-consensus.md`](slow-consensus.md)). The API addresses them per *block* and per *request*. This
doc describes the bridge: how effects are identified, decomposed onto blocks, and linked back to the
requests that caused them — including the withdrawal case, which needs a consensus-safe side-channel.

## Effect identity

Every effect is addressed by an `l1TxId` (a `TransactionHash`):

- a real L1 tx (initialization, settlement, fallback, rollout, finalization, refund) uses its
  **`tx.id`** (the hash of the tx body);
- a **standalone evacuation commitment (SEC)** is not a transaction, so it uses a synthetic id:
  `blake2b_256(sec.header)` (`EffectIds.secL1TxId`). `sec.header` is the on-chain commitment record,
  not a block header despite the name.

`EffectIds.allL1TxIds(hardConfirmed)` enumerates every effect id a hard-confirmed stack carries, in a
stable order. `EffectKind` names the kinds: `Initialization, Settlement, Fallback, Rollout,
Finalization, Refund, Sec`.

Because `tx.id` is the hash of the tx *body*, it is stable from unsigned → signed (multisig
witnesses do not change it). So an effect's id computed at stack close equals the id persisted at
hard-confirmation.

## Reverse-index column families

All effect queries resolve through small reverse-index CFs (see
[`persistence-and-crash-recovery`](../design/persistence-and-crash-recovery.md) §7 for the CF model).
Each is keyed for its query and written in the same atomic batch as the data it indexes; none is read
by recovery — they exist only to serve these queries.

| CF | key → value | written by / when |
|---|---|---|
| `RequestBlockIndex` | request id (packed i64) → `(blockNum, validity)` | `JointLedger`, in the block bundle — the block that locally processed the request |
| `DepositAbsorptionIndex` | deposit request id (packed i64) → `blockNum` | `JointLedger`, in the block bundle — the **major** block that absorbed the deposit |
| `WithdrawalEffectIndex` | `(request id i64 : 8)(l1TxId : 32)` → *(empty)* | `StackComposer` at stack close — one row per (withdrawing request, paying effect); prefix-scan by request id yields many effects |
| `BlockStackIndex` | `blockNum` → `stackNum` | `SlowConsensusActor` at hard-confirmation |
| `EffectStackIndex` | `l1TxId` → `stackNum` | `SlowConsensusActor` at hard-confirmation — backs `GET /head/effects/{l1TxId}` |

The request/deposit indices are single-valued; `WithdrawalEffectIndex` is **multi-valued** (one
request → many effects), so both ids live in the key and the value is empty — a prefix scan by the
8-byte request-id prefix returns every paying `l1TxId` (the trailing 32 bytes), with no
read-modify-write.

## Block-effects decomposition

Effects persist per stack, structured per **partition**; a partition is a *run of blocks*
(`StackPartition.partition`: a leading run of minors; a major + its trailing same-version minors; the
final block alone). `EffectsResolver` decomposes a hard-confirmed stack's partition effects back onto
individual blocks by replaying that same partition grouping over the stack's block briefs
(`partitionGroups`) and attributing each effect:

- **SEC** → its own `commitment.blockNum` (self-identified — the last minor of a run);
- **settlement / fallback / rollouts** → the partition **opener** (the major block);
- **finalization** (+ its rollouts) → the final block;
- **post-dated refund** → the block where its deposit was **registered**, via
  `RequestBlockIndex[refund.requestId]` (`attributeRefund`).

`resolveStack(stackNum)` returns all of a stack's effects with per-block attribution;
`blockEffects(blockNum)` filters to one block; `effectById(l1TxId)` resolves a single effect via
`EffectStackIndex → resolveStack → find`. These back the block-effects endpoints and the
`/head/effects/{l1TxId}` reverse lookup. `docs/openapi.yaml` is the authoritative endpoint list.

## `relatedEffects`: request → effects

`EffectsResolver.relatedEffects(requestId)` dispatches on the request type. Effects only resolve once
the covering stack is hard-confirmed (a not-yet-confirmed stack has no effects), so `relatedEffects`
is empty until then.

**Transaction.** Its carriers are the effects of its inclusion-block partition — the **SEC** for a
minor partition, the **settlement** for a major, the **finalization** for the final. Plus, if the tx
withdrew, the effect tx(s) that pay its L1-bound outputs (see the withdrawal section). The two are
unioned and **deduped by `l1TxId`** (a withdrawal's settlement is also a carrier when settled in the
same partition).

**Deposit.** Two links, disjoint from the transaction path:

- its **post-dated refund** — the `RefundTx.PostDated` in the deposit's *registration*-block
  partition whose `requestId` matches. The refund carries the request id and is attributed to the
  registration block, so `RequestBlockIndex` reaches it with no extra storage.
- once absorbed, the **settlement** of its *absorbing* block's partition. The absorbing block is a
  later major block, distinct from the registration block, so it is not derivable from
  `RequestBlockIndex` — `DepositAbsorptionIndex` records it, and the resolver reads that block's
  partition settlement.

## Withdrawal effect tracking

An L2 transaction can mark several outputs L1-bound (withdrawals); they leave the L2 active set and
are paid out on L1 by a **settlement** tx and, on overflow, a chain of **rollout** txs — or, for the
final block's own withdrawals, the **finalization** tx and its rollouts. One tx's withdrawals can
fan out across several of these effects. `WithdrawalEffectIndex` records the request → effect links so
`relatedEffects` can surface them, including when the paying settlement lands in a *later* stack than
the tx's inclusion block (which the resolver cannot forward-walk otherwise).

### The consensus invariant

Payout obligations are consensus-critical. A `Payout.Obligation` is just a
`KeepRaw[TransactionOutput]`, and its `utxo.value` / `utxo.raw.length` feed **three** committed
things: the effect-tx outputs (`Send(obligation.utxo.value)` — multisigned and submitted on L1, so
every peer must build byte-identical txs), the evacuation-map **KZG commitment** (committed into the
settlement treasury datum, the fallback vote datum, and the SEC header, enforced by the rule-based
scripts), and the size-based packing decision (`outputSize` drives which obligation lands in which
tx). `Payout.Obligation` also crosses the remote-ledger WebSocket boundary.

So **all request-provenance is a pure side-channel**: it never touches `Payout.Obligation`, the
evacuation map, the KZG commitment, an effect-tx body, or the obligation ordering. It rides on
new *local-only* fields plus a sibling CF, deterministic across peers — which the builder/codec
property tests confirm leaves tx bytes byte-identical.

### Provenance capture (ledger-agnostic)

Each `L2LedgerCommand.ApplyTransaction` carries a `requestId`, and by the `L2Ledger` contract every
payout it produces belongs to that request — for the built-in EUTXO ledger and the remote backend
alike. So at `L2Ledger.fromApplyTransaction`, `L2LedgerState` carries a parallel `payoutRequestIds`
vector next to `payouts`:

```
payouts          ++ res._2
payoutRequestIds ++ Vector.fill(res._2.length)(req.requestId)
```

The two grow in the same expression, so `payouts(i) ↔ payoutRequestIds(i)` by construction;
`L2LedgerState`'s private constructor enforces the invariant (you cannot add a payout without its
tag). It flows `L2LedgerState.payoutRequestIds → BlockResult.payoutRequestIds` (per block). No
per-output correspondence is needed — the tag is per-command, expanded to per-payout here.

### Where each payout lands: packing and `payoutOffset`

The payout vector is per **block** (a settlement runs over `major.payoutObligations`; the final tx
over `finalBlockWithdrawals ++ residualBalances`). It splits in two steps:

1. **Size-pack into a rollout chain** (`RolloutTx.AddPayouts.go`). Each tx has a byte budget ≈
   `maxTxSize − dummySignatures − nativeScriptSize − (rolloutOutput, if any) − 500`. The packer walks
   the vector in order, summing each `outputSize`, filling a tx until the next obligation would
   overflow, then starting a new one — cutting the vector into **contiguous size-bounded chunks**,
   one per tx. Membership is a pure function of (order, `outputSize`, budget), so every peer cuts
   identically.
2. **The settlement / finalization tx absorbs the leftover chunk if it fits** (`TryMerge`): it pulls
   the first rollout's (smallest, leftover) chunk directly into itself and re-balances — `Merged` if
   it fits (that rollout drops out), else `NotMerged` (the treasury tx pays nothing directly).

Each effect tx therefore owns one contiguous `[payoutOffset, payoutOffset + payoutCount)` of the
block's vector. `payoutOffset` is recorded as an **off-tx** field on `RolloutTx.Last/NotLast`,
`SettlementTx.WithPayouts`, and `FinalizationTx.WithPayouts` (alongside the existing `payoutCount`),
computed as `total − nePayoutObligationsRemaining.length` (the build was handed a suffix of the
original vector, so its chunk starts where the suffix starts). Being off-tx, it never affects the tx
bytes; the persistence codecs carry it since it is deterministic. `payoutCount` alone cannot say
*which* chunk a tx holds — the offset can.

**Worked example** — 1000 payouts (indices `0..999`), ~300 fitting per tx → chunks `300+300+300+100`.
The settlement tries to absorb the 100-chunk:

`Merged` (settlement takes the leftover chunk) — 4 effect txs:

| effect tx | `payoutOffset` | `payoutCount` | pays |
|---|--:|--:|---|
| settlement (direct) | 900 | 100 | 900..999 |
| rollout #1 | 600 | 300 | 600..899 |
| rollout #2 | 300 | 300 | 300..599 |
| rollout #3 | 0 | 300 | 0..299 |

`NotMerged` (settlement pays nothing directly) — 5 effect txs, the settlement with `payoutCount = 0`
(contributing no rows) and four rollouts at offsets `900/600/300/0`.

The chain is assembled back-to-front (the tail rollout is built first from the full vector and takes
`[0, k)`), which is why offset `0` lands on the *last* rollout. Chunk sizes vary in practice (the tail
rollout has no rollout output, so a slightly larger budget); the offsets/counts are **read off** the
built txs, never assumed.

### Computing and persisting `withdrawalTracking`

`StackEffectsBuilder.mkEffectsRegular` holds both the ordered obligations and the built tx-seq (each
effect tx's `payoutOffset`/`payoutCount` and final `tx.id`). Per partition its `trackWithdrawals`
helper builds a `provenance` vector — settlement: `major.payoutRequestIds.map(Some)`; finalization:
`fin.payoutRequestIds.map(Some) ++ Vector.fill(residualCount)(None)` — and slices it per effect tx:

```
provenance.slice(payoutOffset, payoutOffset + payoutCount).flatten.distinct.map(r => (r, l1TxId))
```

One `(requestId, l1TxId)` link per distinct withdrawing request in the tx's chunk; residual
(finalization-only, `None`) positions produce none. The resulting `withdrawalTracking` is returned as
a **side value** — not inside the hard-ack-signed `StackEffects`, which would break multisig.
`StackComposer` threads it through `ComposedStack` and writes it into `WithdrawalEffectIndex` at
**stack close** (`persistOwnStackClose`), where it is locally in hand; reaching hard-confirmation would
mean threading it through the consensus rounds, and the resolver gates on hard-confirmation via
`EffectStackIndex` anyway.

`ConsensusStoreReader.withdrawalEffects(requestId)` prefix-scans the CF; the resolver resolves each
`l1TxId` via `effectById` and unions them into a transaction's `relatedEffects`.

### The finalization payout fix

Withdrawals in a regular block force a **major** block, so they are settled by that block's
settlement. The **final** block is the exception: it is not a major, so its own withdrawals were
previously dropped — `completeBlockFinal` set `payoutObligations = Nil`, and finalization paid only
the residual evacuation-map drain. They are now wired through: `completeBlockFinal` populates
`payoutObligations` from `p.l2LedgerState.payouts`, and the `StackEffectsBuilder` Final branch pays
`finalBlockWithdrawals ++ residual`.

The residual must be the **true post-final** evacuation map, not the pre-final one. A final block's
window can include L2 transactions that spend pre-existing evacuation-map utxos (nothing forces such
a window to be drain-only), so the final block — like a major — emits its own `evacuationMapDiff`
(`p.l2LedgerState.diffs`, the window's L2 mutations), and the Final branch folds it into the running
map before draining: `residual = applyDiffs(runningMap, fin.evacuationMapDiff).outputs`. This deletes
each withdrawal's spent inputs and adds its change outputs, so a withdrawn utxo is not paid twice
(once as a residual, once via `payoutObligations`); the withdrawal outputs themselves are L1-bound and
were never map members. The fast side still does not maintain the *cumulative* map — the whole-head
drain is the running map read off after this diff, then reset to empty.

This changes finalization-tx bytes, so it is consensus-relevant, but deterministic across peers; it
is a no-op when the final block has no L2 activity, and balances by construction (those withdrawals
were never removed from the treasury, since the final block never got settled).

## Scope and limitations

- **Any L2 ledger.** Provenance is captured at the ledger-agnostic `L2Ledger` command boundary, so
  withdrawal tracking works for the built-in EUTXO ledger and the remote backend.
- **Explicit withdrawals only.** Finalization also pays out *residual L2 balances* (everyone's
  leftover utxos); those are not requests and are not tracked (their `provenance` entries are `None`).
- **Request → withdrawal effects is stored; transaction carriers and deposit refunds are query-time.**
  The stored `WithdrawalEffectIndex` and `DepositAbsorptionIndex` exist because those links reach a
  block the request's `RequestBlockIndex` entry does not; the transaction-carrier and deposit-refund
  links fall out of `RequestBlockIndex` + the partition decomposition with no extra storage.
