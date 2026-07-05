# Transient L2 Tokens

## Overview

The EUTXO L2 ledger accepts transactions that **mint and burn tokens**. Minted tokens are
**transient**: they exist only inside the head, scoped to its lifetime, and can never be
withdrawn or evacuated to L1. Every L1-remittable invariant the head had before this feature
still holds, enforced by the same upstream conservation rule.

The whitepaper article *Transient L2 tokens* (`/whitepaper/future-work/cardano/transient-l2-tokens`)
is the source design; this doc describes the implementation in `multisig/ledger/eutxol2/`.

## Terminology

- **main compartment** — the L1-valid utxo set, held in `EutxoL2Ledger.State.activeUtxos`.
  Exactly what the evacuation map reflects; ADA and any L1-native tokens that entered via
  deposits or the initial L2 state.
- **transient-token compartment** (the *overlay*) — `State.transientTokens: TransientTokens`,
  a `Map[TransactionInput, MultiAsset]` associating a main-compartment utxo id with the
  transient bundle it carries. Bundles are `MultiAsset`, so ADA can never be transient by
  construction. Overlay keys are always a subset of main keys.
- **combined view** — the per-utxo sum of both compartments
  (`TransientTokens.mkCombinedUtxos`). The only view the Cardano ledger rules and scripts
  ever see.
- **declaration** — the `transientOutputs` metadata field: per output index, the transient
  content of that output. The declarations are the only thing that tells the compartments
  apart.

Policy ids do **not** differentiate the compartments. The same policy id may simultaneously
name an L1-native token in the main compartment and an L2-minted transient token in the
overlay; nothing anywhere dispatches on policy id.

## Transaction metadata

The head-label metadatum (`Word64(CIP67.Tags.head)`, tag 4937) accepts two shapes, parsed in
`L2Tx.parse` (`eutxol2/tx/L2Tx.scala`):

- **bare list** — `List(Int(1|2) per output)`, the per-output L1-bound/L2-bound markers.
  Means "no transient outputs"; every pre-existing transaction is in this shape.
- **map** — `Map(Text("outputs") -> List(...), Text("transientOutputs") -> ...)`, where the
  `outputs` marker list is required and `transientOutputs` is optional.

The `transientOutputs` codec lives in `eutxol2/tx/TransientOutputs.scala`:

```
Map(Int(outputIndex) -> Map(Bytes(policyId /*28B*/) -> Map(Bytes(assetName /*<=32B*/) -> Int(quantity))))
```

Quantities are in `[1, Long.MaxValue]` (metadata integers are i64, matching the `Long`
quantities of `MultiAsset`). An output index absent from the map carries no transient
tokens; a declared index with an empty bundle is malformed.

Parse-time checks (`L2Tx.parse`, state-independent):

1. every declared index refers to an existing output;
2. **an L1-bound (withdrawal-marked) output must declare nothing** — a withdrawal carrying
   transient tokens is rejected outright, never silently stripped;
3. each declared bundle is a sub-value of its output's assets (component-wise `<=`). This is
   *not* implied by the conservation rules below: a negative asset in one projected output
   could otherwise offset a positive excess in another, so the per-output check is
   independent and required.

## Validation

`HydrozoaTransactionMutator.transit` (`eutxol2/Mutator.scala`) takes and returns
`Compartments(main, transientTokens)`. Conceptually there are three balances; two are
checked, the third follows:

1. **Full transaction, combined view.** The pre-existing validator list — scripts (minting
   policies included), signatures, redeemers, sizes, and `ValueNotConservedUTxOValidator`
   *with* the mint field — runs against the combined utxo set. Minting policies are ordinary
   L2 scripts: native or Plutus V3, inline or via reference inputs, standard policy-id
   derivation, executed by the upstream `PlutusScriptsTransactionMutator` with combined
   resolved values (the `Mint` redeemer tag is allowed by L2 conformance).
2. **L1 projection, main view.** `L2Tx.projectToL1` rebuilds the transaction with the mint
   field stripped and each output's value reduced by its declared bundle.
   `ValueNotConservedUTxOValidator` re-runs on this projection against the main compartment
   alone. This single re-run is the entire enforcement story for the main compartment:
   - minting an L1-native token fails (the minted tokens stay in the projected outputs with
     no input source);
   - burning an L1-native token fails (main inputs exceed projected outputs with no mint
     field to absorb the difference);
   - moving overlay tokens into main without declaring them fails (they surface in the
     projected outputs with no main source).
3. **Transient balance.** `overlay_in + mint = declared transients`. With zero fees and no
   withdrawals/certificates on L2 (both enforced by L2 conformance and load-bearing here),
   balance (1) equals (2) + (3), so any two imply the third. The transient balance has no
   runtime execution path; `TransientTokensConservationTest` pins the implication.

The other validators need no projection run: input resolution on main equals resolution on
combined (overlay keys are a subset of main keys), and min-ADA / value-size on the combined
view are at least as strict as on the projection (monotone in value content; coin is
untouched by the split).

## Mutation

The mutation runs once, over the combined view, and the result splits back into
compartments:

1. the upstream mutator removes spent inputs and adds all outputs (combined values);
2. `EvacuatingMutator` removes the L1-bound outputs (unchanged — those never have overlay
   entries, rejected at parse);
3. the next overlay = previous overlay minus spent inputs, plus `L2Tx.mkTransientUtxos`
   (the declarations keyed by the new utxo ids);
4. the next main = the combined result minus the next overlay
   (`TransientTokens.projectMainUtxos`).

Unspent overlaid utxos keep their combined values through step 1, so subtracting the
carried-over overlay restores their main values exactly.

## Ledger integration

`activeUtxos` keeps its meaning (main compartment), so everything derived from it is
untouched:

- `EutxoL2Ledger.sendApplyTransaction` derives `EvacuationDiff`s as the symmetric difference
  over `activeUtxos` — diffs carry projected values only;
- `Payout.Obligation`s come from the transaction's L1-bound outputs as written, which equal
  their projected values because declarations on them are rejected;
- the slow side (JointLedger's `BlockResult.evacuationMapDiff`, StackComposer's cumulative
  evacuation map, StackEffectsBuilder's KZG at majors and last-of-partition minors, every
  SEC) needs zero changes — the evacuation map can never contain a transient token because
  the diff stream never mentions one.

On evacuation the overlay is simply irrelevant: the main compartment is remitted and the
transient tokens cease to exist, per the source design.

## Persistence

`L2Snapshot` carries `transientTokens` (list-of-pairs JSON, `MultiAsset` via the CBOR-hex
codec in `L2StoreCodecs`). A snapshot without the field decodes to an empty overlay — no
overlay could have existed before the field did. The command log is unchanged: declarations
ride inside the opaque `ApplyTransaction` payload, and the recovery re-fold reproduces the
overlay through the same `applyMutation` the live path uses.

## Tests

All under `src/test/scala/hydrozoa/multisig/ledger/eutxol2/`; the shared fixture
`L2TxFixtures` provides a deterministic config, a native single-key policy, a compiled
always-valid Plutus V3 policy (the repo's first use of the Scalus compiler plugin), and
`buildSignedL2Tx`.

| suite | pins |
|---|---|
| `TransientTokensTest` | `projectMainUtxos` inverts `mkCombinedUtxos`; foreign overlay keys ignored |
| `TransientOutputsCodecTest` | metadatum round-trip; malformed-shape rejections |
| `TransientTokensMutatorTest` | mint/burn/pass-through happy paths (native + Plutus V3, inline + reference script); the projection rejections; same policy id in both compartments; legacy metadata |
| `TransientTokensConservationTest` | the transient balance on every accepted tx; one-token perturbations rejected in both directions |
| `TransientTokensEvacuationPurityTest` | diffs and payouts carry projected values at the `sendApplyTransaction` level |
| `EutxoL2LedgerRecoveryTest` / `RocksDbL2StoreTest` | overlay across a snapshot boundary; codec round-trip and missing-field default |

The end-to-end demo is `examples/.../TransientTokenDemo.scala` (tutorial:
`examples/tutorials/transient-tokens.md`): a live two-peer head mints, circulates, and burns
a transient supply; a withdrawal carrying the tokens is rejected; only the backing ADA lands
on L1.

## Not implemented

The finalization gate — refusing to produce a Final block while the transient-token
compartment is non-empty — is specified in the whitepaper article but deliberately out of
scope for this branch. Until it exists, a normal closure with outstanding transient tokens
behaves like an evacuation with respect to them: the tokens vanish and holders receive only
the backing value.
