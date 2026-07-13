# Transient L2 tokens

*Runs with:* `sbtn "examples/testOnly *TransientTokenDemo*"` — fully in-process against a mock L1.

## What this demo shows

Hydrozoa's L2 ledger accepts Cardano transactions, but until now it prohibited minting and burning:
every asset inside the head had to be a real L1 asset, because the head must always be able to
*evacuate* — remit its entire L2 state back to L1 in one forced settlement. A token minted inside
the head has no L1 existence and could never be part of that settlement.

**Transient tokens** lift the prohibition by making the scope explicit. A transient token is minted
inside the head and scoped to the head's lifetime:

- it lives in a dedicated **transient-token compartment** — a per-utxo overlay on top of the main
  (L1-valid) compartment;
- scripts and ledger rules see the **combined** value, so a transient token behaves like any other
  asset inside L2 — it can gate spending, ride along in trades, be split and merged;
- it can never leave: the evacuation map only ever reflects the main compartment, and a withdrawal
  carrying transient tokens is **rejected**;
- to close the loop, holders burn the tokens and withdraw the L1-valid value that backed them.

## How the ledger tells the compartments apart

Policy IDs do *not* differentiate the compartments — the same policy ID may simultaneously name an
L1-native token (that entered via a deposit) and an L2-minted transient token. Instead, the
transaction itself declares which output tokens are transient, via a new optional metadata field
under the head's label:

```
{HYDR}: {
  "outputs":          [1|2, ...]                      # per-output L1/L2 marker, as before
  "transientOutputs": { outputIndex: { policyId: { assetName: quantity } } }
}
```

Validation then runs twice over two views of the same transaction:

1. **the full transaction** — scripts (minting policies included), signatures, and value
   conservation *with* the mint field — against the combined utxo values;
2. **the main projection** — the mint field stripped, each output reduced by its declared transient
   content — must balance against the main compartment alone.

The projection rule is the entire enforcement story. Minting an L1-native token fails it (the
minted tokens remain in the projected outputs with no input source); burning an L1-native token
fails it the other way; smuggling transient tokens out of the overlay without declaring them fails
it too. No policy-ID checks exist anywhere.

## The demo, step by step

The demo boots a real two-peer head over a plain 100-ADA pot (seeded into the head's initial L2
state), with genuine fast + slow consensus against a mock L1. Watch the `[transient-demo] ...`
lines:

1. **Boot.** The head initializes; stack 0 hard-confirms; the init tx lands on the mock L1. The
   pot utxo exists only in L2.
2. **Rejected withdrawal (submitted first).** A withdrawal marked `1` (L1-bound) whose output
   declares the transient bundle. The head rejects it at validation — transient tokens cannot
   leave the head. Its input stays unspent, which is what lets the rest of the chain proceed: the
   later burn spends the same utxo, so the burn's success doubles as proof of this rejection.
3. **Mint.** The issuer submits an L2 transaction minting 1,000 DEMO under a native single-key
   policy. The output declares the whole bundle transient; the ledger stores the ADA in the main
   compartment and the DEMO in the overlay.
4. **Circulate.** The tokens move issuer → holder → issuer through ordinary L2 transactions. Each
   hop re-declares the bundle on its output — pass-through needs no mint field, just declarations.
5. **Burn.** The issuer burns the full supply (`mint = -1000`, no declarations). The overlay
   entry disappears; the backing ADA remains in the main compartment.
6. **Withdraw.** A plain withdrawal sends the freed ADA to a recipient. The slow consensus settles
   it onto L1; the demo polls the mock chain until the pot lands.

The final assertions read the mock L1 directly: the recipient holds the backing ADA, **no token of
any kind appears on L1**, and the head produced no actor errors.

## What to take away

- Minting/burning on L2 uses the standard Cardano mechanisms — mint fields, native or Plutus V3
  policies, redeemers — with one addition: the `transientOutputs` declaration.
- The evacuation invariant is preserved *by construction*: the main compartment is validated by
  the same battle-tested conservation rule that validated it before this feature existed.
- Applications issuing transient tokens should provide a redemption path (like the burn here), so
  holders can exit cleanly; on a forced evacuation, transient tokens vanish and holders receive
  the L1-valid value that backed their utxos.
