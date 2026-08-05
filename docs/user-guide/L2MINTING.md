# Minting and burning tokens on L2

The reference **EUTXO L2 ledger** lets a head mint and burn **transient tokens** — tokens that live
only inside the head and can never reach L1. They ride on top of an ordinary L2 transaction: a normal
Cardano `mint` field plus a minting policy, and a metadata declaration that marks which minted tokens
are transient.

Read [L2TXS.md](L2TXS.md) first — a minting tx is a basic L2 tx with extra metadata. Background and
the full design: [`../spec/transient-tokens.md`](../spec/transient-tokens.md). A complete worked
example lives in [`examples/tutorials/transient-tokens.md`](https://github.com/cardano-hydrozoa/hydrozoa/blob/main/examples/tutorials/transient-tokens.md)
(driven by `examples/src/test/scala/hydrozoa/examples/transient/TransientTokenDemo.scala`).

## The model: two compartments

The L2 ledger keeps each UTxO's value in two compartments:

- the **main compartment** — the L1-valid value (ADA and L1-native tokens), exactly what a head
  closure would pay out; and
- the **transient compartment** — an overlay of L2-minted tokens keyed by UTxO. **ADA is never
  transient.**

Cardano ledger rules and your scripts only ever see the **combined** view (main + transient). The
`transientOutputs` metadata declaration is the *only* thing that tells the two apart — nothing keys
off the policy id, so the same policy id can name an L1-native token in main and a transient token in
the overlay at once.

## What you can mint or burn

Only **transient** tokens. The rules, enforced by value conservation (no policy-id check anywhere):

- **Mint** (`mint` positive) or **burn** (`mint` negative) tokens that you declare as transient.
- You **cannot** mint or burn L1-native (main-compartment) tokens — the arithmetic makes it
  impossible.
- Per transaction: `overlay_in + mint = total declared transient`. (This balances for free because L2
  forces `fee = 0` and forbids reward withdrawals and certificates, so the combined-view and
  main-projection conservation checks together pin the transient delta.)
- Each declared bundle must be **≤** the assets actually on that output.
- A **withdrawal** output (marked `Int(1)`; see [L2TXS.md](L2TXS.md)) **must not** declare transient
  tokens — transient tokens cannot leave the head.

## Metadata: declaring transient outputs

A minting tx uses the **map** shape of the `4937` output-designation metadatum (the bare-list shape
is for non-minting txs only), plus the `4936` headId pin from [L2TXS.md](L2TXS.md):

```text
4937 → {
  "outputs":          List(Int, …),   // one L1/L2 marker per output (Int(1)=withdrawal, Int(2)=L2)
  "transientOutputs": <declarations>,  // which minted tokens are transient, per output
}
4936 → Text(<headId hex>)
```

The `transientOutputs` declarations map an output index to the transient bundle it carries:

```text
transientOutputs = Map(
  Int(outputIndex) → Map(
    Bytes(policyId /* 28 bytes */) → Map(
      Bytes(assetName /* ≤ 32 bytes */) → Int(quantity /* 1 … Long.MaxValue (i64) */)
    )
  )
)
```

An index with no declaration carries no transient tokens; a declared index with an empty bundle is
malformed. The overlay is keyed by the *new* UTxO id, so if you move transient tokens to another L2
output in a later tx, you must **re-declare** them there.

## Value conservation (why a mint is accepted)

The ledger runs conservation **twice**:

1. against the **combined** view — the full Cardano check including the `mint` field, scripts, and
   signatures; and
2. against the **main projection** — the tx rebuilt with `mint = None` and every output reduced by
   its declared transient bundle. This second run is what forbids minting/burning L1-native tokens or
   smuggling overlay tokens into the main compartment.

So: keep ADA and L1-native value conserved in the main projection, and let the `mint` field plus the
`transientOutputs` declarations account for exactly the transient delta.

## Preparing a minting tx

1. Build a basic L2 tx (spend an L2 UTxO, add outputs, `fee = 0`) — but **spend/quote values in the
   combined view** (main + any transient the input already holds). ⚠️ The L2 utxo query
   (`GET /l2/cardano-eutxo/utxos/{address}`) returns **main-compartment value only** — the transient
   tokens overlaid on a utxo are *not* shown. You must track a utxo's transient content yourself
   (from what you minted or last declared onto it) and add it in, or the combined view won't balance
   and the tx is rejected.
2. Add a Cardano **`mint`** step under your minting policy (native or PlutusV3): positive to mint,
   negative to burn.
3. Put the minted (or remaining) transient tokens on an **L2-bound** output (`Int(2)`), and declare
   them in `transientOutputs` for that output index.
4. Attach the `4937` map metadatum (`"outputs"` + `"transientOutputs"`) and the `4936` headId pin.
5. Sign (the policy may require specific signatures) and submit as in [L2TXS.md](L2TXS.md).

To **burn**, spend the UTxO holding the transient tokens, set `mint` negative for the burned amount,
and declare only what remains (or `transientOutputs = {}` if none remains).

## Caveat: closing a head with live transient tokens

There is currently **no finalization gate** that blocks closing a head while transient tokens are
outstanding. If a head closes with a non-empty transient compartment, those tokens simply cease to
exist — holders receive only the backing ADA of the UTxO, not the transient tokens. Burn transient
tokens back before closing if their disappearance would matter. See
[`../spec/transient-tokens.md`](../spec/transient-tokens.md) (§ Not implemented).
