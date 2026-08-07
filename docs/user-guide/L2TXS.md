# Building an L2 transaction

An L2 transaction on the reference **EUTXO L2 ledger** is an **ordinary Cardano `Transaction`** —
same bytes, same signing, same script model — with a small amount of mandatory head metadata and a
few conformance rules. You submit the signed tx (its CBOR) to the head in a small JSON request body —
no head-specific signature. It self-authenticates through ordinary vkey witnesses over the tx id.

This guide covers **basic** transactions: spending and sending L2 UTxOs, and **withdrawals** (moving
value back to L1). Minting/burning is a superset — see [L2MINTING.md](L2MINTING.md).

Background — *what an L2 transaction must carry* — is covered below. The `hydrozoa submit-l2-tx`
subcommand
(`src/main/scala/hydrozoa/app/cli/SubmitL2Transaction.scala`) is the runnable worked example.

## Mandatory metadata

Attach **one** auxiliary-data entry at label **`4937`** (the Hydrozoa head-tag label). It reuses the same layout
as Hydrozoa's L1 transactions: the tag points at a role map, keyed by the role `"L2"`, pointing at a
head-id map, pointing at the transaction's fields.

```
4937 → {
  "L2": {
    <headId hex>: {
      "l1BoundOutputs":    List(Int),           // indices of outputs leaving for L1 (withdrawals)
      "l2TransientTokens": Map(Int → <bundle>)  // minting only (see L2MINTING.md); omitted when empty
    }
  }
}
```

- **HeadId pin** — the `<headId hex>` key binds the tx to this head so it can't be replayed against
  another; it's the first thing the ledger screens. Required unless the head runs with identity
  isomorphism. Like the deposit pin, it's authenticated for free via the tx's `auxiliaryDataHash`.
- **`l1BoundOutputs`** — the indices (in output order) of outputs bound for L1, i.e. **withdrawals**.
  Every output whose index is **absent** from this list stays on L2, so a plain send uses the empty
  list. An index must be a non-negative `Int` within the output count, or the tx is rejected.
- **`l2TransientTokens`** — minting only; omit it for a basic tx. See [L2MINTING.md](L2MINTING.md).

## Conformance rules

The ledger strips a tx down to a fixed shape; a tx that violates these is rejected:

- **Zero fee** — `fee = 0`, and inputs must **exactly** balance outputs (value conserved). Build with
  the protocol params' fees zeroed and a pre-balanced diff handler so nothing is left for a fee.
- Outputs are **Babbage**, **inline datums only**, reference scripts only Native or PlutusV3. **Every
  output** — destination, change, and withdrawal alike — must clear the protocol **min-ADA**, or the
  tx is rejected.
- **No** collateral, certificates, protocol withdrawals field, voting/proposal procedures, treasury
  value/donation, bootstrap witnesses, or PlutusV1/V2 scripts. (A withdrawal on L2 is expressed by
  listing the output's index in `l1BoundOutputs` — *not* by Cardano's `withdrawals` body field, which
  must be empty.)
- Outputs should use **Shelley payment addresses** (no delegation part) — the intended L2 shape, and
  what the CLI produces. (The ledger's address-shape check is currently unwired, so a non-conforming
  address is not actually rejected *today*; build to the intended shape regardless.)

## Withdrawals

To move value from L2 back to L1, add the payout output as normal and list its index in the
`4937` **`l1BoundOutputs`** field. On apply, the ledger:

- removes that output from the active L2 UTxO set (it never persists as an L2 UTxO), and
- turns it into a **payout obligation** — the output must clear **min-ADA** or the tx is rejected.

The obligation is settled onto L1 by a later settlement transaction the head produces; the recipient
receives the value at the output's address on L1.

## Steps

1. **Find your L2 UTxOs**: `GET {headUri}/l2/cardano-eutxo/utxos/{yourAddress}`
   (see the [API reference](../api/)).
2. **Build** a native Cardano tx: spend one or more L2 UTxOs; add outputs (destination + change);
   set `fee = 0`; attach the metadata above (empty `l1BoundOutputs` for a plain send, or list any
   withdrawal outputs' indices). Keep inputs and outputs balanced.
3. **Sign** with your wallet's vkey — an ordinary Cardano signature.
4. **Submit** the signed tx's CBOR to the head: `POST {headUri}/head/requests` with a
   `TransactionRequest`. You get back a `requestId` to track it.

## Worked example

`hydrozoa submit-l2-tx` fetches your L2 UTxOs, prompts for a destination and value, builds the tx
(one spend, destination + optional change output, `fee = 0`, metadata = the `4937` `L2` entry with
an empty `l1BoundOutputs` and the headId pin), signs, and submits. To make a withdrawal, list the
payout output's index in `l1BoundOutputs`. Read
`src/main/scala/hydrozoa/app/cli/SubmitL2Transaction.scala` alongside this guide.
