# Building a deposit

A deposit moves value from L1 into a head's L2 ledger. For the reference **EUTXO L2 ledger**, a
deposit is two artifacts that travel together:

- an **`l2Payload`** — the L2 UTxOs the deposit will spawn once the head absorbs it (out-of-band data);
- an **`l1Payload`** — an ordinary Cardano deposit transaction whose metadata carries the
  `blake2b_256` **hash** of the `l2Payload`, pinning one to the other.

The head screens the deposit, then you sign and submit the L1 tx yourself. The head never submits it.

Background (the *why*, and the pin's role): *a deposit is pinned to its L2 payload*. The packaged
`hydrozoa submit-deposit` subcommand
(`src/main/scala/hydrozoa/app/cli/SubmitDeposit.scala`) is the runnable reference client, and
[DEPLOYMENT.md](DEPLOYMENT.md) § *Deposit into the head* is the end-to-end walkthrough.

## 1. Build the `l2Payload`

The payload is a CBOR list of **`GenesisObligation`s** — one per L2 UTxO to spawn. Each obligation
is the well-formed shape of an L2-conformant output: payment address (Shelley, **no delegation
part**), network, optional inline datum, value, optional native/PlutusV3 reference script.

Build one obligation per intended L2 output, then serialize:

```scala
// GenesisObligation.serialize(NonEmptyList[GenesisObligation]): ByteString
//   = CBOR of a list of Babbage outputs (borer). See eutxol2/tx/L2Genesis.scala.
val l2Payload: ByteString = GenesisObligation.serialize(obligations)
```

The total value across the obligations is the L2 value the deposit delivers; the deposit's L1 output
must cover it (below).

## 2. Build the deposit L1 transaction

The deposit tx is an ordinary Cardano transaction with these head-specific requirements:

- **Deposit output** — a Babbage output at the head's multisig address, value = `l2Value +
  depositFee`, with an **inline datum** `DepositUtxo.Datum(Refund.Instructions.Onchain(...))` (the
  refund instructions in their on-chain form). The refund
  instructions (address + optional datum + refund-start time) let anyone reclaim the funds if the
  deposit is never absorbed; they live in the datum, not the metadata, so the refund cannot be
  intercepted.
- **Reference input** — reference the head's multisig-regime UTxO, so the deposit rolls back cleanly
  if the head's initialization is rolled back. Get it from `GET /head/info` — the
  `multisigRegimeUtxo` field (`{transaction_id, index}`). That same endpoint also serves the
  `headAddress`, `headId`, and `submissionDurationSeconds` the other requirements here need.
- **Validity (TTL)** — set `ValidityEndSlot` to the deposit **submission deadline**. The head
  derives the accept-by deadline from the TTL: `acceptBy = ttl − depositSubmissionDuration`. A
  missing or malformed TTL is rejected.
- **Metadata — the pin.** Under the head metadata label **`4937`** (the `HYDR` tag), nested by
  tx-type and head id:

  ```text
  4937 → { "Deposit" → { <headId hex> → {
            "depositIx":     Int,    // output index of the deposit output (0 in the CLI)
            "depositFee":    Int,    // the deposit fee, in lovelace
            "l2PayloadHash": Bytes,  // blake2b_256(l2Payload) — the pin
          } } }
  ```

  The pin is `blake2b_256` over the **exact `l2Payload` bytes** from step 1. It needs no separate
  signature: metadata is covered by the tx body's `auxiliaryDataHash`, so signing the deposit tx
  commits to the pin. The head recomputes `blake2b_256(l2Payload)` on receipt and rejects a mismatch.

Value sizing: the deposit value must exceed the refund UTxO's min-ADA plus the maximum refund-tx fee,
so a refund can always be paid. The reference client builds the post-dated refund tx alongside the
deposit to guarantee this.

## 3. Register, sign, and submit

1. **Register** the deposit with the head so it screens the payload and schedules absorption:
   `POST {headUri}/head/requests` with a `DepositRequest` carrying `l1Payload` (the **unsigned**
   deposit-tx CBOR) and `l2Payload` (from step 1). The head checks the pin and the accept-by
   deadline; a rejection here is soft — fix and retry.
2. **Sign** the deposit tx with your wallet key and **submit it to L1 yourself** (e.g. via
   Blockfrost). The head does not submit deposits.
3. **Wait** for the deposit UTxO to appear on L1 and for the head to absorb it; the L2 UTxOs
   described by your `l2Payload` then appear in the L2 ledger. You can watch L2 state via
   `GET /l2/cardano-eutxo/utxos/{address}` (see the [API reference](../api/)).

## Worked example

`hydrozoa submit-deposit` does all of the above interactively: it prompts for a funding UTxO and the
L2 outputs, serializes the `l2Payload`, builds the deposit+refund pair (writing the metadata pin),
registers with the head, then signs and submits to L1 and polls until the deposit lands. Read
`src/main/scala/hydrozoa/app/cli/SubmitDeposit.scala` alongside this guide, and
[DEPLOYMENT.md](DEPLOYMENT.md) § *Deposit into the head* for an example session.
