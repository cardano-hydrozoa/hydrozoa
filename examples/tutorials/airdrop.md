# Tutorial: Airdrop — streamlining L1 operations

## What this example demonstrates

Airdropping a token to many recipients on Cardano L1 normally costs the publisher one transaction
per recipient — every claim is its own on-chain payment, with its own fee, and (if the claim logic
is non-trivial) its own Plutus script execution. That cost scales linearly with the number of
claimers.

This example moves the whole claiming process onto a Hydrozoa head (L2). The publisher mints the
entire token volume once, opens a head over it, and every claim becomes a cheap L2 transaction
inside the head. Only the net result — the actual token payouts — settles back to L1, batched into
the head's ordinary settlement transactions. The publisher pays for one head-initialization tx on
L1, not one transfer per recipient, while keeping the same custodial guarantees the head gives
every participant.

Concretely the demo:

1. Seeds 10,000 airdrop tokens into the head's initial L2 state as a single "pot" utxo owned by
   the publisher (the equivalent of the publisher minting the full supply up front on L1).
2. Boots a live 2-peer head over that pot on an in-process mock L1 — real fast + slow consensus, no
   external services.
3. Runs the claiming process as a chain of L2 transactions: each claim spends the pot, sends a
   small amount (≤ 50 tokens) to a fresh recipient address marked to be withdrawn to L1, and keeps
   the remainder in L2 for the next claim.
4. Lets the head settle: the marked outputs accumulate into the head's block effects and roll out to
   L1 as ordinary settlement/rollout transactions.

The demo asserts, by reading the mock L1 directly, that every one of the 10,000 tokens ends up
withdrawn to a recipient address — full drain, conservation of supply, zero consensus errors.

## Running it

```bash
sbtn "examples/testOnly *AirdropDemo*"
```

It's a real-clock integration run, so it takes a minute or two.

## Walkthrough

The run narrates each step on stdout as `[airdrop-demo]` lines. A typical run:

```
[airdrop-demo] minting 10000 tokens into the head's initial L2 state; 222 claims queued
[airdrop-demo] head hard-confirmed stack 0; airdrop pot is live in L2
[airdrop-demo] init tx landed on L1: true
[airdrop-demo] submitted 50/222 claims
[airdrop-demo] submitted 100/222 claims
[airdrop-demo] submitted 150/222 claims
[airdrop-demo] submitted 200/222 claims
[airdrop-demo] all 222 claims submitted; settling withdrawals on L1
[airdrop-demo] withdrawn to L1: 10000 / 10000 tokens; actor errors=0
```

Step by step:

1. **Mint + seed.** The publisher's full 10,000-token supply is placed into the head's initial L2
   ledger as one pot utxo. In Hydrozoa terms this is the head's *initial evacuation map* — the set
   of utxos the L2 ledger boots with — so the tokens exist on L2 the moment the head starts, backed
   by the head treasury on L1. Because the claim amounts are random (≤ 50), draining the whole
   supply takes ~222 claims here; the exact count varies per run.

2. **Boot.** The head is brought up live and hard-confirms *stack 0* (its initialization). The init
   tx lands on the mock L1 — this is the single L1 transaction the publisher pays for. From here on,
   everything the claimers do happens on L2.

3. **Claim.** Each claim is a normal L2 transaction that spends the current pot and produces two
   outputs:

   - a small payment to a fresh recipient, tagged for *withdrawal* (it leaves the head and lands on
     L1);
   - a change output back to the publisher, tagged to *stay* in L2 so the next claim can spend it.

   These tags are Hydrozoa-standardized metadata on the L2 transaction, identifying which of its
   outputs are L1-bound. All 222 claims are submitted to the head's request sequencer, which orders
   them into blocks.

4. **Settle.** As the head produces major blocks, the withdrawal-tagged outputs are collected and
   rolled out to L1 as ordinary settlement/rollout transactions — many recipient payouts batched
   into a handful of L1 txs. The demo polls the mock L1 until every airdrop token has moved out of
   the treasury and into a recipient address.

5. **Result.** All 10,000 tokens are withdrawn to L1, spread across ~222 distinct recipient
   addresses, having cost the publisher one init tx plus the head's batched settlement — instead of
   222 individual L1 transfers. If the claim logic were a real Plutus validator (the common case),
   that script would run on L2 at head cost too, and only the settled payouts would touch L1.
