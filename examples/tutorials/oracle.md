# Tutorial: Oracles — publishing a data feed from L2

## What this example demonstrates

Smart contracts on Cardano frequently need access to real-world data: asset prices, exchange rates,
index values, and other signals that originate outside the chain. Delivering this data reliably is
the oracle problem. A Hydrozoa head can serve as a publication channel for oracle data, using its
existing deposit and transaction-request mechanisms with no new infrastructure required.

The mechanism (from the [oracles design](https://gummiworm.net/whitepaper/future-work/cardano/oracles)):

- **Publish.** An oracle operator publishes data by producing an oracle utxo at the head's
  native script address carrying the current data. Any Cardano transaction can reference it as a
  CIP-31 reference input to read the data.
- **Stability window.** The oracle utxo sits at the native script address and cannot be spent
  without head and coil signatures, so it is a stable, contention-free reference target for the
  duration of its lifetime.
- **Authenticate.** An authentication token in the utxo proves provenance — consuming contracts
  trust data only from a utxo bearing the operator's token policy.
- **Refresh.** The operator keeps their auth tokens inside the head (L2). Each refresh is an L2
  transaction request that produces an L1-bound payout to the native script address, carrying an auth
  token and the new data. The head's settlement machinery lands it on L1 — so after the initial
  funding, the whole feed is driven from L2, at head cost, with no repeated external L1 transactions.

For the feed's data, the demo uses the [Hofstadter Q-sequence](https://oeis.org/A005185) —
`Q(1)=Q(2)=1, Q(n)=Q(n−Q(n−1))+Q(n−Q(n−2))` → `1, 1, 2, 3, 3, 4, 5, 5, 6, 6, 6, 8, …`. It's a
self-contained, deterministic stand-in for any real-world feed: each refresh advances the oracle to
the next term.

The demo publishes Q(1)..Q(25), then reads the mock L1 back — acting as a consuming contract — and
asserts every term appears in an oracle utxo bearing the provider's auth token, with the payloads
decoding to exactly the Q-sequence.

## Running it

```bash
sbtn "examples/testOnly *OracleDemo*"
```

It's a real-clock integration run, so it takes a minute or two.

## Walkthrough

A typical run narrates on stdout as `[oracle-demo]` lines:

```
[oracle-demo] publishing the Hofstadter Q-sequence: 25 terms 1, 1, 2, 3, 3, 4, 5, 5, 6, 6, 6, 8, ...
[oracle-demo] head hard-confirmed stack 0; oracle provider funded in L2
[oracle-demo] init tx landed on L1: true
[oracle-demo] submitted 5/25 oracle refreshes
[oracle-demo] submitted 10/25 oracle refreshes
[oracle-demo] submitted 15/25 oracle refreshes
[oracle-demo] submitted 20/25 oracle refreshes
[oracle-demo] submitted 25/25 oracle refreshes
[oracle-demo] all 25 refreshes submitted; settling oracle utxos on L1
[oracle-demo] oracle utxos on L1: 25 / 25; terms=1,1,2,3,3,4,5,5,6,6,6,8,8,8,9,10,10,11,11,12,12,12,12,14,16; actor errors=0
```

Step by step, the example:

1. **Computes the feed.** Generates the first 25 terms of the Hofstadter Q-sequence in-process — the
   data the oracle will publish.

2. **Seeds the operator.** Builds the head's initial L2 state holding the operator's stock of
   authentication tokens (one per refresh) plus ADA to fund each oracle utxo, all in the operator's
   L2 account. The mock L1 is seeded with the funding utxos that back this state.

3. **Builds the publications.** Ahead of time, assembles a chain of 25 signed L2 transactions. Each
   spends the operator's current pot and produces two outputs:

   - an oracle output, sent to the head's native script address, carrying one auth token and the
     next term in its datum, tagged to be withdrawn to L1;
   - a change output that keeps the remaining tokens and ADA in L2 for the next refresh.

4. **Boots the head.** Brings up a live 2-peer head over the seeded state on the in-process mock L1
   (real fast + slow consensus). The head hard-confirms stack 0 and submits its initialization
   transaction, which lands on L1 — the single L1 transaction the operator pays for up front.

5. **Publishes / refreshes.** Submits all 25 publication transactions to the head's request
   sequencer, which orders them into blocks. Each is an ordinary L2 transaction, processed at head
   speed and cost.

6. **Settles to L1.** As the head produces settlement (and rollout) transactions, each oracle output
   rolls out to L1 as a utxo at the native script address, where any Cardano contract can reference it.

7. **Consumes the feed.** Acting as a consuming contract, the demo polls the mock L1 for utxos at the
   native script address bearing the operator's authentication-token policy, and decodes the oracle
   payload from each.

8. **Verifies.** Asserts that the init tx landed, that exactly 25 oracle utxos appeared, that their
   decoded payloads equal exactly Q(1)..Q(25), and that the head produced no consensus errors.

## Notes and future work

- **Token recycling.** The whitepaper's fully-streamlined refresh keeps a *single* auth token
  cycling: each settlement absorbs the previous oracle utxo (returning the token to L2) and produces
  the next one atomically — an internal deposit. That re-absorption is not yet implemented, so
  this demo spends one token of the provider's policy per publication. Since consumers authenticate
  on the token *policy*, not a specific token, the security model is identical from their side.
- **Oracle compartment.** A further streamlining — managing oracle utxos as dedicated treasury
  fragments rather than deposits — depends on fragmented-treasury support (CIP-112 / CIP-118) and is
  out of scope here.
