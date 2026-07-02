# Tutorial: Oracles — publishing a data feed from L2

## What this example demonstrates

Smart contracts on Cardano frequently need access to real-world data: asset prices, exchange rates,
index values, and other signals that originate outside the chain. Delivering this data reliably is
the oracle problem. A Hydrozoa head can serve as a publication channel for oracle data, using its
existing deposit and transaction-request mechanisms with no new infrastructure required.

The mechanism (from the [oracles design](https://gummiworm.net/whitepaper/future-work/cardano/oracles)):

- **Publish.** An oracle operator publishes data by producing an oracle utxo at the head's native
  script address — structurally a deposit — carrying the current data. Any Cardano transaction can
  reference it as a CIP-31 reference input to read the data.
- **Stability window.** A deposit is not absorbed by the head until it is *mature*. Throughout that
  maturity window the oracle utxo sits on L1 and cannot be spent (the native script requires head
  and coil signatures, and the head will not sign an early-absorbing settlement) — a guaranteed,
  contention-free window in which contracts can reference it.
- **Authenticate.** An authentication token in the utxo proves provenance — consuming contracts
  trust data only from a utxo bearing the operator's token policy.
- **Refresh.** When the head absorbs the oracle deposit, its data leaves L1. To refresh, the operator
  submits an L2 transaction request that produces an L1-bound payout back to the native script
  address with the next value — an internal deposit, which starts a fresh stability window. A single
  auth token cycles L1 → L2 → L1 forever; after the initial funding the whole feed is driven from
  L2, at head cost, with no repeated external L1 transactions.

For the feed's data, the demo uses the [Hofstadter Q-sequence](https://oeis.org/A005185) —
`Q(1)=Q(2)=1, Q(n)=Q(n−Q(n−1))+Q(n−Q(n−2))` → `1, 1, 2, 3, 3, 4, …`. It's a self-contained,
deterministic stand-in for any real-world feed: each refresh advances the oracle to the next term.

The demo publishes term 1 as an external deposit and terms 2..N as internal deposits, cycling one
auth token. For each term it confirms the oracle utxo appeared on L1 (its stability window) and was
then absorbed, and asserts the observed feed equals the Q-sequence exactly, with no consensus errors.

## Running it

```bash
sbtn "examples/testOnly *OracleDemo*"
```

It's a real-clock integration run that exercises the full deposit lifecycle (with the maturity window
shortened to ~10s so the windows are watchable), so it takes a few minutes.

## Walkthrough

A typical run narrates on stdout as `[oracle-demo]` lines:

```
[oracle-demo] oracle feed = Hofstadter Q-sequence: 1, 1, 2, 3, 3, 4
[oracle-demo] head hard-confirmed stack 0; L2 empty, provider funded on L1
[oracle-demo] init tx landed on L1: true
[oracle-demo] Q(1)=1 (external): external deposit submitted; awaiting its stability window on L1
[oracle-demo] Q(1)=1 (external): present on L1 (stability window open, referenceable)
[oracle-demo] Q(1)=1 (external): absorbed by settlement (window closed)
[oracle-demo] Q(2)=1 (internal): refresh payout submitted; awaiting settlement onto L1
[oracle-demo] Q(2)=1 (internal): present on L1 (stability window open, referenceable)
[oracle-demo] Q(2)=1 (internal): registered as internal deposit; awaiting absorption
[oracle-demo] Q(2)=1 (internal): absorbed by settlement (window closed)
[oracle-demo] Q(3)=2 (internal): refresh payout submitted; awaiting settlement onto L1
... (Q(4)..Q(6) likewise) ...
[oracle-demo] published all 6 terms through external+internal deposits; observed feed=1,1,2,3,3,4; actor errors=0
```

Step by step, the example:

1. **Computes the feed.** Generates the first terms of the Hofstadter Q-sequence in-process — the
   data the oracle will publish.

2. **Funds the operator on L1.** Seeds the operator's single authentication token plus some ADA into
   a utxo on the mock L1. The head boots with an empty L2 ledger; the token enters the head via the
   first deposit.

3. **Boots the head.** Brings up a live 2-peer head on the in-process mock L1 (real fast + slow
   consensus). It hard-confirms stack 0 and lands its initialization transaction — the single L1
   transaction the operator pays for up front.

4. **Publishes term 1 as an external deposit.** Builds and submits a deposit transaction carrying the
   auth token + Q(1). The deposit sits on L1 through its ~10s maturity window — its stability window,
   during which a contract could reference it — and is then absorbed by a settlement, which moves the
   token into the operator's L2 account.

5. **Refreshes terms 2..N as internal deposits.** For each next term the operator:

   - submits an L2 payout that spends the token in L2 and sends it (with the new value in the datum)
     to the native script address, tagged for withdrawal;
   - the head settles that payout onto L1 as a new oracle utxo — its stability window opens;
   - the operator registers that settled utxo as an internal deposit, so the head absorbs it after
     maturity, returning the token to L2 for the next refresh.

   So one token cycles L1 → L2 → L1, and only ~one oracle utxo is live at any moment.

6. **Consumes + verifies.** Acting as a consuming contract, the demo reads each oracle utxo off L1
   (by the operator's auth-token policy, decoding the payload) as it appears, and confirms it is then
   absorbed. It asserts the observed feed equals the Q-sequence in order, and that the head produced
   no consensus errors.

## Notes and future work

- **Internal deposits.** Registering an already-settled utxo as a deposit (so it is absorbed and
  recreated on the next refresh) is the whitepaper's *internal deposit* streamlining. Hydrozoa does
  not yet expose this on its public interface, so the demo drives it through a minimal internal
  deposit request added for this purpose. The maturity window is shortened to ~10s so the cycle is
  observable in minutes; in production it would be on the order of the head's maturity duration.
- **Oracle compartment.** A further streamlining — managing oracle utxos as dedicated treasury
  fragments rather than deposits — depends on fragmented-treasury support (CIP-112 / CIP-118) and is
  out of scope here.
