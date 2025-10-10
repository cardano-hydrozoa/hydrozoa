Hydrozoa is a protocol for a group of people to manage their funds and information via a virtual (L2) ledger with rules that they collectively enforce.
The group holds the funds collectively on L1, but in the L2 ledger they can be allocated to any individual, group, or script.
Transactions (txs) in the group's L2 ledger are confirmed as soon as every peer signs off that the transactions comply with the L2 ledger rules.
Hydrozoa collects L2 txs into blocks, so that a whole block can be sent out for signatures as soon as the previous one is confirmed.
Each peer can submit L2 txs and receive quick, definitive feedback on the txs' success/failure via a brisk cadence of confirmed blocks.
The streams of txs originating at each peer are continuously merged and packaged into blocks. Each peer's L2 txs must be processed in order.
The group can set L2 fees as it likes, and only the group's peers need to see L2 txs to confirm them.
Thus, the peers can transact with each other quickly, cheaply, and privately! 🚀

But how do you set up a Hydrozoa head and get funds into/out of it?
This is crucial: if it's too complicated or there's too much overhead, then why bother? — it would be cheaper and easier to use L1!
The Hydrozoa team has thought a lot about how to streamline this process. 🧠

To prepare, a group of peers should first agree (offchain) on:
- The L2 ledger rules/parameters they'll use
- The automated keys they'll use to sign L2 blocks
- The IP addresses/ports they'll use
- The funds each peer will bring in and how to allocate them on L2
  
With those preparations in hand, the peers can initialize the head with a single L1 transaction that spends the peers' 
initial deposits and sets up the head's initial state.
No complicated ritual or Plutus scripts needed. Just a single, cheap L1 tx multi-signed by the peers! 💸

After a head is initialized, how do you get funds into it?

First, declare your intentions to the peers. This polite act lets the peers know to expect new deposits, 
while you get quick feedback on whether the peers intend to accept your deposits, before you pay anything on L1.

Second, you submit an L1 tx that puts the deposited funds into utxos at the head's multisig address, 
with instructions in the datums for each deposit:
- Its expiry time (do not absorb into the head after this)
- Its L2 address+datum if absorbed
- Its L1 address+datum if refunded

More specifically, you declare your intentions to the peers by showing them the unsigned deposit tx.
If the peers intend to accept, they'll provide a multisigned post-dated refund tx for your deposits.
With this refund tx, you can recover your funds if they never get absorbed.

Each deposit is assigned a "maturity time", offset into the future by a fixed duration from the time 
when the deposit was declared to the peers. At that time, if the deposit exists on L1, the peers will decide to absorb it. 
If it doesn't, they will decide to never absorb it.

For the deposits the peers have decided to absorb, as part of the L2 block confirmation process, 
the peers will sign an L1 settlement tx that spends those deposits and puts their funds into the head's L1 treasury.
Correspondingly, the deposits will be added to L2 as instructed.

OK, now how do you get funds out of the head?
As it turns out, it's quite simple: every L2 transaction indicates which of its outputs should stay in the L2 ledger 
and which should be paid out on L1. It indicates this with special metadata that Hydrozoa can interpret.

The beauty is that every Cardano offchain framework can build Hydrozoa L2 txs, even those with L1-bound outputs, 
because L2 txs are just regular txs with some special tx metadata for Hydrozoa.
Furthermore, unifying withdrawals into L2 txs gives the most flexibility for users.

For the L1-bound outputs of L2 txs, as part of the L2 block confirmation process, the peers will sign an L1 settlement tx 
that pays them out on L1 out of the head's treasury. This is the same settlement tx that I mentioned above, 
which can spend deposits to absorb them, too.

In this way, Hydrozoa can absorb many deposits and pay out many withdrawals in parallel for each confirmed L2 block! ⚡
Withdrawals per L2 block can actually be unlimited because the settlement tx can always be followed by 
a chain of "rollout" txs that pay out more outputs.

How do the peers stop operating a head? Any peer can signal that he'd like the group to stop operating the head. 
Consequently, the next L2 block will include an L1 finalization tx for the peers to sign, 
which pays out all the funds out of the treasury according to the L2 ledger.

All of these L1 txs (initialization, settlement, rollout, finalization) don't contain any Plutus scripts,
which makes them very cheap and increases the number of inputs/outputs they can contain.

Hydrozoa's design also cleverly (via determinism) allows L2 transaction submission/confirmation to continue 
without stopping for any of the L1 settlement/rollout transactions to be confirmed.

Even deposits into the head can be made without waiting for the initialization tx to be confirmed.
Hydrozoa guarantees that all deposits txs will be safely rolled back 
if the initialization tx is permanently rejected by L1 for any reason.

As long as the peers continue confirming new L2 blocks, Hydrozoa provides a smooth experience for setting up and 
finalizing a head and for depositing, transacting, and withdrawing funds in/within/out of a head.

But what if the peers cannot confirm new L2 blocks?
Recall that Hydrozoa requires all peers' sign-off to confirm L2 blocks. 
So, all it takes is one non-cooperating peer to stop this process.
For this reason, in such a situation, Hydrozoa can shift to a "rule-based regime" 
where the head's funds are controlled by Plutus scripts.

In the rule-based regime, each peer gets one vote to indicate which L2 ledger state was the latest one confirmed by all peers.
The votes are tallied and then funds can be withdrawn from the treasury according to that L2 ledger state, 
without requiring all peers' signatures.
