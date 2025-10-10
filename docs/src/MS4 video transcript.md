
Hi, everyone. Welcome to HydroZOA's third video update. Today, we'll be talking about Milestone 4. 
My name is George Flerovsky, and joining me are Ilya Rodionov and Peter Dragos of the Hydrozoa team. 
Today is August 1st, 2025. Let's begin. 

Quick outline. Today, we'll begin with a recap of Milestone 2 and Milestone 3 concepts, 
specifically the L1 state and its transactions, and the L2 events and the blocks they get packaged up into. 
Then we'll move on to the new content of Milestone 4, beginning with the L1 effects of L2 blocks 
and the KZG BLS commitments corresponding to the blocks. 

We'll move on to the L2 consensus protocol and finish with the roadmap of the project. 
As always, if you're interested in the code base of the project, it's available at this link. 
And if you want the spec, it's always kept up to date at this link. 

So first, the state model. Hydrozoa has two regimes, the multisig regime, which is intended for the happy path, 
where the peers are responsive to each other, and are able to continuously multisign new blocks. 
Correspondingly, the L1 state under the multisig regime is held under a native script that says as long as 
the peers' signatures are all present in an update that gets sent to L1, the script is happy. 

The other regime is the rule-based regime in which the happy path is does not hold. 
Instead, we can no longer rely on the peers being able to produce new multi-signed transactions. 
And therefore, the L1 state has to live in Plutus UPLC scripts, which impose certain objective rules 
on the process by which the Hydrozoa head operates. 
Within the regimes, two different sets of transactions update their state. 
In the multi-sig regime, we begin with the initialization transaction, which then allows the peers 
to start depositing funds into the head. Each deposit has a corresponding post-dated refund transaction, 
which ensures that if heads consensus collapses unexpectedly, the peers can recover their funds 
from the deposits via the refund transactions. However, if the head does not collapse, 
the peers will sign settlement transactions which will absorb deposits into the head and release payouts, 
payout UTXOs out of the head corresponding to the L2 withdrawal requests that the peers have submitted on L2. 

Lastly, we have the finalization transaction, which releases all of the funds in the head as payout UTXOs 
and cleans up all state of the head. We also have rollout transactions, which may assist settlement 
and finalization transactions by releasing any payout UTXOs that didn't fit into those respective transactions. 
The rule-based regime has its own set of transactions that modify its state, 
but we will not be going into too much detail for these transactions today because we'll be more focused
on what happens in the multisig regime and how the L2 consensus protocol creates new effects that have to be sent to L1. 
In milestone three, we talked about the L2 ledger, which is basically a timeline of L2 ledger events arranged into 
a sequence by the L2 consensus protocol. We have genesis events, L2 transactions, and L2 withdrawals. 
Genesis events correspond to the deposits that are being absorbed by settlement transactions on layer one. 
They create in the layer two ledger, they create new UTXOs that correspond to the absorbed deposits. 
So every L1 deposit that got absorbed will have a corresponding equivalent new UTXO on L2 as a result of the genesis event.

L2 transactions should be very familiar to people because they're very similar to L1 transactions. 
You can spend some UTXOs, you can reference some UTXOs, and you can produce some UTXOs.
And the value on the right-hand side should match the value on the left-hand side. 
Note that in Hydrozoa, at least currently, there is no minting or burning of tokens.  

L2 blocks. These exist for efficiency of the Hydrazoa head. Rather than collecting the peer signatures for L2 event individually, 
we package up events into blocks and gather the peer signatures for every block. 
This reduces the network chatter and increases the efficiency of the HydroZOA head. 
We have three types of blocks. Minor blocks contain only L2 transactions. 
Major blocks contain at least one genesis event or one or more withdrawal event. 
And final blocks are triggered by finalization requests. 
They can contain transactions or withdrawals, but they cannot contain any genesis events because 
it makes no sense to bring in funds into the head when you're planning to immediately release 
all of the funds in the head as a result of this block. 

Each L2 block, when confirmed, has one or more L1 effects, or zero or more if we consider minor blocks that should take effect on layer 1. 
Minor blocks do not contain any L1 effects, neither immediate nor post-dated, because they only have L2 transactions 
that simply rearrange the funds that are already inside of the head. 
Major blocks contain immediate effects and a post-dated effect. 
The immediate effect of a major block is a settlement transaction which will absorb the deposits 
corresponding to the major block's genesis event and release the funds corresponding to the major block's withdrawal events. 
The major block can also have some associated rollout immediate effects that assist with the settlement transaction with releasing the funds.

The postdated L1 effect for a major block is the fallback transaction which ensures that we can transition 
to the rule-based regime if this major block ends up being the last major block that the peers 
have agreed on before consensus collapsed.

The final block contains a finalization transaction and potentially some rollout transaction as its immediate L1 effects 
and it has no postdated L1 effects because there can be no collapse of consensus after the final block because 
the peers are done after the final block. 

Additionally, each L2 block will contain a KZG-BLS commitment that summarizes the L2 ledger state that 
results from the L2 events of that block. KZG-BLS commitments are clever cryptographic mechanisms 
that allow you to efficiently represent a set of any size really as a string of fixed size on the chain. 
They are more efficient than Merkle trees because we can reuse the same proof to show membership for multiple UTXOs 
and therefore withdraw them in the rule-based regime. 

Additionally, the membership proof happens to be the same as the updated ledger state in the rule-based regime, 
which excludes the UTXOs that we just withdrew so that they can't be withdrawn a second time from this treasury, 
thus preventing the double withdrawal attack. 
However, one negative of the BLS regime relative to Merkle is that they require a trusted setup. 
In the case of the Hydrozoa head, where the consensus is already multi-signature, 
the security assumptions of Hydrozoa's L2 consensus actually align with the security assumptions of 
the trusted setup for KZG-BLS, which means that we can use KZG-BLS in Hydrozoa without any issues.

And furthermore, we can reuse the setup that Ethereum's KZG summoning ceremony spent 14 months creating.
So over the course of 14 months there were 141,000 contributions from many many different people 
on the internet from a variety of different countries using a variety of different equipment 
and with several different independent implementations of the software with which they can produce their contributions.
As a result they came up with fairly secure setups for KZG-BLS commitments.
For example, we can use the Ethereum KZG setup to support L2 UTXO sets with up to 32,000 UTXOs, 
which should be sufficient for most Hydrozoa heads in typical usage. 
And we have some ideas in case the 32,000 UTXOs ends up being too restrictive, 
we have some ideas of how to expand that further while using the same KZG vectors. 
If you want more technical details about the cryptography, I included a link to a pretty good paper 
that I think explains the concepts in an approachable manner. 

Let's move on to the L2 consensus protocol. 
So the consensus protocol answer five questions. 
- Who gets to propose the next block? 
- Which events get included in that block? 
- How do the peers verify the proposed block? 
- How do the peers indicate that they want this block to be confirmed after verification? 
- And how can the peers gracefully terminate the protocol? 
  Or more specifically, how can they signal that the next block should be final. 

We'll start with the leadership schedule. 
So in Hydrozoa, we have a very simple round-robin schedule with which we select the leaders. 
So for example, if we have five peers, let's say Alice is currently the leader, 
Bob is the candidate, which means that the schedule will have him be the next leader once Alice is done.
Carol, Daniela, and they are waiting for their turn.

And of course, in the L2 consensus protocol, all of them are connected to each other with direct peer-to-peer links 
in this network so that they can send each other messages as needed for the L2 consensus protocol. 
All right, so Alice is the leader. The other peers are expecting her to produce and broadcast the block. 
How does she actually do this? Well, first she takes a look at the L1 deposits and she filters them down 
by the so-called mature L1 deposits. 

This means the L1 deposits that have been created a sufficient amount of time ago that we don't have any worries 
about the transactions that created the deposits falling through because of L1 rollbacks, right? 
Alice also takes a at her local queue of pending L2 events, in which she stores the transactions, 
the L2 transactions and the withdrawal request in the order that they arrive at her Hydrozoa node. 

When it's time for her to produce a block, she will iterate over the pending L2 events, checking which ones are valid, 
and she will partition them into the sequence of valid L2 events and a set of valid L2 events. 
And she will insert references to these events in her proposed block. As well, after processing all the L2 events, 
she will end up with some sort of active L2 UTXO set, i.e. The UTXOs that can still be spent after 
all of the events of the block have been processed. 
And she will summarize this set with this KZG-BLS commitment for the block. 
Lastly, she will take as many mature L1 deposits as she can fit into a settlement transaction 
and include references to them within the proposed block. Based on the contents of the block, 
she will indicate what type of block it is, major, final, or minor, and she will increment the version accordingly.

Major blocks increment the major version and they set the minor version to zero.
Minor blocks keep the major version the same and they increment the minor version.
And final blocks increment the major version and set the minor version to zero.

Okay. Follower peers are very passive while they're waiting for the leader to broadcast the next block.
L2 events arrive at each peer, at each follower peer, they don't actually validate or process them in any way.
They just store them in their local queues. 
They only validate L2 events when they actually receive a block from the leader that mentions those L2 events. 
And it's important for L2 consensus then that every peer verifies for themselves that the block is correct.
What do they look at? Well first they're going to take a look at the valid L2 events.

They will apply them in the sequence declared in the block to the active UTXO state 
from the previous block and they will end up with some active UTXO state at the end.

They will compare that resulting UTXO state to the summary of it corresponding to the KZG-BLS commitment. 
And then they will evaluate the invalid L2 events mentioned in the block. 
Each one of these L2 invalid events must be invalid relative to the active UTXO set of the block. 
They'll take a look at the deposits that the block wants to absorb. 
Specifically, they must exist on layer 1 from the perspective of the peer doing the verification, 
and they must be mature, which I explained in the previous slide. 
Lastly, the peer will check that the block is correctly labeled as major, minor, or final, 
and that the version is correctly incremented. 
If all of this checks out, then the peer is ready to acknowledge the block to help it get confirmed. 
So how does this actually happen?

Well, the process for confirming a block needs to ensure that either all of the block's effects 
are confirmed together or none of them are.
Because we don't want to end up in a situation where somehow we've gathered the signatures for 
only some of the block's effects but others we didn't gather the signatures for. 
This would be an invalid valid state to end up in, and it's very hard to troubleshoot it and recover. 
So to prevent that, we need to make sure that our consensus protocol is all or nothing. 
For a major block, Alice begins, Alice the leader, begins by broadcasting the new major block. 
Each peer, and I've lumped together the candidate and all of the followers on the right-hand side here, 
When they receive Alice's broadcast, they will each verify that the block is correct. 
And if it is, each peer will send out their first acknowledgment of the block.

This acknowledgment will include signatures for the KZG or BLS commitment of the block, 
for the fallback transaction of the block, and for any rollout transactions that must be generated for the block.
And then, once the first round of acknowledgments is received, each peer will send out the second acknowledgment, 
in which they provide that peer's signature for the settlement transaction of the major block.
And thus, with the second acknowledgment received, we have full confirmation of all of the effects 
and the KZG BLS commitment of that block, and we can consider that block to be confirmed.

For minor blocks, the process is very similar, except that we only need one round of confirmation or acknowledgement, 
because the minor block really only has the KZG BLS commitment to multisign.

For a final block, we again have two rounds of acknowledgement, because we need to get multi-signatures 
for any rollout effects of the finalization.
And we need to multi-sign the KZG-BLS commitment of the finalization as well.
And then once we obtain those signatures, it is safe to then send out the signature for not the settlement, 
but the finalization transaction corresponding to that final block.

So this is how block confirmation works for the three types of blocks.
The last thing I want to talk about is how do we actually trigger finalization?
Well, this turns out to be very simple.
In the acknowledgment messages, we have a flag that can be activated to signal that that peer wants the next block to be final.
As a result, the next leader must respect this signal when preparing the next block.
So the next leader must make that block be final.

And all the peers will withhold their acknowledgments of that next block unless it is final, 
as was requested in the confirmation of the previous block.
