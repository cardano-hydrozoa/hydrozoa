# Introduction to Hydrozoa

This article aims to be a very quick introduction to Hydrozoa.

The latest specifications are always kept up to date at [this](TODO) link.

Hydrozoa is a Layer 2 solution (L2) for Cardano blockchain.

Hydrozoa is bifurcated into two regimes, one for the happy path and one for the sad path.
What does happy path mean?
It means that all the peers of the Hydrozoa head are able to reach unanimous consent
on what they want to do next.
They are responsive to each other, and they participate in the L2 protocol properly.
In this regime, the L1 smart contracts get out of the peer's way
and simply say that you guys are allowed to do whatever you want with your money on L1
as long as you all unanimously agree on what you are doing.

And therefore, the entire state on L1 of the Hydrozoa head is held under a native script.
It's a multi-sig native script for unanimous consent.

In the rule-based regime, the assumption is that the peers can no longer reach unanimous consent
on what they all collectively want to do next.
In that case, what needs to be done is we need to find out on-chain
what was the last thing that all the peers agreed on
when they were still communicating with each other in the off-chain L2 consensus protocol.

And that's what the dispute resolution process is all about.
Once we have figured it out, we have resolved the dispute,
and we can proceed to release funds from the treasury
according to what we have determined to be the last resolved confirmed state.

Let's take a look at every transaction.

The first transaction that enters into a Hydrozoa head is the initialization transaction.
It initializes the treasury UTXO of the multi-sig regime under the native script of the head.
We mint a head beacon token to allow the peers to detect where UTXO is in the entire Cardano blockchain ledger.
And the input to this transaction, or one of the inputs to this transaction, we call it the UTXO nonce,
and it is the basis by which we can uniquely identify the head. That's why we call it a nonce.

The next type of transaction is the deposit transaction.
Here a user spends some of his or her funds in order to take them into the head.
We don't really care where they come from or what form they are in,
and puts some of them into a deposit UTXO under the head's native script address,
along with some instructions for how this deposit should be processed
and how it should look on the L2 ledger when it makes it there.

As a backup, in case the deposit isn't absorbed into the treasury
or processed by the head before the user's deadline,
which the user set in the instructions of the deposit,
we have this refund transaction.
It's a post-dated multi-sign transaction,
multi-signed by all the peers, which spends the deposit UTXO from the head native script
and sends back the funds to the user.
cccordingly to the user's indicated refund address and the refund datum that the user requested to be set.

The next transaction type is settlement.
The purpose of settlement is to absorb some deposits into the head's treasury
and pay out some UTXOs based on the withdrawal requests that the peers had submitted in the L2 consensus protocol.
If the settlement transaction is not able to process all of the withdrawal requests that have happened on L2
since the previous settlement, then it will try to pay out as many as it can directly in the settlement transaction
and place the rest of the payouts funds into a rollout UTXO,
which defers these payouts for subsequent transactions.
We will get to this a little bit later.

The finalization transaction is very similar to a settlement,
except that it does not absorb any deposits.
And the purpose of finalization is to withdraw all funds from the treasury.
So you'll notice that the treasury UTXO is spent on the left side,
but it is not reproduced on the right side,
the way that it was in settlement.
Instead, we pay out as many UTXOs we can.
We produce a rollout UTXO to defer some payouts if necessary,
and there may be a change UTXO for some of the funds
that the initiator of the head way back in the beginning,
the initialization transaction,
may have put some initial funds into the head that weren't represented on the L2 ledger.

The final transaction of the multi-sig regime is the rollout transaction.
This is what I alluded to before in terms of deferred payouts of the withdrawals.
The way it works is it spends a rollout UTXO that was produced by a settlement or finalization transaction,
and it pays out some UTXOs out of it.
And if any funds are left over, i.e. there's more payouts, but they just didn't fit into the single transaction,
we can create another rollout UTXO, and then a subsequent rollout transaction will spend it
and pay out progressively more UTXOs until we reach the very end,
and we have handled all the funds as payouts.

Now we should talk about the rule-based regime.
The way we reach the rule-based regime is essentially a dead man switch.
So after each settlement transaction, we set a timeout
and if this timeout allows
and the peers have not submitted a new settlement transaction to overwrite the treasury UTXO,
then there's this fallback transaction that becomes valid
that allows the peers to transition the treasury from the native script
to a UPLC script that is dedicated to the treasury
and applies some smart contract rules from then on that govern how the treasury needs to be managed.

Additionally, what this fallback transaction does is it initializes the dispute resolution mechanism for the head,
the purpose of which is to determine what was the latest L2 block that all the peers had agreed to
when the L2 consensus was still running, i.e. by the time the peers
were still able to multi-sign new blocks in the L2 consensus.

It means a default vote that simply votes for whatever the ledger state was as of the last settlement transaction
and it means one vote UTXO per peer that gives that peer one opportunity to vote
for what they consider to be the latest L2 block confirmed by the peers.

So how does voting work? Well, voting works by each is able to submit a transaction
that spends their assigned vote UTXO and replaces its empty vote with that peer's vote
for some particular minor version, like a particular L2 block.
And within the vote, the peer also provides a summary of what the L2 ledger state is corresponding
to the minor version that the peer voted for.
When the peers have voted, we can start tallying the votes.

Tallying works by spending two vote UTXOs.
Let's say the first vote UTXO was voting for version J,
the second vote UTXO was voting for version J',
and then we take the maximum between them, i.e. the latest version,
and we keep the corresponding L2 ledger state of that latest version.

When we've tallied all the votes, we should end up with only a single vote UTXO left,
and it should contain all of the vote tokens,
i.e. it has resulted from combining all of the vote UTXOs together
and finding what the latest version among them was.
In this case, we can then spend that last vote UTXO together with the treasury,
the treasury has an unresolved state at this point, in order to resolve the treasury state.
So then in the treasury state, we set its major version to be I,
this corresponds to the major version of the settlement transaction
from which we transitioned into this whole dispute resolution process,
with minor version J, and this comes from the last vote UTXO's minor version.
And we set the ledger state accordingly. This allows withdrawals to happen.

Because now that we've resolved what the latest block was
and we know what the latest ledger state was,
this is when we allow people to start withdrawing funds from the treasury.
And note that withdrawal here is a single sign transaction,
which means that the peers don't have to gather up all the other peers' signatures.

In order to withdraw any funds from then on, you can withdraw
simply by proving that the UTXOs you're trying to withdraw
and the ones that are being paid out here,
were members of the ledger state in the resolved treasury.
And when you do this, you'll update the treasury to exclude those UTXOs from the ledger state.

This way the peers can progressively withdraw more and more UTXOs,
and eventually they'll probably end up with an empty treasury
and this means that all the UTXOs have been withdrawn.

At that point threy can clean up the residuals of the head.
The treasury UTXO can be spent, burning all of the beacon tokens and all the vote tokens.

_These are basically all the bookkeeping internal protocol tokens of the Hydrazoa head._

You'll burn them all to kind of clean up you know, your state on the Cardano ledger.
And if there are any funds remaining in the treasury UTXO,
they can be distributed according to what the group decides they want to do.

And it's fine for this to be multi-signed because the actual funds
that the users had deposited formally into the head
will have been withdrawn unilaterally by those users using the withdrawal transactions.

So at this point, when you're operating with a deinit
you shouldn't have any user funds really left over.
Alternatively, the peers can actually use the deinit as a way to override
the whole dispute resolution process and simply say that,
okay, we have regained our capability to multi-sign transactions
and thus we want to do something else,
we don't want to proceed with the dispute resolution process,
we just want to arrange the UTXOs in some arbitrary way that we all agree on.
