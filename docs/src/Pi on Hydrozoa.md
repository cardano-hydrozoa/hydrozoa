Hydrozoa is, if not officially then at least in spirit, an evolution of Hydra. 
It pauses to reflect, with several years of hindsight, on how the protocol could be improved and ultimately simplified. 
It's a particular favorite of mine, because it really gets at that core question of "what is protocol design". 
Rather than just deciding "welp, we have smart contracts on Cardano, so lets implement what we need in smart contracts", 
it looks deeply at how the actors in the protocol communicate and what can be guaranteed simply 
by the nature of the messages and signatures they share amongst each other.

The basic idea of Hydra (and all state channels) is one I've explained a number of times: 
lock up funds in a smart contract on the Layer 1, transact however the hell you want offchain, 
signing periodic snapshots along the way that allow you to eventually unlock those funds according 
to everyone's latest balance.

Hydrozoa asks the question: how complicated do the smart contracts on the L1 *really* have to be? 
Sure, in the worst case, someone is trying to screw you over by refusing to sign transactions or presenting 
an "earlier" snapshot than the final one, but in the optimistic case, Hydra would work with a simple multisig: 
everyone deposits funds into the multisig, keep personal accounting off-chain, 
occasionally send more funds to the multisig address or sign transactions that withdraw partial funds from the address, 
and finally sign a transaction to fan out all remaining funds.

Hydra really only needs to be a plutus script when there's a conflict about what that final snapshot should be. 
So is there a way to split the protocol into two "regimes": the first only uses multisigs, but we ensure that, 
in the case of a dispute, it can fall back to a second which uses plutus scripts to resolve disputes.

It does this by ensuring that, before accepting any funds into the protocol, the head operators set a ticking time-bomb: 
the head operators sign a transaction that has a "validFrom" in the future that pays the treasury into a plutus script; 
That transaction isn't valid yet, but if we can't reach consensus by (for example, 2 days from now), 
it says "obviously consensus has broken down, and we'll need the plutus scripts to help us resolve the dispute".

So for example, suppose I want to deposit funds into a Hydrozoa head. I build a transaction 
that pays my funds into a multisig script controlled by the head operators. But, before signing and submitting it, 
I show this transaction to the other head operators, and request a refund transaction. 
Leveraging the determinism of Cardano, the head operators can build a transaction which spends 
the output of my deposit transaction back to me, and sign it. This transaction has a "validFrom" in the near future.

Once I have that multisignature, I can go ahead and submit my deposit transaction, locking funds at the multisig script. 
I know that if the other head operators suddenly refuse to sign any transactions, 
I'll have this "refund" transaction to get my funds back.

This deposit transaction isn't yet "part" of the L2; instead, it has to be spent and gathered into the set of funds 
that *are* considered part of the L2 ("The treasury"). We employ the same trick: build a transaction that spends 
the current treasury, and many of the pending deposits, and pays them all out to a new treasury output. 
But, before signing this transaction, we build and sign a transaction chained off of it, post-dated to 2 days from now, 
that pays the treasury to the dispute resolution script. 
Only once we have a signature of that ticking-time-bomb transaction do we sign and submit the "absorption" transaction.

Thus, if the other head operators start misbehaving and trying to extort me, or refusing to sign transactions, 
I know I just need to wait for 2 days before we go into that dispute resolution process.

In between these "major" updates, we can do many minor updates. This works similarly to Hydra: 
we're signing snapshots that, *if need be*, can be used in the dispute resolution process to fan out our funds as of 
the latest version. But we anticipate that we won't need them because if we're cooperating we can just spend the multisig script.

At some point we can also request some funds be withdrawn, and this triggers another "major" update: 
we build a transaction that spends the treasury UTxO, absorbs some deposits, and fans out the requested withdrawals. 
Before signing that transaction, we obtain signatures for a follow-up, post-dated "dispute fallback" transaction, 
and then sign and submit our major update.

This means that in the optimistic case, all we're doing is signing multisig transactions on the L1 every once in a while, 
until we fan out all funds.

This has a number of benefits:
1) The simplicity in the L1 contracts means things like incremental commits or updating the head members 
   are cheaper to execute, simpler to write, and less prone to bugs.
2) The protocol leverages Cardano's determinism; We don't need to wait for most L1 transactions to be final, 
   because if a transaction gets rolled back, we can just re-submit the pre-signed transaction.
   Because nothing can invalidate that transaction, we have high guarantees on settlement even if L1 consensus initially rejects it.
3) These two combined means Hydrozoa can be much more "active", bringing funds rapidly in and out of the head. 
   Whereas opening a Hydra head, or their incremental commit mechanism are seen as more "heavy weight", 
   discouraging the flow of funds *too* often, Hydrozoa is plenty happy to bring funds in and out with 
   every Cardano block if needed, for very cheap.

With this improved complexity budget, Hydrozoa also leverages some fancier cryptography to make the state commitments cheaper.
I'll do another thread about this soon, but this fancier crypto lets you prove multiple withdrawals with a single fixed size proof,
allowing for fanning out tens or hundreds of thousands of UTxOs onto the L1 very cheaply. 
There's no reason Hydra can't employ the same techniques, and indeed that is the eventual plan, 
but the simplifications that Hydrozoa brings makes it *easier* to implement these mechanisms, 
because in the optimistic case it's just peers agreeing on a state update; and in the case of a dispute, 
you only need to fan out funds, not worry about these complex state updates.
