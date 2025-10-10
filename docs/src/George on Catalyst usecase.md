Could Cardano's @Catalyst_onX run on Hydrozoa?
A speculative thread exploring a usecase. 🧵⬇️

Catalyst's encrypted voting (with homomorphic tally 💪) is too technical for this thread (we could still probably do it on Hydrozoa though):
https://input-output-hk.github.io/catalyst-libs/architecture/08_concepts/catalyst_voting/crypto/

Instead, I'll focus on the financial and milestone management aspect for funded projects.

Suppose Fund 16 uses Hydrozoa. For each funded project, we want to:
- track its milestones
- make payouts for its approved milestones

We initialize a head for each category with funding for its projects' initial milestones, keeping the rest safe in cold multisig wallets.

Each head is run by several geo-distributed peer nodes.

Hydrozoa's unanimous consensus keeps Catalyst's funds safe, as long as even one of the nodes remains uncompromised.
Even if all nodes are compromised, at most only the projects' next milestone payout would be lost.

For extra security, we can rotate the peer nodes' automated keys via Hydrozoa's dynamic membership mechanism. 
Essentially, the old keys unanimously multisign an L2 governance action such that, henceforth, new keys will be used by the peers.

On L2, set up the milestones under simple smart contracts that track whose signatures are still needed for approval of proofs of achievement:
- First, the reviewers
- Second, the Catalyst team members

The smart contracts can also enforce payout schedules according to fund rules.

Catalyst team uses L2 transactions to:
- Assign reviewers to milestones
- Sign-off on milestones after both reviewers approve them
- Trigger scheduled payouts on L1 for fully-approved milestones

Reviewers use L2 txs to attach their approvals to milestones.

Whenever Catalyst team triggers the next wave of payouts, a bot submits the necessary L2 txs to withdraw funds from L2 and pay them out on L1 to projects.

The L2 smart contracts ensure that the bot pays out the correct scheduled amount and only for fully-approved milestones.

For each payout wave, Hydrozoa constructs an L1 tx chain to settle the payouts on L1, and the peer nodes automatically multi-sign and submit them.

For security, we could also require manual signatures to ratify the L1 txs, or add any custom L1 logic a la
@SundaeSwap's Gummiworm.

Every once in a while, probably after a payout wave, Catalyst can top-off the funding in each category's head by depositing funds into it from Catalyst's cold wallet.

Deposits into open Hydrozoa heads can be done cheaply, easily, and in parallel.

What if one of the peer nodes goes offline?

If it's only for a short time, then the head can resume as soon as the node reconnects with the other nodes.

If it's longer, then the head goes to the rule-based regime, controlled by L1 Plutus scripts instead of unanimous multisig.

Hydrozoa's F14 R&D project will research how to safely support M-of-N consensus (it's kinda tricky). 
For example, 9/10 consensus would mean that the other 9 peer nodes could keep progressing L2 state even if one node goes down for a while.

The rule-based regime is a really good safety net for any funds held under a multisig address. For Catalyst, we could customize it so that the head's funds either:
- go to Catalyst's cold multisig wallet
- go to Cardano's treasury
  (or something else that makes sense 🤷)

Even if a Catalyst category's head stalls for a long time, Catalyst team could easily start up another head with funds from its cold multisig wallet and the saved L2 state.

This would mitigate service interruption, while the funds are calmly recovered from the stalled head.

And that's my sketch for Catalyst on Hydrozoa. Benefits:
- use several geo-distributed nodes with unanimous consensus and key rotation to manage milestone status and payouts
- automate L1 payout txs
- isolate the main "stash" in a cold wallet, away from day-to-day operations

More benefits:
- be more responsive/transparent to projects about milestone status and payout schedule (via L2 state queries)
- trigger payouts more frequently (because L1 txs automated)

I used Catalyst as the concrete example here, but I think a lot of the above extends to any business 
that wants to pay crypto to suppliers/subcontractors and receive crypto from clients/lenders, 
while managing its funds and info privately on a virtual L2 ledger.

Many companies incubated by Catalyst start off as a handful of co-founders manually managing a multisig wallet. 
But as those businesses scale, at a certain point they might need an L2 ledger that enforces company policy 
and allocates funds internally to teams.
