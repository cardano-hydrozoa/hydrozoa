# Rulebased Regime Utxo Model

The model based test for the integration test will be based on a petri net.

The model state itself will be a petri net with the transitions, places, and an initial marking as described below.

The goal of the model is to have a structured, graphical, analytic tool to bi-simulate a meaningful subset of the behavior of rulebased behavior. To this end, the encoded semantics are intended to be as rich as is practical -- preferring "net-native" interpretations (how many tokens are in this place? Which transitions are fireable? What states are reachable? What terminal states exist?) to arbitrary logic, but falling back to the latter to make trickier assertions when necessary.

## Places
 
In the utxo model, places either represent a semantic interpretation of a utxo OR a synthetic place (stylized as $place$) that is used only internally to the model.

The model may have a more or less refined semantic interpretation of the utxos than the actual hydrozoa node; the point is that the translation from the utxo model to actual chain state and to node state should allow us to make useful assertions.

The places are as follows.

- DepositUtxo: corresponding to a deposit utxo leftover after fallback
- RefundUtxo: corresponding to the payout utxo after a deposit has been refunded
- Rollout N=???: corresponding to a rollout utxo that requires `???` more rollout transactions to consume fully
- MRW Utxo: the multisig regime utxo
- Multisig Treasury: the multisig treasury utxo
- Equity Payout: the utxo per head peer for equity payout after fallback
- Unresolved: the unresolved rule based regime treasury utxo
- Collateral: the per-peer collateral utxo used during dispute resolution
- Unvoted: an unvoted Vote utxo
- Voted : a voted Vote utxo
- DisputeRef: the dispute resolution script ref utxo
- TreasuryRef: the rule based treasury script ref utxo
- $TallyBuffer$: a synthetic place to count the number of tallies that have already occured. The purpose is to ensure that the model only sees the ResolutionTx as fireable when the correct number of TallyTxs have fired.
- $EvacuationCommitment$: a synthetic place containing evacuation commitments.
- EvacuationOutput: utxos produced to consume evacuation commitments
- Ambient Utxo: fee/change utxos consumed (with arbitrary arity) and produced during evacuation transaction firings.

> Future versions of the model may include additional synthetic places for timing-related, signature, or minting-related aspects of the rule based regime. For instance, a timed petri net could fire a transition that places a token in a $VotingDeadlinePassed$ place, which enables an empty/empty or empty/full tally tx in a net-native way.

### Token Identity

Tokens in places will have a place-dependent identity and associated metadata. Where possible, we will try to avoid encoding data that may affect the _fireability_ of a transition in the token (i.e., the data will be strictly _metadata_ and not arbitrary token "coloring"), as this can complicated net analysis. Instead, tokens in a place should be considered fungible, and the metadata should only be used for calculating isomorphism classes of the model's net markings with the SUT's utxo state.

## Transitions

Transitions in the model's net correspond to transactions undertaken autonomously by peers.
However, the transitions in the net are not 1-1; the tally transaction, for instance, is split into 3 versions depending on whether the tallied utxos are both Unvoted, Unvoted + Voted, or both Voted.

This refinement is intended to help us make more precise assertions about the models state. 

In addition, the consumption or production of a token into or out of a transition does not have the exact semantic of "spending" or "sending" a utxo. The utxo model is only aware of whether there is a token in the place or not. If all pre-set places to a transition have the requisite tokens, the transition is fireable; otherwise not.

Explicitly, this means that a reference utxo or collateral utxo appears in the net as being both the input and output to a transaction, even though it is not spent.

> Future versions of the model may make distinctions between spending, using as collateral, and referencing a utxo. Such a distinction might introduce synthetic transitions that encode the "role" a utxo places (such as being referenced or spent) in the eyes of the model; such a synthetic transition would be a no-op from the viewpoint of the SUT.

The transition are listed below. We only list the names and a brief description where the meaning may be unclear; a graphical depiction of the open networks (with one transition each) can be loaded at https://pes.vsb.cz/petrineteditor/#/model or view with tikz.

- RefundTx: the transactions that must be issued to spend leftover deposit utxos after fallback
- RolloutTx N=???: The rollout transactions that must be issued to complete unfinished rollouts.
- FallbackTx
- Unvoted/Voted Tally
- Unvoted/Unvoted Tally
- Full/Full tally
- Vote
- Resolve
- Evacuation
- Deinit

## Initial Markings

The model state will be a petri net, striving to limit the need for additional metadata.

To generate the initial state, we thus need to determine the topology of the net and the initial marking.

Each will be determined based on the following generated variables:

- Leftover deposits: the number of leftover deposit utxos that must be refunded, which contribute to the initial marking of the DepositUtxo place
- Leftover rollouts: the leftover rollout utxos, which contribute to the initial markings of the various `RolloutUtxo N=???` places
- The number of head peers, which determines the number of vote utxos, collateral utxos, and the arity of tokens produced or consumed in associated transitions
- The number of evaucation commitments, which contributes to the initial marking of the synthetic $EvacuationCommitments$ place, and the arity of tokens produced or consumed in associated transitions.
- 1 initial token in each of the following places:
  - Multisig regime witness Utxo
  - Multisig treasury
  - Dispute Ref Script Utxo
  - Treasury Ref Script Utxo

## Execution strategy

Proceeds as follows:

- Initialize
  - Generate the variables as above
  - Apply it to create a net topology
- Generate commands
  - Command generation consists of observing the net and picking `n` fireable transitions. This allows us to simulate concurrency, such as two mutually independent tallying transactions occuring simultaneously. 
- Apply The commands
  - To the model: 
    - The commands will be interpreted by the model as a firing. 
  - To the SUT:
    -  Unlike the multisig regime, which is reactive to user input, the rulebased regime is proactive and inherently exposed to race conditions and utxo contention. This means we cannot apply the same strategy as the mutlisig regime.
    - To address this, we will add a proxy for tx submission that only permits the intended transactions to go through at a given step. Certain transactions may be re-ordered or may be dropped entirely.
    - This additionally tests that peers are resilient to the various failure conditions that may organically result in the rule-based regime.
- Reach a terminal state
  - The model reaches a terminal state when no more transactions are fireable. For a given net topology, there should be exactly one terminal state up to isomorphism.
  - If the model has reached a terminal state, the SUT should also reach a terminal state
- Simulate rollbacks
  - The model will maintain a sequence of fired transitions in its state, as well as a copy of the initial marking. From this, any state in the sequence can be rolled back and replayed. This can simulate rollbacks, with new transitions being generated from a previous state.
  - The SUT will likewise need to accomodate rollbacks. Instrumentation will need to be built to manipulate on-chain state accordingly.


## Assertions

> Note: some of these assertions are straight forward and can be accomplished in a computationally feasible way without a detailed understanding of petri net analysis. Others (marked as strech goals) may require additional research and development to understand efficient techniques (such as matrix representations of nets) for verifying certain properties. It is my current understanding that all of these exist in the literature, but certain analysis techniques on certain types of nets are known to be computationally intractable; nonetheless, it may be worthwhile to attempt these on small test cases (low number of peers, evacuation commitments, etc). 

Assertions on the test case will include:

- **Fully autonomous operation reaches the terminal state**: The model will be constructed with an initial state and immediately fire all transitions in order to reach a unique (up to isomorphism) terminal state. The SUT will be seeded with the initial state, and then left to operate fully autonomously, with no filtering or re-ordering of peer behavior. The terminal state reached by the model and the terminal state reached by the SUT should be isomorphic.
- **Autonomous operation leads to an equivalence class of observered transitions**: As before, the model will be constructed with an initial state and immediately fire all transitions to reach a unique terminal state. The SUT will act fully autonomously. We will examine the model net's firings and the observed transactions submitted by the SUT and build equivalence classes, such as "every firing/transaction sequence should have `N` tallying transactions".
- **Filtered operation leads to deterministic transitions with isomorphic net marking <=> utxo state**: The model net's initial marking and the SUT will be seeding according to some initial state. A cardano backend will be instrumented to whitelist transactions according to commands, such as "only this RefundTx, this VoteTx, and this TallyTx are currently allowed" (note: this explicitly encodes parallelism). The commands issues to the model will select from available firings; the same commands will be interpreted by the SUT to whitelist the equivalent transactions. At each step, we will inspect the marking and the chain state (after all actors have settled) for equivalence.
- (strech goal) **Reachability analysis from the model**: the model and SUT will be initialized as before, but the model will not fire. The SUT will be allowed to act autonomously, but the cardano backend will gate all transactions after each submission. After each transaction is submitted, the test case will attempt to determine whether the resulting utxo state is reachable from the model's initial marking, determine the valid firing sequence(s) if so, and only pass test cases where the actual transactions submitted by the SUT match a valid firing sequence in the model.
- (somewhat strech goal) **Additional assertions on initial, intermediate, and terminal states** Petri nets can be analyzed algorthimically. If the net and the model can be tested for bi-similarity, then this means that examining the _model itself_ for certain properties can build confidence that the SUT also has these properties. Thus, assertions on the model itself may include include things like:
  - All terminal states result in deinitialization (and the corresponding marking)
  - Certain unreachability conditions, such as "there can never be a token each in the Unresolved and the Resolved place".
  - Certain live-lock or dead-lock conditions that can be calculated, such as "Unless condition X is met, a marking where a VoteTx is possible is always reachable"
  - Absence of cycles
- (stretch goal) **Exhaustive state bi-simulation**: We assume that the utxo model always begins with an initial state (given by the fallback transaction and the left-over rollouts and deposits) and ends in a terminal state, with no possibility for cycles. Thus, there is a finite set of transition sequences that reach the terminal state. For small models (small numbers of peers, small evacuation map, small number of rollout/deposit leftovers), we can iterate over these, filter the submission of transactions from peers to simulate each one, and ensure that there are (combinatorially) no other possible sequences of transitions that were rejected.

## (Stretch Goal) Visualizing and analyzing net topology and execution from traces

Various (maintained and unmaintained) tools, standards, and libraries exist for building, simulating, analyzing, serializing (as in, serialization and deserialization, not serial vs. parallel), and visualizing petri nets.

- https://github.com/chimenkamp/YAPNE-Yet-Another-Petri-Net-Editor
- https://2025.models-conf.com/details/models-2025-educators-symposium/6/Introducing-MyPetri-net-A-Petri-Net-Editor-and-Simulator-for-Students
  - mypetri.net

They typically come with a serialization format, some of which are standardized. 

It would be nice to find an open-source library that could perform analysis and simulation according to our model. YAPE, linked above, boasts some neat tools for formal verification (including z3 SMT solving); TBD whether its actually usable.


# Resources

- https://scg.unibe.ch/download/lectures/cp/CP-11-PetriNets-animated.pdf


