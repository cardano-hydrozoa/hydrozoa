# Rulebased Regime Utxo Model

The integration tests for the rule-based regime (RBR) are based on a petri net model. The tests include:
- fully autonomous property tests, where multiple actors are initialized in the RBR and allowed to run to completion
  without interference.
- Model-based tests, where actors are constrained at intermediate states (i.e., after every successful transaction) and
  the intermediate states are evaluated for validity.

The ultimate goal is establish evidence for bi-similarity between the _utxo model_ and the system under test (SUT), such
that we can then prove certain properties on the model and be confident that they should also hold on the SUT itself..

The petri net model establishes an abstraction function $\alpha$ mapping a UTxO set and a list of transaction onto
the marking of a petri net and a list of transitions:

$$\alpha: Utxos \times List(Transactions) \to (Marking \times List(Transition)) $$

In terms of the scalacheck-derived model based tests, model state itself _is_ a petri net with the transitions, places, 
and an initial marking as described below. Depending on the test, the command generation is:

- **Model Driven**: selects from enabled transitions in the model to drive both the model and the SUT forward, checking
  that they proceed in equivalent ways. This requires a transaction-submission proxy that can constrain submission 
  to white-listed transactions.
- **SUT Driven**: the command generation forces a transaction-submission proxy to pause after each successful
  transaction, but does _not_ try to "rig" the race conditions of the rule-based regime. The test suite then 
  checks whether the intermediate state should be reachable from the initial state and/or previous state.

The goal of the model is to have a structured, graphical, analytic tool to bi-simulate a meaningful subset of the
behavior of rulebased behavior. To this end, the encoded semantics are intended to be as rich as is practical -- 
preferring "net-native" interpretations (how many tokens are in this place? Which transitions are fireable? What states
are reachable? What terminal states exist?) to arbitrary logic, but falling back to the latter to make trickier
assertions when necessary.

## Petri Net Model

Petri nets come with many different extensions which affect the expressiveness of the models. Generally, more expressive
models are less tractable to analyze, with certain combinations of extensions allowing for turing completeness.

It is a design goal of the library to allow for extensible, well-typed nets to be built, composed, and analyzed in a
principled way. Thus, we want to retain the _ability_ for developers to implement nets with varying syntax and 
semantics, while provide validation tools and type-level guidance to aid in understanding the constraints that arise
in analysis. 

The _default_ set of capabilities we target are those of
[YAPNE](https://github.com/chimenkamp/YAPNE-Yet-Another-Petri-Net-Editor), which in turn tries to be compliant with the
semantics described in [Murata, 1989](https://people.disim.univaq.it/adimarco/teaching/bioinfo15/paper.pdf). 
Choosing YAPNE as a compliance target gives us access to javascript-based simulation and visualization libraries, and
allows us to use:

- PT, TP, Inhibitor, Read, and Reset Arcs
- Data-aware petri nets, which allow net-global data and transition-local guards and post-condition updates
- Access to a suite of formal verification tooling, driven by Z3.
- Graphical editing with serialized import/export
- Event log generation

> A future goal is to allow _every_ net with the appropriate codecs to _drive_ a YAPNE-based visualizer in the browser
> by emiiting effects on every enabledness check, test-firing, firing, etc. 

JSON export of YAPNE-nets is currently implemented, except for the data expression features.

## Places
 
In the RBR utxo model, places either represent a semantic interpretation of a utxo OR a synthetic place (
stylized as \$place\$) that is used only internally to the model.

The model may have a more or less refined semantic interpretation of the utxos than the actual hydrozoa node; 
the point is that the translation from the utxo model to actual chain state and to node state should allow us to make
useful assertions.

The places are organized according to `PlaceId`s. The current (WIP) implementation can be found in 
`hydrozoa.integration.rbr.model.petri.net.Places`.

The full list will look something like:

```scala
enum RBRPlaceId:
    // Not yet implemented
    case DepositPlaceId
    case RefundUtxoPlaceId
    case RolloutPlaceId
    case MRWUtxoPlaceId
    case MultisigTreasuryPlaceId
    case EquityPayoutPlaceId
    case TallyBufferPlaceId // synthetic
    // Currently implemented
    case TreasuryRefPlaceId
    case DisputeRefPlaceId
    case UnresolvedTreasuryPlaceId
    case ResolvedTreasuryPlaceId
    case UnvotedPlaceId
    case VotedPlaceId
    case PayoutObligationsPlaceId // synthetic
    case EvacuationOutputPlaceId
    case CollateralPlaceId
    case AmbientPlaceId
```
Ground-truth `Utxos` sets are classified according to various criteria derived from the `NodeConfig`. See the 
`hydrozoa.lib.classification` package for the library implementation, and 
`hydrozoa.integration.rbr.property.RBRClassifier` for the (WIP) classifier of the rule based regime.

> Eventually, the classifier should be extended over the entire hydrozoa protocol, including the multisig regime.

> Future versions of the model may include additional synthetic places for timing-related, signature, or 
> minting-related aspects of the rule based regime. For instance, a timed petri net could fire a transition that places 
> a token in a \$VotingDeadlinePassed\$ place, which enables an empty/empty or empty/full tally tx in a net-native way.

### Token Identity

A petri net extended with "token identity" treats tokens within the same place as _non-fungible_. Such nets are 
sometimes called "colored petri nets". The library does NOT currently support token identity; it is not currently
supported in YAPNE, and will require some additional logic to support token selection and identity-updating. 

> Adding _arbitrary_ updates to token identity as they enter a place could obviously make the nets turning complete. 
> Thus, restricting these updates to something a solver can work with is probably a good idea.

Note that the lack of token identity DOES NOT mean that our classifier function is unable to perform its duties -- it 
only means that the classifier treats _every token in the same place_ as fungible.

## Arcs

YAPNE, and thus our library, currently supports 5 types of arcs:

- `PT`: The standard weighted arc that remove tokens from a place
- `TP`: The standard weighted arc that adds tokens from a place
- `Inhibitor`: An arc that only allows the connected transition to fire if there are no tokens in the connected place
- `Reset`: An arc that drains all tokens from the connected place
- `Read`: A weighted arc that only allows the connected transition to fire if the connected place carries at least 
  the requisite amount of tokens.

This allows us to build the utxo model is semantically sound ways:
- Collateral and reference inputs can be modeled with `Read` arcs.
- Resolution, which can only happen when there are no more unvoted utxos left, can be implemented with an `Inhibitor` 
  arc on the `Unvoted` place
- Regular spending and sending can be modeled with `PT` and `TP` arcs, respectively.

## Transitions

Transitions in the model's net correspond to transactions undertaken autonomously by peers.
However, the transitions in the net are not 1-1; the tally transaction, for instance, is split into 3 versions 
depending on whether the tallied utxos are both Unvoted, Unvoted + Voted, or both Voted.

This refinement is intended to help us make more precise assertions about the models state. 

In addition, the consumption or production of a token into or out of a transition does not have the exact semantic 
of "spending" or "sending" a utxo. The utxo model is only aware of whether a transition is enabled or if it is not, 
according to a run-time combination of `Net.Semantics`, `Transition.Semantics`, `Arc.Semantics`, and `Place.Semantics`,
as well as the compile-time aggregate of the individual `*.Syntax` of each component.

> Future versions of the model may make distinctions between spending, using as collateral, and referencing a utxo. 
> Such a distinction might introduce synthetic transitions that encode the "role" a utxo has
> (such as being referenced or spent) in the eyes of the model; such a synthetic transition would be a no-op from the 
> viewpoint of the SUT.

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

> YAPNE also supports net-wide _data expressions_ in a simple algebraic DSL. These expressions are associated with 
> transitions, and can guard (pre-condition) firing, as well as update and constraint post-conditions. 
> 
> Using Data-Aware Petri Nets (DPN) may allow for more expressive, comprehensible models without sacrificing analytic
> capabilities. To implement this, we'll need a better understanding of exactly the syntax and semantics of YAPNE data
> language.

## Initial Markings

To generate the initial state, we thus need to determine the topology of the net and the initial marking.

Each will be determined based on the following generated variables:

- Leftover deposits: the number of leftover deposit utxos that must be refunded, which contribute to the initial marking
  of the DepositUtxo place
- Leftover rollouts: the leftover rollout utxos, which contribute to the initial markings of the various 
  `RolloutUtxo` places
- The number of head peers, which determines the number of vote utxos, collateral utxos, and the arity of tokens 
  produced or consumed in associated transitions
- The number of evaucation commitments, which contributes to the initial marking of the synthetic
  $PayoutObligation$ place, and the arity of tokens produced or consumed in associated transitions.
- 1 initial token in each of the following places:
  - Multisig regime witness Utxo
  - Multisig treasury
  - Dispute Ref Script Utxo
  - Treasury Ref Script Utxo

## Assertions

> Note: some of these assertions are straight forward and can be accomplished in a computationally feasible way without 
> a detailed understanding of petri net analysis. Others (marked as strech goals) may require additional research and 
> development to understand efficient techniques (such as matrix representations of nets) for verifying certain
> properties. It is my current understanding that all of these exist in the literature, but certain analysis 
> techniques on certain types of nets are known to be computationally intractable; nonetheless, it may be worthwhile 
> to attempt these on small test cases (low number of peers, evacuation commitments, etc). 

Assertions on the test case will include:

- **Fully autonomous operation reaches the terminal state**: The model will be constructed with an initial state and 
   immediately fire all transitions in order to reach a unique (up to isomorphism) terminal state. The SUT will be 
   seeded with the initial state, and then left to operate fully autonomously, with no filtering or re-ordering 
   of peer behavior. The terminal state reached by the model and the terminal state reached by the SUT should agree.
- **Autonomous operation leads to an equivalence class of observered transitions**: As before, the model will be 
   constructed with an initial state and immediately fire all transitions to reach a unique terminal state. The 
  SUT will act fully autonomously. We will examine the model net's firings and the observed transactions submitted by 
  the SUT and build equivalence classes, such as "every firing/transaction sequence should have `N` tallying
  transactions".
- **Filtered operation leads to deterministic transitions with isomorphic net marking <=> utxo state**: The model
  net's initial marking and the SUT will be seeding according to some initial state. A cardano backend will be 
  instrumented to whitelist transactions according to commands, such as "only this RefundTx, this VoteTx, and this 
  TallyTx are currently allowed" (note: this explicitly encodes parallelism). The commands issues to the model will
  select from available firings; the same commands will be interpreted by the SUT to whitelist the equivalent
  transactions. At each step, we will inspect the marking and the chain state (after all actors have settled) for 
  equivalence.
- (strech goal) **Reachability analysis from the model**: the model and SUT will be initialized as before, but the 
  model will not fire. The SUT will be allowed to act autonomously, but the cardano backend will gate all transactions 
  after each submission. After each transaction is submitted, the test case will attempt to determine whether the 
  resulting utxo state is reachable from the model's initial marking, determine the valid firing sequence(s) if so, 
  and only pass test cases where the actual transactions submitted by the SUT match a valid firing sequence in the 
  model.
- (somewhat strech goal) **Additional assertions on initial, intermediate, and terminal states** Petri nets can be
  analyzed algorthimically. If the net and the model can be tested for bi-similarity, then this means that examining
  the _model itself_ for certain properties can build confidence that the SUT also has these properties. Thus, 
  assertions on the model itself may include include things like:
  - All terminal states result in deinitialization (and the corresponding marking)
  - Certain unreachability conditions, such as "there can never be a token each in the Unresolved and the Resolved
    place".
  - Certain live-lock or dead-lock conditions that can be calculated, such as "Unless condition X is met, a marking
    where a VoteTx is possible is always reachable"
  - Absence of cycles
- (stretch goal) **Exhaustive state bi-simulation**: We assume that the utxo model always begins with an initial state
  (given by the fallback transaction and the left-over rollouts and deposits) and ends in a terminal state, with no 
  possibility for cycles. Thus, there is a finite set of transition sequences that reach the terminal state.
  For small models (small numbers of peers, small evacuation map, small number of rollout/deposit leftovers), we can 
  iterate over these, filter the submission of transactions from peers to simulate each one, and ensure that there are
  (combinatorially) no other possible sequences of transitions that were rejected.

# Resources

- https://scg.unibe.ch/download/lectures/cp/CP-11-PetriNets-animated.pdf


