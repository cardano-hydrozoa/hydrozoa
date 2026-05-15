# Petri Net Library Documentation

This documents outline the design and motivations of the petri net library. It is currently an informal prose specification.

## Motivations

Fundamentally, the _description_ of a petri net has to be done imperatively.

At the very least, you are constrained by a temporal ordering -- which arcs or nodes do you declare first. The whole net can't come into existence at once.

### Properties of Nets

#### Topologies

Nets have _topologies_. They have, at minimum, transitions, places, and arcs (non-directional) that go between places and transitions. Certain representations of topologies are capable of type-safe topolgies, such as adjacency matricies or tensors (for arcs with multiplicity). 

Other representations, such as `Map[ArcId, (PlaceId, TransitionId)]`, may be more straight-forward to build, simulate, or display, at the cost of permitting the construction of invalid states. Such an invalid state would be a "dangling arc", where an `ArcId` points to a `PlaceId` or `TransitionId` that does not exist. In addition, a user may unintentionally overwrite a given `ArcId` in the `Map` if the API is poorly designed or carelessly used.

Thus, the convenient repesentations are in need of _soundess validtion_.

In addition, there are different restrictions or extensions that can be added to the notion of net topology. For example, can there be more than one arc between a given place and transition? If so, how is that collection of arcs represented? Is it ordered or unordered?

#### Semantics

Nets also have Semantics. The semantics decompose into _enabling semantics_, which determine whether a given transition is enabled, and _firing semantics_, which determines how the net updates when an enabled transition fires, and _auto-firing_ semantics which determines _which_ of potentially many enabled transitions fire at any given time.

For places, the semantics might include:
- Token coloring, identity, or net-observable data
- Capacity bounds
- Whether the place contributes to a final marking assessment
- Timed places, where tokens can only be added or removed after certain amounts of time
- Firing semantics such as "mixing functions" for non-fungible tokens in a place, to determine how tokens flow in or out.

**Enabling Condition E1**: For a transition $t$ to be enabled, it is necessary (but insufficient) that a "test" firing updates all connected places $p$ in a manner that does not violate the place semantics.

For arcs, the _enabling semantics_ define a predicate over the attached place, $p$, that is a necessary (but insufficient) condition for the attached transition $t$ to fire. 

- For "regular" arcs (weighted), this includes whether the tokens flow into or out of a place, as well as the associated weights
- Types of arcs with _only_ enabling conditions, such as:
  - Inhibitor arcs (unweighted) , which only enable if the place has 0 tokens. (This is strictly more expressive that _not_ having inhibitor arcs)
  - Read arcs (weighted), which only enable if `p.tokens >= weight`
- Other extensions include timed arcs (that take a certain amount of time to fire, or can't be refired until a certain amount of time passes), or stochastic arcs that fire probabilitically or consume variable numbers of tokens
- Reset arcs, that drain all tokens from a place

**Enabling Condition E2**: For a transition $t$ to be enabled, it is necessary that, for all arcs $a$ connected to $t$, `a.enablingPredicate(p) == true`.

For arcs, the _firing semantics_ are endomorphisms on the attached places. The endomorphisms determine how tokens are modified when an enabled transition fires.

For Transitions, the enabling semantics might include:
- As with arcs, timing or stochastic transitions.
- For Data-aware petri nets, the evaluation of pre-condition (guard) and post-condition _expressions_ contributes to the enabledness

Finally, nets as a whole sometimes must make meaningful semantic choices. One major example is when the choice of _topology_ forces this. If multiple arcs are allowed between the same place $p$ and the same place $t$, we must answer questions like:
- Do the enabledness predicate have to hold _throughout_ the firing of multiple arcs, or only at the beginning and end?
- If the firing endomorphisms are not commutative under composition, what order do we pick?
- Are transitions genuinely fireable in parallel? Or must we serialize firings? 
- What does it mean for two firings to conflict? How are they resolved?


#### Instantiation (a.k.a.: "Marking")

Even after setting the topology and semantics, we still need to actually apply data to the net. For places, this is setting the tokens (including quantity, color, data, etc.). For arcs, this is _usually_ empty, but some versions might want to assign data or variable logic to the arcs. For transitions in data-aware petri nets, this would include assigning pre/post-condition expressions.

But clearly whether a given instantion is _valid_ depends on the semantic -- a place with a maximum capacity of 5 must have between 0 and 5 tokens. Outside of dependently typed languages, this can be tricky to enforce. 

So we have a notion of whether a given marking _satisfies_ a given semantics.


#### Presentation

After all of this, we have addition presentation-related data, such as graphical labels and positions for nodes and paths for arcs.

### So what is a valid net?
  
It depends, and its not easy to answer in the general case. 

Its clear that each of these above properties has a notion of "total coverage" -- does every arc, place, or transition have an associated Topology, Semantics, Instantiation, and/or Presentation?

And for Topology and Instantion there is an additional notion of _soundness_ -- are there dangling arcs? Do the value assigned to each component obey the semantics of the net?

In fact, depending on the choice of Semantics, there may even be a notion of _unsatisfiability_, such that there are _no_ possible Instantiations that will satisfy the Semantics. Or, if the Semantics are satisfiable (trivially or otherwise), it may occur that no transitions are ever firable.

Thus the question moves from "what is a valid net" to "how do we determine whether a net is valid?"

In the general case -- allowing for arbitrary extensions to semantics -- its probably not possible to decide. But for _restricted_ cases, this can _probably_ be proved either formally or asserted informally and verified with model based testing. 

So the question becomes: if we have two restricted cases that we know have total coverage and are sound, can we combine them in such a way that we know the results are total and sound?

This brings us to...

### Gluing and Monoidal Combination (not supported yet)

If we have two total, sound, valid net representations, then we should be able to combine them monoidally (in parallel) and still have a total, sound, valid net represntation. (For the map representation, this would require taking the disjoint union of ids).

We also can "glue" places together if the two nets share a place semantic. 

## Design Goals

Clearly, there are many, many possible choices to make regarding topological representation, semantics, instantiation, and presentation.

(1): The primary design goal of this library is to separate these four concerns, such that we can interact with each one of these independently -- we should not need to know how places are connected to transitions in order to discuss how many tokens that can hold (capacity) or actually _do_ hold (instantiation), and we certinly should not need to specify their coordinates on a visualizer.

(2): The second goal is to allow for (future) polymorphic extensibility -- a user should be able to say "I want the places in this net to only have regular TP and PT arcs -- no inhibitor arcs, no timed arcs. I want a non-data-aware net, so transitions have no semantic. And I only want to allow at most one arc between any given place and transition", and this should be captured _at the type level_. Doing so allows us to then doing more refined analysis than ad-hoc extensibility would allow. But doing this also requires that users will have to define, for a given combinations of extensions, the composite semantics and soundness criterion. The library attempts to do this in a principled way, but it is not yet known if its it expressive enough or _too_ expressive.

(3): The third goal is to (eventually) allow polymorphic nets to be _combined_ via gluing and monoidal combination (see above) -- this means that if nets `N_1` and `N_2` have semantic `S_1` and `S_2`, respectively, we should be able to _combine those semantics_ as well. 

My _hypothesis_ is that this actually can be done in a generic way, but I don't have proof yet. Without row types/polymorphic variants, it may get ugly, but I believe it will be possible.

(4): Allow for _effectful_ transitions. This is where things start to get really interesting -- where you start to define the effects that transitions have, and the composite effects that nets can have, and then the composite effects that composed nets can have.

(5): Enable functional purity. All builders -- for topology, semantics, instantiation, and presentation -- should be monad transformer stacks (or similar) that are capable of raising errors and transforming types. In general, we should be looking at f-bounded polymorphism and `IndexedStateT` over `Either` or `Validated`.

Similarly, simulation and analysis tools should have functionally pure variants. Calling `def fire(tid : TransitionId)` should not throw an exception by default. However, for performance reasons, exception-throwing variants should also be exposed.

(6): Serializtion and Visualization by default. Every net should be serializable, as well as all firing logs. Right now I'm targeting YAPNE, but PNML or other custom serializtion formats (tikz, mermaid, etc) could be supported in the future.

(7, eventually): High performance simulation and analysis. Contrary to (5), there should be "mutable", "unsafe" variants that rely primarily on references to avoid traversing large data structures when reading net state or firing transitions, if necessary. In addition, there should be specific representations (incidence matricies/tensors) that enable fast, parallelizable simulation, and there should be tooling exposed to translate from convenient representations to performant ones.

(8, eventually): Multiplayer, concurrent updates. Nets should be able to queried and fired over API by multiple user simultaneouly. Similarly, they should be able to edit collaboratively, a la CRDT, excalidraw, or similar. 
