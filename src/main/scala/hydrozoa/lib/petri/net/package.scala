package hydrozoa.lib.petri.net

/** Package-level documentation for [[hydrozoa.lib.petri.net]].
  *
  * TODO: Document the following patterns used throughout this package (see also
  * [[hydrozoa.lib.petri.net.components]] for component-level pattern documentation):
  *   - F-bounded polymorphism for net-level traits
  *   - IndexedStateT builder monad transformer stack
  *   - How net-level traits compose with component-level traits
  *
  * ## Ontological ordering
  *
  * Following the same ordering as the component layer:
  *   - [[Id]]: the authoritative sets of place, transition, and arc IDs
  *   - [[Topology]]: structural constraints (what connects to what, arc multiplicity rules)
  *   - [[Syntax]]: the actual data collections and net-wide syntax (data variables, etc.)
  *   - [[Semantics]]: enabling rules, firing strategy, autofiring policy
  *   - [[Presentation]]: visual layout
  *
  * ## Arc multiplicity and firing endomorphism ordering
  *
  * See [[hydrozoa.lib.petri.net.components]] for the general discussion. At the net level: the
  * default topology restricts to at most one arc per (place, transition) direction, making firing
  * endomorphisms trivially order-independent. Topologies that permit multi-arcs must either impose
  * an explicit ordering or prove/property-test commutativity for their arc combinations.
  *
  * ## Net composition
  *
  * Two operations are planned (neither implemented yet):
  *   - **Monoidal combination** (parallel): the disjoint union of two nets. Requires disjoint ID
  *     sets; each net retains full ownership of its own places, transitions, and arcs.
  *   - **Gluing**: identifies a subset of places from one net with a subset from another,
  *     effectively merging them into shared interface places. Gluing is a _separate composition
  *     operator_, not something expressible within a single net's topology. After gluing, the
  *     resulting net still "owns" its transitions independently — enabling and firing delegate to
  *     whichever sub-net owns the transition.
  *
  * Autofiring semantics for composed nets (interleaved, prioritized, synchronized, etc.) are
  * themselves a semantic choice encoded in the composed net's [[Semantics]], not inherited
  * automatically from the sub-nets.
  */
