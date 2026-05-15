package hydrozoa.lib.petri.net

/** Structural transformation operations on a net. Separate from [[Topology]] (validation)
  * and [[hydrozoa.lib.petri.Simulator]] (firing semantics).
  *
  * Laws:
  *   - `pruneInvalid.validTopology == true` (assuming the net also extends [[Topology]] and
  *     its `validTopology` is decidable)
  *   - `compact` is idempotent: `n.compact.compact == n.compact`
  */
trait Normalization[Self <: Normalization[Self]] { self: Self =>

    /** Remove all topologically invalid elements according to implementation-specific
      * logic. For MapNet the default will be:
      *   - Remove all arcs whose `arcPlaceId` or `arcTransitionId` is absent from the
      *     net's ID sets.
      *   - For each (place, transition) pair with duplicate arcs, retain only the first
      *     in ArcId ordering; remove the rest.
      *
      * Post-condition:
     *
     * Laws:
     *   - Functionally pure. It does not need to throw errors, because the empty net is always trivially valid.
     *   - if this net also extends [[Topology]], then `this.pruneInvalid.validTopology == true`.
     *   - idempotent
      */
    def pruneInvalid: Self


    /** Produce a isomorphic net in a normalised form. Uses include stable
      * JSON serialisation, simulation performance, and isomorphism testing.
      *
      * Laws:
     *   - Must be idempotent: `n.compact.compact == n.compact`.
     *   - Loosely stated, this net should be isomorphic. In practice, this means that _most_ exposed operations
     *     from the net's interface (net.Topology, net.Syntax, net.Semantics, and Simulator) should return identical
     *     results before and after compaction.
      */
    def compact: Self
}
