package hydrozoa.lib.petri.hlpn

/** A minimal HLPN transition for mode search: the variables it binds (ISO `V`), its guard `Φ`, and
  * its arcs (each an [[ArcSemanticsH]] connected to a place). Direction-neutral — an arc constrains
  * enabling only through its own `pre` / `sideCondition`, so output arcs never block. This is the
  * standalone aggregate the enabling engine runs against, ahead of full net integration; it fixes a
  * single color type `C`, which suffices while place colors are base classes rather than products.
  */
final case class TransitionH[PlaceId, C](
    variables: List[Var[C]],
    guard: Guard,
    arcs: List[(PlaceId, ArcSemanticsH[C])]
)
