package hydrozoa.lib.petri

import hydrozoa.lib.number.PositiveInt
import hydrozoa.lib.petri.net.components.Place
import hydrozoa.lib.petri.net.components.Place.Semantics.Bounded.Error.TooManyTokens
import spire.math.Natural

case class UnboundedPlace(
    override val label: String,
    override val marking: Natural = Natural.zero,
    override val position: (Int, Int) = (0, 0),
    override val radius: PositiveInt = PositiveInt.unsafeApply(20),
) extends Place.Topology,
      Place.Syntax[UnboundedPlace],
      Place.Semantics[UnboundedPlace],
      Place.Presentation {
    // UnboundedPlace has no marking predicates, so markingError is always None.
    override type PlaceMarking = Natural
    override def mark(n: Natural): UnboundedPlace = copy(marking = n)
}

case class BoundedPlace private (
    override val label: String,
    override val marking: Natural,
    override val bound: PositiveInt,
    override val position: (Int, Int),
    override val radius: PositiveInt,
) extends Place.Topology,
      Place.Syntax[BoundedPlace],
      Place.Semantics.Bounded[BoundedPlace],
      Place.Presentation {
    override type PlaceMarking = Natural
    override def mark(n: Natural): BoundedPlace = copy(marking = n)
}

object BoundedPlace {
    def apply(
        label: String,
        marking: Natural = Natural.zero,
        bound: PositiveInt,
        position: (Int, Int) = (0, 0),
        radius: PositiveInt = PositiveInt.unsafeApply(20)
    ): Either[TooManyTokens, BoundedPlace] =
        if marking > Natural(bound.toInt.toLong) then Left(TooManyTokens(marking, bound))
        else Right(new BoundedPlace(label, marking, bound, position, radius))

    def unsafeApply(
        label: String,
        marking: Natural = Natural.zero,
        bound: PositiveInt,
        position: (Int, Int) = (0, 0),
        radius: PositiveInt = PositiveInt.unsafeApply(20)
    ): BoundedPlace =
        new BoundedPlace(label, marking, bound, position, radius)
}
