package hydrozoa.lib.petri

import hydrozoa.lib.number.{NonNegativeInt, PositiveInt}
import hydrozoa.lib.petri.net.components.Place
import hydrozoa.lib.petri.net.components.Place.Semantics.Bounded.Error.TooManyTokens

case class UnboundedPlace(
    override val label: String,
    override val marking: NonNegativeInt = NonNegativeInt.unsafeApply(0),
    override val position: (Int, Int) = (0, 0),
    override val radius: PositiveInt = PositiveInt.unsafeApply(20),
) extends Place.Topology,
      Place.Syntax[UnboundedPlace],
      Place.Semantics[UnboundedPlace],
      Place.Presentation {
    // UnboundedPlace has no marking predicates, so markingError is always None.
    override type PlaceMarking = NonNegativeInt
    override def mark(n: NonNegativeInt): UnboundedPlace = copy(marking = n)
}

case class BoundedPlace private (
    override val label: String,
    override val marking: NonNegativeInt,
    override val bound: PositiveInt,
    override val position: (Int, Int),
    override val radius: PositiveInt,
) extends Place.Topology,
      Place.Syntax[BoundedPlace],
      Place.Semantics.Bounded[BoundedPlace],
      Place.Presentation {
    override type PlaceMarking = NonNegativeInt
    override def mark(n: NonNegativeInt): BoundedPlace = copy(marking = n)
}

object BoundedPlace {
    def apply(
        label: String,
        marking: NonNegativeInt = NonNegativeInt.unsafeApply(0),
        bound: PositiveInt,
        position: (Int, Int) = (0, 0),
        radius: PositiveInt = PositiveInt.unsafeApply(20)
    ): Either[TooManyTokens, BoundedPlace] =
        if marking > bound then Left(TooManyTokens(marking, bound))
        else Right(new BoundedPlace(label, marking, bound, position, radius))

    def unsafeApply(
        label: String,
        marking: NonNegativeInt = NonNegativeInt.unsafeApply(0),
        bound: PositiveInt,
        position: (Int, Int) = (0, 0),
        radius: PositiveInt = PositiveInt.unsafeApply(20)
    ): BoundedPlace =
        new BoundedPlace(label, marking, bound, position, radius)
}
