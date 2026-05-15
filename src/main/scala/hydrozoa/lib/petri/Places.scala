package hydrozoa.lib.petri

import hydrozoa.lib.number.{NonNegativeInt, PositiveInt}
import hydrozoa.lib.petri.net.components.Place
import hydrozoa.lib.petri.net.components.Place.Semantics.Bounded.Error.TooManyTokens

case class UnboundedPlace(
    override val label: String,
    override val tokens: NonNegativeInt = NonNegativeInt.unsafeApply(0),
    override val position: (Int, Int) = (0, 0),
    override val radius: PositiveInt = PositiveInt.unsafeApply(20),
) extends Place.Topology,
      Place.Semantics[UnboundedPlace],
      Place.Syntax.HasTokens[UnboundedPlace],
      Place.Presentation {
    // UnboundedPlace has no marking predicates, so markingErrors is always empty.
    override def withTokens(n: NonNegativeInt): UnboundedPlace = this.copy(tokens = n)

    override def getMarking: NonNegativeInt = tokens
}

case class BoundedPlace private (
    override val label: String,
    override val tokens: NonNegativeInt,
    override val bound: PositiveInt,
    override val position: (Int, Int) = (0, 0),
    override val radius: PositiveInt = PositiveInt.unsafeApply(20),
) extends Place.Topology,
      Place.Semantics.Bounded[BoundedPlace],
      Place.Presentation {
    override def withTokens(n: NonNegativeInt): BoundedPlace = this.copy(tokens = n)

    override def getMarking: NonNegativeInt = tokens
}

object BoundedPlace {
    def apply(
        label: String,
        tokens: NonNegativeInt = NonNegativeInt.unsafeApply(0),
        bound: PositiveInt,
        position: (Int, Int) = (0, 0),
        radius: PositiveInt = PositiveInt.unsafeApply(20)
    ): Either[TooManyTokens, BoundedPlace] =
        if tokens > bound then Left(TooManyTokens(tokens, bound))
        else Right(new BoundedPlace(label, tokens, bound, position, radius))

    def unsafeApply(
        label: String,
        tokens: NonNegativeInt = NonNegativeInt.unsafeApply(0),
        bound: PositiveInt,
        position: (Int, Int) = (0, 0),
        radius: PositiveInt = PositiveInt.unsafeApply(20)
    ): BoundedPlace =
        new BoundedPlace(label, tokens, bound, position, radius)
}
