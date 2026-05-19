package hydrozoa.integration.rbr.model.petri.net

import hydrozoa.lib.number.NonNegativeInt
import hydrozoa.lib.petri.net.components.Place
import hydrozoa.lib.petri.net.components.Place.Semantics.MarkingError

/** Common base for all RBR places. Using a concrete `Self = RBRPlace` (rather than an F-bounded
  * trait) lets the type be used directly as the `P` parameter in `MapNet`.
  *
  * Every place carries an optional `finalMarking` (default `None`). Concrete place types override
  * it with a domain-appropriate default and provide covariant implementations of `mark` and
  * `withFinalMarking`.
  */
sealed abstract class RBRPlace
    extends Place.Topology,
      Place.Syntax.HasFinalMarking[RBRPlace],
      Place.Semantics[RBRPlace] {
    final override type PlaceMarking = NonNegativeInt
}

object RBRPlaceId {
    given Ordering[RBRPlaceId] = Ordering.by(_.ordinal)
}

enum RBRPlaceId:
    case TreasuryRefPlaceId
    case ResolvedTreasuryPlaceId
    case ResolvedPlaceId
    case AmbientPlaceId
    case PayoutObligationsPlaceId // synthetic
    case EvacuationOutputPlaceId
    case CollateralPlaceId
    case VotedPlaceId
    case UnvotedPlaceId
    case DisputeRefPlaceId

object TreasuryRefPlace {
    case object OnlyOneTokenAllowed extends MarkingError {
        override def getMessage: String = "TreasuryRef place must have exactly one token"
    }
}

case class TreasuryRefPlace(
    override val marking: NonNegativeInt = NonNegativeInt.unsafeApply(1),
    override val finalMarking: Option[NonNegativeInt] = Some(NonNegativeInt.unsafeApply(1)),
) extends RBRPlace {
    override def mark(n: NonNegativeInt): TreasuryRefPlace = copy(marking = n)
    override def withFinalMarking(m: Option[NonNegativeInt]): TreasuryRefPlace =
        copy(finalMarking = m)

    override def markingErrors: List[MarkingError] =
        if marking.toInt != 1 then TreasuryRefPlace.OnlyOneTokenAllowed :: super.markingErrors
        else super.markingErrors
}

case class ResolvedPlace(
    override val marking: NonNegativeInt = NonNegativeInt.unsafeApply(0),
    override val finalMarking: Option[NonNegativeInt] = None,
) extends RBRPlace {
    override def mark(n: NonNegativeInt): ResolvedPlace = copy(marking = n)
    override def withFinalMarking(m: Option[NonNegativeInt]): ResolvedPlace =
        copy(finalMarking = m)
}

case class AmbientPlace(
    override val marking: NonNegativeInt = NonNegativeInt.unsafeApply(0),
    override val finalMarking: Option[NonNegativeInt] = None,
) extends RBRPlace {
    override def mark(n: NonNegativeInt): AmbientPlace = copy(marking = n)
    override def withFinalMarking(m: Option[NonNegativeInt]): AmbientPlace =
        copy(finalMarking = m)
}

case class PayoutObligationsPlace(
    override val marking: NonNegativeInt = NonNegativeInt.unsafeApply(0),
    override val finalMarking: Option[NonNegativeInt] = Some(NonNegativeInt.unsafeApply(0)),
) extends RBRPlace {
    override def mark(n: NonNegativeInt): PayoutObligationsPlace = copy(marking = n)
    override def withFinalMarking(m: Option[NonNegativeInt]): PayoutObligationsPlace =
        copy(finalMarking = m)
}

case class EvacuationOutputPlace(
    override val marking: NonNegativeInt = NonNegativeInt.unsafeApply(0),
    override val finalMarking: Option[NonNegativeInt] = None,
) extends RBRPlace {
    override def mark(n: NonNegativeInt): EvacuationOutputPlace = copy(marking = n)
    override def withFinalMarking(m: Option[NonNegativeInt]): EvacuationOutputPlace =
        copy(finalMarking = m)
}

object CollateralPlace {
    case class WrongCount(expected: Int, got: Int) extends MarkingError {
        override def getMessage: String =
            s"Wrong number of collateral utxos. Expected: $expected; got $got"
    }
}

// expectedCount is the required number of collateral utxos (typically nHeadPeers).
case class CollateralPlace(
    override val marking: NonNegativeInt,
    val expectedCount: Int,
    override val finalMarking: Option[NonNegativeInt] = None,
) extends RBRPlace {
    override def mark(n: NonNegativeInt): CollateralPlace = copy(marking = n)
    override def withFinalMarking(m: Option[NonNegativeInt]): CollateralPlace =
        copy(finalMarking = m)

    override def markingErrors: List[MarkingError] =
        if marking.toInt != expectedCount
        then CollateralPlace.WrongCount(expectedCount, marking.toInt) :: super.markingErrors
        else super.markingErrors
}
