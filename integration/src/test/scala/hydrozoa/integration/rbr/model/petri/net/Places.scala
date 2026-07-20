package hydrozoa.integration.rbr.model.petri.net

import hydrozoa.lib.petri.net.components.Place
import hydrozoa.lib.petri.net.components.Place.Semantics.MarkingError
import spire.math.Natural

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
    final override type PlaceMarking = Natural
}

object RBRPlaceId {
    given Ordering[RBRPlaceId] = Ordering.by(_.ordinal)
}

enum RBRPlaceId:
    case TreasuryRefPlaceId
    case DisputeRefPlaceId
    case RegimeRefPlaceId
    case SetupLadderRefPlaceId
    case UnresolvedTreasuryPlaceId
    case ResolvedTreasuryPlaceId
    case UnvotedPlaceId
    case VotedPlaceId
    case PayoutObligationsPlaceId // synthetic
    case EvacuationOutputPlaceId
    case CollateralPlaceId
    case AmbientPlaceId

object TreasuryRefPlace {
    case object OnlyOneTokenAllowed extends MarkingError {
        override def getMessage: String = "TreasuryRef place must have exactly one token"
    }
}

case class TreasuryRefPlace(
    override val marking: Natural = Natural.one,
    override val finalMarking: Option[Natural] = Some(Natural.one),
) extends RBRPlace {
    override def mark(n: Natural): TreasuryRefPlace = copy(marking = n)
    override def withFinalMarking(m: Option[Natural]): TreasuryRefPlace =
        copy(finalMarking = m)

    override def markingError: Option[MarkingError] =
        if marking != Natural.one then Some(TreasuryRefPlace.OnlyOneTokenAllowed) else None

}

object RegimeRefPlace {
    case object OnlyOneTokenAllowed extends MarkingError {
        override def getMessage: String = "RegimeRef place must have exactly one token"
    }
}

/** The rule-based regime utxo (HRWT beacon + head-identity datum): created by fallback, read by
  * the dispute-side txs, consumed only at deinit.
  */
case class RegimeRefPlace(
    override val marking: Natural = Natural.one,
    override val finalMarking: Option[Natural] = Some(Natural.one),
) extends RBRPlace {
    override def mark(n: Natural): RegimeRefPlace = copy(marking = n)
    override def withFinalMarking(m: Option[Natural]): RegimeRefPlace =
        copy(finalMarking = m)

    override def markingError: Option[MarkingError] =
        if marking != Natural.one then Some(RegimeRefPlace.OnlyOneTokenAllowed) else None
}

/** The seven deployed-once G2 setup-ladder rungs, read (one per tx) by evacuations. */
case class SetupLadderRefPlace(
    override val marking: Natural = Natural(7L),
    override val finalMarking: Option[Natural] = Some(Natural(7L)),
) extends RBRPlace {
    override def mark(n: Natural): SetupLadderRefPlace = copy(marking = n)
    override def withFinalMarking(m: Option[Natural]): SetupLadderRefPlace =
        copy(finalMarking = m)
}

case class ResolvedPlace(
    override val marking: Natural = Natural.zero,
    override val finalMarking: Option[Natural] = None,
) extends RBRPlace {
    override def mark(n: Natural): ResolvedPlace = copy(marking = n)
    override def withFinalMarking(m: Option[Natural]): ResolvedPlace =
        copy(finalMarking = m)
}

case class AmbientPlace(
    override val marking: Natural = Natural.zero,
    override val finalMarking: Option[Natural] = None,
) extends RBRPlace {
    override def mark(n: Natural): AmbientPlace = copy(marking = n)
    override def withFinalMarking(m: Option[Natural]): AmbientPlace =
        copy(finalMarking = m)
}

case class PayoutObligationsPlace(
    override val marking: Natural = Natural.zero,
    override val finalMarking: Option[Natural] = Some(Natural.zero),
) extends RBRPlace {
    override def mark(n: Natural): PayoutObligationsPlace = copy(marking = n)
    override def withFinalMarking(m: Option[Natural]): PayoutObligationsPlace =
        copy(finalMarking = m)
}

case class EvacuationOutputPlace(
    override val marking: Natural = Natural.zero,
    override val finalMarking: Option[Natural] = None,
) extends RBRPlace {
    override def mark(n: Natural): EvacuationOutputPlace = copy(marking = n)
    override def withFinalMarking(m: Option[Natural]): EvacuationOutputPlace =
        copy(finalMarking = m)
}

object CollateralPlace {
    case class WrongCount(expected: Int, got: Natural) extends MarkingError {
        override def getMessage: String =
            s"Wrong number of collateral utxos. Expected: $expected; got $got"
    }
}

// expectedCount is the required number of collateral utxos (typically nHeadPeers).
case class CollateralPlace(
    override val marking: Natural,
    val expectedCount: Int,
    override val finalMarking: Option[Natural] = None,
) extends RBRPlace {
    override def mark(n: Natural): CollateralPlace = copy(marking = n)
    override def withFinalMarking(m: Option[Natural]): CollateralPlace =
        copy(finalMarking = m)

    override def markingError: Option[MarkingError] =
        if marking != Natural(expectedCount.toLong)
        then Some(CollateralPlace.WrongCount(expectedCount, marking))
        else None
}
