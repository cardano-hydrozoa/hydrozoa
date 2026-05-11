package hydrozoa.lib.petri

import hydrozoa.lib.number.{NonNegativeInt, PositiveInt}
import hydrozoa.lib.petri.Place.PlaceCapacity
import hydrozoa.lib.petri.Place.PlaceCapacity.{Bounded, Unlimited}

object Place {
    case class PlacePresentation[Label](
        label: Label,
        position: (Int, Int),
        // Perhaps positive? In general, this should be sensible
        radius: NonNegativeInt
    )

    enum PlaceCapacity:
        case Unlimited
        case Bounded(positiveInt: PositiveInt)

    case class TooManyTokens(tokens: NonNegativeInt, bound: PositiveInt)

    def apply[PlaceId](
        id: PlaceId,
        tokens: NonNegativeInt,
        capacity: PlaceCapacity,
        finalMarking: Option[NonNegativeInt]
    ): Either[TooManyTokens, Place[PlaceId]] = capacity match {
        case Unlimited => Right(new Place(id, tokens, capacity, finalMarking))
        case Bounded(bound) =>
            if tokens.convert > capacity.ordinal
            then Left(TooManyTokens(tokens, bound))
            else Right(new Place(id, tokens, capacity, finalMarking))
    }
}

case class Place[Id] private (
    // Identifier for the place, must be unique
    id: Id,
    tokens: NonNegativeInt,
    capacity: Place.PlaceCapacity,
    finalMarking: Option[NonNegativeInt]
)
