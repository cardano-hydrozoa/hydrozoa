package hydrozoa.lib.petri.net.components

import hydrozoa.lib.number.{NonNegativeInt, PositiveInt}

import Place.Error.TooManyTokens
import Place.PlaceCapacity
import Place.PlaceCapacity.{Bounded, Unlimited}

object Place {

    sealed trait Error extends Throwable

    object Error {
        case class TooManyTokens(tokens: NonNegativeInt, bound: PositiveInt) extends Error {
            override def getMessage: String = "Failed constructing place: too many tokens. " +
                s"Max: $bound. Received: ${tokens.convert}"
        }
    }

    trait Id[PlaceId] {
        val id: PlaceId
    }

    // Stub: there are not currently any topological properties. In the future, there may be typed places
    trait Topology

    trait Semantics {
        val capacity: PlaceCapacity
        val finalMarking: Option[NonNegativeInt]
    }

    // We might eventually expand this for colored petri nets, token-nets, or data-carrying tokens.
    // OVERRIDE THIS
    trait Simulation {
        val tokens: NonNegativeInt
    }

    trait Presentation {
        val label: String
        val position: (Int, Int)
        val radius: PositiveInt
    }

    enum PlaceCapacity:
        case Unlimited
        case Bounded(positiveInt: PositiveInt)
}

/** A canonical "Place" case class, without an ID.
  */
// QUESTION: Where should we validate capacity bounds? Should we allow for constructing an invalid place and
// validating later?
// TODO: update this to take a `Place.Topology`, `Place.Semantics`, `Place.Simulation`, `Place.Presentation` directly,
// and re-expose the methods
case class PlaceNoId private (
    override val label: String,
    override val capacity: PlaceCapacity,
    override val finalMarking: Option[NonNegativeInt],
    override val tokens: NonNegativeInt,
    override val position: (Int, Int),
    override val radius: PositiveInt
) extends Place.Topology,
      Place.Semantics,
      Place.Simulation,
      Place.Presentation {

    def setTokens(tokens: NonNegativeInt): Either[TooManyTokens, PlaceNoId] =
        PlaceNoId.apply(label, capacity, finalMarking, tokens, position, radius)

    // TODO: Restrict this, perhaps?
    def setTokensUnsafe(tokens: NonNegativeInt): PlaceNoId =
        new PlaceNoId(label, capacity, finalMarking, tokens, position, radius)
}

object PlaceNoId {
    def apply(
        label: String,
        capacity: PlaceCapacity = PlaceCapacity.Unlimited,
        finalMarking: Option[NonNegativeInt] = None,
        tokens: NonNegativeInt = NonNegativeInt.unsafeApply(0),
        position: (Int, Int) = (0, 0),
        radius: PositiveInt = PositiveInt.unsafeApply(20)
    ): Either[TooManyTokens, PlaceNoId] =
        val place = new PlaceNoId(label, capacity, finalMarking, tokens, position, radius)

        capacity match {
            case Unlimited                                 => Right(place)
            case Bounded(b) if b.convert >= tokens.convert => Right(place)
            case Bounded(b)                                => Left(TooManyTokens(tokens, b))
        }

    // TODO: wrap the error handling in a builder monad rather than exposing unsafe methods
    def unsafeApply(
        label: String,
        capacity: PlaceCapacity = PlaceCapacity.Unlimited,
        finalMarking: Option[NonNegativeInt] = None,
        tokens: NonNegativeInt = NonNegativeInt.unsafeApply(0),
        position: (Int, Int) = (0, 0),
        radius: PositiveInt = PositiveInt.unsafeApply(20)
    ): PlaceNoId =
        new PlaceNoId(label, capacity, finalMarking, tokens, position, radius)
}
