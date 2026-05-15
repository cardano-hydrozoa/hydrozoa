package hydrozoa.lib.petri.net.components

import cats.data.NonEmptyList
import hydrozoa.lib.number.{NonNegativeInt, PositiveInt}
import hydrozoa.lib.petri.net.components.Place.Semantics.Bounded.Error.TooManyTokens

/** A "Place" defines the following properties in the following ontological order:
  *   - [[Place.Topology]]: any restrictions on what or how many arcs can connect to this place.
  *     Generally this empty.
  *   - [[Place.Syntax]]: what type of data this place carries, and how to update the place with a
  *     new marking.
  *   - [[Place.Semantics]]: how to interpret the marking of this place. This includes whether a
  *     marking is valid, and is where things like codecs or quotient functions should live
  *   - [[Place.Presentation]]: how to render this place for a specific visualizer
  *
  * Syntax and semantics are defined via _mixins_. The idea is that we should be able to mixin
  * multiple pieces of syntax (independent data) and then multiple predicates over that data. See
  * [[BoundedPlace]] and [[UnboundedPlace]] for examples. If implemented properly, mixin order
  * SHOULD NOT matter; all of the functions SHOULD be commutative under composition.
  *
  * Due to the mixin strategy, you MUST override certain protected defs. IDEA won't prompt you,
  * because we also _need_ a base case of List.empty.
  *
  * Note that the _type_ of [[Place.Syntax#PlaceMarking]] _also_ gives semantics the place -- using
  * a `NonNegativeInt` means that the place cannot be constructed with negative tokens _ever_. This
  * is _not_ the same as having a place with `PlaceMarking = Int` and then setting the
  * [[Place.Semantics]] to check for a non-negative number -- one is a compile-time check, one is a
  * run-time check. Prefer compile time when possible. *
  */
object Place {

    trait Id[PlaceId] {
        val id: PlaceId
    }

    // Stub. Component-side acceptance predicates (e.g. "acceptArc") are deferred until the
    // builder (IndexedStateT stack) is designed. Type-level arc-type compatibility is enforced
    // by MapNet's `A` type bound at compile time. Net-wide structural validation (dangling
    // arcs, multiplicity) lives in net.Topology.
    trait Topology

    /** Otherwise known as a "marking"
      *
      *   - Laws:
      *     - mark(marking).getMarking == marking
      */
    trait Syntax[Self <: Syntax[Self]] {
        self: Self =>
        type PlaceMarking

        def getMarking: PlaceMarking

        /** Each extension of this should append an endomorphism to this list, updating `Self`
          * _specifically_ with its own data.
          *
          * Note that if you have a laws-compliant instance, it is likely that the endomorphisms
          * will need to commute under composition.
          * @param newMarking
          * @return
          */
        protected def markingEndos(newMarking: PlaceMarking): List[Self => Self] = List.empty

        final def mark(newMarking: PlaceMarking): Self =
            markingEndos(newMarking).foldLeft(this)((acc, endo) => endo(acc))

    }

    object Syntax {
        trait HasTokens[Self <: HasTokens[Self]] extends Syntax[Self] {
            self: Self =>
            override type PlaceMarking = NonNegativeInt
            val tokens: NonNegativeInt
            def withTokens(n: NonNegativeInt): Self

            abstract override def markingEndos(newMarking: NonNegativeInt): List[Self => Self] =
                ((acc: Self) => acc.withTokens(newMarking)) :: super.markingEndos(newMarking)
        }

        /** Mixin for places that carry a target terminal token count. Extends [[HasTokens]] because
          * `finalMarking: Option[NonNegativeInt]` is only interpretable when tokens are a
          * `NonNegativeInt` count — this would not make sense on, e.g., a colored net.
          */
        trait HasFinalMarking[Self <: HasFinalMarking[Self]] extends HasTokens[Self] {
            self: Self =>
            val finalMarking: Option[NonNegativeInt]
            def withFinalMarking(m: Option[NonNegativeInt]): Self
        }
    }

    trait Semantics[Self <: Place.Syntax[Self] & Semantics[Self]] { self: Self =>
        type MarkingError = Place.Semantics.MarkingError

        def markingErrors: List[MarkingError] = List.empty

        final def validMarking: Boolean = markingErrors.isEmpty

        final def markValid(newMarking: PlaceMarking): Either[NonEmptyList[MarkingError], Self] =
            val candidateMarking = mark(newMarking)
            NonEmptyList.fromList(candidateMarking.markingErrors).toLeft(candidateMarking)
    }

    object Semantics {

        trait MarkingError extends Throwable

        trait Bounded[Self <: Syntax.HasTokens[Self] & Semantics[Self]]
            extends Semantics[Self]
            with Syntax.HasTokens[Self] {
            self: Self =>
            val bound: PositiveInt

            override def markingErrors: List[Place.Semantics.MarkingError] =
                (if this.tokens.toInt > bound.toInt then List(TooManyTokens(tokens, bound))
                 else Nil) ++ super.markingErrors

        }

        object Bounded {
            object Error {
                case class TooManyTokens(tokens: NonNegativeInt, bound: PositiveInt)
                    extends MarkingError {
                    override def getMessage: String =
                        "Failed constructing place: too many tokens. " +
                            s"Max: $bound. Received: ${tokens.convert}"
                }
            }
        }
    }

    trait Presentation {
        val label: String
        val position: (Int, Int)
        val radius: PositiveInt
    }

}
