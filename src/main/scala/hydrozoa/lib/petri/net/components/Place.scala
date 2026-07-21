package hydrozoa.lib.petri.net.components

import hydrozoa.lib.number.PositiveInt
import hydrozoa.lib.petri.net.components.Place.Semantics.Bounded.Error.TooManyTokens
import spire.math.Natural

/** A "Place" defines the following properties in the following ontological order:
  *   - [[Place.Topology]]: any restrictions on what or how many arcs can connect to this place.
  *     Generally this empty.
  *   - [[Place.Syntax]]: what type of data this place carries, and how to update the place with a
  *     new marking.
  *   - [[Place.Semantics]]: how to interpret the marking of this place. This includes whether a
  *     marking is valid, and is where things like codecs or quotient functions should live
  *   - [[Place.Presentation]]: how to render this place for a specific visualizer
  *
  * Note that the _type_ of [[Place.Syntax#PlaceMarking]] _also_ gives semantics the place -- using
  * a `NonNegativeInt` means that the place cannot be constructed with negative tokens _ever_. This
  * is _not_ the same as having a place with `PlaceMarking = Int` and then setting the
  * [[Place.Semantics]] to check for a non-negative number -- one is a compile-time check, one is a
  * run-time check. Prefer compile time when possible.
  */
object Place {

    trait Id[PlaceId] {
        val id: PlaceId
    }

    // Stub. Component-side acceptance predicates (e.g. "acceptArc") are deferred. Type-level
    // arc↔place color compatibility is enforced by the net builder; net-wide structural validation
    // (dangling arcs) lives in net.Topology.
    trait Topology

    /** Otherwise known as a "marking"
      *
      *   - Laws:
      *     - mark(marking).marking == marking
      */
    trait Syntax[Self <: Syntax[Self]] {
        self: Self =>
        type PlaceMarking

        def marking: PlaceMarking

        /** Replace the current marking with `newMarking`. Every concrete place type must implement
          * this explicitly.
          */
        def mark(newMarking: PlaceMarking): Self
    }

    object Syntax {

        /** Refinement alias pinning the marking type — used as the place bound of simulators. */
        type Marked[Self <: Syntax[Self], M] = Syntax[Self] { type PlaceMarking = M }

        /** The common case of token-counting places: [[PlaceMarking]] fixed to the unbounded
          * [[Natural]] (ISO markings are ℕ-valued, Concept 8).
          */
        type WithTokens[Self <: Syntax[Self]] = Marked[Self, Natural]

        /** Mixin for places that carry a target terminal marking. Parametric over [[PlaceMarking]]
          * so it applies equally to token-counting places, colored nets, or any other marking type.
          */
        trait HasFinalMarking[Self <: HasFinalMarking[Self]] extends Syntax[Self] {
            self: Self =>
            val finalMarking: Option[PlaceMarking]
            def withFinalMarking(m: Option[PlaceMarking]): Self
        }
    }

    trait Semantics[Self <: Place.Syntax[Self] & Semantics[Self]] { self: Self =>
        type MarkingError = Place.Semantics.MarkingError

        def markingError: Option[MarkingError] = None

        final def validMarking: Boolean = markingError.isEmpty

        final def markValid(newMarking: PlaceMarking): Either[MarkingError, Self] =
            val candidateMarking = mark(newMarking)
            candidateMarking.markingError.toLeft(candidateMarking)
    }

    object Semantics {

        trait MarkingError extends Throwable

        trait Bounded[Self <: Place.Syntax.WithTokens[Self] & Semantics[Self]]
            extends Semantics[Self] {
            self: Self =>
            val bound: PositiveInt

            override def markingError: Option[Place.Semantics.MarkingError] =
                Option.when(marking > Natural(bound.toInt.toLong))(TooManyTokens(marking, bound))
        }

        object Bounded {
            object Error {
                case class TooManyTokens(tokens: Natural, bound: PositiveInt) extends MarkingError {
                    override def getMessage: String =
                        "Failed constructing place: too many tokens. " +
                            s"Max: $bound. Received: $tokens"
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
