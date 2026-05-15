package hydrozoa.lib.petri.net.components

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
    }

    trait Semantics[Self <: Place.Syntax[Self] & Semantics[Self]] { self: Self =>
        type MarkingError = Place.Semantics.MarkingError
        def markingError(marking: PlaceMarking): MarkingError
        protected def markingPredicate: List[Boolean] = List.empty

        final def validMarking: Boolean =
            markingPredicate.forall(identity)
        final def markValid(newMarking: PlaceMarking): Either[MarkingError, Self] =
            val candidateMarking = mark(newMarking)
            if candidateMarking.validMarking
            then Right(candidateMarking)
            else Left(markingError(newMarking))
    }

    object Semantics {

        trait MarkingError extends Throwable


        trait Bounded[Self <: Syntax.HasTokens[Self] & Semantics[Self]]
            extends Semantics[Self]
            with Syntax.HasTokens[Self] {
            self: Self =>
            val bound: PositiveInt
            
            override protected def markingPredicate: List[Boolean] =
                (this.tokens.toInt <= bound.toInt) :: super.markingPredicate

        }
        
        object Bounded {
            object Error {
                case class TooManyTokens(tokens: NonNegativeInt, bound: PositiveInt) extends MarkingError {
                    override def getMessage: String = "Failed constructing place: too many tokens. " +
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

case class UnboundedPlace(
    override val label: String,
    override val tokens: NonNegativeInt = NonNegativeInt.unsafeApply(0),
    override val position: (Int, Int) = (0, 0),
    override val radius: PositiveInt = PositiveInt.unsafeApply(20),
) extends Place.Topology,
      Place.Semantics[UnboundedPlace],
      Place.Syntax.HasTokens[UnboundedPlace],
      Place.Presentation {
    // UnboundedPlace has no marking predicates, so validMarking is always true and
    // markingError can never be called from markValid. Nothing encodes "no error possible".
    override def markingError(marking: NonNegativeInt): Nothing =
        scala.sys.error("BUG: UnboundedPlace cannot have a marking error")

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
    
    override def markingError(marking: NonNegativeInt): MarkingError = TooManyTokens(marking, bound)

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
