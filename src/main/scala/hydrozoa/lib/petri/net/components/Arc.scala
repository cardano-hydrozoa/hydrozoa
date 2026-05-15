package hydrozoa.lib.petri.net.components

import cats.data.Kleisli
import cats.implicits.*
import hydrozoa.lib.cats.data.Kendo.{Kendo as KendoT, kendoFold}
import hydrozoa.lib.number.NonNegativeInt

import scala.collection.immutable.Queue

/** This encodes the topological, configuration, simulation, and presentation data for arcs. Right
  * now, I'm going to keep it simple. But petri nets come in many, many flavors. Eventually, we may
  * need to parameterize this on different types of arcs, multiarcs, data/logic-arcs, and possibly
  * even encode effectful firing logic for things like timed arcs.
  */
object Arc {
    trait Id[ArcId] {
        val id: ArcId
    }

    // TODO: This should probably take the Place and Transition _topology_ instead of just their ID? 
    trait Topology[PlaceId, TransitionId] {
        def arcPlaceId: PlaceId
        def arcTransitionId: TransitionId
    }

    // There does not appear to be any arc-specific syntax right now. In a more generic setup, there
    // might be logic or data needed here in the future.
    trait Syntax

    // YAPNE supports 4 "types" of arcs:
    //  - Regular arcs (which add/remove tokens from places)
    //  - Inhibitor arcs, which only enable if the connect place is empty
    //  - Reset arcs, which remove all tokens from the connect place
    //  - Read, which only allow for enabling the connected place has the requisite number of tokens.
    //  I believe this is problematic (see https://github.com/chimenkamp/YAPNE-Yet-Another-Petri-Net-Editor/issues/13),
    //  so we're going to refine it as follows

    /** Arc semantics:
      *   - The arc has _enabling semantics_, in the form of an _arc-side_ enabling predicate. This
      *     is a predicate that evaluates against the actual state (marking) of the place when the
      *     arc is fired, and thus only applies to that _specific_ arc-place combination.
      *     - This is in contrast to the _place-side_ enabling predicate (given by Place.Semantics),
      *       which is a predicate on the place instantiation that has to hold for _all_ arcs going
      *       into the place.
      *   - The arc has _firing semantics_, given by an endomorphism on the place marking. The arc
      *     is type-parameterized on a specific concrete `Place.Instantiation` and, via a value of
      *     this type, knows how to update it.
      *     - Firing can _fail_ due to reason encoded in the type of the place state. For instance,
      *       on a regular PT arc, the place syntax is [[Place.Syntax.HasTokens]], which encodes a
      *       non-negative int. If the firing were to bring the tokens into the negative, it would
      *       not be able to construct a new [[PlaceMarking]]. This is distinct from a
      *       [[Place.Semantics]] check, where the [[Place.Syntax]] is valid, but the marking
      *       predicates fail.
      *     - Note that `fire` returns a typed error and `fireUnsafe` throws, but neither checks
      *       that the resulting marking is valid. Validity is a _Place_ semantics concern. This
      *       coherence condition must be checked at the net-wide/simulator level.
      *   - Trait mixins are linearized. This means that if you mixin two traits that act on the
      *     same place data, they may not commute. If all traits act on only "their own" place
      *     syntax, then everything should commute.
      */
    trait Semantics[P <: Place.Syntax[P]] {
        /** The error type that [[fire]] and [[fireUnsafe]] can produce. Defaults to the common
          * base [[Arc.Semantics.FiringError]]; concrete arc implementations may narrow this to a
          * more specific subtype.
          */
        type FiringError = Arc.Semantics.FiringError

        protected def enablingPredicates: List[P => Boolean] = List.empty
        // Each endo is a Kendo: a Kleisli endomorphism parameterized on Either[FiringError, _].
        // Using Kendo here makes it explicit that firing endos are effectful computations. In
        // the future, the effect type can be swapped out by the simulator (e.g., for IO or
        // StateT), without changing the arc semantics.
        protected def firingEndos: List[KendoT[[X] =>> Either[FiringError, X], P]] = List.empty

        final def enabled(p: P): Boolean = enablingPredicates.forall(_(p))

        /** Returns a typed error if firing fails. Does not check enabledness or place-side
          * validity — both are the simulator's concern.
          */
        final def fire(p: P): Either[FiringError, P] = kendoFold(firingEndos).run(p)

        /** Does not check enabledness or place-side validity — both are the simulator's concern.
          *
          * @throws Arc.Semantics.FiringError if firing fails (e.g.,
          *   [[Arc.Semantics.PT.InsufficientTokens]])
          */
        @throws[Arc.Semantics.FiringError]("if firing results in a FiringError")
        final def fireUnsafe(p: P): P = fire(p).fold(e => throw e, identity)
    }

    object Semantics {
        /** Base trait for arc firing errors. Not sealed; extend to add custom firing errors. */
        trait FiringError extends Throwable

        trait Weighted {
            val weight: NonNegativeInt
        }

        /** Removes `weight` tokens from the place. Enabled if place has >= weight tokens. */
        trait PT[P <: Place.Syntax.HasTokens[P]](val weight: NonNegativeInt)
            extends Semantics[P],
              Weighted {
            abstract override protected def enablingPredicates: List[P => Boolean] =
                ((p: P) => p.tokens >= weight) :: super.enablingPredicates
            abstract override protected def firingEndos: List[KendoT[[X] =>> Either[Arc.Semantics.FiringError, X], P]] =
                Kleisli((p: P) =>
                    (NonNegativeInt(p.tokens - weight)
                        .map(p.withTokens)
                        .toRight(PT.InsufficientTokens(p.tokens, weight))
                    ): Either[Arc.Semantics.FiringError, P]
                ) :: super.firingEndos
        }

        object PT {
            /** Raised when a PT arc fires but the place has fewer tokens than the arc weight. */
            case class InsufficientTokens(tokens: NonNegativeInt, weight: NonNegativeInt)
                extends FiringError {
                override def getMessage: String =
                    s"Insufficient tokens to fire PT arc: place has ${tokens.toInt}, arc requires ${weight.toInt}"
            }
        }

        /** Adds `weight` tokens to the place. Always arc-side enabled. */
        trait TP[P <: Place.Syntax.HasTokens[P]](val weight: NonNegativeInt)
            extends Semantics[P],
              Weighted {
            abstract override protected def firingEndos: List[KendoT[[X] =>> Either[Arc.Semantics.FiringError, X], P]] =
                Kleisli((p: P) =>
                    (NonNegativeInt(p.tokens + weight)
                        .map(p.withTokens)
                        .toRight(TP.TokenOverflow(p.tokens, weight))
                    ): Either[Arc.Semantics.FiringError, P]
                ) :: super.firingEndos
        }

        object TP {
            /** Raised when a TP arc fires but adding the weight would overflow NonNegativeInt. */
            case class TokenOverflow(tokens: NonNegativeInt, weight: NonNegativeInt)
                extends FiringError {
                override def getMessage: String =
                    s"Token overflow firing TP arc: place has ${tokens.toInt}, arc adds ${weight.toInt}"
            }
        }

        /** Enabled only if the place is empty. Does not fire. */
        trait Inhibitor[P <: Place.Syntax.HasTokens[P]] extends Semantics[P] {
            abstract override protected def enablingPredicates: List[P => Boolean] =
                ((p: P) => p.tokens == NonNegativeInt.unsafeApply(0)) :: super.enablingPredicates
        }

        /** Drains all tokens from the place. Always arc-side enabled. */
        trait Reset[P <: Place.Syntax.HasTokens[P]] extends Semantics[P] {
            abstract override protected def firingEndos: List[KendoT[[X] =>> Either[Arc.Semantics.FiringError, X], P]] =
                Kleisli((p: P) =>
                    (Right(p.withTokens(NonNegativeInt.unsafeApply(0))): Either[Arc.Semantics.FiringError, P])
                ) :: super.firingEndos
        }

        /** Enabled if the place has >= weight tokens. Does not fire. */
        trait Read[P <: Place.Syntax.HasTokens[P]](val weight: NonNegativeInt)
            extends Semantics[P],
              Weighted {
            abstract override protected def enablingPredicates: List[P => Boolean] =
                ((p: P) => p.tokens >= weight) :: super.enablingPredicates
        }
    }

    trait Presentation {
        val label: String
        // The arc always starts from the source and ends at the target. These are additional points that go along
        // the way. I don't believe this is integrated into the YAPNE GUI yet.
        val points: Queue[(Int, Int)]
    }

}
