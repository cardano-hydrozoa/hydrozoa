package hydrozoa.lib.petri.net.components

import hydrozoa.lib.number.{NonNegativeInt, PositiveInt}
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
    /** Sometimes known as an "inscription" in the literature
      */
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
      *
      * Each concrete arc type must explicitly implement both [[enablingError]] and [[fire]]:
      *   - Return `None` from [[enablingError]] if the arc is always enabled.
      *   - Return `Right(p)` from [[fire]] if the arc has no effect on the place.
      */
    trait Semantics[P <: Place.Syntax[P]] {

        /** The single enabling condition for this arc against the given place state. Returns `None`
          * if the arc is enabled, or `Some(error)` with the reason it is not.
          */
        def enablingError(p: P): Option[Arc.Semantics.EnablingError]

        /** Apply this arc's firing effect to the place. Does not check enabledness or place-side
          * validity — both are the simulator's concern.
          */
        def fire(p: P): Either[Arc.Semantics.FiringError, P]

        // Kleisli[ Either [E, _], P, P] -- Kendo[Either[E,_], P]
        // Kendo[ EitherT[F[_], _], E, _], P]

        final def enabled(p: P): Boolean = enablingError(p).isEmpty

        /** Does not check enabledness or place-side validity — both are the simulator's concern.
          *
          * @throws Arc.Semantics.FiringError
          *   if firing fails (e.g., [[Arc.Semantics.PT.InsufficientTokens]])
          */
        @throws[Arc.Semantics.FiringError]("if firing results in a FiringError")
        final def fireUnsafe(p: P): P = fire(p).fold(e => throw e, identity)
    }

    object Semantics {

        /** Base trait for arc enabling errors. Not sealed; extend to add custom enabling errors. */
        trait EnablingError extends Throwable

        /** Base trait for arc firing errors. Not sealed; extend to add custom firing errors. */
        trait FiringError extends Throwable

        trait Weighted {
            val weight: PositiveInt
        }

        /** Removes `weight` tokens from the place. Enabled if place has >= weight tokens. */
        trait PT[P <: Place.Syntax.WithTokens[P]] extends Semantics[P], Weighted {
            override def enablingError(p: P): Option[Arc.Semantics.EnablingError] =
                if p.marking >= weight then None
                else Some(PT.NotEnabled(p.marking, weight))

            override def fire(p: P): Either[Arc.Semantics.FiringError, P] =
                NonNegativeInt(p.marking - weight)
                    .map(p.mark)
                    .toRight(PT.InsufficientTokens(p.marking, weight))
        }

        object PT {

            /** Raised when a PT arc's enabling check fails (place has fewer tokens than weight). */
            case class NotEnabled(tokens: NonNegativeInt, weight: PositiveInt)
                extends EnablingError {
                override def getMessage: String =
                    s"PT arc not enabled: place has ${tokens.toInt} tokens, requires ${weight.toInt}"
            }

            /** Raised when a PT arc fires but the subtraction underflows NonNegativeInt. In
              * practice this should not occur if [[NotEnabled]] is checked first.
              */
            case class InsufficientTokens(tokens: NonNegativeInt, weight: PositiveInt)
                extends FiringError {
                override def getMessage: String =
                    s"Insufficient tokens to fire PT arc: place has ${tokens.toInt}, arc requires ${weight.toInt}"
            }
        }

        /** Adds `weight` tokens to the place. Always arc-side enabled. */
        trait TP[P <: Place.Syntax.WithTokens[P]] extends Semantics[P], Weighted {
            override def enablingError(p: P): Option[Arc.Semantics.EnablingError] = None

            override def fire(p: P): Either[Arc.Semantics.FiringError, P] =
                NonNegativeInt(p.marking + weight)
                    .map(p.mark)
                    .toRight(TP.TokenOverflow(p.marking, weight))
        }

        object TP {

            /** Raised when a TP arc fires but adding the weight would overflow NonNegativeInt. */
            case class TokenOverflow(tokens: NonNegativeInt, weight: PositiveInt)
                extends FiringError {
                override def getMessage: String =
                    s"Token overflow firing TP arc: place has ${tokens.toInt}, arc adds ${weight.toInt}"
            }
        }

        /** Enabled only if the place is empty. Does not fire (identity on place state). */
        trait Inhibitor[P <: Place.Syntax.WithTokens[P]] extends Semantics[P] {
            override def enablingError(p: P): Option[Arc.Semantics.EnablingError] =
                if p.marking == NonNegativeInt.unsafeApply(0) then None
                else Some(Inhibitor.NotEnabled(p.marking))

            override def fire(p: P): Either[Arc.Semantics.FiringError, P] = Right(p)
        }

        object Inhibitor {

            /** Raised when an inhibitor arc's enabling check fails (place is non-empty). */
            case class NotEnabled(tokens: NonNegativeInt) extends EnablingError {
                override def getMessage: String =
                    s"Inhibitor arc not enabled: place has ${tokens.toInt} tokens (requires 0)"
            }
        }

        /** Drains all tokens from the place. Always arc-side enabled. */
        trait Reset[P <: Place.Syntax.WithTokens[P]] extends Semantics[P] {
            override def enablingError(p: P): Option[Arc.Semantics.EnablingError] = None

            override def fire(p: P): Either[Arc.Semantics.FiringError, P] =
                Right(p.mark(NonNegativeInt.unsafeApply(0)))
        }

        /** Enabled if the place has >= weight tokens. Does not fire (identity on place state). */
        trait Read[P <: Place.Syntax.WithTokens[P]] extends Semantics[P], Weighted {
            override def enablingError(p: P): Option[Arc.Semantics.EnablingError] =
                if p.marking >= weight then None
                else Some(Read.NotEnabled(p.marking, weight))

            override def fire(p: P): Either[Arc.Semantics.FiringError, P] = Right(p)
        }

        object Read {

            /** Raised when a read arc's enabling check fails (place has fewer tokens than weight).
              */
            case class NotEnabled(tokens: NonNegativeInt, weight: PositiveInt)
                extends EnablingError {
                override def getMessage: String =
                    s"Read arc not enabled: place has ${tokens.toInt} tokens, requires ${weight.toInt}"
            }
        }
    }

    trait Presentation {
        val label: String
        // The arc always starts from the source and ends at the target. These are additional points that go along
        // the way. I don't believe this is integrated into the YAPNE GUI yet.
        val points: Queue[(Int, Int)]
    }

}
