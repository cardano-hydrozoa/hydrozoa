package hydrozoa.lib.petri.hlpn

import cats.implicits.*
import hydrozoa.lib.petri.net.components.Place

/** A high-level Petri net at a single color type `C`: colored places (each carrying its marking and
  * declared domain), transition declarations (variables + guard), and arcs (a mode-relative
  * [[ArcSemanticsH]] connecting a place and a transition). Enabling and firing delegate to
  * [[ModeSearch]]; firing checks each updated place's E1 validity (in-domain colors, non-negative
  * multiplicities) via [[ColoredPlace.markingError]].
  *
  * Single-color-type simplification: place colors are base classes here. A net mixing base and
  * product colors needs `C` to be a universal sum type; that is deferred.
  */
final case class HlNet[PlaceId, TransitionId, ArcId, C](
    places: Map[PlaceId, ColoredPlace[C]],
    transitions: Map[TransitionId, HlNet.Transition[C]],
    arcs: Map[ArcId, HlNet.Arc[PlaceId, TransitionId, C]]
):
    import HlNet.FiringError

    /** The current marking of a place. Assumes it exists — a validated net has no dangling arcs. */
    private def markingOf(pid: PlaceId): MultiSet[C] = places(pid).marking

    /** The transition's arcs bundled with its declaration for mode search. */
    private def transitionH(tid: TransitionId): TransitionH[PlaceId, C] =
        val decl = transitions(tid)
        val connected = arcs.values.toList.collect {
            case a if a.transition == tid => a.place -> a.semantics
        }
        TransitionH(decl.variables, decl.guard, connected)

    /** Enabled modes of `tid` at the current marking. */
    def enabledModes(tid: TransitionId): LazyList[Binding] =
        ModeSearch.enabledModes(transitionH(tid), markingOf)

    /** Whether `tid` has any enabled mode. */
    def isEnabled(tid: TransitionId): Boolean = enabledModes(tid).nonEmpty

    /** Fire `tid` under `mode`, returning the net with updated place markings. Fails if the
      * transition is unknown, the mode is not enabled, evaluation fails, or a resulting marking
      * violates a place's domain / non-negativity invariant.
      */
    def fire(
        tid: TransitionId,
        mode: Binding
    ): Either[FiringError[TransitionId, PlaceId], HlNet[PlaceId, TransitionId, ArcId, C]] =
        if !transitions.contains(tid) then Left(FiringError.TransitionNotFound(tid))
        else
            val t = transitionH(tid)
            if !ModeSearch.isModeEnabled(t, mode, markingOf) then Left(FiringError.NotEnabled(tid))
            else
                ModeSearch
                    .fire(t, mode, markingOf)
                    .leftMap(FiringError.EvalFailed(tid, _))
                    .flatMap { updated =>
                        updated.toList
                            .traverse { case (pid, m) =>
                                val fired = places(pid).mark(m)
                                fired.markingError match
                                    case Some(e) => Left(FiringError.PlaceInvalid(pid, e))
                                    case None    => Right(pid -> fired)
                            }
                            .map(updates => copy(places = places ++ updates))
                    }

object HlNet:

    /** A transition declaration: the variables it binds (ISO `V`) and its guard `Φ`. */
    final case class Transition[C](variables: List[Var[C]], guard: Guard)

    /** An arc: mode-relative semantics connecting a place and a transition. Direction-neutral —
      * consume vs produce lives inside `semantics`.
      */
    final case class Arc[PlaceId, TransitionId, C](
        place: PlaceId,
        transition: TransitionId,
        semantics: ArcSemanticsH[C]
    )

    /** Why firing failed. */
    sealed trait FiringError[+TransitionId, +PlaceId]

    object FiringError:
        final case class TransitionNotFound[TransitionId](transition: TransitionId)
            extends FiringError[TransitionId, Nothing]

        final case class NotEnabled[TransitionId](transition: TransitionId)
            extends FiringError[TransitionId, Nothing]

        final case class EvalFailed[TransitionId](transition: TransitionId, error: EvalError)
            extends FiringError[TransitionId, Nothing]

        final case class PlaceInvalid[PlaceId](place: PlaceId, error: Place.Semantics.MarkingError)
            extends FiringError[Nothing, PlaceId]
