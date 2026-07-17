package hydrozoa.lib.petri.hlpn

import cats.data.{NonEmptyList, State, ValidatedNel}
import cats.syntax.all.*

/** The build program: a `State` over the accumulating declarations. Building never fails — `place`
  * / `transition` / `arc` only record and hand back typed refs — so this is a plain state monad,
  * threaded with a `for`-comprehension. Errors are deferred entirely to [[NetBuilder.build]], which
  * validates with accumulation.
  */
type Build[PlaceId, TransitionId, ArcId, A] =
    State[NetBuilder.Accum[PlaceId, TransitionId, ArcId], A]

/** A typed assembly for an [[HlNet]]: each place is declared with its own color type and yields a
  * typed [[PlaceRef]]; an arc wires a place ref to a transition ref, and its color must equal the
  * ref's — a `Peer`-arc on a `Vote`-place does not compile. [[build]] runs the program, then
  * validates with `ValidatedNel` so every id clash and dangling reference is reported at once, and
  * erases the per-place colors to `Any` (sound because agreement was checked at wiring).
  *
  * Fix the three id types once per net, then assemble in a `for`-comprehension:
  * {{{
  * val b = NetBuilder[String, String, String]()
  * val program = for
  *     in  <- b.place("in", somePlace)          // PlaceRef[String, Peer]
  *     t   <- b.transition("t", List(x), guard)
  *     _   <- b.arc("a", in, t, Consume(wPeer)) // wPeer: ArcSemanticsH[Peer] — checked against `in`
  * yield ()
  * val net = b.build(program)                   // ValidatedNel[Error, HlNet[..., Any]]
  * }}}
  */
final class NetBuilder[PlaceId, TransitionId, ArcId]:
    import NetBuilder.{Accum, Error}

    private type B[A] = Build[PlaceId, TransitionId, ArcId, A]

    /** Declare a colored place; returns a typed handle for wiring its arcs. */
    def place[C](id: PlaceId, place: ColoredPlace[C]): B[PlaceRef[PlaceId, C]] =
        State { accum =>
            val erased = place.asInstanceOf[ColoredPlace[Any]]
            (accum.copy(places = accum.places :+ (id -> erased)), PlaceRef.wrap(id))
        }

    /** Declare a transition (its variables and guard); returns a handle for wiring. */
    def transition(
        id: TransitionId,
        variables: List[Var[?]],
        guard: Guard
    ): B[TransitionRef[TransitionId]] =
        State { accum =>
            val decl = HlNet.Transition(variables.asInstanceOf[List[Var[Any]]], guard)
            (accum.copy(transitions = accum.transitions :+ (id -> decl)), TransitionRef.wrap(id))
        }

    /** Wire an arc. The arc semantics' color `C` unifies with the place ref's `C`, so a color
      * mismatch does not compile.
      */
    def arc[C](
        id: ArcId,
        place: PlaceRef[PlaceId, C],
        transition: TransitionRef[TransitionId],
        semantics: ArcSemanticsH[C]
    ): B[Unit] =
        State { accum =>
            val arc = HlNet.Arc(place.id, transition.id, semantics.asInstanceOf[ArcSemanticsH[Any]])
            (accum.copy(arcs = accum.arcs :+ (id -> arc)), ())
        }

    /** Run the program and validate it, accumulating every id clash and dangling reference. */
    def build[A](
        program: B[A]
    ): ValidatedNel[Error[PlaceId, TransitionId, ArcId], HlNet[PlaceId, TransitionId, ArcId, Any]] =
        val accum = program.runS(Accum.empty).value
        val placeIds = accum.places.map(_._1).toSet
        val transitionIds = accum.transitions.map(_._1).toSet

        val placesV = uniqueMap(accum.places)(Error.DuplicatePlace(_))
        val transitionsV = uniqueMap(accum.transitions)(Error.DuplicateTransition(_))
        val arcsV = uniqueMap(accum.arcs)(Error.DuplicateArc(_)).andThen { arcs =>
            val dangling = accum.arcs.flatMap { case (arcId, arc) =>
                Option
                    .unless(placeIds.contains(arc.place))(
                      Error.ArcMissingPlace(arcId, arc.place)
                    )
                    .toList ++
                    Option
                        .unless(transitionIds.contains(arc.transition))(
                          Error.ArcMissingTransition(arcId, arc.transition)
                        )
                        .toList
            }
            NonEmptyList.fromList(dangling).fold(arcs.validNel)(_.invalid)
        }

        (placesV, transitionsV, arcsV).mapN(HlNet(_, _, _))

    /** Collect the entries into a map, reporting one error per duplicated id. */
    private def uniqueMap[K, V, E](entries: List[(K, V)])(dup: K => E): ValidatedNel[E, Map[K, V]] =
        val dups = entries.groupBy(_._1).collect { case (k, vs) if vs.sizeIs > 1 => dup(k) }.toList
        NonEmptyList.fromList(dups).fold(entries.toMap.validNel)(_.invalid)

object NetBuilder:

    /** The accumulated declarations, colors erased to `Any` as they are recorded. */
    final case class Accum[PlaceId, TransitionId, ArcId](
        places: List[(PlaceId, ColoredPlace[Any])],
        transitions: List[(TransitionId, HlNet.Transition[Any])],
        arcs: List[(ArcId, HlNet.Arc[PlaceId, TransitionId, Any])]
    )

    object Accum:
        def empty[PlaceId, TransitionId, ArcId]: Accum[PlaceId, TransitionId, ArcId] =
            Accum(Nil, Nil, Nil)

    /** Why assembly failed. */
    enum Error[+PlaceId, +TransitionId, +ArcId]:
        case DuplicatePlace(id: PlaceId)
        case DuplicateTransition(id: TransitionId)
        case DuplicateArc(id: ArcId)
        case ArcMissingPlace(arc: ArcId, place: PlaceId)
        case ArcMissingTransition(arc: ArcId, transition: TransitionId)
