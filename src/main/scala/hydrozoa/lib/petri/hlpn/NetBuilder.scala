package hydrozoa.lib.petri.hlpn

import cats.data.{NonEmptyList, State, ValidatedNel}
import cats.syntax.all.*
import hydrozoa.lib.petri.net.components.Arc

/** The build program: a `State` over the accumulating declarations. Building never fails — `place`
  * / `transition` / `input` / `output` only record and hand back typed refs — so this is a plain
  * state monad, threaded with a `for`-comprehension. Errors are deferred entirely to
  * [[NetBuilder.build]], which validates with accumulation.
  */
type Build[PlaceId, TransitionId, A] = State[NetBuilder.Accum[PlaceId, TransitionId], A]

/** A typed assembly for an [[HlNet]]: each place is declared with its own color type and yields a
  * typed [[PlaceRef]]; an arc is wired with [[input]] (`(p,t) ∈ F`) or [[output]] (`(t,p) ∈ F`),
  * and its inscription's color must equal the place ref's — a `Peer`-inscription on a `Vote`-place
  * does not compile. [[build]] runs the program, then validates with `ValidatedNel` — structure (id
  * clashes, duplicate flow elements, dangling references) and then well-sortedness ([[SortCheck]]:
  * undeclared variables, `Succ`/`Lt` on unordered classes, mismatched guard/inscription sorts) —
  * and erases the per-place colors to `Any` (sound because agreement was checked at wiring).
  *
  * Fix the two id types once per net, then assemble in a `for`-comprehension:
  * {{{
  * val b = NetBuilder[String, String]()
  * val program = for
  *     in  <- b.place("in", somePlace)              // PlaceRef[String, Peer]
  *     t   <- b.transition("t", List(x), guard)
  *     _   <- b.input(in, t, wPeer)                 // wPeer: Inscription[Peer] — checked against `in`
  * yield ()
  * val net = b.build(program)                       // ValidatedNel[Error, HlNet[String, String, Any]]
  * }}}
  */
final class NetBuilder[PlaceId, TransitionId]:
    import NetBuilder.{Accum, Error}

    private type B[A] = Build[PlaceId, TransitionId, A]

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
            val decl = HlTransition[Any](variables.asInstanceOf[List[Var[Any]]], guard)
            (accum.copy(transitions = accum.transitions :+ (id -> decl)), TransitionRef.wrap(id))
        }

    /** Wire an input arc `(p,t) ∈ F` with annotation `W(p,t) = inscription`. The inscription's
      * color `C` unifies with the place ref's, so a color mismatch does not compile.
      */
    def input[C](
        place: PlaceRef[PlaceId, C],
        transition: TransitionRef[TransitionId],
        inscription: Inscription[C]
    ): B[Unit] =
        State { accum =>
            val entry =
                Arc.Flow.Pt(place.id, transition.id) ->
                    InscribedArc(inscription).asInstanceOf[InscribedArc[Any]]
            (accum.copy(arcs = accum.arcs :+ entry), ())
        }

    /** Wire an output arc `(t,p) ∈ F` with annotation `W(t,p) = inscription`. */
    def output[C](
        transition: TransitionRef[TransitionId],
        place: PlaceRef[PlaceId, C],
        inscription: Inscription[C]
    ): B[Unit] =
        State { accum =>
            val entry =
                Arc.Flow.Tp(transition.id, place.id) ->
                    InscribedArc(inscription).asInstanceOf[InscribedArc[Any]]
            (accum.copy(arcs = accum.arcs :+ entry), ())
        }

    /** Run the program and validate it: first structure (id clashes, duplicate flow elements,
      * dangling references), then — on a structurally sound net — well-sortedness via
      * [[SortCheck]]. All violations of a passing stage are accumulated; the sort check runs only
      * once the net is coherent enough to assemble.
      */
    def build[A](
        program: B[A]
    ): ValidatedNel[Error, HlNet[PlaceId, TransitionId, Any]] =
        val accum = program.runS(Accum.empty).value
        val placeIds = accum.places.map(_._1).toSet
        val transitionIds = accum.transitions.map(_._1).toSet

        val placesV = uniqueMap(accum.places)(Error.DuplicatePlace(_))
        val transitionsV = uniqueMap(accum.transitions)(Error.DuplicateTransition(_))
        val arcsV = uniqueMap(accum.arcs)(Error.DuplicateArc(_)).andThen { arcs =>
            val dangling = accum.arcs.flatMap { (flow, _) =>
                Option.unless(placeIds.contains(flow.place))(Error.ArcMissingPlace(flow)).toList ++
                    Option
                        .unless(transitionIds.contains(flow.transition))(
                          Error.ArcMissingTransition(flow)
                        )
                        .toList
            }
            NonEmptyList.fromList(dangling).fold(arcs.validNel)(_.invalid)
        }

        (placesV, transitionsV, arcsV).mapN(HlNet(_, _, _)).andThen { net =>
            NonEmptyList
                .fromList(SortCheck.errors(net).map(Error.NotWellSorted(_)))
                .fold(net.validNel)(_.invalid)
        }

    /** Collect the entries into a map, reporting one error per duplicated key. */
    private def uniqueMap[K, V](entries: List[(K, V)])(
        dup: K => Error
    ): ValidatedNel[Error, Map[K, V]] =
        val dups = entries.groupBy(_._1).collect { case (k, vs) if vs.sizeIs > 1 => dup(k) }.toList
        NonEmptyList.fromList(dups).fold(entries.toMap.validNel)(_.invalid)

object NetBuilder:

    /** The accumulated declarations, colors erased to `Any` as they are recorded. */
    final case class Accum[PlaceId, TransitionId](
        places: List[(PlaceId, ColoredPlace[Any])],
        transitions: List[(TransitionId, HlTransition[Any])],
        arcs: List[(Arc.Flow[PlaceId, TransitionId], InscribedArc[Any])]
    )

    object Accum:
        def empty[PlaceId, TransitionId]: Accum[PlaceId, TransitionId] =
            Accum(Nil, Nil, Nil)

    /** Why assembly failed. */
    sealed trait Error

    object Error:
        /** A place id declared twice. */
        final case class DuplicatePlace[PlaceId](id: PlaceId) extends Error

        /** A transition id declared twice. */
        final case class DuplicateTransition[TransitionId](id: TransitionId) extends Error

        /** A flow element wired twice — `W` must be a function on `F`. */
        final case class DuplicateArc[PlaceId, TransitionId](
            flow: Arc.Flow[PlaceId, TransitionId]
        ) extends Error

        /** An arc references a place that was never declared. */
        final case class ArcMissingPlace[PlaceId, TransitionId](
            flow: Arc.Flow[PlaceId, TransitionId]
        ) extends Error

        /** An arc references a transition that was never declared. */
        final case class ArcMissingTransition[PlaceId, TransitionId](
            flow: Arc.Flow[PlaceId, TransitionId]
        ) extends Error

        /** A term in the assembled net is not well-sorted (see [[SortCheck]]). */
        final case class NotWellSorted(error: SortError) extends Error
