package hydrozoa.lib.petri

import hydrozoa.lib.number.PositiveInt
import hydrozoa.lib.petri.net.components.{Arc, Place}
import scala.collection.immutable.Queue

/** Generic place-to-transition arc. Removes `weight` tokens from the place on firing. */
case class PTArc[PlaceId, TransitionId, P <: Place.Syntax.WithTokens[P]](
    override val arcPlaceId: PlaceId,
    override val arcTransitionId: TransitionId,
    override val weight: PositiveInt,
    override val label: String,
    override val points: Queue[(Int, Int)] = Queue.empty,
) extends Arc.Topology[PlaceId, TransitionId],
      Arc.Syntax,
      Arc.Semantics.PT[P],
      Arc.Presentation

/** Generic transition-to-place arc. Adds `weight` tokens to the place on firing. */
case class TPArc[PlaceId, TransitionId, P <: Place.Syntax.WithTokens[P]](
    override val arcPlaceId: PlaceId,
    override val arcTransitionId: TransitionId,
    override val weight: PositiveInt,
    override val label: String,
    override val points: Queue[(Int, Int)] = Queue.empty,
) extends Arc.Topology[PlaceId, TransitionId],
      Arc.Syntax,
      Arc.Semantics.TP[P],
      Arc.Presentation

/** Generic inhibitor arc. Enabled only when the place is empty; does not consume tokens. */
case class InhibitorArc[PlaceId, TransitionId, P <: Place.Syntax.WithTokens[P]](
    override val arcPlaceId: PlaceId,
    override val arcTransitionId: TransitionId,
    override val label: String,
    override val points: Queue[(Int, Int)] = Queue.empty,
) extends Arc.Topology[PlaceId, TransitionId],
      Arc.Syntax,
      Arc.Semantics.Inhibitor[P],
      Arc.Presentation

/** Generic reset arc. Drains all tokens from the place on firing; always arc-side enabled. */
case class ResetArc[PlaceId, TransitionId, P <: Place.Syntax.WithTokens[P]](
    override val arcPlaceId: PlaceId,
    override val arcTransitionId: TransitionId,
    override val label: String,
    override val points: Queue[(Int, Int)] = Queue.empty,
) extends Arc.Topology[PlaceId, TransitionId],
      Arc.Syntax,
      Arc.Semantics.Reset[P],
      Arc.Presentation

/** Generic read arc. Enabled when the place has >= `weight` tokens; does not consume tokens. */
case class ReadArc[PlaceId, TransitionId, P <: Place.Syntax.WithTokens[P]](
    override val arcPlaceId: PlaceId,
    override val arcTransitionId: TransitionId,
    override val weight: PositiveInt,
    override val label: String,
    override val points: Queue[(Int, Int)] = Queue.empty,
) extends Arc.Topology[PlaceId, TransitionId],
      Arc.Syntax,
      Arc.Semantics.Read[P],
      Arc.Presentation
