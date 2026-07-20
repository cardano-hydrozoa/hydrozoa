package hydrozoa.lib.petri

import hydrozoa.lib.number.PositiveInt
import hydrozoa.lib.petri.net.components.Arc

/** Generic P/T arc value: the weight annotation `W(f)` for a flow element (Concept 8). Direction
  * lives in the [[Arc.Flow]] key, not here.
  */
case class WeightedArc(override val weight: PositiveInt) extends Arc.Syntax.Weighted
