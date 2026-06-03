package hydrozoa.lib.petri

import hydrozoa.lib.number.NonNegativeInt
import hydrozoa.lib.petri.net.components.Transition
import scala.concurrent.duration.{DurationInt, FiniteDuration}

case class TransitionNoId(
    override val label: String,
    override val width: NonNegativeInt = NonNegativeInt.unsafeApply(40),
    override val height: NonNegativeInt = NonNegativeInt.unsafeApply(10),
    override val position: (Int, Int) = (0, 0),
    override val delay: FiniteDuration = 0.millis,
    override val silent: Boolean = false,
    override val priority: NonNegativeInt = NonNegativeInt.unsafeApply(0),
) extends Transition.Topology,
      Transition.Semantics,
      Transition.Syntax.HasSilent[TransitionNoId],
      Transition.Syntax.HasPriority[TransitionNoId],
      Transition.Presentation {
    override def withSilent(s: Boolean): TransitionNoId = this.copy(silent = s)
    override def withPriority(p: NonNegativeInt): TransitionNoId = this.copy(priority = p)
}
