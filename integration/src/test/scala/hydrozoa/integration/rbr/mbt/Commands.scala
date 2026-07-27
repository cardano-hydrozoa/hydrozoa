package hydrozoa.integration.rbr.mbt

import org.scalacheck.commands.{CommandLabel, CommandProp}
import scala.concurrent.duration.FiniteDuration

object Commands:

    /** Advance time before the next action. Pre-fallback pacing only — no model effect. */
    final case class DelayCommand(duration: FiniteDuration):
        override def toString: String = s"DelayCommand($duration)"

    // All checking is deferred to `beforeFinalize` (the terminal autonomous match), as in stage4.
    given CommandProp[DelayCommand, Unit, ModelState] with {}

    given CommandLabel[DelayCommand] with
        override def label(cmd: DelayCommand): String = "Delay"

end Commands
