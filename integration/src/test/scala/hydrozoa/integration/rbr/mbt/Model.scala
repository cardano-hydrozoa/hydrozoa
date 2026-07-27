package hydrozoa.integration.rbr.mbt

import cats.MonadThrow
import cats.data.StateT
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.integration.rbr.mbt.Commands.DelayCommand
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg}
import java.time.Instant
import org.scalacheck.commands.ModelCommand
import scala.concurrent.duration.FiniteDuration
import test.TestPeers

/** Pre-fallback model state for the RBR fallback→evacuation MBT.
  *
  * For now it only carries the generated config and the seed parameters; the L2/deposit accumulator
  * (which will seed the net's committed obligations) arrives with the deposit/L2-tx commands.
  */
final case class ModelState(
    multiNodeConfig: MultiNodeConfig,
    takeoffTime: Option[Instant],
    testPeers: TestPeers,
    nHeadPeers: Int,
    nCoilPeers: Int,
    maxVersionMinor: Int,
)

/** Model-side command transitions. */
object ModelCommands:

    given ModelCommand[DelayCommand, Unit, ModelState] with
        override def runState[M[_]: MonadThrow](
            cmd: DelayCommand
        )(using log: ContraTracer[M, Slf4jMsg]): StateT[M, ModelState, Unit] =
            StateT.pure(())

        override def delay(cmd: DelayCommand): FiniteDuration = cmd.duration

end ModelCommands
