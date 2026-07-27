package hydrozoa.integration.rbr.mbt

import cats.effect.IO
import hydrozoa.integration.rbr.mbt.Commands.{DelayCommand, given}
import hydrozoa.integration.rbr.mbt.ModelCommands.given
import hydrozoa.integration.rbr.mbt.SutCommands.given
import org.scalacheck.commands.{AnyCommand, ScenarioGen}
import org.scalacheck.util.Pretty
import org.scalacheck.{Gen, PropertyM}
import scala.concurrent.duration.*

/** Trivial pre-fallback generator. For now it only emits `Delay` commands (pacing with no model
  * effect); the deposit / L2-tx commands that accumulate committed obligations arrive next. The
  * settlement firewall trips fallback independently of the generated commands.
  */
object RbrMbtScenarioGen extends ScenarioGen[ModelState, Sut]:

    private given (AnyCommand[ModelState, Sut] => Pretty) = c => Pretty(_ => c.toString)

    override def genNextCommand(state: ModelState): PropertyM[IO, AnyCommand[ModelState, Sut]] =
        PropertyM.pick(
          Gen.choose(1, 3).map(s => AnyCommand.apply(DelayCommand(s.seconds)))
        )
