package hydrozoa.integration.rbr.mbt

import cats.effect.IO
import org.scalacheck.commands.{AnyCommand, ScenarioGen, noOp}
import org.scalacheck.util.Pretty
import org.scalacheck.{Gen, PropertyM}

/** Trivial generator. There are no pre-fallback commands yet (deposits / L2 txs arrive in later
  * build steps), so every step is a no-op — the scenario is just run-to-fallback plus the SUT's
  * autonomous evacuation, checked in `beforeFinalize`.
  */
object RbrMbtScenarioGen extends ScenarioGen[ModelState, Sut]:

    private given (AnyCommand[ModelState, Sut] => Pretty) = c => Pretty(_ => c.toString)

    override def genNextCommand(state: ModelState): PropertyM[IO, AnyCommand[ModelState, Sut]] =
        PropertyM.pick(Gen.const(noOp[ModelState, Sut]))
