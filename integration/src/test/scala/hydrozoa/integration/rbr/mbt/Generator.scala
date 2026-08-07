package hydrozoa.integration.rbr.mbt

import cats.effect.IO
import hydrozoa.integration.rbr.mbt.SutCommands.given
import hydrozoa.integration.stage4.Commands.TxStrategy
import hydrozoa.integration.stage4.Model.ModelState
import hydrozoa.integration.stage4.SetupScenarioGen
import hydrozoa.rulebased.ledger.l1.RbrDatumSentinels
import org.scalacheck.commands.{AnyCommand, ScenarioGen}
import org.scalacheck.util.Pretty
import org.scalacheck.{Gen, PropertyM}
import scala.concurrent.duration.*

/** Pre-fallback generator for the RBR MBT: stage4's setup-phase scenario generator
  * ([[SetupScenarioGen]]) driven verbatim — same Poisson superposition, same L2-tx/deposit command
  * mix, same absorption handling — with only two RBR-specific knobs. Every L2 output carries the
  * "evacuation" datum sentinel so the RBRClassifier buckets the eventual L1 evacuation outputs, and
  * deposit validity is shortened so deposits absorb within the suite's commit window. Reusing
  * stage4's picker means any new setup-phase path it grows (new strategies, new command types)
  * flows into the RBR setup phase automatically.
  */
object RbrMbtScenarioGen extends ScenarioGen[ModelState, Sut]:

    private given (AnyCommand[ModelState, Sut] => Pretty) = c => Pretty(_ => c.toString)

    /** Short deposit validity so a deposit is absorbable ~this + maturity after submission — well
      * inside the suite's commit window — instead of the stage4 default 2min, which always outlasts
      * the window and forces deposits down the refund path.
      */
    private val setupConfig: SetupScenarioGen.Config = SetupScenarioGen.Config(
      l2OutputDatum = RbrDatumSentinels.inline("evacuation"),
      depositValidityDuration = 20.seconds,
      // Regular only: every L2 output must stay on L2 to become a committed-map obligation that
      // evacuates. `RandomWithdrawals` would exit datum-stamped value to L1, inflating `beta` past
      // the committed map `N` (see [[SetupScenarioGen.Config.l2TxStrategies]]).
      l2TxStrategies = Gen.const(TxStrategy.Regular),
    )

    override def genNextCommand(state: ModelState): PropertyM[IO, AnyCommand[ModelState, Sut]] =
        PropertyM.pick[IO, AnyCommand[ModelState, Sut]](
          SetupScenarioGen.genNextCommand(state, setupConfig)
        )
