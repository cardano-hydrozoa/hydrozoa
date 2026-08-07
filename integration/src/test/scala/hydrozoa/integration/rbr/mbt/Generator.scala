package hydrozoa.integration.rbr.mbt

import cats.effect.IO
import hydrozoa.integration.rbr.mbt.SutCommands.given
import hydrozoa.integration.rbr.property.RbrSeed
import hydrozoa.integration.stage4.Model.ModelState
import hydrozoa.integration.stage4.SetupScenarioGen
import hydrozoa.rulebased.ledger.l1.RbrDatumSentinels
import org.scalacheck.PropertyM
import org.scalacheck.commands.{AnyCommand, ScenarioGen}
import org.scalacheck.util.Pretty
import scala.concurrent.duration.*

/** Pre-fallback generator for the RBR MBT: stage4's setup-phase scenario generator
  * ([[SetupScenarioGen]]) driven verbatim — same Poisson superposition, same L2-tx/deposit command
  * mix (including `RandomWithdrawals`), same absorption handling — with only RBR-specific stamping.
  *
  * Two distinct datum sentinels let `beta` bucket the two L1 fates apart: l2-bound (flag-2) outputs
  * carry "evacuation" (they end up in the committed `EvacuationMap` `N` and evacuate under fallback),
  * while withdrawal (flag-1) outputs carry "withdrawal" and are pinned to the script `payoutAddress`
  * so they can't be re-spent as peer fee/collateral. Deposit validity is shortened so deposits
  * absorb within the suite's commit window. Reusing stage4's picker means any new setup-phase path
  * it grows (new strategies, new command types) flows into the RBR setup phase automatically.
  */
object RbrMbtScenarioGen extends ScenarioGen[ModelState, Sut]:

    private given (AnyCommand[ModelState, Sut] => Pretty) = c => Pretty(_ => c.toString)

    private val setupConfig: SetupScenarioGen.Config = SetupScenarioGen.Config(
      l2OutputDatum = RbrDatumSentinels.inline("evacuation"),
      withdrawalDatum = RbrDatumSentinels.inline("withdrawal"),
      withdrawalAddress = Some(RbrSeed.payoutAddress),
      // Short deposit validity so a deposit is absorbable ~this + maturity after submission — well
      // inside the suite's commit window — instead of the stage4 default 2min, which always
      // outlasts the window and forces deposits down the refund path.
      depositValidityDuration = 20.seconds,
      // l2TxStrategies inherits the default (full stage4 mix, including RandomWithdrawals): the
      // "withdrawal" sentinel + the model's WithdrawalOutput place account for the withdrawn value.
    )

    override def genNextCommand(state: ModelState): PropertyM[IO, AnyCommand[ModelState, Sut]] =
        PropertyM.pick[IO, AnyCommand[ModelState, Sut]](
          SetupScenarioGen.genNextCommand(state, setupConfig)
        )
