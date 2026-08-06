package hydrozoa.integration.rbr.mbt

import cats.effect.IO
import hydrozoa.integration.rbr.mbt.SutCommands.given
import hydrozoa.integration.stage4.CommandGenerators
import hydrozoa.integration.stage4.Commands.given
import hydrozoa.integration.stage4.Model.{ModelState, given}
import hydrozoa.rulebased.ledger.l1.RbrDatumSentinels
import org.scalacheck.commands.{AnyCommand, ScenarioGen, noOp}
import org.scalacheck.util.Pretty
import org.scalacheck.{Gen, PropertyM}
import scala.concurrent.duration.*

/** Pre-fallback generator: submit L1 deposits (reusing stage4's `genRegisterDepositCommand`, which
  * builds a real signed deposit tx from the peer's L1 funding). Each deposit's L2 outputs carry the
  * "evacuation" datum sentinel so the RBRClassifier buckets the eventual L1 evacuation outputs, and
  * the terminal `alpha == beta` then sees the full accumulated obligation count. A peer with no L1
  * funding left contributes a no-op.
  */
object RbrMbtScenarioGen extends ScenarioGen[ModelState, Sut]:

    private given (AnyCommand[ModelState, Sut] => Pretty) = c => Pretty(_ => c.toString)

    private val evacuationDatum = RbrDatumSentinels.inline("evacuation")

    /** Short deposit validity so a deposit is absorbable ~this + maturity (7s) after submission —
      * well inside the suite's commit window — instead of the stage4 default 2min, which always
      * outlasts the window and forces deposits down the refund path.
      */
    private val depositValidityDuration = 20.seconds

    // TODO: L2 tx coverage. This generator only submits L1 deposits; stage4's `genL2TxCommand`
    // (stage4/Generator.scala) is not wired in. Mix it into the picker via `Gen.frequency`
    // alongside `genRegisterDepositCommand` (mirroring stage4/Generator.scala's ~10:1 weighting)
    // so pre-fallback runs also exercise the submissionClient + L2 ledger path.

    override def genNextCommand(state: ModelState): PropertyM[IO, AnyCommand[ModelState, Sut]] =
        PropertyM.pick(
          for
              peer <- Gen.oneOf(state.params.multiNodeConfig.nodeConfigs.keys.toList)
              cmd <-
                  if state.peerUtxosL1(peer).isEmpty then Gen.const(noOp[ModelState, Sut])
                  else
                      CommandGenerators
                          .genRegisterDepositCommand(
                            peer,
                            1.second,
                            evacuationDatum,
                            // L2 outputs go to the peer's own address (default), so the deposits and
                            // initial seed are peer-owned and spendable by L2 txs. They still carry
                            // the "evacuation" marker, so beta counts them at the (snapshot-queried)
                            // peer addresses.
                            depositValidityDuration = depositValidityDuration
                          )(state)
                          .map(_.map(AnyCommand.apply(_)).getOrElse(noOp[ModelState, Sut]))
          yield cmd
        )
