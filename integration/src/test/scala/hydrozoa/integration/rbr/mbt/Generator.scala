package hydrozoa.integration.rbr.mbt

import cats.effect.IO
import hydrozoa.integration.rbr.mbt.SutCommands.given
import hydrozoa.integration.stage4.CommandGenerators
import hydrozoa.integration.stage4.Commands.given
import hydrozoa.integration.stage4.Model.{ModelState, given}
import org.scalacheck.commands.{AnyCommand, ScenarioGen, noOp}
import org.scalacheck.util.Pretty
import org.scalacheck.{Gen, PropertyM}
import scala.concurrent.duration.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.toData

/** Pre-fallback generator: submit L1 deposits (reusing stage4's `genRegisterDepositCommand`, which
  * builds a real signed deposit tx from the peer's L1 funding). Each deposit's L2 outputs carry the
  * "evacuation" datum sentinel so the RBRClassifier buckets the eventual L1 evacuation outputs, and
  * the terminal `alpha == beta` then sees the full accumulated obligation count. A peer with no L1
  * funding left contributes a no-op.
  */
object RbrMbtScenarioGen extends ScenarioGen[ModelState, Sut]:

    private given (AnyCommand[ModelState, Sut] => Pretty) = c => Pretty(_ => c.toString)

    // Same inline-datum sentinel the initial evacuation map carries (InitializationParametersGen).
    private val evacuationDatum = Some(Inline(toData(ByteString.fromString("evacuation"))))

    override def genNextCommand(state: ModelState): PropertyM[IO, AnyCommand[ModelState, Sut]] =
        PropertyM.pick(
          for
              peer <- Gen.oneOf(state.params.multiNodeConfig.nodeConfigs.keys.toList)
              cmd <-
                  if state.peerUtxosL1(peer).isEmpty then Gen.const(noOp[ModelState, Sut])
                  else
                      CommandGenerators
                          .genRegisterDepositCommand(peer, 1.second, evacuationDatum)(state)
                          .map(_.map(AnyCommand.apply(_)).getOrElse(noOp[ModelState, Sut]))
          yield cmd
        )
