package hydrozoa.integration.rbr.mbt

import cats.effect.IO
import hydrozoa.integration.rbr.mbt.SutCommands.given
import hydrozoa.integration.stage4.CommandGenerators
import hydrozoa.integration.stage4.Commands.{TxMutator, TxStrategy, given}
import hydrozoa.integration.stage4.Model.{ModelState, given}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.rulebased.ledger.l1.RbrDatumSentinels
import org.scalacheck.commands.{AnyCommand, ScenarioGen, noOp}
import org.scalacheck.util.Pretty
import org.scalacheck.{Gen, PropertyM}
import scala.concurrent.duration.*
import scalus.cardano.address.ShelleyAddress

/** Pre-fallback generator: submit L1 deposits and L2 transactions (reusing stage4's
  * `genRegisterDepositCommand` / `genL2TxCommand`). Both stamp their L2 outputs with the
  * "evacuation" datum sentinel so the RBRClassifier buckets the eventual L1 evacuation outputs, and
  * the committed evacuation map the head resolves to under fallback then reflects the full
  * deposit + L2-tx obligation set. Weighting mirrors stage4 (~10:3 L2:deposit). A peer with nothing
  * to spend (no peer-owned L2 utxo, no L1 funding) contributes a no-op.
  */
object RbrMbtScenarioGen extends ScenarioGen[ModelState, Sut]:

    private given (AnyCommand[ModelState, Sut] => Pretty) = c => Pretty(_ => c.toString)

    private val evacuationDatum = RbrDatumSentinels.inline("evacuation")

    /** Short deposit validity so a deposit is absorbable ~this + maturity (7s) after submission —
      * well inside the suite's commit window — instead of the stage4 default 2min, which always
      * outlasts the window and forces deposits down the refund path.
      */
    private val depositValidityDuration = 20.seconds

    override def genNextCommand(state: ModelState): PropertyM[IO, AnyCommand[ModelState, Sut]] =
        PropertyM.pick(
          for
              peer <- Gen.oneOf(state.params.multiNodeConfig.nodeConfigs.keys.toList)
              cmd <- genCommandForPeer(peer, state)
          yield cmd
        )

    /** Pick an L2 tx or a deposit for `peer` (mirroring stage4's `genCommandForPeer`, minus the
      * happy-path oracle bookkeeping). L2 txs spend the peer's own L2 utxos (the initial seed and
      * absorbed deposits, both peer-owned); deposits spend the peer's L1 funding. Both stamp the
      * "evacuation" marker so their committed outputs are counted on L1.
      */
    private def genCommandForPeer(
        peer: HeadPeerNumber,
        state: ModelState,
    ): Gen[AnyCommand[ModelState, Sut]] =
        val peerAddress = state.params.multiNodeConfig.addressOf(peer)
        val ownedL2Utxos = state.utxosL2Active.filter((_, o) =>
            o.address.asInstanceOf[ShelleyAddress] == peerAddress
        )
        val availableL1 = state.peerUtxosL1(peer)

        val genL2TxOpt: Gen[Option[AnyCommand[ModelState, Sut]]] =
            if ownedL2Utxos.isEmpty then Gen.const(None)
            else
                CommandGenerators
                    .genL2TxCommand(
                      peer,
                      1.second,
                      TxStrategy.Regular,
                      TxMutator.Identity,
                      l2OutputDatum = evacuationDatum,
                    )(state)
                    .map(_.map(AnyCommand.apply(_)))

        val genDepositOpt: Gen[Option[AnyCommand[ModelState, Sut]]] =
            if availableL1.isEmpty then Gen.const(None)
            else
                CommandGenerators
                    .genRegisterDepositCommand(
                      peer,
                      1.second,
                      evacuationDatum,
                      depositValidityDuration = depositValidityDuration,
                    )(state)
                    .map(_.map(AnyCommand.apply(_)))

        if ownedL2Utxos.isEmpty && availableL1.isEmpty then Gen.const(noOp[ModelState, Sut])
        else Gen.frequency(10 -> genL2TxOpt, 3 -> genDepositOpt).map(_.getOrElse(noOp))
