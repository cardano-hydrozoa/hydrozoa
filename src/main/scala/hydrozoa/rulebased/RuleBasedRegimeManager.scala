package hydrozoa.rulebased

import cats.*
import cats.effect.*
import cats.syntax.all.*
import com.suprnation.actor.Actor.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.protocol.types.AckBlock.HeaderSignature
import hydrozoa.rulebased.ledger.dapp.script.plutus.DisputeResolutionValidator.OnchainBlockHeader
import hydrozoa.{L1, UtxoSetL2, rulebased}

/** This doesn't actually do much of anything right now. It just starts the dispute and liquidation
  * actors, and those proceed autonomously. I don't think we need actors for these.
  */
case class RuleBasedRegimeManager[Config <: DisputeActor.Config & LiquidationActor.Config](
    staticConfig: Config,
    collateralUtxo: hydrozoa.Utxo[L1],
    blockHeader: OnchainBlockHeader,
    signatures: List[HeaderSignature],
    cardanoBackend: CardanoBackend[IO],
    utxosToWithdrawL2: UtxoSetL2,
    votingDeadline: QuantizedInstant,
) extends Actor[IO, Unit] {

    // Start the dispute and liquidation actors
    override def preStart: IO[Unit] = for {
        disputeActor <- context.actorOf(
          DisputeActor(
            collateralUtxo = collateralUtxo,
            blockHeader = blockHeader,
            signatures = signatures,
            staticConfig = staticConfig,
            cardanoBackend = cardanoBackend,
            utxosToWithdrawL2 = utxosToWithdrawL2,
            votingDeadline = votingDeadline
          )
        )
        liquidationActor <- context.actorOf(
          LiquidationActor(
            utxosToWithdraw = utxosToWithdrawL2,
            cardanoBackend = cardanoBackend,
            staticConfig = staticConfig
          )
        )
    } yield ()
}
