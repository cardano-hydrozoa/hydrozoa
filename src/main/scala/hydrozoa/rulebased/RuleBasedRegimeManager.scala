package hydrozoa.rulebased

import cats.*
import cats.effect.*
import cats.syntax.all.*
import com.suprnation.actor.Actor.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.multisig.ledger.joint.EvacuationMap
import scalus.cardano.ledger.{TransactionHash, Utxo}

/** This doesn't actually do much of anything right now. It just starts the dispute and liquidation
  * actors, and those proceed autonomously. I don't think we need actors for these.
  */
case class RuleBasedRegimeManager(config: RuleBasedRegimeManager.Config)(
    collateralUtxo: Utxo,
    blockHeader: BlockHeader.Minor.Onchain,
    signatures: List[BlockHeader.Minor.HeaderSignature],
    cardanoBackend: CardanoBackend[IO],
    votingDeadline: QuantizedInstant,
    toEvacuate: EvacuationMap,
    evacuationMapAtFallback: EvacuationMap,
    fallbackTxHash: TransactionHash
) extends Actor[IO, Unit] {

    // Start the dispute and liquidation actors
    override def preStart: IO[Unit] = {

        for {
            _ <- context.actorOf(
              DisputeActor(config)(
                collateralUtxo = collateralUtxo,
                blockHeader = blockHeader,
                signatures = signatures,
                cardanoBackend = cardanoBackend,
              )
            )
            _ <- context.actorOf(
              EvacuationActor(config)(
                toEvacuate = toEvacuate,
                cardanoBackend = cardanoBackend,
                evacuationMapAtFallback = evacuationMapAtFallback,
                fallbackTxHash = fallbackTxHash
              )
            )
        } yield ()
    }
}

object RuleBasedRegimeManager {
    type Config = EvacuationActor.Config & DisputeActor.Config
}
