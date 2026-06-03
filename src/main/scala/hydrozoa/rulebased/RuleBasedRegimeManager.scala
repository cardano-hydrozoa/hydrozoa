package hydrozoa.rulebased

import cats.*
import cats.effect.*
import cats.syntax.all.*
import com.suprnation.actor.Actor.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.logging.Tracer
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.multisig.ledger.stack.StandaloneEvacuationCommitment
import scalus.cardano.ledger.TransactionHash

/** This doesn't actually do much of anything right now. It just starts the dispute and evacuation
  * actors, and those proceed autonomously. I don't think we need actors for these.
  */
case class RuleBasedRegimeManager(
    sec: StandaloneEvacuationCommitment.Onchain,
    signatures: List[BlockHeader.Minor.HeaderSignature],
    cardanoBackend: CardanoBackend[IO],
    votingDeadline: QuantizedInstant,
    toEvacuate: EvacuationMap,
    evacuationMapAtFallback: EvacuationMap,
    fallbackTxHash: TransactionHash,
    tracerLocal: IOLocal[Tracer]
)(using config: RuleBasedRegimeManager.Config)
    extends Actor[IO, Unit] {

    // Start the dispute and evacuation actors
    override def preStart: IO[Unit] = {

        for {
            _ <- context.actorOf(
              DisputeActor(
                sec = sec,
                signatures = signatures,
                cardanoBackend = cardanoBackend,
                tracerLocal
              )
            )
//            _ <- context.actorOf(
//              EvacuationActor(
//                toEvacuate = toEvacuate,
//                cardanoBackend = cardanoBackend,
//                evacuationMapAtFallback = evacuationMapAtFallback,
//                fallbackTxHash = fallbackTxHash
//              )
//            )
        } yield ()
    }
}

object RuleBasedRegimeManager {
    type Config = EvacuationActor.Config & DisputeActor.Config
}
