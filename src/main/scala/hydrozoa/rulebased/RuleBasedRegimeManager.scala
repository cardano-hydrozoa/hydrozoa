package hydrozoa.rulebased

import cats.*
import cats.data.NonEmptyList
import cats.effect.*
import cats.syntax.all.*
import com.suprnation.actor.Actor.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.logging.Tracer
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.multisig.ledger.l1.tx.FallbackTx
import hydrozoa.multisig.ledger.stack.{PartitionEffects, StackEffects, StandaloneEvacuationCommitment}
import hydrozoa.multisig.persistence.{BackendStore, Markers, Persistence, StoreKey}
import scalus.cardano.ledger.TransactionHash
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.fromData

/** This doesn't actually do much of anything right now. It just starts the dispute and evacuation
  * actors, and those proceed autonomously. I don't think we need actors for these.
  *
  * On boot it reads the rule-based regime's recovery inputs (SEC, peer signatures, the evacuation
  * map at fallback, the fallback tx hash) from [[Persistence]] so the same code path serves first
  * run and crash recovery.
  */
case class RuleBasedRegimeManager(
    cardanoBackend: CardanoBackend[IO],
    persistence: Persistence[IO],
    backend: BackendStore[IO],
    votingDeadline: QuantizedInstant,
    tracerLocal: IOLocal[Tracer]
)(using config: RuleBasedRegimeManager.Config)
    extends Actor[IO, Unit] {

    // Start the dispute and evacuation actors
    override def preStart: IO[Unit] = {
        for {
            state <- loadRuleBasedState
            _ <- context.actorOf(
              DisputeActor(
                sec = state.sec,
                signatures = state.signatures,
                cardanoBackend = cardanoBackend,
                tracerLocal = tracerLocal
              )
            )
            _ <- context.actorOf(
              EvacuationActor(
                thisNodeEvacuates = state.toEvacuate,
                cardanoBackend = cardanoBackend,
                evacuationMapAtFallback = state.evacuationMapAtFallback,
                fallbackTxHash = state.fallbackTxHash,
                tracerLocal = tracerLocal
              )
            )
        } yield ()
    }

    /** Read the rule-based recovery inputs from persistence: the latest hard-confirmed stack
      * carries the SEC + peer signatures (latest SEC in partition order) and the fallback tx; the
      * SEC's blockNum keys the evacuation map.
      */
    private def loadRuleBasedState: IO[RuleBasedRegimeManager.State] =
        for {
            markers <- Markers.derive(backend, config.ownHeadPeerNum)
            stackNum <- markers.hardConfirmed.liftTo[IO](
              RuleBasedRegimeManager.MissingState("no hard-confirmed stack on disk")
            )
            effects <- persistence
                .get(StoreKey.HardConfirmation(stackNum))
                .flatMap(
                  _.liftTo[IO](
                    RuleBasedRegimeManager.MissingState(s"HardConfirmation($stackNum) missing")
                  )
                )
            partitions <- effects match {
                case r: StackEffects.HardConfirmed.Regular => IO.pure(r.partitions)
                case _: StackEffects.HardConfirmed.Initial =>
                    IO.raiseError(
                      RuleBasedRegimeManager.MissingState(
                        s"hard-confirmed stack $stackNum is Initial — no SEC"
                      )
                    )
            }
            multiSec <- RuleBasedRegimeManager
                .lastSec(partitions)
                .liftTo[IO](
                  RuleBasedRegimeManager.MissingState(s"no SEC in stack $stackNum")
                )
            fallbackTx <- RuleBasedRegimeManager
                .lastFallback(partitions)
                .liftTo[IO](
                  RuleBasedRegimeManager.MissingState(s"no fallback tx in stack $stackNum")
                )
            sec = RuleBasedRegimeManager.toOnchain(multiSec.commitment)
            evacMap <- persistence
                .get(StoreKey.EvacuationMap(multiSec.commitment.blockNum))
                .flatMap(
                  _.liftTo[IO](
                    RuleBasedRegimeManager.MissingState(
                      s"EvacuationMap(${multiSec.commitment.blockNum}) missing"
                    )
                  )
                )
        } yield RuleBasedRegimeManager.State(
          sec = sec,
          signatures = multiSec.headerMultiSigned,
          toEvacuate = evacMap,
          evacuationMapAtFallback = evacMap,
          fallbackTxHash = fallbackTx.tx.id
        )
}

object RuleBasedRegimeManager {
    type Config = EvacuationActor.Config & DisputeActor.Config

    /** The five rule-based recovery inputs pulled from persistence at boot. */
    final case class State(
        sec: StandaloneEvacuationCommitment.Onchain,
        signatures: List[BlockHeader.Minor.HeaderSignature],
        toEvacuate: EvacuationMap,
        evacuationMapAtFallback: EvacuationMap,
        fallbackTxHash: TransactionHash
    )

    /** Raised when the loader can't reconstruct the rule-based state from persistence — e.g. no
      * hard-confirmed stack on disk, the latest stack is Initial, or the SEC/fallback/evacuation
      * map entries it expects are absent.
      */
    final case class MissingState(message: String) extends RuntimeException(message)

    /** Walk the partitions in reverse and return the latest SEC. Major's SEC is optional (only
      * present when the partition has trailing minors); Minor's SEC is mandatory.
      */
    private def lastSec(
        partitions: NonEmptyList[PartitionEffects[StandaloneEvacuationCommitment.MultiSigned]]
    ): Option[StandaloneEvacuationCommitment.MultiSigned] =
        partitions.toList.reverseIterator.collectFirst {
            case PartitionEffects.Minor(sec, _)                => sec
            case PartitionEffects.Major(_, _, _, _, Some(sec)) => sec
        }

    /** Walk the partitions in reverse and return the latest fallback tx. Only Major partitions
      * carry one (Treasury rotation happens via Major.settlement or Final.finalization).
      */
    private def lastFallback(
        partitions: NonEmptyList[PartitionEffects[StandaloneEvacuationCommitment.MultiSigned]]
    ): Option[FallbackTx] =
        partitions.toList.reverseIterator.collectFirst {
            case PartitionEffects.Major(_, fallback, _, _, _) => fallback
        }

    /** Decode the SEC's `Serialized` header bytes back into the on-chain datum form the dispute
      * resolution script consumes. The bytes are `serialiseData(Onchain.toData)` per
      * [[StandaloneEvacuationCommitment.Onchain.Serialized.apply]], so we round-trip through
      * `Data.fromCbor` + `fromData`.
      */
    private def toOnchain(
        commitment: StandaloneEvacuationCommitment
    ): StandaloneEvacuationCommitment.Onchain = {
        val bytes: Array[Byte] = commitment.header
        fromData[StandaloneEvacuationCommitment.Onchain](Data.fromCbor(bytes))
    }
}
