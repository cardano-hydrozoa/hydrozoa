package hydrozoa.rulebased

import cats.*
import cats.data.NonEmptyList
import cats.effect.*
import cats.syntax.all.*
import com.suprnation.actor.Actor.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.logging.Tracer
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.peer.PeerId
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.multisig.ledger.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.multisig.ledger.l1.tx.FallbackTx
import hydrozoa.multisig.ledger.stack.{PartitionEffects, StackEffects, StandaloneEvacuationCommitment}
import hydrozoa.multisig.persistence.{BackendStore, Cf, LaneKey, Markers, Persistence, StoreKey}
import hydrozoa.rulebased.RuleBasedRegimeManager.DisputeAction
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
                action = state.action,
                cardanoBackend = cardanoBackend,
                tracerLocal = tracerLocal
              )
            )
            _ <- context.actorOf(
              EvacuationActor(
                candidateEvacMaps = state.candidateEvacMaps,
                cardanoBackend = cardanoBackend,
                fallbackTxHash = state.fallbackTxHash,
                tracerLocal = tracerLocal
              )
            )
        } yield ()
    }

    /** Read the rule-based recovery inputs from persistence. The latest hard-confirmed stack always
      * provides a fallback tx. It provides a SEC + peer signatures (=> [[DisputeAction.Vote]]) only
      * when the latest partition is a Minor or a Major with trailing minors; otherwise (Initial
      * stack, or Major-with-no-trailing-minors) the recovered action is [[DisputeAction.Abstain]]
      * and the dispute resolves via the on-chain Abstain branch.
      */
    private def loadRuleBasedState: IO[RuleBasedRegimeManager.State] =
        for {
            // Rule-based recovery via the head-peer markers is head-only; coil-side recovery lands
            // with the coil-persistence workstream.
            ownHeadPeerNum <- config.ownPeerId match {
                case PeerId.Head(n) => IO.pure(n)
                case PeerId.Coil(_) =>
                    IO.raiseError(
                      new IllegalStateException(
                        "rule-based recovery via Markers.derive is head-only (coil recovery deferred)"
                      )
                    )
            }
            markers <- Markers.derive(backend, ownHeadPeerNum)
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
            res <- effects match {
                case i: StackEffects.HardConfirmed.Initial =>
                    IO.pure(
                      RuleBasedRegimeManager.State(
                        action = DisputeAction.Abstain,
                        candidateEvacMaps = Map(
                          config.initialEvacuationMap.kzgCommitment ->
                              config.initialEvacuationMap
                        ),
                        fallbackTxHash = i.fallbackTx.tx.id
                      )
                    )
                case r: StackEffects.HardConfirmed.Regular =>
                    // TODO: hoist the StackBrief + EvacuationMap recovery lookup into a shared
                    // helper once SC's recovery routine lands (open draft PR on another branch);
                    // both readers want `EvacuationMap[StackLane[hardAcked].lastBlockNum]` per
                    // design/persistence-and-crash-recovery.md §5.2 / §6.
                    val stackKey = LaneKey.Stack(stackNum)
                    for {
                        fallbackTx <- RuleBasedRegimeManager
                            .lastFallback(r.partitions)
                            .liftTo[IO](
                              RuleBasedRegimeManager.MissingState(
                                s"no fallback tx in stack $stackNum"
                              )
                            )
                        action = RuleBasedRegimeManager.lastSec(r.partitions) match {
                            case None => DisputeAction.Abstain
                            case Some(multiSec) =>
                                DisputeAction.Vote(
                                  sec = RuleBasedRegimeManager.toOnchain(multiSec.commitment),
                                  signatures = multiSec.headerMultiSigned,
                                  coilSignatures = Nil // coil-side recovery is deferred
                                )
                        }
                        // Default-vote map — what the multisig treasury was committed to at
                        // fallback time. The default vote utxo carries this kzg, so if peers
                        // never tally onto a newer SEC the resolution will land here.
                        stackBriefBytes <- backend
                            .get(Cf.Stack, stackKey.encode)
                            .flatMap(
                              _.liftTo[IO](
                                RuleBasedRegimeManager.MissingState(
                                  s"StackLane[$stackNum] missing"
                                )
                              )
                            )
                        lastBlockNum = stackKey.codec.decode(stackBriefBytes).payload.lastBlockNum
                        defaultMap <- persistence
                            .get(StoreKey.EvacuationMap(lastBlockNum))
                            .flatMap(
                              _.liftTo[IO](
                                RuleBasedRegimeManager.MissingState(
                                  s"EvacuationMap($lastBlockNum) missing"
                                )
                              )
                            )
                        // SEC maps — every candidate SEC peers could vote for, keyed by its
                        // kzg commitment. The dispute resolution writes whichever wins into
                        // the treasury's Resolved.evacuationActive, so the EvacuationActor
                        // looks it up here at runtime.
                        secMaps <- RuleBasedRegimeManager
                            .allSecs(r.partitions)
                            .traverse { multiSec =>
                                persistence
                                    .get(
                                      StoreKey.EvacuationMap(multiSec.commitment.blockNum)
                                    )
                                    .flatMap(
                                      _.liftTo[IO](
                                        RuleBasedRegimeManager.MissingState(
                                          s"EvacuationMap(${multiSec.commitment.blockNum})" +
                                              " missing for candidate SEC"
                                        )
                                      )
                                    )
                                    .map(map => multiSec.commitment.kzgCommitment -> map)
                            }
                    } yield RuleBasedRegimeManager.State(
                      action = action,
                      candidateEvacMaps =
                          ((defaultMap.kzgCommitment -> defaultMap) +: secMaps).toMap,
                      fallbackTxHash = fallbackTx.tx.id
                    )
            }
        } yield res
}

object RuleBasedRegimeManager {
    type Config = EvacuationActor.Config & DisputeActor.Config

    /** What action this peer's [[DisputeActor]] should take when it observes its own `AwaitingVote`
      * vote utxo on L1.
      *
      *   - [[Vote]]: the latest hard-confirmed stack ended with a Minor (or Major-with-trailing-
      *     minors) — we have a signed SEC + peer header signatures, so the actor builds and submits
      *     a `VoteTx` that flips the datum to `Voted`.
      *   - [[Abstain]]: the latest hard-confirmed stack is Initial or Major-with-no-trailing-minors
      *     — there is no SEC to vote with, so the actor publicly abstains via the on-chain Abstain
      *     branch and the dispute is tallied/resolved from there.
      */
    enum DisputeAction:
        case Vote(
            sec: StandaloneEvacuationCommitment.Onchain,
            signatures: List[BlockHeader.Minor.HeaderSignature],
            coilSignatures: List[Option[BlockHeader.Minor.HeaderSignature]]
        )
        case Abstain

    /** The rule-based recovery inputs pulled from persistence at boot.
      *
      * `candidateEvacMaps` enumerates every evacuation map peers might end up tallying onto — each
      * candidate SEC in the hard-confirmed partitions plus the "default vote" map (what the
      * multisig treasury was committed to at fallback time). At runtime, once the dispute
      * resolution lands, the EvacuationActor reads the resolved treasury's kzg commitment and picks
      * the matching map.
      */
    final case class State(
        action: DisputeAction,
        candidateEvacMaps: Map[KzgCommitment, EvacuationMap],
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

    /** Every SEC carried by the partitions, in partition order — these are the candidates peers may
      * tally onto and the dispute resolution may settle on.
      */
    // TODO: Truncate this to the actual votable SECs -- anything lower than the default vote won't work
    private def allSecs(
        partitions: NonEmptyList[PartitionEffects[StandaloneEvacuationCommitment.MultiSigned]]
    ): List[StandaloneEvacuationCommitment.MultiSigned] =
        partitions.toList.flatMap {
            case PartitionEffects.Minor(sec, _)                => List(sec)
            case PartitionEffects.Major(_, _, _, _, Some(sec)) => List(sec)
            case _                                             => Nil
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
