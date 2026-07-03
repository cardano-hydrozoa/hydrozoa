package hydrozoa.rulebased

import cats.*
import cats.data.NonEmptyList
import cats.effect.*
import cats.syntax.all.*
import com.suprnation.actor.Actor.*
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.peer.PeerId
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.multisig.ledger.l1.tx.FallbackTx
import hydrozoa.multisig.ledger.stack.{PartitionEffects, StackEffects, StandaloneEvacuationCommitment}
import hydrozoa.multisig.persistence.{Markers, Persistence, StoreKey}
import hydrozoa.rulebased.RuleBasedRegimeManager.DisputeAction
import hydrozoa.rulebased.ledger.l1.state.VoteState.secFromData
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.fromData

/** Spawns the [[RuleBasedActor]] and supplies it with persistence-backed loaders for the inputs
  * that can't be derived from chain state.
  */
case class RuleBasedRegimeManager(
    cardanoBackend: CardanoBackend[IO],
    persistence: Persistence[IO],
    tracer: ContraTracer[IO, RuleBasedActorEvent],
)(using config: RuleBasedRegimeManager.Config)
    extends Actor[IO, Unit] {

    override def preStart: IO[Unit] =
        tracer.traceWith(RuleBasedActorEvent.Lifecycle.RegimeManagerPreStartEntered) >>
            context
                .actorOf(
                  RuleBasedActor(
                    loadAction = loadAction,
                    loadEvacuationInputs = loadEvacuationInputs,
                    cardanoBackend = cardanoBackend,
                    tracer = tracer
                  )
                )
                .void
                .handleErrorWith(e =>
                    tracer.traceWith(
                      RuleBasedActorEvent.Lifecycle.RegimeManagerPreStartFailed(
                        errorClass = e.getClass.getName,
                        message = Option(e.getMessage).getOrElse("")
                      )
                    ) >> IO.raiseError(e)
                )

    /** Re-read just enough state from persistence to decide whether this peer should vote or
      * abstain. Called by [[RuleBasedActor]] on each tick that observes its own `AwaitingVote`
      * ballot box, so it stays narrow: markers + the latest hard-confirmed stack's effects.
      */
    private def loadAction: IO[DisputeAction] =
        for {
            ownHeadPeerNum <- config.ownPeerId match {
                case PeerId.Head(n) => IO.pure(n)
                case PeerId.Coil(_) =>
                    IO.raiseError(
                      new IllegalStateException(
                        "rule-based recovery via Markers.derive is head-only (coil recovery deferred)"
                      )
                    )
            }
            markers <- Markers.derive(persistence.backend, PeerId.Head(ownHeadPeerNum))
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
        } yield effects match {
            case _: StackEffects.HardConfirmed.Initial => DisputeAction.Abstain
            case r: StackEffects.HardConfirmed.Regular =>
                RuleBasedRegimeManager.lastSec(r.partitions) match {
                    case None => DisputeAction.Abstain
                    case Some(multiSec) =>
                        DisputeAction.Vote(
                          sec = RuleBasedRegimeManager.toOnchain(multiSec.commitment),
                          signatures = multiSec.headerMultiSigned,
                          coilSignatures = Nil // coil-side recovery is deferred
                        )
                }
        }

    /** Re-read the evacuation-side rule-based inputs from persistence. Called by [[RuleBasedActor]]
      * on each tick that runs the evacuation branch.
      */
    private def loadEvacuationInputs: IO[RuleBasedActor.EvacuationInputs] =
        for {
            ownHeadPeerNum <- config.ownPeerId match {
                case PeerId.Head(n) => IO.pure(n)
                case PeerId.Coil(_) =>
                    IO.raiseError(
                      new IllegalStateException(
                        "rule-based recovery via Markers.derive is head-only (coil recovery deferred)"
                      )
                    )
            }
            markers <- Markers.derive(persistence.backend, PeerId.Head(ownHeadPeerNum))
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
                      RuleBasedActor.EvacuationInputs(
                        candidateEvacMaps = Map(
                          config.initialEvacuationMap.kzgCommitment ->
                              config.initialEvacuationMap
                        ),
                        fallbackTxHash = i.fallbackTx.tx.id
                      )
                    )
                case r: StackEffects.HardConfirmed.Regular =>
                    for {
                        fallbackTx <- RuleBasedRegimeManager
                            .lastFallback(r.partitions)
                            .liftTo[IO](
                              RuleBasedRegimeManager.MissingState(
                                s"no fallback tx in stack $stackNum"
                              )
                            )
                        // Default-vote map — what the multisig treasury was committed to at
                        // fallback time. The default vote utxo carries this kzg, so if peers
                        // never tally onto a newer SEC the resolution will land here. The closing
                        // stack's `lastBlockNum` comes from the `UnsignedStack` every peer persists
                        // on every close (atomic with the hard-ack), so it is present for any
                        // hard-confirmed stack — unlike the StackLane brief (leader-authored only).
                        unsignedStack <- persistence
                            .get(StoreKey.UnsignedStack(stackNum))
                            .flatMap(
                              _.liftTo[IO](
                                RuleBasedRegimeManager.MissingState(
                                  s"UnsignedStack($stackNum) missing"
                                )
                              )
                            )
                        lastBlockNum = unsignedStack.brief.lastBlockNum
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
                    } yield RuleBasedActor.EvacuationInputs(
                      candidateEvacMaps =
                          ((defaultMap.kzgCommitment -> defaultMap) +: secMaps).toMap,
                      fallbackTxHash = fallbackTx.tx.id
                    )
            }
        } yield res
}

object RuleBasedRegimeManager {
    type Config = RuleBasedActor.Config

    /** What action this peer's [[RuleBasedActor]] should take when it observes its own
      * `AwaitingVote` vote utxo on L1.
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
