package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastOps
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.owninfo.OwnPeerPublic
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.HeadMultisigRegimeManager
import hydrozoa.multisig.consensus.ack.{SoftAck, SoftAckId}
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockHeader, BlockNumber}
import hydrozoa.multisig.metrics.PeerMetrics
import hydrozoa.multisig.persistence.recovery.ReplayCursors
import hydrozoa.multisig.persistence.{Persistence, StoreKey, Timestamped, WriteBatch}
import scala.util.control.NonFatal
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.{ByteString, platform}

/** Fast-consensus actor.
  *
  * ==Overview==
  *
  * Coordinates the soft-confirmation of block briefs among head peers via a single round of Ed25519
  * signatures over the brief's [[BlockHeader.Section.signingBytes]] (see `consensus/fast-consensus`
  * in the whitepaper).
  *
  * This actor produces soft-confirmations only. L1 effect signatures (settlement, fallback,
  * rollouts, refunds, finalization) are handled by [[SlowConsensusActor]], not here.
  *
  * ==State==
  *
  * The actor maintains a [[ConsensusCell]] per in-flight block number. Each cell tracks the block's
  * brief (received from the local joint ledger once produced or reproduced) and the soft acks from
  * each head peer (received from peer liaisons; the local peer's own ack is also fed in by the
  * joint ledger). When a cell becomes saturated (brief present, every peer's ack collected), it
  * produces a [[Block.SoftConfirmed.Next]] and fans it out to the downstream actors.
  *
  * ==Postponed acks==
  *
  * A peer's own soft-ack for block N+1 can arrive before block N is soft-confirmed: the local joint
  * ledger may complete block N+1 (as a follower replicating the next leader's brief) before the
  * consensus cell for block N has saturated. The N+1 own ack is then stashed on the cell for block
  * N and announced as soon as cell N completes. This preserves the cross-peer invariant that a
  * peer's own block-N+1 ack is broadcast strictly after the same peer has seen block N
  * soft-confirmed.
  */
object FastConsensusActor:
    // `& CardanoNetwork.Section`: the SoftConfirmation codec (Block.SoftConfirmed) is
    // `Section`-dependent; the full configs passed in already satisfy it (same ones that feed
    // JointLedger's `HeadConfig.Section`).
    type Config = OwnPeerPublic.Section & HeadPeers.Section & CardanoNetwork.Section

    final case class Connections(
        blockWeaver: BlockWeaver.Handle,
        cardanoLiaison: CardanoLiaison.Handle,
        requestSequencer: Option[RequestSequencer.Handle],
        headPeerLiaisons: List[liaison.PeerLiaisonHeadToHead.Handle],
        stackComposer: StackComposer.Handle,
        /** A hub's coil relay (§5.4) [doc-ref]: this actor's **own** soft-ack is sent here so the
          * hub's coil peers receive it. `None` off a hub.
          */
        coilRelay: Option[CoilRelay.Handle] = None,
    )

    /** One cell per in-flight block number. A cell knows the block number it is collecting for, may
      * have a brief, accumulates acks from each peer (indexed by verification key), and can
      * postpone the local peer's own ack for the next block if it arrives early.
      */
    final case class ConsensusCell(
        blockNum: BlockNumber,
        brief: Option[BlockBrief.Next],
        acks: Map[VerificationKey, SoftAck],
        postponedNextBlockOwnAck: Option[SoftAck]
    ):
        // The block number is not re-checked here: `withCell` already selects this cell by the
        // message's block number, so the cell's `blockNum` always matches.
        def acceptBrief(b: BlockBrief.Next): Either[CollectingError, ConsensusCell] =
            if brief.isDefined then Left(CollectingError.UnexpectedBlock(blockNum))
            else Right(copy(brief = Some(b)))

        def acceptAck(ack: SoftAck, vk: VerificationKey): Either[CollectingError, ConsensusCell] =
            if acks.contains(vk) then Left(CollectingError.UnexpectedAck(blockNum, ack.peerNum))
            else Right(copy(acks = acks + (vk -> ack)))

        def acceptPostponedAck(ack: SoftAck): Either[CollectingError, ConsensusCell] =
            if postponedNextBlockOwnAck.isDefined then Left(CollectingError.PostponedAckAlreadySet)
            else if ack.blockNum != blockNum.increment then
                Left(CollectingError.UnexpectedPostponedAck)
            else Right(copy(postponedNextBlockOwnAck = Some(ack)))

        def isSaturated(allVKeys: Set[VerificationKey]): Boolean =
            brief.isDefined && acks.keySet == allVKeys

    object ConsensusCell:
        def fresh(blockNum: BlockNumber): ConsensusCell =
            ConsensusCell(blockNum, brief = None, acks = Map.empty, postponedNextBlockOwnAck = None)

    final case class State(cells: Map[BlockNumber, ConsensusCell])
    object State:
        def initial: State = State(Map.empty)

    type Request = PreStart.type | BlockBrief.Next | SoftAck

    type Handle = ActorRef[IO, Request]

    case object PreStart

    private final case class Env(
        config: Config,
        persistence: Persistence[IO],
        tracer: ContraTracer[IO, FastConsensusActorEvent],
        metrics: PeerMetrics
    ) extends CardanoNetwork.Section:
        def cardanoNetwork: CardanoNetwork = config.cardanoNetwork
        def connected(connections: Connections): Env.Connected = Env.Connected(this, connections)

    private object Env:
        final class Connected(env: Env, val connections: Connections)
            extends CardanoNetwork.Section:
            export env.*

    def apply(
        config: Config,
        pendingConnections: HeadMultisigRegimeManager.PendingConnections |
            FastConsensusActor.Connections,
        tracer: ContraTracer[IO, FastConsensusActorEvent],
        persistence: Persistence[IO],
        metrics: PeerMetrics
    ): IO[FastConsensusActor] =
        for {
            stateRef <- Ref[IO].of(State.initial)
        } yield new FastConsensusActor(
          config = config,
          pendingConnections = pendingConnections,
          stateRef = stateRef,
          tracer = tracer,
          persistence = persistence,
          metrics = metrics
        )

    enum Error extends RuntimeException:
        case AlienAckAnnouncement
        case UnexpectedPreviousBlockCell

    enum CollectingError extends RuntimeException:
        case UnexpectedAck(blockNum: BlockNumber, peerNum: HeadPeerNumber)
        case UnexpectedBlock(blockNum: BlockNumber)
        case UnexpectedPeer(peer: HeadPeerNumber)
        case PostponedAckAlreadySet
        case UnexpectedPostponedAck

        override def getMessage: String = this match
            case UnexpectedAck(b, p)    => s"Duplicate ack for block $b from peer $p"
            case UnexpectedBlock(b)     => s"Duplicate brief for block $b"
            case UnexpectedPeer(p)      => s"Unknown peer number: $p"
            case PostponedAckAlreadySet => "Postponed ack already set"
            case UnexpectedPostponedAck => "Unexpected postponed ack"

    enum CompletionError extends RuntimeException:
        case WrongHeaderSignature(vkey: ByteString)

end FastConsensusActor

class FastConsensusActor(
    config: FastConsensusActor.Config,
    pendingConnections: HeadMultisigRegimeManager.PendingConnections |
        FastConsensusActor.Connections,
    stateRef: Ref[IO, FastConsensusActor.State],
    tracer: ContraTracer[IO, FastConsensusActorEvent],
    persistence: Persistence[IO],
    metrics: PeerMetrics
) extends Actor[IO, FastConsensusActor.Request]:
    import FastConsensusActor.*

    private given env: Env = Env(config, persistence, tracer, metrics)

    override def preStart: IO[Unit] = context.self ! FastConsensusActor.PreStart

    override def receive: Receive[IO, Request] = PartialFunction.fromFunction {
        case FastConsensusActor.PreStart =>
            for {
                // Suspends on the start barrier, so connections are in place before any real
                // message is processed.
                given Env.Connected <- initializeConnections
                _ <- context.become(PartialFunction.fromFunction(receiveConnected))
            } yield ()
        case x =>
            IO.raiseError(RuntimeException(s"Unexpected message received before PreStart: $x"))
    }

    private def receiveConnected(req: Request)(using Env.Connected): IO[Unit] = req match {
        case FastConsensusActor.PreStart =>
            IO.raiseError(RuntimeException("Unexpected duplicate PreStart"))
        case brief: BlockBrief.Next => handleBrief(brief)
        case ack: SoftAck           => handleAck(ack)
    }

    private def initializeConnections: IO[Env.Connected] = {
        val connections: IO[FastConsensusActor.Connections] =
            HeadMultisigRegimeManager.resolveConnections(pendingConnections)(c =>
                FastConsensusActor.Connections(
                  // Soft-block fan-out goes via the rate limiter on the
                  // FastConsensusActor → BlockWeaver lane (see
                  // hydrozoa.multisig.consensus.limiter.Limiter).
                  blockWeaver = c.blockWeaverLimiter,
                  cardanoLiaison = c.cardanoLiaison,
                  requestSequencer = c.requestSequencer,
                  headPeerLiaisons = c.headPeerLiaisons,
                  stackComposer = c.stackComposer,
                  coilRelay = c.coilRelay,
                )
            )
        connections.map(env.connected)
    }

    private def handleBrief(brief: BlockBrief.Next)(using Env.Connected): IO[Unit] =
        withCell(brief.blockNum)(_.acceptBrief(brief))

    private def handleAck(ack: SoftAck)(using env: Env.Connected): IO[Unit] = {
        val isOwn = config.ownPeerId == PeerId.Head(ack.peerNum)
        for {
            _ <- tracer.traceWith(
              FastConsensusActorEvent.AckReceived(ack.blockNum, ack.peerNum, "soft", isOwn)
            )
            // Validate peer
            vk <- config
                .headPeerVKey(ack.peerNum)
                .liftTo[IO](CollectingError.UnexpectedPeer(ack.peerNum))
            _ <- withCell(ack.blockNum)(_.acceptAck(ack, vk))
            // Own ack scheduling: if this is the local peer's own ack, broadcast it (or postpone
            // if the previous block's cell still exists, to maintain the spec ordering).
            _ <- IO.whenA(isOwn)(scheduleOwnAck(ack))
        } yield ()
    }

    /** Decide whether to broadcast a fresh own ack immediately or postpone it onto the previous
      * block's cell. See the [[FastConsensusActor]] class-level doc for postponed-ack semantics.
      */
    private def scheduleOwnAck(ack: SoftAck)(using env: Env.Connected): IO[Unit] = for {
        _ <- IO.raiseWhen(config.ownPeerId != PeerId.Head(ack.peerNum))(
          Error.AlienAckAnnouncement
        )
        state <- stateRef.get
        prevBlockNum = ack.blockNum match {
            case n if (n: Int) == 0 => None
            case n                  => Some(n.decrement)
        }
        _ <- prevBlockNum match {
            case None => announceAck(ack)
            case Some(p) =>
                state.cells.get(p) match {
                    case Some(prevCell) =>
                        // Previous cell still in flight: postpone this ack until it completes.
                        for {
                            updated <- IO.fromEither(prevCell.acceptPostponedAck(ack))
                            _ <- stateRef.update(s =>
                                s.copy(cells = s.cells.updated(updated.blockNum, updated))
                            )
                        } yield ()
                    case None => announceAck(ack)
                }
        }
    } yield ()

    /** Run [[f]] against the cell for [[blockNum]] (creating a fresh one if absent), persist the
      * update, and — if the resulting cell is saturated — complete it and propagate the
      * [[Block.SoftConfirmed.Next]] downstream.
      */
    private def withCell(blockNum: BlockNumber)(
        f: ConsensusCell => Either[CollectingError, ConsensusCell]
    )(using env: Env.Connected): IO[Unit] = for {
        state <- stateRef.get
        cell = state.cells.getOrElse(blockNum, ConsensusCell.fresh(blockNum))
        updated <- IO.fromEither(f(cell))
        _ <- stateRef.update(s => s.copy(cells = s.cells.updated(blockNum, updated)))
        _ <- IO.whenA(updated.isSaturated(config.headPeerVKeys.iterator.toSet))(
          completeCell(updated)
        )
    } yield ()

    private def completeCell(cell: ConsensusCell)(using env: Env.Connected): IO[Unit] = {
        import env.connections
        for {
            brief <- cell.brief.liftTo[IO](
              new IllegalStateException(s"Saturated cell ${cell.blockNum} without a brief")
            )
            // Verify every ack's signature against the brief's signingBytes.
            msg = brief.header.signingBytes
            _ <- cell.acks.toList
                .traverse_((vk, ack) => verifyHeaderSig(vk, ack.headerSignature, msg))

            finalizationRequested = cell.acks.values.exists(_.finalizationRequested)
            confirmed = mkSoftConfirmed(brief, cell.acks, finalizationRequested)

            confirmedBlockType = brief match {
                case _: BlockBrief.Minor => "minor"
                case _: BlockBrief.Major => "major"
                case _: BlockBrief.Final => "final"
            }
            _ <- tracer.traceWith(
              FastConsensusActorEvent.BlockSoftConfirmed(
                confirmed.blockNum,
                confirmedBlockType,
                confirmed.blockBrief.blockVersion.major: Int,
                confirmed.blockBrief.blockVersion.minor: Int
              )
            )
            // Peer stats (docs/spec/peer-stats-endpoint.md): count the block and its events. A final
            // block is bucketed with major for the minor/major split.
            isMajorBlock = brief match
                case _: BlockBrief.Minor => false
                case _                   => true
            _ <- IO(
              metrics.onBlockConfirmed(
                (confirmed.blockNum: Int).toLong,
                isMajorBlock,
                brief.requests.size
              )
            )

            // Persist the SoftConfirmation record (header + aggregated multisig) before fanning out
            // (CR4 write-before-send). `softConfirmed` derives as max(SoftConfirmation.key); we keep
            // the subsumed soft-acks (no compaction on confirmation). The value carries this node's
            // local confirmation moment as an arrival stamp — a wall-clock instant is derived on
            // read via the per-generation zero-time anchor, so none is stored.
            stamp <- persistence.arrivalStamp
            _ <- persistence.write(
              WriteBatch.start.put(StoreKey.SoftConfirmation(confirmed.blockNum))(
                Timestamped(stamp, confirmed)
              )
            )

            // Fan out the soft-confirmed block. (Peer liaisons no longer receive BlockConfirmed: the
            // new lane protocol prunes per-reply, not on local confirmation.)
            _ <- connections.blockWeaver ! confirmed
            _ <- connections.stackComposer ! confirmed

            // Backpressure: tell the sequencer and the mesh liaisons this block's per-author
            // high-water request number so they can advance their confirmed-high-water windows. A
            // block carries only the authors that appear in it, so an empty map is a no-op
            // (docs/spec/fast-consensus).
            requestHighWater = ReplayCursors.maxRequestNumberPerPeer(confirmed.requests.map(_._1))
            _ <- IO.whenA(requestHighWater.nonEmpty) {
                val msg = SoftConfirmedHighWater(requestHighWater)
                connections.requestSequencer.traverse_(_ ! msg) >>
                    connections.headPeerLiaisons.traverse_(_ ! msg)
            }

            // Announce any postponed own-ack for the next block now that this cell is done.
            _ <- cell.postponedNextBlockOwnAck.traverse_(announceAck)

            // Drop the completed cell.
            _ <- stateRef.update(s => s.copy(cells = s.cells - cell.blockNum))
        } yield ()
    }

    private def verifyHeaderSig(
        vk: VerificationKey,
        sig: BlockHeader.HeaderSignature,
        msg: ByteString
    ): IO[Unit] =
        IO.delay(platform.verifyEd25519Signature(vk, msg, sig))
            .handleErrorWith {
                case NonFatal(_) =>
                    IO.raiseError(CompletionError.WrongHeaderSignature(vk))
                case e => IO.raiseError(e)
            }
            .void

    // Broadcast this peer's own soft-ack to the head-peer mesh, and (on a hub) to CoilRelay so its
    // coil peers receive it.
    private def announceAck(ack: SoftAck)(using env: Env.Connected): IO[Unit] = {
        import env.connections
        (connections.headPeerLiaisons ! ack).parallel >> connections.coilRelay.traverse_(_ ! ack)
    }

    private def mkSoftConfirmed(
        brief: BlockBrief.Next,
        acks: Map[VerificationKey, SoftAck],
        finalizationRequested: Boolean
    ): Block.SoftConfirmed.Next = {
        // Build the ordered list of header signatures keyed by peer-number order so each peer
        // arrives at the same canonical sequence.
        val sigsByPeer: List[BlockHeader.HeaderSignature] = acks.toList
            .sortBy((_, ack) => ack.peerNum: Int)
            .map((_, ack) => ack.headerSignature)

        brief match {
            case b: BlockBrief.Minor =>
                Block.SoftConfirmed.Minor(b, sigsByPeer, finalizationRequested)
            case b: BlockBrief.Major =>
                Block.SoftConfirmed.Major(b, sigsByPeer, finalizationRequested)
            case b: BlockBrief.Final =>
                Block.SoftConfirmed.Final(b, sigsByPeer)
        }
    }
end FastConsensusActor
