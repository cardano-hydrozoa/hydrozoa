package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastOps
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.owninfo.OwnHeadPeerPublic
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.ack.{AckId, SoftAck}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockHeader, BlockNumber}
import hydrozoa.multisig.ledger.joint.JointLedger
import scala.util.control.NonFatal
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.{ByteString, platform}

/** Fast-consensus actor.
  *
  * ==Overview==
  *
  * Coordinates the soft-confirmation of block briefs among head peers via a single round of
  * Ed25519 signatures over the brief's [[BlockHeader.Section.signingBytes]] (see
  * `consensus/fast-consensus` in the whitepaper).
  *
  * Per the fast/slow split, this actor produces soft-confirmations only. L1 effect signatures
  * (settlement, fallback, rollouts, refunds, finalization) belong to the slow cycle and live in
  * [[SlowConsensusActor]], which is currently parked.
  *
  * ==State==
  *
  * The actor maintains a [[ConsensusCell]] per in-flight block number. Each cell tracks the
  * block's brief (received from the local joint ledger once produced or reproduced) and the soft
  * acks from each head peer (received from peer liaisons; the local peer's own ack is also fed in
  * by the joint ledger). When a cell becomes saturated (brief present, every peer's ack
  * collected), it produces a [[Block.SoftConfirmed.Next]] and broadcasts it.
  *
  * ==Postponed acks==
  *
  * A peer's own soft-ack for block N+1 can arrive before block N is soft-confirmed: the local
  * joint ledger may complete block N+1 (as a follower replicating the next leader's brief)
  * before the consensus cell for block N has saturated. The N+1 own ack is then stashed on the
  * cell for block N and announced as soon as cell N completes. This preserves the cross-peer
  * invariant that a peer's own block-N+1 ack is broadcast strictly after the same peer has seen
  * block N soft-confirmed.
  */
object ConsensusActor:

    type Config = OwnHeadPeerPublic.Section & HeadPeers.Section

    final case class Connections(
        blockWeaver: BlockWeaver.Handle,
        cardanoLiaison: CardanoLiaison.Handle,
        eventSequencer: EventSequencer.Handle,
        peerLiaisons: List[PeerLiaison.Handle],
        jointLedger: JointLedger.Handle,
        tracer: hydrozoa.lib.tracing.ProtocolTracer = hydrozoa.lib.tracing.ProtocolTracer.noop,
    )

    /** One cell per in-flight block number. A cell knows the block number it is collecting for,
      * may have a brief, accumulates acks from each peer (indexed by verification key), and can
      * postpone the local peer's own ack for the next block if it arrives early.
      */
    final case class ConsensusCell(
        blockNum: BlockNumber,
        brief: Option[BlockBrief.Next],
        acks: Map[VerificationKey, SoftAck],
        postponedNextBlockOwnAck: Option[SoftAck]
    ):
        def withBrief(b: BlockBrief.Next): IO[ConsensusCell] = for {
            _ <- IO.raiseWhen(b.blockNum != blockNum)(
              CollectingError.UnexpectedBlockNumber(blockNum, b.blockNum)
            )
            _ <- IO.raiseWhen(brief.isDefined)(CollectingError.UnexpectedBlock(blockNum))
        } yield copy(brief = Some(b))

        def withAck(ack: SoftAck, vk: VerificationKey): IO[ConsensusCell] = for {
            _ <- IO.raiseWhen(ack.blockNum != blockNum)(
              CollectingError.UnexpectedBlockNumber(blockNum, ack.blockNum)
            )
            _ <- IO.raiseWhen(acks.contains(vk))(
              CollectingError.UnexpectedAck(blockNum, ack.peerNum)
            )
        } yield copy(acks = acks + (vk -> ack))

        def withPostponed(ack: SoftAck): IO[ConsensusCell] = for {
            _ <- IO.raiseWhen(postponedNextBlockOwnAck.isDefined)(
              CollectingError.PostponedAckAlreadySet
            )
            _ <- IO.raiseWhen(ack.blockNum != blockNum.increment)(
              CollectingError.UnexpectedPostponedAck
            )
        } yield copy(postponedNextBlockOwnAck = Some(ack))

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

    def apply(
        config: Config,
        pendingConnections: MultisigRegimeManager.PendingConnections | ConsensusActor.Connections
    ): IO[ConsensusActor] =
        for {
            stateRef <- Ref[IO].of(State.initial)
        } yield new ConsensusActor(
          config = config,
          pendingConnections = pendingConnections,
          stateRef = stateRef
        )

    enum Error extends Throwable:
        case AlienAckAnnouncement
        case UnexpectedPreviousBlockCell

    enum CollectingError extends Throwable:
        case UnexpectedBlockNumber(cellBlockNum: BlockNumber, msgBlockNum: BlockNumber)
        case UnexpectedAck(blockNum: BlockNumber, peerNum: HeadPeerNumber)
        case UnexpectedBlock(blockNum: BlockNumber)
        case UnexpectedPeer(peer: HeadPeerNumber)
        case PostponedAckAlreadySet
        case UnexpectedPostponedAck

        override def getMessage: String = this match
            case UnexpectedBlockNumber(c, m) =>
                s"Block-number mismatch: cell=$c, message=$m"
            case UnexpectedAck(b, p)         => s"Duplicate ack for block $b from peer $p"
            case UnexpectedBlock(b)          => s"Duplicate brief for block $b"
            case UnexpectedPeer(p)           => s"Unknown peer number: $p"
            case PostponedAckAlreadySet      => "Postponed ack already set"
            case UnexpectedPostponedAck      => "Unexpected postponed ack"

    enum CompletionError extends Throwable:
        case WrongHeaderSignature(vkey: ByteString)

end ConsensusActor

class ConsensusActor(
    config: ConsensusActor.Config,
    pendingConnections: MultisigRegimeManager.PendingConnections | ConsensusActor.Connections,
    stateRef: Ref[IO, ConsensusActor.State]
) extends Actor[IO, ConsensusActor.Request]:
    import ConsensusActor.*

    private val logger = Logging.loggerIO(s"ConsensusActor.${config.ownHeadPeerNum}")

    private val connections = Ref.unsafe[IO, Option[ConsensusActor.Connections]](None)

    private def getConnections: IO[Connections] =
        connections.get.flatMap(
          _.fold(
            IO.raiseError(
              java.lang.Error("Consensus Actor is missing its connections to other actors.")
            )
          )(IO.pure)
        )

    private def initializeConnections: IO[Unit] = pendingConnections match {
        case x: MultisigRegimeManager.PendingConnections =>
            for {
                _connections <- x.get
                _ <- connections.set(
                  Some(
                    ConsensusActor.Connections(
                      blockWeaver = _connections.blockWeaver,
                      cardanoLiaison = _connections.cardanoLiaison,
                      eventSequencer = _connections.eventSequencer,
                      peerLiaisons = _connections.peerLiaisons,
                      jointLedger = _connections.jointLedger,
                      tracer = _connections.tracer,
                    )
                  )
                )
            } yield ()
        case x: ConsensusActor.Connections => connections.set(Some(x))
    }

    override def preStart: IO[Unit] = context.self ! ConsensusActor.PreStart

    override def receive: Receive[IO, Request] = PartialFunction.fromFunction(receiveTotal)

    private def receiveTotal(req: Request): IO[Unit] = req match {
        case ConsensusActor.PreStart => initializeConnections
        case brief: BlockBrief.Next  => handleBrief(brief)
        case ack: SoftAck            => handleAck(ack)
    }

    private def handleBrief(brief: BlockBrief.Next): IO[Unit] = for {
        _ <- logger.debug(
          s"handleBrief: block=${brief.blockNum} type=${brief.getClass.getSimpleName}"
        )
        _ <- withCell(brief.blockNum)(_.withBrief(brief))
    } yield ()

    private def handleAck(ack: SoftAck): IO[Unit] = for {
        conn <- getConnections
        _ <- logger.debug(
          s"handleAck: block=${ack.blockNum} peer=${ack.peerNum}" +
              (if ack.peerNum == config.ownHeadPeerNum then " (own)" else "")
        )
        _ <- conn.tracer.ack(ack.blockNum: Int, ack.peerNum: Int, "soft")
        // Validate peer
        vk <- config
            .headPeerVKey(ack.peerNum)
            .liftTo[IO](CollectingError.UnexpectedPeer(ack.peerNum))
        _ <- withCell(ack.blockNum)(_.withAck(ack, vk))
        // Own ack scheduling: if this is the local peer's own ack, broadcast it (or postpone
        // if the previous block's cell still exists, to maintain the spec ordering).
        _ <- IO.whenA(ack.peerNum == config.ownHeadPeerNum)(scheduleOwnAck(ack))
    } yield ()

    /** Decide whether to broadcast a fresh own ack immediately or postpone it onto the previous
      * block's cell. See the [[ConsensusActor]] class-level doc for postponed-ack semantics.
      */
    private def scheduleOwnAck(ack: SoftAck): IO[Unit] = for {
        _ <- IO.raiseWhen(ack.peerNum != config.ownHeadPeerNum)(Error.AlienAckAnnouncement)
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
                            updated <- prevCell.withPostponed(ack)
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
        f: ConsensusCell => IO[ConsensusCell]
    ): IO[Unit] = for {
        state <- stateRef.get
        cell = state.cells.getOrElse(blockNum, ConsensusCell.fresh(blockNum))
        updated <- f(cell)
        _ <- stateRef.update(s => s.copy(cells = s.cells.updated(blockNum, updated)))
        _ <- IO.whenA(updated.isSaturated(config.headPeerVKeys.iterator.toSet))(
          completeCell(updated)
        )
    } yield ()

    private def completeCell(cell: ConsensusCell): IO[Unit] = for {
        conn <- getConnections
        brief <- cell.brief.liftTo[IO](
          new IllegalStateException(s"Saturated cell ${cell.blockNum} without a brief")
        )
        // Verify every ack's signature against the brief's signingBytes.
        msg = brief.header.signingBytes
        _ <- cell.acks.toList.traverse_((vk, ack) => verifyHeaderSig(vk, ack.header, msg))

        finalizationRequested = cell.acks.values.exists(_.finalizationRequested)
        confirmed = mkSoftConfirmed(brief, cell.acks, finalizationRequested)

        confirmedBlockType = brief match {
            case _: BlockBrief.Minor => "minor"
            case _: BlockBrief.Major => "major"
            case _: BlockBrief.Final => "final"
        }
        _ <- logger.info(
          s"block soft-confirmed: block=${confirmed.blockNum} type=$confirmedBlockType " +
              s"v${confirmed.blockBrief.blockVersion.major: Int}.${confirmed.blockBrief.blockVersion.minor: Int}"
        )
        _ <- conn.tracer.blockConfirmed(
          confirmed.blockNum: Int,
          confirmedBlockType,
          confirmed.blockBrief.blockVersion.major: Int,
          confirmed.blockBrief.blockVersion.minor: Int
        )

        // Fan out the soft-confirmed block.
        _ <- conn.blockWeaver ! confirmed
        _ <- (conn.peerLiaisons ! confirmed).parallel
        _ <- conn.jointLedger ! confirmed

        // Announce any postponed own-ack for the next block now that this cell is done.
        _ <- cell.postponedNextBlockOwnAck.traverse_(announceAck)

        // Drop the completed cell.
        _ <- stateRef.update(s => s.copy(cells = s.cells - cell.blockNum))
    } yield ()

    private def verifyHeaderSig(
        vk: VerificationKey,
        sig: BlockHeader.HeaderSignature,
        msg: BlockHeader.Minor.Onchain.Serialized
    ): IO[Unit] =
        IO.delay(platform.verifyEd25519Signature(vk, msg, sig))
            .handleErrorWith {
                case NonFatal(_) =>
                    IO.raiseError(CompletionError.WrongHeaderSignature(vk))
                case e => IO.raiseError(e)
            }
            .void

    private def announceAck(ack: SoftAck): IO[Unit] =
        getConnections.flatMap(conn => (conn.peerLiaisons ! ack).parallel)

    private def mkSoftConfirmed(
        brief: BlockBrief.Next,
        acks: Map[VerificationKey, SoftAck],
        finalizationRequested: Boolean
    ): Block.SoftConfirmed.Next = {
        // Build the ordered list of header signatures keyed by peer-number order so each peer
        // arrives at the same canonical sequence.
        val sigsByPeer: List[BlockHeader.HeaderSignature] = acks
            .toList
            .sortBy((_, ack) => ack.peerNum: Int)
            .map((_, ack) => ack.header)

        brief match {
            case b: BlockBrief.Minor =>
                Block.SoftConfirmed.Minor(b, sigsByPeer, finalizationRequested)
            case b: BlockBrief.Major =>
                Block.SoftConfirmed.Major(b, sigsByPeer, finalizationRequested)
            case b: BlockBrief.Final =>
                Block.SoftConfirmed.Final(b, sigsByPeer)
        }
    }
end ConsensusActor
