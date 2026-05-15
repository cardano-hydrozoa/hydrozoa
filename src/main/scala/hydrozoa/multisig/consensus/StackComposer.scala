package hydrozoa.multisig.consensus

import cats.effect.{IO, IOLocal, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.node.owninfo.OwnHeadPeerPrivate
import hydrozoa.lib.logging.{Logging, Tracer}
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.ledger.block.{Block, BlockNumber, BlockResult}
import hydrozoa.multisig.ledger.joint.JointLedger
import hydrozoa.multisig.ledger.stack.{StackBrief, StackNumber}

/** Slow-consensus stack composer (M5 — first iteration).
  *
  * Pairs [[BlockResult]] (from JointLedger) with [[Block.SoftConfirmed]] (from ConsensusActor) by
  * `blockNum`, holding paired blocks in a `ready` queue ordered by blockNum. Receives
  * [[StackBrief]] from PeerLiaisons (follower path) and `PreviousStackHardConfirmation` from
  * SlowConsensusActor (gating signal for closing the next stack).
  *
  * Future iterations will add the BlockWeaver-style Leader/Follower mode switch:
  *   - Leader: closes a stack when previous-stack hard-confirmation arrives AND the longest
  *     contiguous prefix of `ready` is non-empty; broadcasts `StackBrief` directly to PeerLiaisons;
  *     signs all hard-acks upfront; hands `Stack.Unsigned` + own acks to SlowConsensusActor.
  *   - Follower: validates incoming `StackBrief` against its `ready` queue, re-derives effects,
  *     signs own hard-acks, hands them to SlowConsensusActor.
  */
final case class StackComposer(
    config: StackComposer.Config,
    pendingConnections: MultisigRegimeManager.PendingConnections | StackComposer.Connections,
    tracerLocal: IOLocal[Tracer]
) extends Actor[IO, StackComposer.Request] {
    import StackComposer.*

    private val logger = Logging.loggerIO(s"StackComposer.${config.ownHeadPeerNum}")
    given IOLocal[Tracer] = tracerLocal

    private val connections = Ref.unsafe[IO, Option[Connections]](None)

    val state: Ref[IO, State] = Ref.unsafe[IO, State](State.empty)

    override def preStart: IO[Unit] = for {
        _ <- context.self ! PreStart
        _ <- context.become(receive)
    } yield ()

    override def receive: Receive[IO, Request] = PartialFunction.fromFunction {
        case PreStart =>
            for {
                _ <- Tracer.routeLocal(s"StackComposer.${config.ownHeadPeerNum}")
                _ <- initializeConnections
                _ <- logger.info("StackComposer started.")
            } yield ()
        case r: BlockResult =>
            handleBlockResult(r)
        case b: Block.SoftConfirmed =>
            handleSoftConfirmed(b)
        case b: StackBrief =>
            handleIncomingStackBrief(b)
        case p: PreviousStackHardConfirmation =>
            handlePreviousStackHardConfirmation(p)
    }

    private def handleBlockResult(r: BlockResult): IO[Unit] = for {
        _ <- logger.debug(s"BlockResult received for block ${r.brief.blockNum}")
        _ <- state.update(_.withBlockResult(r))
        _ <- state.update(_.tryPair(r.brief.blockNum))
    } yield ()

    private def handleSoftConfirmed(b: Block.SoftConfirmed): IO[Unit] = for {
        _ <- logger.debug(s"Block.SoftConfirmed received for block ${b.blockNum}")
        _ <- state.update(_.withSoftConfirmed(b))
        _ <- state.update(_.tryPair(b.blockNum))
    } yield ()

    private def handleIncomingStackBrief(brief: StackBrief): IO[Unit] = for {
        _ <- logger.debug(s"StackBrief received for stack ${brief.stackNum}")
        _ <- state.update(_.withInboundLeaderBrief(brief))
    } yield ()

    private def handlePreviousStackHardConfirmation(
        p: PreviousStackHardConfirmation
    ): IO[Unit] = for {
        _ <- logger.debug(s"PreviousStackHardConfirmation received for stack ${p.stackNum}")
        _ <- state.update(_.withPreviousStackHardConfirmed(p.stackNum))
        // TODO(M5 leader/follower): trigger close-stack here when leader and ready prefix
        // non-empty.
    } yield ()

    private def initializeConnections: IO[Unit] = pendingConnections match {
        case x: MultisigRegimeManager.PendingConnections =>
            for {
                c <- x.get
                _ <- connections.set(
                  Some(
                    Connections(
                      jointLedger = c.jointLedger,
                      consensusActor = c.consensusActor,
                      peerLiaisons = c.peerLiaisons
                    )
                  )
                )
            } yield ()
        case x: StackComposer.Connections => connections.set(Some(x))
    }
}

object StackComposer {
    type Handle = ActorRef[IO, Request]

    type Config = HeadConfig.Section & OwnHeadPeerPrivate.Section

    final case class Connections(
        jointLedger: JointLedger.Handle,
        consensusActor: ConsensusActor.Handle,
        peerLiaisons: List[PeerLiaison.Handle]
    )

    type Request = PreStart.type | BlockResult | Block.SoftConfirmed | StackBrief |
        PreviousStackHardConfirmation

    case object PreStart

    /** Sent by [[SlowConsensusActor]] when stack `stackNum` reaches hard-confirmation; signals the
      * StackComposer that it may close the next stack (single-flight serialization).
      */
    final case class PreviousStackHardConfirmation(stackNum: StackNumber)

    /** Per-block bag held while waiting for the other half of the (BlockResult, SoftConfirmed) pair
      * to arrive.
      */
    final case class PendingBlock(
        result: Option[BlockResult],
        softConfirmed: Option[Block.SoftConfirmed]
    ) {
        def withResult(r: BlockResult): PendingBlock = copy(result = Some(r))
        def withSoftConfirmed(b: Block.SoftConfirmed): PendingBlock = copy(softConfirmed = Some(b))
        def isPaired: Boolean = result.isDefined && softConfirmed.isDefined
    }

    object PendingBlock {
        val empty: PendingBlock = PendingBlock(None, None)
    }

    /** A block whose (BlockResult, SoftConfirmed) pair has arrived. Eligible for inclusion in the
      * next stack, subject to the longest-contiguous-prefix gate.
      */
    final case class ReadyBlock(
        result: BlockResult,
        softConfirmed: Block.SoftConfirmed
    )

    final case class State(
        pending: Map[BlockNumber, PendingBlock],
        ready: Map[BlockNumber, ReadyBlock],
        inboundLeaderBrief: Map[StackNumber, StackBrief],
        lastClosedStackNum: StackNumber,
        lastClosedBlockNum: BlockNumber,
        previousStackHardConfirmed: Boolean
    ) {
        def withBlockResult(r: BlockResult): State = {
            val pb = pending.getOrElse(r.brief.blockNum, PendingBlock.empty).withResult(r)
            copy(pending = pending.updated(r.brief.blockNum, pb))
        }

        def withSoftConfirmed(b: Block.SoftConfirmed): State = {
            val pb = pending.getOrElse(b.blockNum, PendingBlock.empty).withSoftConfirmed(b)
            copy(pending = pending.updated(b.blockNum, pb))
        }

        def withInboundLeaderBrief(brief: StackBrief): State =
            copy(inboundLeaderBrief = inboundLeaderBrief.updated(brief.stackNum, brief))

        def withPreviousStackHardConfirmed(stackNum: StackNumber): State =
            copy(
              lastClosedStackNum = stackNum,
              previousStackHardConfirmed = true
            )

        /** If `pending(blockNum)` is now fully paired, move it to `ready`. */
        def tryPair(blockNum: BlockNumber): State =
            pending.get(blockNum) match {
                case Some(pb) if pb.isPaired =>
                    val rb = ReadyBlock(pb.result.get, pb.softConfirmed.get)
                    copy(
                      pending = pending - blockNum,
                      ready = ready.updated(blockNum, rb)
                    )
                case _ => this
            }
    }

    object State {
        val empty: State = State(
          pending = Map.empty,
          ready = Map.empty,
          inboundLeaderBrief = Map.empty,
          lastClosedStackNum = StackNumber.zero,
          lastClosedBlockNum = BlockNumber.zero,
          // Initial stack 0 boots with the trigger pre-armed (per spec: stack-0 hard-confirmation
          // bootstraps the trigger chain).
          previousStackHardConfirmed = true
        )
    }
}
