package hydrozoa.multisig.consensus

import cats.data.NonEmptyList
import cats.effect.{IO, IOLocal, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastOps
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.node.owninfo.OwnHeadPeerPrivate
import hydrozoa.lib.logging.{Logging, Tracer}
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.ledger.block.{Block, BlockNumber, BlockResult, BlockType}
import hydrozoa.multisig.ledger.joint.JointLedger
import hydrozoa.multisig.ledger.stack.*

/** Slow-consensus stack composer (M5).
  *
  * Pairs [[BlockResult]] (from JointLedger) with [[Block.SoftConfirmed]] (from ConsensusActor) by
  * `blockNum`. Once a block's pair is in the `ready` map, it becomes eligible for inclusion in the
  * next stack subject to the **longest-contiguous-prefix** gate.
  *
  * This actor combines Leader and Follower behaviour in a single flat receive (state-shared across
  * modes — pairing happens in both; only stack-close trigger differs):
  *
  *   - **Leader** (when `isSlowLeader(nextStackNum)`): on `PreviousStackHardConfirmation` OR after
  *     pairing a new block, attempts to close the next stack from the longest contiguous prefix.
  *     Closing means: build [[StackBrief]] + [[StackEffects]] (via
  *     [[hydrozoa.multisig.ledger.effects.StackEffectsBuilder]] — body is `???` for now), wrap into
  *     [[Stack.Unsigned]], hand off to [[SlowConsensusActor]], and broadcast the brief directly to
  *     PeerLiaisons.
  *   - **Follower**: when an inbound `StackBrief` for `lastClosedStackNum + 1` matches the local
  *     longest-prefix `ready` view, validate composition, re-derive effects locally (deterministic
  *     per spec), wrap into [[Stack.Unsigned]], and hand to SlowConsensusActor.
  *
  * Round-2 / sole-round signing details and effect-derivation bodies are TODO — this iteration
  * exercises the wiring and the close-trigger semantics; downstream consumers (M1, M3 wallet, M6
  * ack-aggregation) fill in the rest.
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
        _ <- tryProgress
    } yield ()

    private def handleSoftConfirmed(b: Block.SoftConfirmed): IO[Unit] = for {
        _ <- logger.debug(s"Block.SoftConfirmed received for block ${b.blockNum}")
        _ <- state.update(_.withSoftConfirmed(b))
        _ <- state.update(_.tryPair(b.blockNum))
        _ <- tryProgress
    } yield ()

    private def handleIncomingStackBrief(brief: StackBrief): IO[Unit] = for {
        _ <- logger.debug(s"StackBrief received for stack ${brief.stackNum}")
        _ <- state.update(_.withInboundLeaderBrief(brief))
        _ <- tryProgress
    } yield ()

    private def handlePreviousStackHardConfirmation(
        p: PreviousStackHardConfirmation
    ): IO[Unit] = for {
        _ <- logger.debug(s"PreviousStackHardConfirmation received for stack ${p.stackNum}")
        _ <- state.update(_.withPreviousStackHardConfirmed(p.stackNum))
        _ <- tryProgress
    } yield ()

    /** Run leader / follower close-stack attempts based on current state and slow-leadership for
      * the next stack. Both paths are gated on `previousStackHardConfirmed` (single-flight
      * serialization).
      */
    private def tryProgress: IO[Unit] = state.get.flatMap { s =>
        val nextStackNum = s.lastClosedStackNum.increment
        if !s.previousStackHardConfirmed then IO.unit
        else if config.ownHeadPeerId.isSlowLeader(nextStackNum) then
            tryCloseAsLeader(s, nextStackNum)
        else tryCloseAsFollower(s, nextStackNum)
    }

    /** Leader close-attempt: drain the longest contiguous prefix of `ready` starting at
      * `lastClosedBlockNum + 1` into a new stack.
      */
    private def tryCloseAsLeader(s: State, nextStackNum: StackNumber): IO[Unit] =
        s.longestReadyPrefix match {
            case Nil => IO.unit
            case prefix =>
                val brief = mkStackBrief(nextStackNum, prefix)
                for {
                    _ <- logger.info(
                      s"Leader closing stack $nextStackNum with blocks " +
                          s"${prefix.head.result.brief.blockNum}..${prefix.last.result.brief.blockNum}"
                    )
                    unsigned = mkUnsigned(brief, prefix)
                    conn <- getConnections
                    // Broadcast brief directly to PeerLiaisons (per plan: briefs go DIRECT,
                    // not via SlowConsensusActor). Each peer's outbox now has a stackBrief
                    // lane (M7).
                    _ <- (conn.peerLiaisons ! brief).parallel
                    // Hand the unsigned stack to SlowConsensusActor (which will collect acks
                    // and emit PreviousStackHardConfirmation back when saturated).
                    _ <- conn.slowConsensusActor ! unsigned
                    _ <- state.update(_.afterClose(nextStackNum, prefix))
                } yield ()
        }

    /** Follower close-attempt: if the leader's brief for the next stack matches our local longest
      * contiguous prefix, accept it and hand the (locally re-derived) Stack.Unsigned to
      * SlowConsensusActor.
      */
    private def tryCloseAsFollower(s: State, nextStackNum: StackNumber): IO[Unit] =
        s.inboundLeaderBrief.get(nextStackNum) match {
            case None => IO.unit
            case Some(brief) =>
                val expectedPrefix = s.longestReadyPrefix
                if briefMatches(brief, expectedPrefix) then {
                    for {
                        _ <- logger.info(
                          s"Follower accepting stack $nextStackNum brief from leader; " +
                              s"blocks ${brief.firstBlockNum}..${brief.lastBlockNum}"
                        )
                        unsigned = mkUnsigned(brief, expectedPrefix)
                        conn <- getConnections
                        _ <- conn.slowConsensusActor ! unsigned
                        _ <- state.update(_.afterClose(nextStackNum, expectedPrefix))
                    } yield ()
                } else {
                    // Mismatch: leader's brief disagrees with our local longest prefix.
                    // TODO(M5): per plan, divergence triggers fallback into the rule-based
                    // regime. For now, just log and stall (composer stays in current state).
                    logger.warn(
                      s"Follower stack $nextStackNum brief mismatch: leader says " +
                          s"[${brief.firstBlockNum}..${brief.lastBlockNum}], local prefix " +
                          s"would be [${expectedPrefix.headOption
                                  .map(_.result.brief.blockNum)}..${expectedPrefix.lastOption
                                  .map(_.result.brief.blockNum)}]"
                    )
                }
        }

    private def briefMatches(brief: StackBrief, prefix: List[ReadyBlock]): Boolean = {
        if prefix.isEmpty then false
        else
            brief.firstBlockNum.convert == prefix.head.result.brief.blockNum.convert &&
            brief.lastBlockNum.convert == prefix.last.result.brief.blockNum.convert
    }

    private def mkStackBrief(stackNum: StackNumber, prefix: List[ReadyBlock]): StackBrief = {
        val first = prefix.head.result.brief
        val last = prefix.last.result.brief
        val firstMajor = prefix
            .map(_.result.brief)
            .find {
                case _: BlockType.Major => true
                case _: BlockType.Final => true
                case _                  => false
            }
            .map(_.blockNum)
        StackBrief(
          stackNum = stackNum,
          firstBlockNum = first.blockNum,
          lastBlockNum = last.blockNum,
          firstMajorBlockNum = firstMajor
        )
    }

    /** Build a [[Stack.Unsigned]] from the prefix. The effects derivation is `???` until M1 lands;
      * we use a placeholder empty `Regular` here so the actor wiring exercises end-to-end without
      * throwing at runtime. Real derivation:
      * `StackEffectsBuilder.deriveRegular(NecessaryEffectsPolicy.selectNecessaryEffects(...))`.
      */
    private def mkUnsigned(brief: StackBrief, prefix: List[ReadyBlock]): Stack.Unsigned = {
        val results = NonEmptyList.fromListUnsafe(prefix.map(_.result))
        val softConfirmations = NonEmptyList.fromListUnsafe(prefix.map(_.softConfirmed))
        // TODO(M1): replace placeholder with the real necessary-effects derivation.
        val placeholderEffects: StackEffects = StackEffects.Regular(
          settlements = Nil,
          fallbacks = Nil,
          rollouts = Nil,
          refunds = Nil,
          evacCommits = Nil,
          finalization = None
        )
        Stack.Unsigned(brief, results, softConfirmations, placeholderEffects)
    }

    private def getConnections: IO[Connections] = for {
        mConn <- connections.get
        conn <- mConn.fold(
          IO.raiseError(
            java.lang.Error("StackComposer is missing its connections to other actors.")
          )
        )(IO.pure)
    } yield conn

    private def initializeConnections: IO[Unit] = pendingConnections match {
        case x: MultisigRegimeManager.PendingConnections =>
            for {
                c <- x.get
                _ <- connections.set(
                  Some(
                    Connections(
                      jointLedger = c.jointLedger,
                      consensusActor = c.consensusActor,
                      slowConsensusActor = c.slowConsensusActor,
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
        slowConsensusActor: SlowConsensusActor.Handle,
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

        /** Longest contiguous run of ready blocks starting at `lastClosedBlockNum + 1`. */
        def longestReadyPrefix: List[ReadyBlock] = {
            def loop(n: BlockNumber, acc: List[ReadyBlock]): List[ReadyBlock] =
                ready.get(n) match {
                    case Some(rb) => loop(n.increment, rb :: acc)
                    case None     => acc.reverse
                }
            loop(lastClosedBlockNum.increment, Nil)
        }

        /** Apply the result of closing a stack: drain the prefix, advance counters, gate the next
          * close on the upcoming PreviousStackHardConfirmation.
          */
        def afterClose(closedStackNum: StackNumber, drained: List[ReadyBlock]): State = {
            val newReady = drained.foldLeft(ready)((m, rb) => m - rb.result.brief.blockNum)
            val newLastBlock = drained.lastOption
                .map(_.result.brief.blockNum)
                .getOrElse(lastClosedBlockNum)
            copy(
              ready = newReady,
              inboundLeaderBrief = inboundLeaderBrief - closedStackNum,
              lastClosedStackNum = closedStackNum,
              lastClosedBlockNum = newLastBlock,
              previousStackHardConfirmed = false
            )
        }
    }

    object State {
        val empty: State = State(
          pending = Map.empty,
          ready = Map.empty,
          inboundLeaderBrief = Map.empty,
          lastClosedStackNum = StackNumber.zero,
          lastClosedBlockNum = BlockNumber.zero,
          // Initial stack 0 boots with the trigger pre-armed (per spec: stack-0
          // hard-confirmation bootstraps the trigger chain).
          previousStackHardConfirmed = true
        )
    }
}
