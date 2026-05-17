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
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckNumber}
import hydrozoa.multisig.ledger.block.{Block, BlockHeader, BlockNumber, BlockResult}
import hydrozoa.multisig.ledger.effects.{NecessaryEffectsPolicy, StackEffectsBuilder}
import hydrozoa.multisig.ledger.joint.JointLedger
import hydrozoa.multisig.ledger.l1.L1LedgerM
import hydrozoa.multisig.ledger.l1.deposits.map.DepositsMap
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
  *     [[hydrozoa.multisig.ledger.effects.StackEffectsBuilder]] — minor-only stacks fully derive;
  *     Major / Final fall through to TODOs in the next slice), wrap into [[Stack.Unsigned]], hand
  *     off to [[SlowConsensusActor]], and broadcast the brief directly to PeerLiaisons.
  *   - **Follower**: when an inbound `StackBrief` for `lastClosedStackNum + 1` matches the local
  *     longest-prefix `ready` view, validate composition, re-derive effects locally (deterministic
  *     per spec), wrap into [[Stack.Unsigned]], and hand to SlowConsensusActor.
  *
  * Effect derivation, wallet signing, and own-ack bundling are live; the [[SlowConsensusActor]]
  * still auto-confirms (M6 next slice will replace it with real ack aggregation across head peers).
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

    /** Slow-side L1 ledger state — owns the treasury chain. The slow side advances treasury on
      * every settlement / finalization tx it produces; the fast side never touches it after the
      * consensus split. The `deposits` field stays empty here: deposits absorption is a fast-side
      * concern handled in [[JointLedger]]'s own [[L1LedgerM]] instance.
      *
      * Initialized from the head config's initialization tx (treasury produced by the init tx is
      * the genesis treasury for stack 1's settlement).
      */
    private val l1State: Ref[IO, L1LedgerM.State] =
        Ref.unsafe[IO, L1LedgerM.State](
          L1LedgerM.State(
            treasury = config.initializationTx.treasuryProduced,
            deposits = DepositsMap.empty
          )
        )

    /** Run an [[L1LedgerM]] action against the slow-side ledger state, advancing the treasury
      * chain. Called from [[mkUnsigned]] when the partition algorithm produces settlement /
      * finalization txs (currently no-ops for minor-only stacks; full impl pending the next slice).
      */
    private def runL1Action[A](action: L1LedgerM[A]): IO[A] =
        l1State.modify { st =>
            action.run(config, st) match {
                case Right((newSt, a)) => (newSt, IO.pure(a))
                case Left(err) =>
                    (
                      st,
                      Tracer.error(s"slow-side L1 action failed: $err") *> IO.raiseError(err)
                    )
            }
        }.flatten

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
                    unsigned <- mkUnsigned(brief, prefix)
                    handoff <- buildHandoff(unsigned)
                    conn <- getConnections
                    // Broadcast brief directly to PeerLiaisons (per plan: briefs go DIRECT,
                    // not via SlowConsensusActor). Each peer's outbox now has a stackBrief
                    // lane (M7).
                    _ <- (conn.peerLiaisons ! brief).parallel
                    // Hand the unsigned stack + own pre-signed hard-acks to SlowConsensusActor
                    // (which manages broadcast scheduling: round-1 / sole immediately, round-2
                    // withheld until local round-1 confirmation).
                    _ <- conn.slowConsensusActor ! handoff
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
                        unsigned <- mkUnsigned(brief, expectedPrefix)
                        handoff <- buildHandoff(unsigned)
                        conn <- getConnections
                        _ <- conn.slowConsensusActor ! handoff
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

    private def mkStackBrief(stackNum: StackNumber, prefix: List[ReadyBlock]): StackBrief =
        StackBrief(
          stackNum = stackNum,
          firstBlockNum = prefix.head.result.brief.blockNum,
          lastBlockNum = prefix.last.result.brief.blockNum
        )

    /** Build a [[Stack.Unsigned]] from the prefix. Runs the necessary-effects partition algorithm
      * and the per-block effect derivation, threading the slow-side L1 ledger state through
      * (treasury rotates on settlement / finalization).
      *
      * Current scope (M1 first slice): minor-only stacks fully derive — refund txs from each
      * Minor's `postDatedRefundTxs`. Stacks containing Major or Final blocks build the same
      * structure but with empty settlement / fallback / rollout / finalization (TODOs in
      * [[StackEffectsBuilder]]).
      */
    private def mkUnsigned(brief: StackBrief, prefix: List[ReadyBlock]): IO[Stack.Unsigned] = {
        val results = NonEmptyList.fromListUnsafe(prefix.map(_.result))
        val softConfirmations = NonEmptyList.fromListUnsafe(prefix.map(_.softConfirmed))
        val partitions = NecessaryEffectsPolicy.selectNecessaryEffects(results)
        runL1Action(StackEffectsBuilder.deriveRegular(partitions)).map { effects =>
            Stack.Unsigned(brief, results, softConfirmations, effects)
        }
    }

    /** Sign this peer's own hard-acks for every round the stack will need, allocating monotonic
      * `HardAckNumber`s from the local counter. Bundles `(unsigned, ownAcks)` into a
      * [[SlowConsensusActor.StackHandoff]] for the slow consensus actor.
      *
      * Per the plan, signing happens upfront at stack close (round-2's sig domain is the unlock tx
      * body, which is fully known at this point). SlowConsensusActor manages outbound broadcast
      * scheduling (round-1 / sole released immediately to PeerLiaisons; round-2 withheld until
      * local round-1 confirmation).
      */
    private def buildHandoff(unsigned: Stack.Unsigned): IO[SlowConsensusActor.StackHandoff] =
        state.modify { s =>
            val effects = unsigned.effects match {
                case r: StackEffects.Regular => r
                case _: StackEffects.Initial =>
                    throw new IllegalStateException(
                      "buildHandoff: Initial stacks not yet supported by StackComposer leader path"
                    )
            }
            val wallet = config.ownHeadWallet
            val stackNum = unsigned.brief.stackNum

            // Header signing bytes by blockNum — the wallet signs evac-commit *headers*
            // (KZG lives on the header), so the composer must surface them here. The wallet
            // never walks the stack itself.
            val headerBytesByBlock: Map[BlockNumber, BlockHeader.Minor.Onchain.Serialized] =
                unsigned.results.toList.map { r =>
                    r.brief.blockNum -> r.brief.header.signingBytes
                }.toMap
            def evacHeaderBytes(
                ec: hydrozoa.multisig.ledger.l1.tx.StandaloneEvacuationCommitment
            ): (BlockNumber, BlockHeader.Minor.Onchain.Serialized) =
                ec.committedBlockNum -> headerBytesByBlock(ec.committedBlockNum)

            // Partition-index keys use list-index as a proxy (single-partition is the common
            // case; multi-partition exercises the same code paths with multiple entries).
            val refundsIn = effects.refunds.zipWithIndex.map { case (tx, i) =>
                (0, i) -> tx.tx
            }.toMap

            val twoPhase = effects.settlements.nonEmpty || effects.finalization.isDefined

            val (acks, newCounter) =
                if twoPhase then {
                    val n1 = s.ownHardAckNum
                    val n2 = n1.increment

                    val firstUnlockIsSettlement = effects.settlements.nonEmpty
                    // Round-1 signs every effect EXCEPT the round-2 unlock (first
                    // settlement, or finalization when there's no settlement).
                    val round1Settlements = effects.settlements.zipWithIndex.collect {
                        case (tx, i) if !(firstUnlockIsSettlement && i == 0) => i -> tx.tx
                    }.toMap
                    val round1Fallbacks = effects.fallbacks.zipWithIndex.map { case (tx, i) =>
                        i -> tx.tx
                    }.toMap
                    val round1Rollouts = effects.rollouts.zipWithIndex.map { case (tx, i) =>
                        (0, i) -> tx.tx
                    }.toMap
                    val round1EvacCommits = effects.evacCommits.map(evacHeaderBytes).toMap
                    val round1Finalization =
                        if firstUnlockIsSettlement then
                            effects.finalization.map(f => 0 -> f.tx).toMap
                        else Map.empty[Int, scalus.cardano.ledger.Transaction]

                    val unlockTx = effects.settlements.headOption
                        .map(_.tx)
                        .orElse(effects.finalization.map(_.tx))
                        .getOrElse(
                          throw new IllegalStateException(
                            "buildHandoff: 2-phase stack has no settlement and no finalization"
                          )
                        )

                    val round1 = wallet.mkHardAckRound1Regular(
                      stackNum = stackNum,
                      hardAckNum = n1,
                      in = HardAck.SigningInputs.Round1Regular(
                        settlements = round1Settlements,
                        fallbacks = round1Fallbacks,
                        rollouts = round1Rollouts,
                        refunds = refundsIn,
                        evacCommits = round1EvacCommits,
                        finalization = round1Finalization
                      )
                    )
                    val round2 = wallet.mkHardAckRound2Regular(
                      stackNum = stackNum,
                      hardAckNum = n2,
                      in = HardAck.SigningInputs.Round2Regular(unlock = unlockTx)
                    )
                    (List(round1, round2), n2.increment)
                } else {
                    // Minor-only stack: exactly one TrailingMinors partition ⇒ exactly one
                    // standalone evac commit.
                    val n = s.ownHardAckNum
                    val ec = effects.evacCommits match {
                        case List(single) => single
                        case other =>
                            throw new IllegalStateException(
                              "buildHandoff: minor-only stack must have exactly one evac " +
                                  s"commit, got ${other.size}"
                            )
                    }
                    val sole = wallet.mkHardAckSole(
                      stackNum = stackNum,
                      hardAckNum = n,
                      in = HardAck.SigningInputs.Sole(
                        refunds = refundsIn,
                        evacCommit = evacHeaderBytes(ec)
                      )
                    )
                    (List(sole), n.increment)
                }

            val handoff = SlowConsensusActor.StackHandoff(unsigned, acks)
            (s.copy(ownHardAckNum = newCounter), handoff)
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
        previousStackHardConfirmed: Boolean,
        /** Monotonic per-peer cursor for this peer's outgoing hard-acks. Advances by 1 for every
          * HardAck this peer produces (round-1 + round-2 for 2-phase stacks; sole for 1-phase).
          * Wire ordering invariant: round-1 always precedes round-2 for the same stackNum.
          */
        ownHardAckNum: HardAckNumber
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
          previousStackHardConfirmed = true,
          ownHardAckNum = HardAckNumber.zero
        )
    }
}
