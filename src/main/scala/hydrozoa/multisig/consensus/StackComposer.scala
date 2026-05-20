package hydrozoa.multisig.consensus

import cats.data.NonEmptyList
import cats.effect.{IO, IOLocal, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastOps
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.node.owninfo.OwnHeadPeerPrivate
import hydrozoa.lib.logging.Tracer
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckId, HardAckNumber}
import hydrozoa.multisig.ledger.block.{Block, BlockNumber, BlockResult}
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
  *     Closing means: build [[StackBrief]] + [[StackEffects]] (via [[StackEffectsBuilder]] —
  *     minor-only stacks fully derive; Major / Final fall through to TODOs in the next slice), wrap
  *     into [[Stack.Unsigned]], hand off to [[SlowConsensusActor]], and broadcast the brief
  *     directly to PeerLiaisons.
  *   - **Follower**: on an inbound `StackBrief` for `lastClosedStackNum + 1`, classify it three
  *     ways — structural divergence (→ rule-based fallback, TODO); not-yet-covered (benign: paired
  *     blocks don't yet span the brief's range — wait silently, re-fires on the next event);
  *     covered (build [[Stack.Unsigned]] from EXACTLY the brief's range — the leader may have
  *     closed earlier than this follower could — re-derive effects locally, sign, hand to
  *     SlowConsensusActor).
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
                _ <- Tracer.info("StackComposer started.")
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
        _ <- Tracer.debug(s"BlockResult received for block ${r.brief.blockNum}")
        _ <- state.update(_.withBlockResult(r))
        _ <- state.update(_.tryPair(r.brief.blockNum))
        _ <- tryProgress
    } yield ()

    private def handleSoftConfirmed(b: Block.SoftConfirmed): IO[Unit] = for {
        _ <- Tracer.debug(s"Block.SoftConfirmed received for block ${b.blockNum}")
        _ <- state.update(_.withSoftConfirmed(b))
        _ <- state.update(_.tryPair(b.blockNum))
        _ <- tryProgress
    } yield ()

    private def handleIncomingStackBrief(brief: StackBrief): IO[Unit] = for {
        _ <- Tracer.debug(s"StackBrief received for stack ${brief.stackNum}")
        _ <- state.update(_.withInboundLeaderBrief(brief))
        _ <- tryProgress
    } yield ()

    private def handlePreviousStackHardConfirmation(
        p: PreviousStackHardConfirmation
    ): IO[Unit] = for {
        _ <- Tracer.debug(s"PreviousStackHardConfirmation received for stack ${p.stackNum}")
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
                    _ <- Tracer.info(
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

    /** Follower close-attempt for the leader's announced stack. Classifies the brief against the
      * local paired-block view into exactly three outcomes:
      *
      *   1. **Structural divergence** — `firstBlockNum != lastClosedBlockNum + 1` (the leader is
      *      composing from a different single-flight position) or `last < first`. This is a genuine
      *      consensus break: per the plan it triggers fallback into the rule-based regime. (TODO:
      *      wire the fallback; for now warn — the brief never actually arrives over the wire yet
      *      since SlowConsensusActor auto-confirms and there is no real multi-peer StackBrief
      *      delivery.)
      *   2. **Not yet covered** — structurally fine, but this follower hasn't paired every block in
      *      `[first, last]` yet (a constituent block's `BlockResult` or its `Block.SoftConfirmed`
      *      is still in flight). This is the common, benign case — NOT a divergence. Do nothing and
      *      stay silent; `tryProgress` re-fires on the next `BlockResult` / `Block.SoftConfirmed` /
      *      `StackBrief`, so the follower naturally waits until it catches up.
      *   3. **Covered** — every block in `[first, last]` is paired. Build the stack from EXACTLY
      *      the brief's range (not the local longest prefix — the leader may have closed earlier
      *      than this follower could), re-derive effects locally, sign, and hand off.
      */
    private def tryCloseAsFollower(s: State, nextStackNum: StackNumber): IO[Unit] =
        s.inboundLeaderBrief.get(nextStackNum) match {
            case None => IO.unit // no brief yet — wait
            case Some(brief) =>
                val expectedFirst = s.lastClosedBlockNum.increment
                val structurallyConsistent =
                    brief.firstBlockNum.convert == expectedFirst.convert &&
                        brief.lastBlockNum.convert >= brief.firstBlockNum.convert

                if !structurallyConsistent then
                    // (1) genuine divergence — leader's composition is inconsistent with our
                    // single-flight position. TODO(M5/M6): fall back to the rule-based regime.
                    Tracer.warn(
                      s"Follower stack $nextStackNum structural divergence: leader brief " +
                          s"[${brief.firstBlockNum}..${brief.lastBlockNum}] but expected to " +
                          s"start at $expectedFirst (lastClosedBlockNum=" +
                          s"${s.lastClosedBlockNum}). TODO: rule-based fallback."
                    )
                else
                    s.readySlice(brief.firstBlockNum, brief.lastBlockNum) match {
                        case None =>
                            // (2) not caught up — benign; wait for more BlockResults /
                            // SoftConfirmeds. tryProgress re-fires on the next event.
                            Tracer.debug(
                              s"Follower waiting for stack $nextStackNum: paired blocks do " +
                                  s"not yet cover [${brief.firstBlockNum}.." +
                                  s"${brief.lastBlockNum}]"
                            )
                        case Some(slice) =>
                            // (3) covered — accept exactly the brief's range.
                            for {
                                _ <- Tracer.info(
                                  s"Follower accepting stack $nextStackNum brief from " +
                                      s"leader; blocks ${brief.firstBlockNum}.." +
                                      s"${brief.lastBlockNum}"
                                )
                                unsigned <- mkUnsigned(brief, slice)
                                handoff <- buildHandoff(unsigned)
                                conn <- getConnections
                                _ <- conn.slowConsensusActor ! handoff
                                _ <- state.update(_.afterClose(nextStackNum, slice))
                            } yield ()
                    }
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
        val partitions = StackPartition.partition(results)
        runL1Action(StackEffectsBuilder.deriveRegular(partitions)).map { effects =>
            Stack.Unsigned(brief, effects)
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
            val wallet = config.ownHeadWallet
            val stackNum = unsigned.brief.stackNum
            val peer = config.ownHeadPeerNum

            // The composer walks the partition-indexed StackEffects and applies the shared
            // PartitionEffects.unlock rule to pack this peer's sigs into two explicit acks
            // (round-1 = everything except the unlock; round-2 = the unlock) or one sole ack.
            // No StackEffectsSigningInputs / HardAckRoundPlan indirection — the effects ARE the
            // canonical structure; SlowConsensusActor verifies/aggregates against the same.

            def slot(
                pe: PartitionEffects[StandaloneEvacuationCommitment],
                idx: Int,
                unlock: PartitionEffects.Unlock
            ): HardAck.Round1Payload.PartitionSigs = pe match {
                case PartitionEffects.Major(settlement, fallback, rollouts, refunds, sec) =>
                    val fallbackSig = wallet.mkTxSignature(fallback.tx)
                    val rolloutSigs = rollouts.map(r => wallet.mkTxSignature(r.tx))
                    val refundSigs = refunds.map(r => wallet.mkTxSignature(r.tx))
                    val secSig = sec.map(s => wallet.mkHeaderSignature(s.header))
                    if unlock == PartitionEffects.Unlock.Settlement(idx) then
                        HardAck.Round1Payload.PartitionSigs.MajorPartial(
                          fallback = fallbackSig,
                          rollouts = rolloutSigs,
                          refunds = refundSigs,
                          sec = secSig
                        )
                    else
                        HardAck.Round1Payload.PartitionSigs.MajorComplete(
                          settlement = wallet.mkTxSignature(settlement.tx),
                          fallback = fallbackSig,
                          rollouts = rolloutSigs,
                          refunds = refundSigs,
                          sec = secSig
                        )
                case PartitionEffects.Final(finalization, rollouts) =>
                    val rolloutSigs = rollouts.map(r => wallet.mkTxSignature(r.tx))
                    if unlock == PartitionEffects.Unlock.Finalization(idx) then
                        HardAck.Round1Payload.PartitionSigs.FinalPartial(rollouts = rolloutSigs)
                    else
                        HardAck.Round1Payload.PartitionSigs.FinalComplete(
                          finalization = wallet.mkTxSignature(finalization.tx),
                          rollouts = rolloutSigs
                        )
                case PartitionEffects.Minor(sec, refunds) =>
                    HardAck.Round1Payload.PartitionSigs.Minor(
                      sec = wallet.mkHeaderSignature(sec.header),
                      refunds = refunds.map(r => wallet.mkTxSignature(r.tx))
                    )
            }

            def unlockSig(
                u: PartitionEffects.Unlock,
                parts: NonEmptyList[PartitionEffects[StandaloneEvacuationCommitment]]
            ) = u match {
                case PartitionEffects.Unlock.Settlement(i) =>
                    parts.toList(i) match {
                        case PartitionEffects.Major(settlement, _, _, _, _) =>
                            wallet.mkTxSignature(settlement.tx)
                        case _ =>
                            throw new IllegalStateException(
                              s"unlock Settlement($i) is not a Major partition"
                            )
                    }
                case PartitionEffects.Unlock.Finalization(i) =>
                    parts.toList(i) match {
                        case PartitionEffects.Final(finalization, _) =>
                            wallet.mkTxSignature(finalization.tx)
                        case _ =>
                            throw new IllegalStateException(
                              s"unlock Finalization($i) is not a Final partition"
                            )
                    }
            }

            val (acks, newCounter) = unsigned.effects match {
                case r: StackEffects.Unsigned.Regular =>
                    PartitionEffects.unlock(r.partitions) match {
                        case Some(u) =>
                            val n1 = s.ownHardAckNum
                            val n2 = n1.increment
                            val slots = r.partitions.zipWithIndex.map { case (pe, i) =>
                                slot(pe, i, u)
                            }
                            val regularPayload: HardAck.Round1Payload.Regular =
                                slots.toList match {
                                    // Case 3: [Partial]
                                    case List(p: HardAck.Round1Payload.PartitionSigs.Partial) =>
                                        HardAck.Round1Payload.Regular.OnlyPartial(p)
                                    // Case 4: [MajorPartial, Complete+] — FinalPartial here is
                                    // impossible (FinalPartial fires only when no Major exists,
                                    // which means no trailing completes either).
                                    case (p: HardAck.Round1Payload.PartitionSigs.MajorPartial) :: rest
                                        if rest.nonEmpty =>
                                        val cs = NonEmptyList.fromListUnsafe(
                                          rest.collect {
                                              case c: HardAck.Round1Payload.PartitionSigs.Complete =>
                                                  c
                                          }
                                        )
                                        HardAck.Round1Payload.Regular.PartialThenCompletes(p, cs)
                                    // Case 1: [Minor, Partial]
                                    case List(
                                          m: HardAck.Round1Payload.PartitionSigs.Minor,
                                          p: HardAck.Round1Payload.PartitionSigs.Partial
                                        ) =>
                                        HardAck.Round1Payload.Regular.MinorThenPartial(m, p)
                                    // Case 2: [Minor, MajorPartial, Complete+] — FinalPartial
                                    // here is impossible for the same reason as case 4.
                                    case (m: HardAck.Round1Payload.PartitionSigs.Minor) ::
                                        (p: HardAck.Round1Payload.PartitionSigs.MajorPartial) :: rest
                                        if rest.nonEmpty =>
                                        val cs = NonEmptyList.fromListUnsafe(
                                          rest.collect {
                                              case c: HardAck.Round1Payload.PartitionSigs.Complete =>
                                                  c
                                          }
                                        )
                                        HardAck.Round1Payload.Regular
                                            .MinorThenPartialThenCompletes(m, p, cs)
                                    case other =>
                                        throw new IllegalStateException(
                                          "unexpected Regular slot layout: " +
                                              other.map(_.getClass.getSimpleName).mkString(", ")
                                        )
                                }
                            val round1 = HardAck(
                              HardAckId(peer, n1),
                              stackNum,
                              regularPayload
                            )
                            val round2 = HardAck(
                              HardAckId(peer, n2),
                              stackNum,
                              HardAck.Round2Payload.Regular(unlockSig(u, r.partitions))
                            )
                            (List(round1, round2), n2.increment)
                        case None =>
                            // Sole / 1-phase: exactly one Minor partition.
                            val n = s.ownHardAckNum
                            val sole = r.partitions.head match {
                                case PartitionEffects.Minor(sec, refunds) =>
                                    HardAck(
                                      HardAckId(peer, n),
                                      stackNum,
                                      HardAck.SolePayload(
                                        sec = wallet.mkHeaderSignature(sec.header),
                                        refunds = refunds.map(rt => wallet.mkTxSignature(rt.tx))
                                      )
                                    )
                                case _ =>
                                    throw new IllegalStateException(
                                      "sole stack's single partition is not Minor"
                                    )
                            }
                            (List(sole), n.increment)
                    }
                case i: StackEffects.Unsigned.Initial =>
                    // Stack 0: structurally 2-phase, exogenous unlock (the init tx).
                    val n1 = s.ownHardAckNum
                    val n2 = n1.increment
                    val round1 = HardAck(
                      HardAckId(peer, n1),
                      stackNum,
                      HardAck.Round1Payload.Initial(
                        fallbackSig = wallet.mkTxSignature(i.fallbackTx.tx)
                      )
                    )
                    val round2 = HardAck(
                      HardAckId(peer, n2),
                      stackNum,
                      HardAck.Round2Payload.Initial(
                        initTxSig = wallet.mkTxSignature(i.initializationTx.tx),
                        individualWitnesses =
                            if StackEffects.spendsFromIndividualAddress(
                                  i.initializationTx,
                                  wallet.exportVerificationKey
                                )
                            then List(wallet.mkVKeyWitness(i.initializationTx.tx))
                            else Nil
                      )
                    )
                    (List(round1, round2), n2.increment)
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

        /** The exact paired blocks for the inclusive range `[first, last]`, or `None` if any block
          * in that range is not yet paired (the follower simply hasn't caught up yet — NOT a
          * divergence). Caller must have already checked structural consistency (`first ==
          * lastClosedBlockNum + 1`, `first <= last`).
          */
        def readySlice(first: BlockNumber, last: BlockNumber): Option[List[ReadyBlock]] = {
            def loop(n: BlockNumber, acc: List[ReadyBlock]): Option[List[ReadyBlock]] =
                if n.convert > last.convert then Some(acc.reverse)
                else
                    ready.get(n) match {
                        case Some(rb) => loop(n.increment, rb :: acc)
                        case None     => None // gap ⇒ not yet covered
                    }
            loop(first, Nil)
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
          // Next hard-ack number to assign. 0-based: the PeerLiaison hard-ack
          // lane is next-expected with an initial cursor of 0 (see the
          // GetMsgBatch cursor protocol in PeerLiaison), so the first hard-ack
          // is number 0. The initial stack, once injected, takes 0 = round-1,
          // 1 = round-2.
          ownHardAckNum = HardAckNumber.zero
        )
    }
}
