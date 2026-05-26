package hydrozoa.multisig.consensus

import cats.data.NonEmptyList
import cats.effect.{IO, IOLocal, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastOps
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming.StackTimes.StackCreationEndTime
import hydrozoa.config.node.owninfo.OwnHeadPeerPrivate
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.logging.Tracer
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckId, HardAckNumber}
import hydrozoa.multisig.ledger.block.{Block, BlockNumber, BlockResult}
import hydrozoa.multisig.ledger.joint.{EvacuationMap, JointLedger}
import hydrozoa.multisig.ledger.l1.utxo.MultisigTreasuryUtxo
import hydrozoa.multisig.ledger.stack.*

/** Stack composer.
  *
  * Pairs [[BlockResult]] (from JointLedger) with [[Block.SoftConfirmed]] (from FastConsensusActor)
  * by `blockNum`. Once a block's pair is in the `ready` map, it becomes eligible for inclusion in
  * the next stack subject to the **longest-contiguous-prefix** gate.
  *
  * This actor combines Leader and Follower behaviour in a single flat receive (state-shared across
  * modes — pairing happens in both; only stack-close trigger differs):
  *
  *   - **Leader** (when `isSlowLeader(nextStackNum)`): on `Stack.HardConfirmed` OR after pairing a
  *     new block, attempts to close the next stack from the longest contiguous prefix. Closing
  *     means: build [[StackBrief]] + [[StackEffects]] (via [[StackEffectsBuilder]]), wrap into
  *     [[Stack.Unsigned]], hand off to [[SlowConsensusActor]], and broadcast the brief directly to
  *     PeerLiaisons.
  *   - **Follower**: on an inbound `StackBrief` for `lastClosedStackNum + 1`, classify it three
  *     ways — structural divergence (→ rule-based fallback); not-yet-covered (benign: paired blocks
  *     don't yet span the brief's range — wait silently, re-fires on the next event); covered
  *     (build [[Stack.Unsigned]] from EXACTLY the brief's range — the leader may have closed
  *     earlier than this follower could — re-derive effects locally, sign, hand to
  *     SlowConsensusActor).
  */
final case class StackComposer(
    config: StackComposer.Config,
    pendingConnections: MultisigRegimeManager.PendingConnections | StackComposer.Connections,
    tracerLocal: IOLocal[Tracer]
) extends Actor[IO, StackComposer.Request] {
    import StackComposer.*

    given IOLocal[Tracer] = tracerLocal

    private val connections = Ref.unsafe[IO, Option[Connections]](None)

    val state: Ref[IO, State] = Ref.unsafe[IO, State](State.initial(config))

    override def preStart: IO[Unit] = for {
        _ <- context.self ! PreStart
        _ <- context.become(receive)
    } yield ()

    override def receive: Receive[IO, Request] = PartialFunction.fromFunction {
        case PreStart =>
            for {
                _ <- Tracer.routeLocal(s"StackComposer.${config.ownHeadPeerNum}")
                _ <- initializeConnections
                _ <- bootstrapInitialStack
                _ <- Tracer.info("StackComposer started.")
            } yield ()
        case r: BlockResult =>
            handleBlockResult(r)
        case b: Block.SoftConfirmed =>
            handleSoftConfirmed(b)
        case b: StackBrief =>
            handleIncomingStackBrief(b)
        case s: Stack.HardConfirmed =>
            handlePreviousStackHardConfirmed(s)
    }

    /** Compose and hand off stack 0 (the init + fallback) at startup.
      *
      * Stack 0 is exogenous — its effects come from the shared head config, not from any
      * BlockResult stream — so EVERY peer derives it locally and runs the Initial 2-phase hard-ack
      * flow over it (round-1 = fallback sig, round-2 = init-tx sig + individual funding
      * signatures). No `StackBrief` is broadcast: there's nothing a leader could tell followers
      * that they can't derive from config. The init + fallback bodies are the UNSIGNED config
      * placeholders; the hard-ack aggregation in [[SlowConsensusActor]] is what multisigns them. On
      * hard-confirmation the resulting [[Stack.HardConfirmed]] flows to CardanoLiaison (submittable
      * init + fallback) and back to this composer as the `PreviousStackHardConfirmation` that
      * unblocks stack 1.
      */
    private def bootstrapInitialStack: IO[Unit] = {
        val brief = StackBrief(
          stackNum = StackNumber.zero,
          firstBlockNum = BlockNumber.zero,
          lastBlockNum = BlockNumber.zero,
          // Stack 0 spans exactly the initial block (block 0), so its creation end-time IS that
          // block's end-time — deterministic, from config (not wall-clock boot time).
          creationEndTime = StackCreationEndTime(config.initialBlock.blockBrief.endTime)
        )
        val unsigned = Stack.Unsigned(
          brief,
          StackEffectsBuilder.mkEffectsInitial(config.initializationTx, config.initialFallbackTx)
        )
        for {
            handoff <- buildHandoff(unsigned)
            conn <- getConnections
            _ <- Tracer.info("Bootstrapping initial stack 0 (init + fallback)")
            _ <- conn.slowConsensusActor ! handoff
        } yield ()
    }

    private def handleBlockResult(r: BlockResult): IO[Unit] = for {
        _ <- Tracer.debug(s"BlockResult received for block ${r.brief.blockNum}")
        _ <- state.update(_.recordBlockResult(r))
        _ <- state.update(_.tryPair(r.brief.blockNum))
        _ <- tryProgress
    } yield ()

    private def handleSoftConfirmed(b: Block.SoftConfirmed): IO[Unit] = for {
        _ <- Tracer.debug(s"Block.SoftConfirmed received for block ${b.blockNum}")
        _ <- state.update(_.recordSoftConfirmed(b))
        _ <- state.update(_.tryPair(b.blockNum))
        _ <- tryProgress
    } yield ()

    private def handleIncomingStackBrief(brief: StackBrief): IO[Unit] = for {
        _ <- Tracer.debug(s"StackBrief received for stack ${brief.stackNum}")
        _ <- state.update(_.withInboundLeaderBrief(brief))
        _ <- tryProgress
    } yield ()

    private def handlePreviousStackHardConfirmed(
        s: Stack.HardConfirmed
    ): IO[Unit] = {
        val stackNum = s.brief.stackNum
        for {
            _ <- Tracer.debug(s"Stack.HardConfirmed received for stack $stackNum")
            _ <- state.update(_.withPreviousStackHardConfirmed(stackNum))
            _ <- tryProgress
        } yield ()
    }

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
                for {
                    now <- realTimeQuantizedInstant(config.slotConfig)
                    brief = mkStackBrief(nextStackNum, prefix, StackCreationEndTime(now))
                    _ <- Tracer.info(
                      s"Leader closing stack $nextStackNum with blocks " +
                          s"${prefix.head.result.brief.blockNum}..${prefix.last.result.brief.blockNum}"
                    )
                    res <- mkStackUnsigned(brief, prefix, s.treasury, s.evacuationMap)
                    (unsigned, newTreasury, newMap) = res
                    handoff <- buildHandoff(unsigned)
                    conn <- getConnections
                    // Broadcast brief directly to PeerLiaisons (briefs go DIRECT, not via
                    // SlowConsensusActor). Each peer's outbox has a stackBrief lane.
                    _ <- (conn.peerLiaisons ! brief).parallel
                    // Hand the unsigned stack + own pre-signed hard-acks to SlowConsensusActor
                    // (which manages broadcast scheduling: round-1 / sole immediately, round-2
                    // withheld until local round-1 confirmation).
                    _ <- conn.slowConsensusActor ! handoff
                    _ <- state.update(_.afterClose(nextStackNum, prefix, newTreasury, newMap))
                } yield ()
        }

    private def mkStackBrief(
        stackNum: StackNumber,
        prefix: List[ReadyBlock],
        creationEndTime: StackCreationEndTime
    ): StackBrief =
        StackBrief(
          stackNum = stackNum,
          firstBlockNum = prefix.head.result.brief.blockNum,
          lastBlockNum = prefix.last.result.brief.blockNum,
          creationEndTime = creationEndTime
        )

    /** Follower close-attempt for the leader's announced stack. Classifies the brief against the
      * local paired-block view into exactly three outcomes:
      *
      *   1. **Structural divergence** — `firstBlockNum != lastClosedBlockNum + 1` (the leader is
      *      composing from a different single-flight position) or `last < first`. This is an
      *      unrecoverable consensus break: warn and panic to halt the node, so the
      *      [[MultisigRegimeManager]] hands over to the rule-based regime.
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
                    // single-flight position. Unrecoverable: warn, then panic to halt the node so
                    // the MultisigRegimeManager hands over to the rule-based regime.
                    Tracer.warn(
                      s"Follower stack $nextStackNum structural divergence: leader brief " +
                          s"[${brief.firstBlockNum}..${brief.lastBlockNum}] but expected to " +
                          s"start at $expectedFirst (lastClosedBlockNum=${s.lastClosedBlockNum})."
                    ) >> panic(
                      s"Stack $nextStackNum structural divergence; consensus is broken."
                    ) >> context.self.stop
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
                                res <- mkStackUnsigned(brief, slice, s.treasury, s.evacuationMap)
                                (unsigned, newTreasury, newMap) = res
                                handoff <- buildHandoff(unsigned)
                                conn <- getConnections
                                _ <- conn.slowConsensusActor ! handoff
                                _ <- state.update(
                                  _.afterClose(nextStackNum, slice, newTreasury, newMap)
                                )
                            } yield ()
                    }
        }

    /** Build a [[Stack.Unsigned]] from the prefix. Runs the necessary-effects partition algorithm
      * and the per-block effect derivation, threading the slow-side treasury through (it rotates on
      * settlement / finalization). All partition kinds derive fully — Minor (SEC + post-dated
      * refunds), Major (settlement + fallback + rollouts, + SEC for a trailing minor), Final
      * (finalization + rollouts); see [[StackEffectsBuilder.mkEffectsRegular]].
      */
    private def mkStackUnsigned(
        brief: StackBrief,
        prefix: List[ReadyBlock],
        treasury: MultisigTreasuryUtxo,
        evacuationMap: EvacuationMap
    ): IO[(Stack.Unsigned, MultisigTreasuryUtxo, EvacuationMap)] = {
        val results = NonEmptyList.fromListUnsafe(prefix.map(_.result))
        val partitions = StackPartition.partition(results)
        StackEffectsBuilder.mkEffectsRegular(config, treasury, partitions, evacuationMap) match {
            case Right((effects, newTreasury, newMap)) =>
                IO.pure((Stack.Unsigned(brief, effects), newTreasury, newMap))
            case Left(err) =>
                Tracer.error(s"slow-side effect derivation failed: $err") *> IO.raiseError(err)
        }
    }

    /** Sign this peer's own hard-acks for every round the stack will need, allocating monotonic
      * `HardAckNumber`s from the local counter. Bundles `(unsigned, ownAcks)` into a
      * [[SlowConsensusActor.StackHandoff]] for the slow consensus actor.
      *
      * Signing happens upfront at stack close (round-2's sig domain is the unlock tx body, which is
      * fully known at this point). SlowConsensusActor manages outbound broadcast scheduling
      * (round-1 / sole released immediately to PeerLiaisons; round-2 withheld until local round-1
      * confirmation).
      *
      * The composer walks the partition-indexed StackEffects and applies the shared
      * PartitionEffects.unlock rule to pack this peer's sigs into two explicit acks (round-1 =
      * everything except the unlock; round-2 = the unlock) or one sole ack.
      */
    private def buildHandoff(
        unsigned: Stack.Unsigned
    ): IO[SlowConsensusActor.StackHandoff] =
        state.modify { s =>
            val stackNum = unsigned.brief.stackNum
            val peer = config.ownHeadPeerNum

            val (acks, newNextOwnHardAckNum) = unsigned.effects match {
                case i: StackEffects.Unsigned.Initial =>
                    // Stack 0: structurally 2-phase, exogenous unlock (the init tx).
                    val n1 = s.nextOwnHardAckNum
                    val n2 = n1.increment
                    val round1 = HardAck(
                      HardAckId(peer, n1),
                      stackNum,
                      EffectSigner.mkInitialRound1Signatures(i)
                    )
                    val round2 = HardAck(
                      HardAckId(peer, n2),
                      stackNum,
                      EffectSigner.mkInitialRound2Signatures(i)
                    )
                    (List(round1, round2), n2.increment)

                case r: StackEffects.Unsigned.Regular =>
                    PartitionEffects.unlock(r.partitions) match {
                        case Some(u) =>
                            // Regular stack, 2-phase.
                            val n1 = s.nextOwnHardAckNum
                            val n2 = n1.increment
                            val unlockIndex = u match {
                                case PartitionEffects.Unlock.Settlement(i)   => i
                                case PartitionEffects.Unlock.Finalization(i) => i
                            }
                            val round1Payloads = r.partitions.zipWithIndex.map { case (pe, i) =>
                                EffectSigner.mkRound1Signatures(pe, isUnlock = i == unlockIndex)
                            }
                            val round1 = EffectSigner.assembleRound1Ack(
                              HardAckId(peer, n1),
                              stackNum,
                              round1Payloads
                            )
                            val round2 = EffectSigner.assembleRound2Ack(
                              HardAckId(peer, n2),
                              stackNum,
                              EffectSigner.mkRound2Signatures(r.partitions.toList(unlockIndex))
                            )
                            (List(round1, round2), n2.increment)
                        case None =>
                            // Sole / 1-phase: exactly one Minor partition.
                            val n = s.nextOwnHardAckNum
                            val sole = HardAck(
                              HardAckId(peer, n),
                              stackNum,
                              EffectSigner.mkSolePayload(r.partitions.head)
                            )
                            (List(sole), n.increment)
                    }
            }

            val handoff = SlowConsensusActor.StackHandoff(unsigned, acks)
            (s.copy(nextOwnHardAckNum = newNextOwnHardAckNum), handoff)
        }

    /** This peer's hard-ack signer over a stack's effects. Holds the head wallet (pulled from
      * config) and produces the per-round [[HardAck]] payloads: `mkInitial*` for stack 0,
      * `mkRound1Signatures` / `mkRound2Signatures` for a regular stack's partitions,
      * `mkSolePayload` for a 1-phase single-Minor stack, and `assembleRound1Ack` to pack the
      * per-partition round-1 payloads into the discriminated [[HardAck.Round1Payload.Regular]] (the
      * inverse of [[HardAck.Round1Payload.Regular.asSlots]]).
      */
    private object EffectSigner {
        private val wallet = config.ownHeadWallet

        /** Stack 0 round 1: sign the locally-derived fallback tx. */
        def mkInitialRound1Signatures(
            i: StackEffects.Unsigned.Initial
        ): HardAck.Round1Payload.Initial =
            HardAck.Round1Payload.Initial(fallbackSig = wallet.mkTxSignature(i.fallbackTx.tx))

        /** Stack 0 round 2: sign the exogenous init tx, plus this peer's individual-address
          * signature iff it funds an init input.
          */
        def mkInitialRound2Signatures(
            i: StackEffects.Unsigned.Initial
        ): HardAck.Round2Payload.Initial =
            HardAck.Round2Payload.Initial(
              initTxSig = wallet.mkTxSignature(i.initializationTx.tx),
              individualSig =
                  if StackEffects.spendsFromIndividualAddress(
                        i.initializationTx,
                        wallet.exportVerificationKey
                      )
                  then Some(wallet.mkTxSignature(i.initializationTx.tx))
                  else None
            )

        /** Round-1 signatures for one partition. `isUnlock` marks the partition whose settlement /
          * finalization is withheld for round 2 (a `Partial` slot); every other partition
          * contributes its full set (a `Complete` slot). Minor partitions are never the unlock.
          */
        def mkRound1Signatures(
            pe: PartitionEffects[StandaloneEvacuationCommitment],
            isUnlock: Boolean
        ): HardAck.Round1Payload.PartitionSigs = pe match {
            case PartitionEffects.Major(settlement, fallback, rollouts, refunds, sec) =>
                val fallbackSig = wallet.mkTxSignature(fallback.tx)
                val rolloutSigs = rollouts.map(r => wallet.mkTxSignature(r.tx))
                val refundSigs = refunds.map(r => wallet.mkTxSignature(r.tx))
                val secSig = sec.map(s => wallet.mkHeaderSignature(s.header))
                if isUnlock then
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
                if isUnlock then
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

        /** Round-2 payload for the unlock partition: its settlement / finalization signature — the
          * one effect withheld from round 1. Mirrors `mkRound1Signatures` (a payload, not a bare
          * signature); the caller wraps it in a [[HardAck]] as it does the round-1 payload.
          */
        def mkRound2Signatures(
            unlockPartition: PartitionEffects[StandaloneEvacuationCommitment]
        ): HardAck.Round2Payload.Regular = {
            val sig = unlockPartition match {
                case PartitionEffects.Major(settlement, _, _, _, _) =>
                    wallet.mkTxSignature(settlement.tx)
                case PartitionEffects.Final(finalization, _) =>
                    wallet.mkTxSignature(finalization.tx)
                case _ =>
                    throw new IllegalStateException(
                      "round-2 unlock partition is neither Major nor Final"
                    )
            }
            HardAck.Round2Payload.Regular(sig)
        }

        /** Sole (1-phase) payload for a single-Minor stack: SEC + post-dated refund signatures. */
        def mkSolePayload(
            pe: PartitionEffects[StandaloneEvacuationCommitment]
        ): HardAck.SolePayload = pe match {
            case PartitionEffects.Minor(sec, refunds) =>
                HardAck.SolePayload(
                  sec = wallet.mkHeaderSignature(sec.header),
                  refunds = refunds.map(rt => wallet.mkTxSignature(rt.tx))
                )
            case _ =>
                throw new IllegalStateException("sole stack's single partition is not Minor")
        }

        /** Pack this peer's per-partition round-1 payloads into the discriminated
          * [[HardAck.Round1Payload.Regular]] by partition layout (the inverse of
          * [[HardAck.Round1Payload.Regular.asSlots]]), then wrap it in a [[HardAck]].
          */
        def assembleRound1Ack(
            ackId: HardAckId,
            stackNum: StackNumber,
            partitionSigs: NonEmptyList[HardAck.Round1Payload.PartitionSigs]
        ): HardAck = {
            val payload: HardAck.Round1Payload.Regular = partitionSigs.toList match {
                // Case 3: [Partial]
                case List(p: HardAck.Round1Payload.PartitionSigs.Partial) =>
                    HardAck.Round1Payload.Regular.OnlyPartial(p)
                // Case 4: [MajorPartial, Complete+] — FinalPartial here is impossible (FinalPartial
                // fires only when no Major exists, which means no trailing completes either).
                case (p: HardAck.Round1Payload.PartitionSigs.MajorPartial) :: rest
                    if rest.nonEmpty =>
                    val cs = NonEmptyList.fromListUnsafe(
                      rest.collect { case c: HardAck.Round1Payload.PartitionSigs.Complete => c }
                    )
                    HardAck.Round1Payload.Regular.PartialThenCompletes(p, cs)
                // Case 1: [Minor, Partial]
                case List(
                      m: HardAck.Round1Payload.PartitionSigs.Minor,
                      p: HardAck.Round1Payload.PartitionSigs.Partial
                    ) =>
                    HardAck.Round1Payload.Regular.MinorThenPartial(m, p)
                // Case 2: [Minor, MajorPartial, Complete+] — FinalPartial here is impossible for
                // the same reason as case 4.
                case (m: HardAck.Round1Payload.PartitionSigs.Minor) ::
                    (p: HardAck.Round1Payload.PartitionSigs.MajorPartial) :: rest
                    if rest.nonEmpty =>
                    val cs = NonEmptyList.fromListUnsafe(
                      rest.collect { case c: HardAck.Round1Payload.PartitionSigs.Complete => c }
                    )
                    HardAck.Round1Payload.Regular.MinorThenPartialThenCompletes(m, p, cs)
                case other =>
                    throw new IllegalStateException(
                      "unexpected Regular round-1 layout: " +
                          other.map(_.getClass.getSimpleName).mkString(", ")
                    )
            }
            HardAck(ackId, stackNum, payload)
        }

        /** Wrap the round-2 unlock signature in a [[HardAck]]. Round 2 carries a single payload (no
          * per-partition packing), so this is trivial — present for symmetry with
          * `assembleRound1Ack`.
          */
        def assembleRound2Ack(
            ackId: HardAckId,
            stackNum: StackNumber,
            payload: HardAck.Round2Payload.Regular
        ): HardAck = HardAck(ackId, stackNum, payload)
    }

    private def getConnections: IO[Connections] = for {
        mConn <- connections.get
        conn <- mConn.fold(
          IO.raiseError(
            java.lang.Error("StackComposer is missing its connections to other actors.")
          )
        )(IO.pure)
    } yield conn

    // Halt the node by failing the actor, so the MultisigRegimeManager (which watches this child)
    // can hand over to the rule-based regime. Mirrors `JointLedger.panic`.
    private def panic(msg: String): IO[Unit] = throw new RuntimeException(msg)

    private def initializeConnections: IO[Unit] = pendingConnections match {
        case x: MultisigRegimeManager.PendingConnections =>
            for {
                c <- x.get
                _ <- connections.set(
                  Some(
                    Connections(
                      jointLedger = c.jointLedger,
                      fastConsensusActor = c.consensusActor,
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
        fastConsensusActor: FastConsensusActor.Handle,
        slowConsensusActor: SlowConsensusActor.Handle,
        peerLiaisons: List[PeerLiaison.Handle]
    )

    /** [[Stack.HardConfirmed]] is sent by [[SlowConsensusActor]] when stack reaches
      * hard-confirmation; signals the StackComposer that it may close the next stack (single-flight
      * serialization). The same message also travels to CardanoLiaison for L1 submission; the
      * [[hydrozoa.multisig.consensus.limiter.Limiter]] sitting on the SlowConsensusActor →
      * StackComposer lane reads the underlying [[StackBrief.creationEndTime]] to throttle stack
      * production rate.
      */
    type Request = PreStart.type | BlockResult | Block.SoftConfirmed | StackBrief |
        Stack.HardConfirmed

    case object PreStart

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
        nextOwnHardAckNum: HardAckNumber,
        /** The treasury chain. Advanced on every settlement / finalization tx the composer
          * produces; [[JointLedger]] never touches the treasury — it owns only the deposits map.
          * [[StackEffectsBuilder.mkEffectsRegular]] rotates it on each close (see [[afterClose]]).
          */
        treasury: MultisigTreasuryUtxo,
        /** Cumulative L2 evacuation map; the KZG commitment is computed from it. Accumulates the
          * per-block `evacuationMapDiff`s from `BlockResult` as stacks are composed, and
          * [[StackEffectsBuilder.mkEffectsRegular]] folds the diffs over this running map to
          * compute KZG only at the blocks that need it (each Major's settlement `nextKzg` and each
          * last-of-partition minor's SEC).
          */
        evacuationMap: EvacuationMap
    ) {
        def recordBlockResult(r: BlockResult): State = {
            val pb = pending.getOrElse(r.brief.blockNum, PendingBlock.empty).withResult(r)
            copy(pending = pending.updated(r.brief.blockNum, pb))
        }

        def recordSoftConfirmed(b: Block.SoftConfirmed): State = {
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

        /** Apply the result of closing a stack: drain the prefix, advance counters, rotate the
          * treasury + evacuation map to the values the close derived, and gate the next close on
          * the upcoming `Stack.HardConfirmed` of this stack.
          */
        def afterClose(
            closedStackNum: StackNumber,
            drained: List[ReadyBlock],
            newTreasury: MultisigTreasuryUtxo,
            newEvacuationMap: EvacuationMap
        ): State = {
            val newReady = drained.foldLeft(ready)((m, rb) => m - rb.result.brief.blockNum)
            val newLastBlock = drained.lastOption
                .map(_.result.brief.blockNum)
                .getOrElse(lastClosedBlockNum)
            copy(
              ready = newReady,
              inboundLeaderBrief = inboundLeaderBrief - closedStackNum,
              lastClosedStackNum = closedStackNum,
              lastClosedBlockNum = newLastBlock,
              previousStackHardConfirmed = false,
              treasury = newTreasury,
              evacuationMap = newEvacuationMap
            )
        }
    }

    object State {
        def initial(config: Config): State = State(
          pending = Map.empty,
          ready = Map.empty,
          inboundLeaderBrief = Map.empty,
          lastClosedStackNum = StackNumber.zero,
          lastClosedBlockNum = BlockNumber.zero,
          // Stack 0 is composed + handed off at PreStart (`bootstrapInitialStack`) and must
          // actually hard-confirm before stack 1 may close. So the trigger starts DISARMED; the
          // stack-0 `Stack.HardConfirmed` arriving back from SlowConsensusActor arms it.
          previousStackHardConfirmed = false,
          // Next hard-ack number to assign. 0-based: the PeerLiaison hard-ack
          // lane is next-expected with an initial cursor of 0 (see the
          // GetMsgBatch cursor protocol in PeerLiaison), so the first hard-ack
          // is number 0. The initial stack, once injected, takes 0 = round-1,
          // 1 = round-2.
          nextOwnHardAckNum = HardAckNumber.zero,
          // Genesis treasury + evacuation map for stack 1: the treasury produced by the head
          // config's initialization tx, and the config's initial evacuation map.
          treasury = config.initializationTx.treasuryProduced,
          evacuationMap = config.initialEvacuationMap
        )
    }
}
