package hydrozoa.multisig.consensus

import cats.data.NonEmptyList
import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastOps
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming.StackTimes.StackCreationEndTime
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.owninfo.OwnPeerPrivate
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.HeadMultisigRegimeManager
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckId, HardAckNumber}
import hydrozoa.multisig.consensus.peer.PeerId
import hydrozoa.multisig.ledger.block.{Block, BlockNumber, BlockResult}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.{EvacuationMap, JointLedger}
import hydrozoa.multisig.ledger.l1.utxo.MultisigTreasuryUtxo
import hydrozoa.multisig.ledger.stack.*
import hydrozoa.multisig.persistence.recovery.BlockResultScan
import hydrozoa.multisig.persistence.{JournalKey, JournalValue, Markers, Persistence, StoreKey, WriteBatch}
import scala.annotation.tailrec
import scalus.cardano.ledger.TransactionHash

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
    pendingConnections: HeadMultisigRegimeManager.PendingConnections | StackComposer.Connections,
    tracer: ContraTracer[IO, StackComposerEvent],
    persistence: Persistence[IO]
) extends Actor[IO, StackComposer.Request] {
    import StackComposer.*

    /** `config` is a `CardanoNetwork.Section` transitively (`HeadConfig.Section` →
      * `HeadConfig.Bootstrap.Section` → `CardanoNetwork.Section`); expose it as a given so the
      * typed `WriteBatch.put` / `Persistence.write` calls used by [[persistOwnStackClose]] pick it
      * up implicitly.
      */
    private given CardanoNetwork.Section = config

    private val connections = Ref.unsafe[IO, Option[Connections]](None)

    val state: Ref[IO, State] = Ref.unsafe[IO, State](State.initial(config))

    override def preStart: IO[Unit] = for {
        _ <- context.self ! PreStart
        _ <- context.become(receive)
    } yield ()

    override def receive: Receive[IO, Request] = PartialFunction.fromFunction {
        case PreStart =>
            for {
                _ <- initializeConnections
                _ <- recoverOrBootstrap
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

    /** Boot seam (R3): a cold store bootstraps stack 0, a non-empty one recovers instead.
      *
      * `Markers.hardAcked.isDefined` is the test — any own hard-ack proves stack 0 was long since
      * composed and hard-confirmed, so re-handing it to [[SlowConsensusActor]] would re-issue acks
      * (CR3). [[State.recover]] mirrors the same marker (it returns `None` exactly when `hardAcked`
      * is empty), so we drive the branch off its result: `Some` ⇒ seed the recovered state and skip
      * bootstrap, `None` ⇒ cold start.
      *
      * Head and coil share the one [[State.recover]] seam with no peer-type branch: the own-ack
      * journal is the one `PeerId`-keyed `HardAck` journal ([[Markers.recoverHardAcked]] takes a
      * [[PeerId]]), and the closing stack's `lastBlockNum` comes from the `UnsignedStack` every
      * peer persists on every close (atomic with the hard-ack). `hardConfirmed` derives the same
      * way on both ([[Markers.recoverHardConfirmed]]).
      */
    private def recoverOrBootstrap: IO[Unit] =
        for {
            hardAcked <- Markers.recoverHardAcked(persistence.backend, config.ownPeerId)
            hardConfirmed <- Markers.recoverHardConfirmed(persistence.backend)
            recovered <- State.recover(persistence, hardAcked, hardConfirmed, config.ownPeerId)
            _ <- recovered match {
                case Some(recoveredState) => state.set(recoveredState)
                case None                 => bootstrapInitialStack
            }
        } yield ()

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
            _ <- tracer.traceWith(StackComposerEvent.InitialStackBootstrapped)
            _ <- conn.slowConsensusActor ! handoff
        } yield ()
    }

    private def handleBlockResult(r: BlockResult): IO[Unit] = for {
        _ <- state.update(_.recordBlockResult(r))
        _ <- state.update(_.tryPair(r.brief.blockNum))
        _ <- tryProgress
    } yield ()

    private def handleSoftConfirmed(b: Block.SoftConfirmed): IO[Unit] = for {
        _ <- state.update(_.recordSoftConfirmed(b))
        _ <- state.update(_.tryPair(b.blockNum))
        _ <- tryProgress
    } yield ()

    private def handleIncomingStackBrief(brief: StackBrief): IO[Unit] = for {
        _ <- state.update(_.withInboundLeaderBrief(brief))
        // Stack briefs led by OTHER heads are relayed to CoilRelay by the hub's mesh liaisons, not
        // here — so this actor relays only its OWN-led briefs (in tryCloseAsLeader). That keeps each
        // brief relayed exactly once, in spine order.
        _ <- tryProgress
    } yield ()

    private def handlePreviousStackHardConfirmed(
        s: Stack.HardConfirmed
    ): IO[Unit] = {
        val stackNum = s.brief.stackNum
        for {
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
        else if config.canLeadSlow(nextStackNum) then tryCloseAsLeader(s, nextStackNum)
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
                    _ <- tracer.traceWith(
                      StackComposerEvent.StackClosed(
                        nextStackNum,
                        prefix.head.result.brief.blockNum,
                        prefix.last.result.brief.blockNum,
                        isLeader = true
                      )
                    )
                    res <- mkStackUnsigned(brief, prefix, s.treasury, s.evacuationMap)
                    ComposedStack(unsigned, newTreasury, newMap, partitions, withdrawalRows) = res
                    handoff <- buildHandoff(unsigned)
                    conn <- getConnections
                    // Persist the close bundle BEFORE anything leaves this node (CR4/CR8): the brief
                    // and the own hard-acks both cross the peer boundary below (the brief direct to
                    // PeerLiaisons, the acks via SlowConsensusActor's broadcast), so they must be
                    // durable first.
                    _ <- persistOwnStackClose(
                      s.evacuationMap,
                      partitions,
                      newTreasury,
                      unsigned,
                      handoff.ownAcks,
                      withdrawalRows
                    )
                    // Broadcast brief directly to PeerLiaisons (briefs go DIRECT, not via
                    // SlowConsensusActor). Each peer's outbox has a stackBrief lane.
                    _ <- (conn.headPeerLiaisons ! brief).parallel
                    // A hub relays its OWN-led stack brief to CoilRelay (§5.4) [doc-ref]; briefs led by other
                    // heads are relayed by the hub's mesh liaisons, so each is relayed once, in spine
                    // order. No-op off a hub.
                    _ <- conn.coilRelay.traverse_(_ ! brief)
                    // Hand the unsigned stack + own pre-signed hard-acks to SlowConsensusActor
                    // (which manages broadcast scheduling: round-1 / sole immediately, round-2
                    // withheld until local round-1 confirmation).
                    _ <- conn.slowConsensusActor ! handoff
                    _ <- state.update(_.afterClose(nextStackNum, prefix, newTreasury, newMap))
                } yield ()
        }

    /** Persist this peer's per-hard-ack stack-close bundle in one atomic `WriteBatch` (CR4 / CR6 /
      * CR8, §6):
      *
      *   - own `StackBrief` → `Stack` lane (**leader only** — the StackLane author for this stack);
      *   - this peer's `HardAck`s for the stack → `HardAck` lane;
      *   - `Treasury` — the singleton cumulative treasury UTXO ref at `hardAcked` (overwritten in
      *     place — there's no rule-based scenario that needs an older treasury);
      *   - `EvacuationMap` — keyed **per block**, but only at the blocks whose map backs an
      *     on-chain KZG commitment: every **major** block (its post-diff map is the settlement's
      *     `nextKzg`) and every **last-of-partition minor** (the minor that gets a SEC). The maps
      *     at the other minors commit to nothing on-chain, so the rule-based dispute can never need
      *     them — persisting them would be dead weight (see [[StoreKey.EvacuationMap]]). Starting
      *     from `startMap` (the map at the prior `hardAcked`), the closing stack's blocks'
      *     `evacuationMapDiff`s are folded in stack order and the running map is persisted under a
      *     block's `blockNum` only when that block is one of the committed ones.
      *
      * Lane values carry the [[hydrozoa.multisig.persistence.ArrivalStamp]] (creation time; §5.4).
      * Takes the [[StackPartition]]s [[mkStackUnsigned]] already built (the prefix is partitioned
      * once; the blocks fold back in stack order as `partitions.flatMap(_.blocks)`) plus the brief
      * and this peer's hard-acks for the stack.
      */
    private def persistOwnStackClose(
        startMap: EvacuationMap,
        partitions: NonEmptyList[StackPartition],
        newTreasury: MultisigTreasuryUtxo,
        unsigned: Stack.Unsigned,
        ownAcks: List[HardAck],
        withdrawalRows: List[(RequestId, TransactionHash)]
    ): IO[Unit] = {
        val brief = unsigned.brief
        val committed = committedBlockNums(partitions)
        val orderedResults = partitions.toList.flatMap(_.blocks.toList)
        for {
            stamp <- persistence.arrivalStamp
            // Own outputs: the unsigned stack (so SCA can re-form its in-flight cell on recovery —
            // every close, leader or follower), StackBrief (leader only — StackLane author), and
            // this peer's HardAcks.
            laneBatch = {
                val base = WriteBatch.start.put(StoreKey.UnsignedStack(brief.stackNum))(unsigned)
                val withBrief =
                    if config.canLeadSlow(brief.stackNum) then
                        base.put(JournalKey.Stack(brief.stackNum))(JournalValue(stamp, brief))
                    else base
                val withAcks = ownAcks.foldLeft(withBrief)((b, ack) =>
                    b.put(JournalKey.HardAck(ack.peerId, ack.hardAckNum))(JournalValue(stamp, ack))
                )
                // One withdrawal-effect reverse-index row per (withdrawing request, paying effect):
                // GET /head/requests/<id> resolves a withdrawal's settlement/rollout/finalization
                // effects through it. Deterministic across peers, so every peer writes the same rows.
                withdrawalRows.foldLeft(withAcks) { case (b, (requestId, l1TxId)) =>
                    b.put(StoreKey.WithdrawalEffectIndex(requestId, l1TxId))(Array.emptyByteArray)
                }
            }
            // Snapshots: rotated treasury + committed evacuation maps, onto the same batch.
            (_, fullBatch) =
                orderedResults.foldLeft(
                  (startMap, laneBatch.put(StoreKey.Treasury)(newTreasury))
                ) { case ((runMap, batch), result) =>
                    val nextMap = EvacuationMap.applyDiffs(runMap, result.evacuationMapDiff)
                    val nextBatch =
                        if committed.contains(result.brief.blockNum) then
                            batch.put(StoreKey.EvacuationMap(result.brief.blockNum))(nextMap)
                        else batch
                    (nextMap, nextBatch)
                }
            _ <- persistence.write(fullBatch)
        } yield ()
    }

    /** The blocks of a closed stack whose evacuation map backs an on-chain KZG commitment — the
      * only maps the rule-based dispute can need, so the only ones [[persistOwnStackClose]] keeps.
      * Mirrors [[StackEffectsBuilder.mkEffectsRegular]]'s settlement / SEC logic over the same
      * [[StackPartition]]s: each **major** block (its post-diff map is the settlement's `nextKzg`)
      * and each **last-of-partition minor** (the minor that gets a SEC). Final blocks drain the map
      * and commit nothing, so they contribute none.
      */
    private def committedBlockNums(
        partitions: NonEmptyList[StackPartition]
    ): Set[BlockNumber] =
        partitions.toList.flatMap { p =>
            p.kind match {
                case StackPartition.Kind.Major =>
                    p.blocks.head.brief.blockNum ::
                        p.blocks.tail.lastOption.map(_.brief.blockNum).toList
                case StackPartition.Kind.Minor =>
                    List(p.blocks.last.brief.blockNum)
                case StackPartition.Kind.Final | StackPartition.Kind.Initial =>
                    Nil
            }
        }.toSet

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
      *      [[HeadMultisigRegimeManager]] hands over to the rule-based regime.
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
                    // the HeadMultisigRegimeManager hands over to the rule-based regime.
                    tracer.traceWith(
                      StackComposerEvent.StructuralDivergence(
                        nextStackNum,
                        brief.firstBlockNum,
                        brief.lastBlockNum,
                        expectedFirst
                      )
                    ) >> panic(
                      s"Stack $nextStackNum structural divergence; consensus is broken."
                    ) >> context.self.stop
                else
                    s.readySlice(brief.firstBlockNum, brief.lastBlockNum) match {
                        case None =>
                            // (2) not caught up — benign; wait for more BlockResults /
                            // SoftConfirmeds. tryProgress re-fires on the next event.
                            IO.unit
                        case Some(slice) =>
                            // (3) covered — accept exactly the brief's range.
                            for {
                                _ <- tracer.traceWith(
                                  StackComposerEvent.StackClosed(
                                    nextStackNum,
                                    brief.firstBlockNum,
                                    brief.lastBlockNum,
                                    isLeader = false
                                  )
                                )
                                res <- mkStackUnsigned(brief, slice, s.treasury, s.evacuationMap)
                                ComposedStack(
                                  unsigned,
                                  newTreasury,
                                  newMap,
                                  partitions,
                                  withdrawalRows
                                ) = res
                                handoff <- buildHandoff(unsigned)
                                conn <- getConnections
                                // Persist the close bundle before the own acks leave this node via
                                // SlowConsensusActor's broadcast (CR4/CR8).
                                _ <- persistOwnStackClose(
                                  s.evacuationMap,
                                  partitions,
                                  newTreasury,
                                  unsigned,
                                  handoff.ownAcks,
                                  withdrawalRows
                                )
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
      *
      * Returns the [[StackPartition]]s alongside the unsigned stack so the prefix is partitioned
      * **once** — [[persistOwnStackClose]] reuses them rather than re-partitioning.
      */
    private def mkStackUnsigned(
        brief: StackBrief,
        prefix: List[ReadyBlock],
        treasury: MultisigTreasuryUtxo,
        evacuationMap: EvacuationMap
    ): IO[ComposedStack] = {
        val results = NonEmptyList.fromListUnsafe(prefix.map(_.result))
        val partitions = StackPartition.partition(results)
        StackEffectsBuilder.mkEffectsRegular(config, treasury, partitions, evacuationMap) match {
            case Right((effects, newTreasury, newMap, withdrawalRows)) =>
                IO.pure(
                  ComposedStack(
                    Stack.Unsigned(brief, effects),
                    newTreasury,
                    newMap,
                    partitions,
                    withdrawalRows
                  )
                )
            case Left(err) =>
                IO.raiseError(err)
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
            val peer: PeerId = config.ownPeerId

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
        private val wallet = config.ownWallet

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

    // Halt the node by failing the actor, so the HeadMultisigRegimeManager (which watches this child)
    // can hand over to the rule-based regime. Mirrors `JointLedger.panic`.
    private def panic(msg: String): IO[Unit] = throw new RuntimeException(msg)

    private def initializeConnections: IO[Unit] = pendingConnections match {
        case x: HeadMultisigRegimeManager.PendingConnections =>
            for {
                c <- x.get
                _ <- connections.set(
                  Some(
                    Connections(
                      jointLedger = c.jointLedger,
                      fastConsensusActor = c.consensusActor,
                      slowConsensusActor = c.slowConsensusActor,
                      headPeerLiaisons = c.headPeerLiaisons,
                      coilRelay = c.coilRelay
                    )
                  )
                )
            } yield ()
        case x: StackComposer.Connections => connections.set(Some(x))
    }
}

object StackComposer {
    type Handle = ActorRef[IO, Request]

    type Config = HeadConfig.Section & OwnPeerPrivate.Section

    final case class Connections(
        jointLedger: JointLedger.Handle,
        fastConsensusActor: FastConsensusActor.Handle,
        slowConsensusActor: SlowConsensusActor.Handle,
        headPeerLiaisons: List[liaison.PeerLiaisonHeadToHead.Handle],
        /** A hub's coil relay (§5.4) [doc-ref]: every stack brief (own-led and received) is sent
          * here so the hub's coil peers get the whole stack spine. `None` off a hub.
          */
        coilRelay: Option[CoilRelay.Handle] = None
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

    /** The product of composing a stack from a prefix of paired blocks: the [[Stack.Unsigned]] to
      * hand to consensus, the rotated treasury + evacuation map the close advanced to, and the
      * [[StackPartition]] layout. The partitions are carried so the prefix is partitioned **once**
      * — the effect derivation and `persistOwnStackClose` both consume them.
      */
    final case class ComposedStack(
        unsigned: Stack.Unsigned,
        newTreasury: MultisigTreasuryUtxo,
        newEvacuationMap: EvacuationMap,
        partitions: NonEmptyList[StackPartition],
        // Withdrawal-effect rows this stack contributes: `(requestId, l1TxId)` linking a withdrawing
        // request to a settlement / rollout / finalization tx paying one of its L1-bound outputs.
        // Local-only; persisted at stack close into the WithdrawalEffectIndex CF.
        withdrawalRows: List[(RequestId, TransactionHash)]
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
            @tailrec
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
            @tailrec
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
          // Next hard-ack number to assign. 0-based: the PeerLiaisonHeadToHead hard-ack
          // lane is next-expected with an initial cursor of 0 (see the
          // GetMsgBatch cursor protocol in PeerLiaisonHeadToHead), so the first hard-ack
          // is number 0. The initial stack, once injected, takes 0 = round-1,
          // 1 = round-2.
          nextOwnHardAckNum = HardAckNumber.zero,
          // Genesis treasury + evacuation map for stack 1: the treasury produced by the head
          // config's initialization tx, and the config's initial evacuation map.
          treasury = config.initializationTx.treasuryProduced,
          evacuationMap = config.initialEvacuationMap
        )

        /** Reconstruct the full [[State]] from the store after a crash, or `None` if the store
          * holds no own hard-ack yet (cold start — the actor keeps [[initial]] and runs
          * `bootstrapInitialStack`). The last own hard-ack's `stackNum` is the last stack this peer
          * closed; from it we restore the treasury + evacuation-map snapshots and the counters, and
          * rebuild `pending` from the `BlockResult`s soft-acked since that stack's last block.
          * `ready` and `inboundLeaderBrief` start empty — the `Block.SoftConfirmed` halves and
          * inbound briefs arrive from replay (R3), re-pairing into `ready`.
          *
          * `hardAcked` is a [[HardAckNumber]] (the satellite key), NOT a stack number, so the stack
          * number is read from the last own `HardAck` **value** (the one `PeerId`-keyed journal, so
          * `own` works for either peer type); the marker number gives only
          * `nextOwnHardAckNum = hardAcked + 1`. The closing stack's `lastBlockNum`, treasury, and
          * evacuation map are present on every peer for any hard-acked stack — the `UnsignedStack`,
          * treasury, and evacuation map are all written by every peer in the same atomic close
          * batch as the hard-ack (`persistOwnStackClose`) — so a missing entry is store corruption.
          */
        def recover(
            persistence: Persistence[IO],
            hardAcked: Option[HardAckNumber],
            hardConfirmed: Option[StackNumber],
            own: PeerId
        )(using CardanoNetwork.Section): IO[Option[State]] =
            hardAcked match
                case None => IO.pure(None)
                case Some(hardAckNum) =>
                    for {
                        // No peer-type branch: the own hard-ack lives in the one `PeerId`-keyed
                        // `HardAck` journal, and the closing stack's `lastBlockNum` comes from the
                        // `UnsignedStack` every peer persists on every close (atomic with the
                        // hard-ack, so always present for a hard-acked stack).
                        lastHardAck <- persistence.getOrFail(JournalKey.HardAck(own, hardAckNum))
                        hardAckedStack = lastHardAck.payload.stackNum
                        unsignedStack <- persistence.getOrFail(
                          StoreKey.UnsignedStack(hardAckedStack)
                        )
                        lastBlockNum = unsignedStack.brief.lastBlockNum
                        treasury <- persistence.getOrFail(StoreKey.Treasury)
                        evacuationMap <- persistence.getOrFail(StoreKey.EvacuationMap(lastBlockNum))
                        blockResults <- BlockResultScan.scanFrom(persistence, lastBlockNum)
                    } yield Some(
                      blockResults.foldLeft(
                        State(
                          pending = Map.empty,
                          ready = Map.empty,
                          inboundLeaderBrief = Map.empty,
                          lastClosedStackNum = hardAckedStack,
                          lastClosedBlockNum = lastBlockNum,
                          previousStackHardConfirmed =
                              hardConfirmed.exists(Ordering[StackNumber].gteq(_, hardAckedStack)),
                          nextOwnHardAckNum = hardAckNum.increment,
                          treasury = treasury,
                          evacuationMap = evacuationMap
                        )
                      )(_.recordBlockResult(_))
                    )
    }
}
