package hydrozoa.multisig.consensus

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import cats.effect.{IO, IOLocal, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastOps
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.node.owninfo.OwnHeadPeerPrivate
import hydrozoa.lib.logging.Tracer
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.StackComposer.PreviousStackHardConfirmation
import hydrozoa.multisig.consensus.ack.HardAck
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.multisig.ledger.l1.tx.{RefundTx, Tx, TxSignature}
import hydrozoa.multisig.ledger.stack.{PartitionEffects, Stack, StackEffects, StackNumber, StandaloneEvacuationCommitment}
import scala.util.control.NonFatal
import scalus.cardano.ledger.{Transaction, TransactionHash, VKeyWitness}
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.platform

/** Slow-consensus actor (M6).
  *
  * Aggregates per-effect hard-acks from all head peers for each closed stack, verifies every
  * signature against the locally-derived, **partition-indexed** effect bodies
  * ([[Stack.Unsigned.effects]] — the canonical structure both StackComposer signs and this actor
  * verifies against; determinism makes the sigs comparable), and on saturation emits
  * `Stack.HardConfirmed` to CardanoLiaison + `PreviousStackHardConfirmation` to StackComposer.
  *
  * It owns no wallet: StackComposer signs all of this peer's hard-acks upfront and bundles them
  * into a [[SlowConsensusActor.StackHandoff]] as two explicit acks (round-1 + round-2) or one sole
  * ack. This actor never splits anything — own acks arrive pre-split; it manages only the outbound
  * *schedule* (round-1 / sole broadcast immediately, round-2 withheld until local round-1
  * confirmation — mirrors the fast-side ConsensusActor's "scheduled own ack" pattern).
  *
  * There is no `HardAckRoundPlan` / `StackEffectsSigningInputs` indirection: the partition-indexed
  * [[StackEffects]] IS the canonical structure. The only shared rule is [[PartitionEffects.unlock]]
  * — which partition's settlement / finalization is the round-2 unlock — used identically by the
  * signer (to pack) and here (to know round-2's target + which round-1 slot is excised).
  *
  * ==Phases==
  *
  *   - 2-phase (a settlement / finalization is present, or the initial stack): round 1 collects
  *     every effect sig except the unlock; on round-1 saturation the held own round-2 ack is
  *     released and round 2 collects the unlock sig.
  *   - 1-phase / sole (minor-only stack — exactly one [[PartitionEffects.Minor]] partition): one
  *     round over the refund sigs + the minor's evac-commitment header sig.
  *
  * Saturation = a verified ack from every head peer (the local peer's own included). When coil
  * peers join (future PR) this additionally requires a coil quorum.
  *
  * ==Orphan acks==
  *
  * A remote peer's ack for stack N can arrive before this peer's StackComposer hands stack N off.
  * Such acks are buffered per stackNum and replayed when the cell is created; verification still
  * happens at replay time against the locally-derived effects.
  *
  * Deterministic-derivation invariant: every head peer derives byte-identical effects from the same
  * `Stack.Unsigned`. A signature that fails to verify means the signer derived different effects —
  * a critical consensus break, not recoverable: the cell raises, the actor halts, and the
  * rule-based fallback / dead-man's switch takes over.
  */
final case class SlowConsensusActor(
    config: SlowConsensusActor.Config,
    pendingConnections: MultisigRegimeManager.PendingConnections | SlowConsensusActor.Connections,
    tracerLocal: IOLocal[Tracer]
) extends Actor[IO, SlowConsensusActor.Request] {
    import SlowConsensusActor.*

    given IOLocal[Tracer] = tracerLocal

    private val connections = Ref.unsafe[IO, Option[Connections]](None)
    private val stateRef = Ref.unsafe[IO, State](State.initial)

    private lazy val allPeers: Set[HeadPeerNumber] =
        config.headPeerNums.toList.toSet

    override def preStart: IO[Unit] = for {
        _ <- context.self ! PreStart
        _ <- context.become(receive)
    } yield ()

    override def receive: Receive[IO, Request] = PartialFunction.fromFunction {
        case PreStart =>
            for {
                _ <- Tracer.routeLocal(s"SlowConsensusActor.${config.ownHeadPeerNum}")
                _ <- initializeConnections
                _ <- Tracer.info("SlowConsensusActor started.")
            } yield ()
        case h: StackHandoff =>
            handleStackHandoff(h)
        case h: HardAck =>
            handleRemoteHardAck(h)
    }

    // ===================================
    // Handoff (own acks) — create the cell, broadcast round-1 / sole, replay orphans
    // ===================================

    /** Dispatch on the (locally-derived) effects shape — NOT a re-derived plan:
      *   - `Initial` ⇒ 2-phase (init flow).
      *   - `Regular` with an unlock ([[PartitionEffects.unlock]] `Some`) ⇒ 2-phase.
      *   - `Regular` with no unlock (all-`Minor`) ⇒ sole / 1-phase.
      */
    private def handleStackHandoff(h: StackHandoff): IO[Unit] =
        Tracer.scopedCtx("stackNum" -> h.unsigned.brief.stackNum.toString) {
            val stackNum = h.unsigned.brief.stackNum
            val ownPeer = config.ownHeadPeerNum

            def own2Phase(r1Label: String, r2Label: String): IO[Unit] = for {
                ownR1 <- h.ownAcks
                    .collectFirst { case a @ HardAck(_, _, p: HardAck.Payload.Round1) =>
                        (a, p)
                    }
                    .liftTo[IO](HandoffError.MissingOwnAck(r1Label))
                ownR2 <- h.ownAcks
                    .collectFirst { case a @ HardAck(_, _, _: HardAck.Payload.Round2) => a }
                    .liftTo[IO](HandoffError.MissingOwnAck(r2Label))
                _ <- start2Phase(stackNum, ownPeer, h.unsigned, ownR1, ownR2)
            } yield ()

            h.unsigned.effects match {
                case _: StackEffects.Unsigned.Initial =>
                    own2Phase("own round-1 (Initial)", "own round-2 (Initial)")
                case r: StackEffects.Unsigned.Regular =>
                    PartitionEffects.unlock(r.partitions) match {
                        case Some(_) =>
                            own2Phase("own round-1 (Regular)", "own round-2 (Regular)")
                        case None =>
                            for {
                                ownSole <- h.ownAcks
                                    .collectFirst {
                                        case a @ HardAck(_, _, p: HardAck.SolePayload) =>
                                            (a, p)
                                    }
                                    .liftTo[IO](HandoffError.MissingOwnAck("own sole"))
                                _ <- verifySole(h.unsigned, ownPeer, ownSole._2)
                                cell = Cell.WaitingSole(
                                  unsigned = h.unsigned,
                                  sole = Map(ownPeer -> ownSole._2)
                                )
                                _ <- putCell(stackNum, cell)
                                _ <- Tracer.info(
                                  s"stack $stackNum handed off (sole); broadcasting own " +
                                      "sole ack"
                                )
                                _ <- broadcast(ownSole._1)
                                _ <- replayOrphans(stackNum)
                                _ <- tryAdvance(stackNum)
                            } yield ()
                    }
            }
        }

    /** Shared 2-phase cell creation (Regular + Initial): verify own acks against the locally
      * derived effects, create WaitingRound1, broadcast own round-1, replay orphans, advance.
      */
    private def start2Phase(
        stackNum: StackNumber,
        ownPeer: HeadPeerNumber,
        unsigned: Stack.Unsigned,
        ownR1: (HardAck, HardAck.Payload.Round1),
        ownR2: HardAck
    ): IO[Unit] = for {
        _ <- verify2PhaseRound1(unsigned, ownPeer, ownR1._2)
        ownR2p <- round2PayloadOf(ownR2)
        _ <- verify2PhaseRound2(unsigned, ownPeer, ownR2p)
        _ <- putCell(
          stackNum,
          Cell.WaitingRound1(
            unsigned = unsigned,
            round1 = Map(ownPeer -> ownR1._2),
            ownRound2 = ownR2,
            round2Stash = Map.empty
          )
        )
        _ <- Tracer.info(
          s"stack $stackNum handed off (2-phase); broadcasting own round-1"
        )
        _ <- broadcast(ownR1._1)
        _ <- replayOrphans(stackNum)
        _ <- tryAdvance(stackNum)
    } yield ()

    private def round2PayloadOf(a: HardAck): IO[HardAck.Payload.Round2] = a.payload match {
        case p: HardAck.Payload.Round2 => IO.pure(p)
        case other => IO.raiseError(CellError.UnexpectedPayload(a.stackNum, other.roundLabel))
    }

    // ===================================
    // Remote acks
    // ===================================

    private def handleRemoteHardAck(h: HardAck): IO[Unit] =
        Tracer.scopedCtx(h.toContext*) {
            stateRef.get.flatMap { s =>
                s.cells.get(h.stackNum) match {
                    case None =>
                        Tracer.debug(
                          s"orphan hard-ack for stack ${h.stackNum} from peer ${h.peerNum} " +
                              "(no cell yet); buffering"
                        ) >> stateRef.update(_.bufferOrphan(h))
                    case Some(_) =>
                        applyRemote(h) >> tryAdvance(h.stackNum)
                }
            }
        }

    /** Apply one remote ack into its (existing) cell, verifying it against the local effects. */
    private def applyRemote(h: HardAck): IO[Unit] = withCell(h.stackNum) { cell =>
        val peer = h.peerNum
        cell match {
            case c: Cell.WaitingRound1 =>
                h.payload match {
                    case p: HardAck.Payload.Round1 =>
                        verify2PhaseRound1(c.unsigned, peer, p).as(
                          c.copy(round1 = c.round1.updated(peer, p))
                        )
                    case p: HardAck.Payload.Round2 =>
                        verify2PhaseRound2(c.unsigned, peer, p).as(
                          c.copy(round2Stash = c.round2Stash.updated(peer, p))
                        )
                    case other =>
                        IO.raiseError(CellError.UnexpectedPayload(h.stackNum, other.roundLabel))
                }
            case c: Cell.WaitingRound2 =>
                h.payload match {
                    case p: HardAck.Payload.Round2 =>
                        verify2PhaseRound2(c.unsigned, peer, p).as(
                          c.copy(round2 = c.round2.updated(peer, p))
                        )
                    case _: HardAck.Payload.Round1 =>
                        Tracer
                            .debug(
                              s"ignoring late round-1 ack for stack ${h.stackNum} from $peer"
                            )
                            .as(c)
                    case other =>
                        IO.raiseError(CellError.UnexpectedPayload(h.stackNum, other.roundLabel))
                }
            case c: Cell.WaitingSole =>
                h.payload match {
                    case p: HardAck.SolePayload =>
                        verifySole(c.unsigned, peer, p).as(c.copy(sole = c.sole.updated(peer, p)))
                    case other =>
                        IO.raiseError(CellError.UnexpectedPayload(h.stackNum, other.roundLabel))
                }
        }
    }

    // ===================================
    // Advancement: round-1 → round-2 release, and completion
    // ===================================

    private def tryAdvance(stackNum: StackNumber): IO[Unit] = stateRef.get.flatMap { s =>
        s.cells.get(stackNum) match {
            case Some(c: Cell.WaitingRound1) if c.round1.keySet == allPeers =>
                completeRound1(stackNum, c)
            case Some(c: Cell.WaitingRound2) if c.round2.keySet == allPeers =>
                completeStack(stackNum, c)
            case Some(c: Cell.WaitingSole) if c.sole.keySet == allPeers =>
                completeStack(stackNum, c)
            case _ => IO.unit
        }
    }

    private def completeRound1(stackNum: StackNumber, c: Cell.WaitingRound1): IO[Unit] = for {
        _ <- Tracer.info(
          s"stack $stackNum round-1 confirmed; releasing own round-2 ack"
        )
        ownPeer = config.ownHeadPeerNum
        ownR2Payload <- round2PayloadOf(c.ownRound2)
        round2 = c.round2Stash.updated(ownPeer, ownR2Payload)
        _ <- putCell(
          stackNum,
          Cell.WaitingRound2(
            unsigned = c.unsigned,
            round1 = c.round1,
            round2 = round2
          )
        )
        _ <- broadcast(c.ownRound2)
        _ <- tryAdvance(stackNum)
    } yield ()

    /** A round saturated. Aggregate every collected per-peer hard-ack signature into
      * `VKeyWitness`es / per-partition SEC header-sig sets, attach them onto the matching effects,
      * and emit the now-multisigned [[Stack.HardConfirmed]] + the `PreviousStackHardConfirmation`
      * that unblocks the next stack. The raw acks have served their purpose (verified + aggregated)
      * and are dropped with the cell.
      */
    private def completeStack(
        stackNum: StackNumber,
        cell: Cell.WaitingRound2 | Cell.WaitingSole
    ): IO[Unit] = for {
        conn <- getConnections
        vkeys <- allPeers.toList.traverse(p => peerVKey(p).map(p -> _)).map(_.toMap)
        wmap = witnessMap(cell, vkeys)
        evac = evacByPartition(cell)
        signed <- attachWitnesses(cell.unsigned, wmap, evac)
        _ <- Tracer.info(
          s"stack $stackNum HARD-CONFIRMED — aggregated witnesses onto ${wmap.size} " +
              "effect tx(s); emitting downstream"
        )
        hardConfirmed = Stack.HardConfirmed(cell.unsigned, signed)
        _ <- conn.cardanoLiaison ! hardConfirmed
        _ <- conn.stackComposer ! PreviousStackHardConfirmation(stackNum)
        _ <- stateRef.update(_.dropCell(stackNum))
    } yield ()

    // ===================================
    // Witness aggregation — per-peer hard-ack sigs → VKeyWitnesses keyed by effect tx hash;
    // SEC header sigs collected per partition.
    // ===================================

    private type WitnessMap = Map[TransactionHash, Set[VKeyWitness]]

    private def witnessOf(vk: VerificationKey, sig: TxSignature): VKeyWitness =
        VKeyWitness(vk, sig)

    extension (m: WitnessMap)
        private def add(h: TransactionHash, w: VKeyWitness): WitnessMap =
            m.updated(h, m.getOrElse(h, Set.empty) + w)
        private def addAll(h: TransactionHash, ws: Iterable[VKeyWitness]): WitnessMap =
            m.updated(h, m.getOrElse(h, Set.empty) ++ ws)
        private def addOpt(
            h: TransactionHash,
            vk: VerificationKey,
            s: Option[TxSignature]
        ): WitnessMap = s.fold(m)(sg => m.add(h, witnessOf(vk, sg)))
        private def addZip(
            txs: List[Transaction],
            vk: VerificationKey,
            sigs: List[TxSignature]
        ): WitnessMap =
            txs.zip(sigs).foldLeft(m) { case (acc, (tx, sg)) =>
                acc.add(tx.id, witnessOf(vk, sg))
            }

    private def regularPartitions(
        u: Stack.Unsigned
    ): Option[NonEmptyList[PartitionEffects[StandaloneEvacuationCommitment]]] =
        u.effects match {
            case r: StackEffects.Unsigned.Regular => Some(r.partitions)
            case _: StackEffects.Unsigned.Initial => None
        }

    private def unlockTxOf(
        parts: NonEmptyList[PartitionEffects[StandaloneEvacuationCommitment]],
        u: PartitionEffects.Unlock
    ): Transaction = u match {
        case PartitionEffects.Unlock.Settlement(i) =>
            parts.toList(i) match {
                case PartitionEffects.Major(settlement, _, _, _, _) => settlement.tx
                case _ =>
                    throw new IllegalStateException(s"unlock Settlement($i) not a Major partition")
            }
        case PartitionEffects.Unlock.Finalization(i) =>
            parts.toList(i) match {
                case PartitionEffects.Final(finalization, _) => finalization.tx
                case _ =>
                    throw new IllegalStateException(
                      s"unlock Finalization($i) not a Final partition"
                    )
            }
    }

    /** Fold one partition's tx-body sigs into the witness map (SEC header sigs are handled
      * separately by [[evacByPartition]] — not tx witnesses).
      */
    private def partitionWitnesses(
        m: WitnessMap,
        vk: VerificationKey,
        e: PartitionEffects[StandaloneEvacuationCommitment],
        s: HardAck.PartitionSig
    ): WitnessMap = (e, s) match {
        case (
              PartitionEffects.Major(settlement, fallback, rollouts, refunds, _),
              HardAck.PartitionSig.Major(sSettlement, sFallback, sRollouts, sRefunds, _)
            ) =>
            m.addOpt(settlement.tx.id, vk, sSettlement)
                .add(fallback.tx.id, witnessOf(vk, sFallback))
                .addZip(rollouts.map(_.tx), vk, sRollouts)
                .addZip(refunds.map(_.tx), vk, sRefunds)
        case (
              PartitionEffects.Final(finalization, rollouts),
              HardAck.PartitionSig.Final(sFinalization, sRollouts)
            ) =>
            m.addOpt(finalization.tx.id, vk, sFinalization)
                .addZip(rollouts.map(_.tx), vk, sRollouts)
        case (PartitionEffects.Minor(_, refunds), HardAck.PartitionSig.Minor(_, sRefunds)) =>
            m.addZip(refunds.map(_.tx), vk, sRefunds)
        case _ => m // verification already rejected kind mismatches
    }

    private def witnessMap(
        cell: Cell.WaitingRound2 | Cell.WaitingSole,
        vkeys: Map[HeadPeerNumber, VerificationKey]
    ): WitnessMap =
        cell match {
            case c: Cell.WaitingRound2 =>
                allPeers.foldLeft(Map.empty: WitnessMap) { (m, peer) =>
                    val vk = vkeys(peer)
                    val afterR1 = (regularPartitions(c.unsigned), c.round1.get(peer)) match {
                        case (Some(parts), Some(r: HardAck.Round1Payload.Regular)) =>
                            parts.toList.zip(r.partitions.toList).foldLeft(m) {
                                case (acc, (e, sg)) => partitionWitnesses(acc, vk, e, sg)
                            }
                        case (None, Some(r: HardAck.Round1Payload.Initial)) =>
                            c.unsigned.effects match {
                                case i: StackEffects.Unsigned.Initial =>
                                    m.add(i.fallbackTx.tx.id, witnessOf(vk, r.fallbackSig))
                                case _ => m
                            }
                        case _ => m
                    }
                    (regularPartitions(c.unsigned), c.round2.get(peer)) match {
                        case (Some(parts), Some(r: HardAck.Round2Payload.Regular)) =>
                            PartitionEffects.unlock(parts) match {
                                case Some(u) =>
                                    afterR1.add(
                                      unlockTxOf(parts, u).id,
                                      witnessOf(vk, r.firstUnlockSig)
                                    )
                                case None => afterR1
                            }
                        case (None, Some(r: HardAck.Round2Payload.Initial)) =>
                            c.unsigned.effects match {
                                case i: StackEffects.Unsigned.Initial =>
                                    val txId = i.initializationTx.tx.id
                                    afterR1
                                        .add(txId, witnessOf(vk, r.initTxSig))
                                        .addAll(txId, r.individualWitnesses)
                                case _ => afterR1
                            }
                        case _ => afterR1
                    }
                }
            case c: Cell.WaitingSole =>
                allPeers.foldLeft(Map.empty: WitnessMap) { (m, peer) =>
                    (regularPartitions(c.unsigned), c.sole.get(peer)) match {
                        case (Some(parts), Some(p)) =>
                            parts.head match {
                                case PartitionEffects.Minor(_, refunds) =>
                                    m.addZip(refunds.map(_.tx), vkeys(peer), p.refunds)
                                case _ => m
                            }
                        case _ => m
                    }
                }
        }

    /** Per-partition SEC header signatures across all peers, in [[allPeers]] order, aligned to the
      * effects' partition list. A partition with no SEC contributes an empty list; a present SEC
      * has one header sig per peer (verification already enforced presence + count). Empty list
      * overall for an Initial stack (no partitions).
      */
    private def evacByPartition(
        cell: Cell.WaitingRound2 | Cell.WaitingSole
    ): List[List[BlockHeader.HeaderSignature]] =
        regularPartitions(cell.unsigned) match {
            case None => Nil
            case Some(parts) =>
                val peerSigParts: List[Option[NonEmptyList[HardAck.PartitionSig]]] =
                    allPeers.toList.map { peer =>
                        cell match {
                            case c: Cell.WaitingRound2 =>
                                c.round1.get(peer).collect {
                                    case r: HardAck.Round1Payload.Regular => r.partitions
                                }
                            case c: Cell.WaitingSole =>
                                c.sole
                                    .get(peer)
                                    .map(p =>
                                        NonEmptyList.one(
                                          HardAck.PartitionSig.Minor(p.sec, p.refunds)
                                        )
                                    )
                        }
                    }
                parts.toList.indices.toList.map { i =>
                    peerSigParts.flatMap {
                        case Some(sps) =>
                            sps.toList.lift(i).flatMap {
                                case HardAck.PartitionSig.Major(_, _, _, _, sec) => sec
                                case HardAck.PartitionSig.Minor(sec, _)          => Some(sec)
                                case _: HardAck.PartitionSig.Final               => None
                            }
                        case None => None
                    }
                }
        }

    private def attachWitnesses(
        unsigned: Stack.Unsigned,
        wmap: WitnessMap,
        evac: List[List[BlockHeader.HeaderSignature]]
    ): IO[StackEffects.HardConfirmed] = unsigned.effects match {
        case r: StackEffects.Unsigned.Regular =>
            r.partitions.toList
                .zip(evac)
                .traverse { (pe, secSigs) =>
                    pe match {
                        case PartitionEffects.Major(
                              settlement,
                              fallback,
                              rollouts,
                              refunds,
                              sec
                            ) =>
                            for {
                                st <- signOne(settlement, wmap)
                                fb <- signOne(fallback, wmap)
                                ro <- rollouts.traverse(signOne(_, wmap))
                                rf <- refunds.traverse { case pd: RefundTx.PostDated =>
                                    signOne(pd, wmap).widen[RefundTx]
                                }
                            } yield PartitionEffects.Major(
                              st,
                              fb,
                              ro,
                              rf,
                              sec.map(StandaloneEvacuationCommitment.MultiSigned(_, secSigs))
                            ): PartitionEffects[StandaloneEvacuationCommitment.MultiSigned]
                        case PartitionEffects.Final(finalization, rollouts) =>
                            for {
                                fi <- signOne(finalization, wmap)
                                ro <- rollouts.traverse(signOne(_, wmap))
                            } yield PartitionEffects.Final(fi, ro): PartitionEffects[
                              StandaloneEvacuationCommitment.MultiSigned
                            ]
                        case PartitionEffects.Minor(sec, refunds) =>
                            refunds
                                .traverse { case pd: RefundTx.PostDated =>
                                    signOne(pd, wmap).widen[RefundTx]
                                }
                                .map(rf =>
                                    PartitionEffects.Minor(
                                      StandaloneEvacuationCommitment.MultiSigned(sec, secSigs),
                                      rf
                                    ): PartitionEffects[
                                      StandaloneEvacuationCommitment.MultiSigned
                                    ]
                                )
                    }
                }
                .map(list => StackEffects.HardConfirmed.Regular(NonEmptyList.fromListUnsafe(list)))
        case i: StackEffects.Unsigned.Initial =>
            for {
                it <- signOne(i.initializationTx, wmap)
                fb <- signOne(i.fallbackTx, wmap)
            } yield StackEffects.HardConfirmed.Initial(
              initializationTx = it,
              fallbackTx = fb
            )
    }

    private def signOne[A <: Tx[A]](a: A, wmap: WitnessMap): IO[A] =
        wmap.get(a.tx.id).filter(_.nonEmpty) match {
            case None => IO.pure(a)
            case Some(ws) =>
                a.addSignatures(ws) match {
                    case Valid(signed) => IO.pure(signed)
                    case Invalid(e)    => IO.raiseError(e.head)
                }
        }

    // ===================================
    // Verification (against the locally-derived effects + the signer's vkey)
    // ===================================

    private def peerVKey(peer: HeadPeerNumber): IO[VerificationKey] =
        config.headPeerVKey(peer).liftTo[IO](CellError.UnknownPeer(peer))

    private def verifyTx(vk: VerificationKey, t: Transaction, sig: TxSignature): IO[Unit] =
        IO.delay(platform.verifyEd25519Signature(vk, t.id, sig))
            .handleErrorWith {
                case NonFatal(_) => IO.pure(false)
                case e           => IO.raiseError(e)
            }
            .flatMap(ok => IO.raiseUnless(ok)(CellError.BadSignature(s"tx ${t.id}")))

    private def verifyHeader(
        vk: VerificationKey,
        msg: BlockHeader.Minor.Onchain.Serialized,
        sig: BlockHeader.HeaderSignature
    ): IO[Unit] =
        IO.delay(platform.verifyEd25519Signature(vk, msg, sig))
            .handleErrorWith {
                case NonFatal(_) => IO.pure(false)
                case e           => IO.raiseError(e)
            }
            .flatMap(ok => IO.raiseUnless(ok)(CellError.BadSignature("evac-commit header")))

    private def verifyTxList(
        vk: VerificationKey,
        label: String,
        txs: List[Transaction],
        sigs: List[TxSignature]
    ): IO[Unit] =
        IO.raiseUnless(txs.length == sigs.length)(
          CellError.KeysetMismatch(label, txs.length.toString, sigs.length.toString)
        ) >> txs.zip(sigs).traverse_ { case (tx, sg) => verifyTx(vk, tx, sg) }

    private def verifyOptUnlock(
        vk: VerificationKey,
        label: String,
        tx: Transaction,
        isUnlock: Boolean,
        sOpt: Option[TxSignature]
    ): IO[Unit] =
        if isUnlock then
            IO.raiseUnless(sOpt.isEmpty)(
              CellError.KeysetMismatch(
                s"$label (unlock ⇒ absent in round-1)",
                "absent",
                "present"
              )
            )
        else
            sOpt match {
                case Some(sg) => verifyTx(vk, tx, sg)
                case None =>
                    IO.raiseError(
                      CellError.KeysetMismatch(
                        s"$label (non-unlock ⇒ present)",
                        "present",
                        "absent"
                      )
                    )
            }

    private def verifySecOpt(
        vk: VerificationKey,
        eSec: Option[StandaloneEvacuationCommitment],
        sSec: Option[BlockHeader.HeaderSignature]
    ): IO[Unit] = (eSec, sSec) match {
        case (None, None)            => IO.unit
        case (Some(sec), Some(hsig)) => verifyHeader(vk, sec.header, hsig)
        case (e, s) =>
            IO.raiseError(
              CellError.KeysetMismatch(
                "partition.sec presence",
                e.isDefined.toString,
                s.isDefined.toString
              )
            )
    }

    private def verifyPartition(
        vk: VerificationKey,
        e: PartitionEffects[StandaloneEvacuationCommitment],
        s: HardAck.PartitionSig,
        isSettlementUnlock: Boolean,
        isFinalizationUnlock: Boolean
    ): IO[Unit] = (e, s) match {
        case (
              PartitionEffects.Major(settlement, fallback, rollouts, refunds, sec),
              HardAck.PartitionSig.Major(sSettlement, sFallback, sRollouts, sRefunds, sSec)
            ) =>
            for {
                _ <- verifyOptUnlock(
                  vk,
                  "partition.settlement",
                  settlement.tx,
                  isSettlementUnlock,
                  sSettlement
                )
                _ <- verifyTx(vk, fallback.tx, sFallback)
                _ <- verifyTxList(vk, "partition.rollouts", rollouts.map(_.tx), sRollouts)
                _ <- verifyTxList(vk, "partition.refunds", refunds.map(_.tx), sRefunds)
                _ <- verifySecOpt(vk, sec, sSec)
            } yield ()
        case (
              PartitionEffects.Final(finalization, rollouts),
              HardAck.PartitionSig.Final(sFinalization, sRollouts)
            ) =>
            for {
                _ <- verifyOptUnlock(
                  vk,
                  "partition.finalization",
                  finalization.tx,
                  isFinalizationUnlock,
                  sFinalization
                )
                _ <- verifyTxList(vk, "partition.rollouts", rollouts.map(_.tx), sRollouts)
            } yield ()
        case (PartitionEffects.Minor(sec, refunds), HardAck.PartitionSig.Minor(sSec, sRefunds)) =>
            verifyHeader(vk, sec.header, sSec) >>
                verifyTxList(vk, "partition.refunds", refunds.map(_.tx), sRefunds)
        case _ =>
            IO.raiseError(
              CellError.KeysetMismatch(
                "partition kind",
                e.getClass.getSimpleName,
                s.getClass.getSimpleName
              )
            )
    }

    private def verifyRound1Regular(
        vk: VerificationKey,
        r: StackEffects.Unsigned.Regular,
        p: HardAck.Round1Payload.Regular
    ): IO[Unit] = {
        val unlock = PartitionEffects.unlock(r.partitions)
        val es = r.partitions.toList
        val ss = p.partitions.toList
        IO.raiseUnless(es.length == ss.length)(
          CellError.KeysetMismatch("round1.partitions", es.length.toString, ss.length.toString)
        ) >> es.zip(ss).zipWithIndex.traverse_ { case ((e, sg), i) =>
            verifyPartition(
              vk,
              e,
              sg,
              isSettlementUnlock = unlock.contains(PartitionEffects.Unlock.Settlement(i)),
              isFinalizationUnlock = unlock.contains(PartitionEffects.Unlock.Finalization(i))
            )
        }
    }

    /** Initial stack round 2. `initTxSig` is this peer's head-multisig contribution over the init
      * tx body (always required). `individualWitnesses` must satisfy the iff / no-extra-witness
      * rule via the SAME shared deterministic predicate the signer used
      * ([[StackEffects.spendsFromIndividualAddress]]).
      */
    private def verifyRound2Initial(
        i: StackEffects.Unsigned.Initial,
        peer: HeadPeerNumber,
        p: HardAck.Round2Payload.Initial
    ): IO[Unit] = for {
        vk <- peerVKey(peer)
        initTx = i.initializationTx
        _ <- verifyTx(vk, initTx.tx, p.initTxSig)
        expected = StackEffects.spendsFromIndividualAddress(initTx, vk)
        _ <- (expected, p.individualWitnesses) match {
            case (false, Nil) => IO.unit
            case (false, extra) =>
                IO.raiseError(
                  CellError.KeysetMismatch(
                    "round2Initial.individualWitnesses (peer funds no init input)",
                    "0",
                    extra.size.toString
                  )
                )
            case (true, Nil) =>
                IO.raiseError(
                  CellError.KeysetMismatch(
                    "round2Initial.individualWitnesses (peer funds an init input)",
                    ">=1",
                    "0"
                  )
                )
            case (true, ws) =>
                ws.traverse_(w =>
                    IO.delay(
                      platform.verifyEd25519Signature(w.vkey, initTx.tx.id, w.signature)
                    ).handleErrorWith {
                        case NonFatal(_) => IO.pure(false)
                        case e           => IO.raiseError(e)
                    }.flatMap(ok =>
                        IO.raiseUnless(ok)(
                          CellError.BadSignature("round2Initial individual witness")
                        )
                    )
                )
        }
    } yield ()

    private def verify2PhaseRound1(
        unsigned: Stack.Unsigned,
        peer: HeadPeerNumber,
        p: HardAck.Payload.Round1
    ): IO[Unit] = (unsigned.effects, p) match {
        case (r: StackEffects.Unsigned.Regular, pr: HardAck.Round1Payload.Regular) =>
            peerVKey(peer).flatMap(vk => verifyRound1Regular(vk, r, pr))
        case (i: StackEffects.Unsigned.Initial, pi: HardAck.Round1Payload.Initial) =>
            peerVKey(peer).flatMap(vk => verifyTx(vk, i.fallbackTx.tx, pi.fallbackSig))
        case _ =>
            IO.raiseError(CellError.PlanPayloadMismatch("round-1", p.roundLabel))
    }

    private def verify2PhaseRound2(
        unsigned: Stack.Unsigned,
        peer: HeadPeerNumber,
        p: HardAck.Payload.Round2
    ): IO[Unit] = (unsigned.effects, p) match {
        case (r: StackEffects.Unsigned.Regular, pr: HardAck.Round2Payload.Regular) =>
            peerVKey(peer).flatMap { vk =>
                PartitionEffects.unlock(r.partitions) match {
                    case Some(u) => verifyTx(vk, unlockTxOf(r.partitions, u), pr.firstUnlockSig)
                    case None =>
                        IO.raiseError(
                          CellError.KeysetMismatch("round-2 unlock", "present", "absent")
                        )
                }
            }
        case (i: StackEffects.Unsigned.Initial, pi: HardAck.Round2Payload.Initial) =>
            verifyRound2Initial(i, peer, pi)
        case _ =>
            IO.raiseError(CellError.PlanPayloadMismatch("round-2", p.roundLabel))
    }

    private def verifySole(
        unsigned: Stack.Unsigned,
        peer: HeadPeerNumber,
        p: HardAck.SolePayload
    ): IO[Unit] = unsigned.effects match {
        case r: StackEffects.Unsigned.Regular =>
            r.partitions.head match {
                case PartitionEffects.Minor(sec, refunds) if r.partitions.tail.isEmpty =>
                    peerVKey(peer).flatMap(vk =>
                        verifyHeader(vk, sec.header, p.sec) >>
                            verifyTxList(vk, "sole.refunds", refunds.map(_.tx), p.refunds)
                    )
                case _ =>
                    IO.raiseError(
                      CellError.KeysetMismatch("sole shape", "single Minor partition", "other")
                    )
            }
        case _: StackEffects.Unsigned.Initial =>
            IO.raiseError(CellError.PlanPayloadMismatch("sole", p.roundLabel))
    }

    // ===================================
    // Plumbing
    // ===================================

    private def broadcast(ack: HardAck): IO[Unit] =
        getConnections.flatMap(conn => (conn.peerLiaisons ! ack).parallel)

    private def putCell(stackNum: StackNumber, cell: Cell): IO[Unit] =
        stateRef.update(s => s.copy(cells = s.cells.updated(stackNum, cell)))

    private def withCell(stackNum: StackNumber)(f: Cell => IO[Cell]): IO[Unit] = for {
        s <- stateRef.get
        cell <- s.cells.get(stackNum).liftTo[IO](CellError.NoCell(stackNum))
        updated <- f(cell)
        _ <- putCell(stackNum, updated)
    } yield ()

    private def replayOrphans(stackNum: StackNumber): IO[Unit] = for {
        s <- stateRef.get
        orphans = s.orphanAcks.getOrElse(stackNum, Nil)
        _ <- IO.whenA(orphans.nonEmpty)(
          Tracer.debug(s"replaying ${orphans.size} orphan ack(s) for stack $stackNum")
        )
        _ <- stateRef.update(_.clearOrphans(stackNum))
        _ <- orphans.traverse_(applyRemote)
    } yield ()

    private def getConnections: IO[Connections] = for {
        mConn <- connections.get
        conn <- mConn.liftTo[IO](
          java.lang.Error("SlowConsensusActor is missing its connections to other actors.")
        )
    } yield conn

    private def initializeConnections: IO[Unit] = pendingConnections match {
        case x: MultisigRegimeManager.PendingConnections =>
            for {
                c <- x.get
                _ <- connections.set(
                  Some(
                    Connections(
                      stackComposer = c.stackComposer,
                      cardanoLiaison = c.cardanoLiaison,
                      peerLiaisons = c.peerLiaisons
                    )
                  )
                )
            } yield ()
        case x: SlowConsensusActor.Connections => connections.set(Some(x))
    }
}

object SlowConsensusActor {

    type Handle = ActorRef[IO, Request]

    type Config = HeadConfig.Section & OwnHeadPeerPrivate.Section

    final case class Connections(
        stackComposer: StackComposer.Handle,
        cardanoLiaison: CardanoLiaison.Handle,
        peerLiaisons: List[PeerLiaison.Handle]
    )

    type Request = PreStart.type | StackHandoff | HardAck

    case object PreStart

    /** Bundle sent by [[StackComposer]] when it closes a stack: the unsigned stack plus this peer's
      * pre-signed hard-acks for every round the stack needs (2-phase ⇒ round1 + round2; sole ⇒
      * one). The SlowConsensusActor schedules their outbound broadcast (round-1 / sole immediately;
      * round-2 withheld until local round-1 confirmation).
      */
    final case class StackHandoff(unsigned: Stack.Unsigned, ownAcks: List[HardAck])

    /** Per-stack aggregation cell. The 2-phase cells store the round-payload *supertypes*
      * (`HardAck.Payload.Round1`/`Round2`) so Regular and Initial share one state machine;
      * verification dispatches on `(unsigned.effects, payload)`.
      */
    sealed trait Cell { def unsigned: Stack.Unsigned }
    object Cell {
        final case class WaitingRound1(
            unsigned: Stack.Unsigned,
            round1: Map[HeadPeerNumber, HardAck.Payload.Round1],
            ownRound2: HardAck,
            round2Stash: Map[HeadPeerNumber, HardAck.Payload.Round2]
        ) extends Cell

        final case class WaitingRound2(
            unsigned: Stack.Unsigned,
            // Carried through from round 1: needed at hard-confirmation to aggregate the
            // non-unlock per-effect signatures into witnesses (round 2 only signs the unlock).
            round1: Map[HeadPeerNumber, HardAck.Payload.Round1],
            round2: Map[HeadPeerNumber, HardAck.Payload.Round2]
        ) extends Cell

        final case class WaitingSole(
            unsigned: Stack.Unsigned,
            sole: Map[HeadPeerNumber, HardAck.SolePayload]
        ) extends Cell
    }

    final case class State(
        cells: Map[StackNumber, Cell],
        orphanAcks: Map[StackNumber, List[HardAck]]
    ) {
        def bufferOrphan(h: HardAck): State =
            copy(orphanAcks =
                orphanAcks.updated(h.stackNum, orphanAcks.getOrElse(h.stackNum, Nil) :+ h)
            )
        def clearOrphans(stackNum: StackNumber): State =
            copy(orphanAcks = orphanAcks - stackNum)
        def dropCell(stackNum: StackNumber): State = copy(cells = cells - stackNum)
    }
    object State {
        def initial: State = State(Map.empty, Map.empty)
    }

    enum HandoffError extends Throwable:
        case MissingOwnAck(what: String)
        override def getMessage: String = this match
            case MissingOwnAck(w) => s"StackHandoff missing $w ack"

    enum CellError extends Throwable:
        case NoCell(stackNum: StackNumber)
        case UnknownPeer(peer: HeadPeerNumber)
        case UnexpectedPayload(stackNum: StackNumber, roundLabel: String)
        case PlanPayloadMismatch(round: String, payloadRound: String)
        case BadSignature(what: String)
        case KeysetMismatch(label: String, expected: String, actual: String)
        override def getMessage: String = this match
            case NoCell(s)      => s"No cell for stack $s"
            case UnknownPeer(p) => s"Unknown head peer $p"
            case UnexpectedPayload(s, r) =>
                s"Unexpected $r payload for stack $s in current phase"
            case PlanPayloadMismatch(r, pr) =>
                s"Effects/payload variant mismatch at $r: payload round=$pr"
            case BadSignature(w) => s"Hard-ack signature verification failed for $w"
            case KeysetMismatch(l, e, a) =>
                s"Hard-ack keyset mismatch for $l: expected $e, got $a"
}
