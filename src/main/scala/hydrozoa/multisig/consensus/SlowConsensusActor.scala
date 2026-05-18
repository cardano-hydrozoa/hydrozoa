package hydrozoa.multisig.consensus

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
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckSigningPlan}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.{BlockHeader, BlockNumber}
import hydrozoa.multisig.ledger.l1.tx.TxSignature
import hydrozoa.multisig.ledger.stack.{Stack, StackNumber}
import scala.util.control.NonFatal
import scalus.cardano.ledger.Transaction
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.platform

/** Slow-consensus actor (M6).
  *
  * Aggregates per-effect hard-acks from all head peers for each closed stack, verifies every
  * signature against the locally-derived effect bodies (the same [[HardAckSigningPlan]] the
  * StackComposer signed against — determinism makes the sigs comparable), and on saturation emits
  * `Stack.HardConfirmed` to CardanoLiaison + `PreviousStackHardConfirmation` to StackComposer.
  *
  * It owns no wallet: StackComposer signs all of this peer's hard-acks upfront and bundles them
  * into a [[SlowConsensusActor.StackHandoff]]. This actor manages only the outbound *schedule* of
  * those own acks — round-1 / sole broadcast immediately, round-2 withheld until local round-1
  * confirmation (mirrors the fast-side ConsensusActor's "scheduled own ack" pattern).
  *
  * ==Phases==
  *
  *   - 2-phase (a settlement / finalization is present, or the initial stack): round 1 collects
  *     every effect sig except the unlock; on round-1 saturation the held own round-2 ack is
  *     released and round 2 collects the unlock sig.
  *   - 1-phase / sole (minor-only stack): one round over the refund sigs + the trailing-minor evac
  *     commitment header sig.
  *
  * Saturation = a verified ack from every head peer (the local peer's own included). When coil
  * peers join (future PR) this additionally requires a coil quorum.
  *
  * ==Orphan acks==
  *
  * A remote peer's ack for stack N can arrive before this peer's StackComposer hands stack N off
  * (it closes N only after N−1 is hard-confirmed locally, and peers run at different paces). Such
  * acks are buffered per stackNum and replayed when the cell is created. Verification still happens
  * at replay time against the locally-derived plan.
  *
  * Deterministic-derivation invariant: every head peer derives byte-identical effects from the same
  * `Stack.Unsigned`. A signature that fails to verify means the signer derived different effects —
  * a critical consensus break, not a recoverable condition: the cell raises, the actor halts, and
  * the rule-based fallback / dead-man's switch takes over (see the slow-consensus plan's
  * "effect-derivation divergence" note).
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

    private def handleStackHandoff(h: StackHandoff): IO[Unit] =
        Tracer.scopedCtx("stackNum" -> h.unsigned.brief.stackNum.toString) {
            val stackNum = h.unsigned.brief.stackNum
            val ownPeer = config.ownHeadPeerNum
            HardAckSigningPlan.from(h.unsigned) match {
                case plan: HardAckSigningPlan.TwoPhase =>
                    for {
                        ownR1 <- h.ownAcks
                            .collectFirst {
                                case a @ HardAck(_, _, p: HardAck.Round1Payload.Regular) => (a, p)
                            }
                            .liftTo[IO](HandoffError.MissingOwnAck("own round-1 (Regular)"))
                        ownR2 <- h.ownAcks
                            .collectFirst {
                                case a @ HardAck(_, _, p: HardAck.Round2Payload.Regular) => (a, p)
                            }
                            .liftTo[IO](HandoffError.MissingOwnAck("own round-2 (Regular)"))
                        _ <- verifyRound1(plan, ownPeer, ownR1._2)
                        _ <- verifyRound2(plan, ownPeer, ownR2._2)
                        cell = Cell.WaitingRound1(
                          unsigned = h.unsigned,
                          plan = plan,
                          round1 = Map(ownPeer -> ownR1._2),
                          ownRound2 = ownR2._1,
                          round2Stash = Map.empty
                        )
                        _ <- putCell(stackNum, cell)
                        _ <- Tracer.info(
                          s"stack $stackNum handed off (2-phase); broadcasting own round-1"
                        )
                        _ <- broadcast(ownR1._1)
                        _ <- replayOrphans(stackNum)
                        _ <- tryAdvance(stackNum)
                    } yield ()
                case plan: HardAckSigningPlan.Sole =>
                    for {
                        ownSole <- h.ownAcks
                            .collectFirst { case a @ HardAck(_, _, p: HardAck.SolePayload) =>
                                (a, p)
                            }
                            .liftTo[IO](HandoffError.MissingOwnAck("own sole"))
                        _ <- verifySole(plan, ownPeer, ownSole._2)
                        cell = Cell.WaitingSole(
                          unsigned = h.unsigned,
                          plan = plan,
                          sole = Map(ownPeer -> ownSole._2)
                        )
                        _ <- putCell(stackNum, cell)
                        _ <- Tracer.info(
                          s"stack $stackNum handed off (sole); broadcasting own sole ack"
                        )
                        _ <- broadcast(ownSole._1)
                        _ <- replayOrphans(stackNum)
                        _ <- tryAdvance(stackNum)
                    } yield ()
            }
        }

    // ===================================
    // Remote acks
    // ===================================

    private def handleRemoteHardAck(h: HardAck): IO[Unit] =
        Tracer.scopedCtx(h.toContext*) {
            stateRef.get.flatMap { s =>
                s.cells.get(h.stackNum) match {
                    case None =>
                        // Cell not created yet — buffer and replay on handoff.
                        Tracer.debug(
                          s"orphan hard-ack for stack ${h.stackNum} from peer ${h.peerNum} " +
                              "(no cell yet); buffering"
                        ) >> stateRef.update(_.bufferOrphan(h))
                    case Some(_) =>
                        applyRemote(h) >> tryAdvance(h.stackNum)
                }
            }
        }

    /** Apply one remote ack into its (existing) cell, verifying it against the plan. */
    private def applyRemote(h: HardAck): IO[Unit] = withCell(h.stackNum) { cell =>
        val peer = h.peerNum
        cell match {
            case c: Cell.WaitingRound1 =>
                h.payload match {
                    case p: HardAck.Round1Payload.Regular =>
                        verifyRound1(c.plan, peer, p).as(
                          c.copy(round1 = c.round1.updated(peer, p))
                        )
                    case p: HardAck.Round2Payload.Regular =>
                        // Early round-2 (peer reached round-1 confirmation before us).
                        // plan.round2 is known at cell creation, so verify now and stash.
                        verifyRound2(c.plan, peer, p).as(
                          c.copy(round2Stash = c.round2Stash.updated(peer, p))
                        )
                    case other =>
                        IO.raiseError(CellError.UnexpectedPayload(h.stackNum, other.roundLabel))
                }
            case c: Cell.WaitingRound2 =>
                h.payload match {
                    case p: HardAck.Round2Payload.Regular =>
                        verifyRound2(c.plan, peer, p).as(
                          c.copy(round2 = c.round2.updated(peer, p))
                        )
                    case _: HardAck.Round1Payload.Regular =>
                        // Round-1 already saturated; a duplicate / late round-1 is benign.
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
                        verifySole(c.plan, peer, p).as(c.copy(sole = c.sole.updated(peer, p)))
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
                completeStack(stackNum, c.unsigned)
            case Some(c: Cell.WaitingSole) if c.sole.keySet == allPeers =>
                completeStack(stackNum, c.unsigned)
            case _ => IO.unit
        }
    }

    /** Round-1 saturated: release the held own round-2 ack, fold in any early-stashed remote
      * round-2 acks, transition to WaitingRound2, then re-check (the stash may already complete
      * it).
      */
    private def completeRound1(stackNum: StackNumber, c: Cell.WaitingRound1): IO[Unit] = for {
        _ <- Tracer.info(
          s"stack $stackNum round-1 confirmed; releasing own round-2 ack"
        )
        ownPeer = config.ownHeadPeerNum
        ownR2Payload <- c.ownRound2.payload match {
            case p: HardAck.Round2Payload.Regular => IO.pure(p)
            case other =>
                IO.raiseError(CellError.UnexpectedPayload(stackNum, other.roundLabel))
        }
        round2 = c.round2Stash.updated(ownPeer, ownR2Payload)
        _ <- putCell(
          stackNum,
          Cell.WaitingRound2(unsigned = c.unsigned, plan = c.plan, round2 = round2)
        )
        _ <- broadcast(c.ownRound2)
        _ <- tryAdvance(stackNum)
    } yield ()

    private def completeStack(stackNum: StackNumber, unsigned: Stack.Unsigned): IO[Unit] = for {
        conn <- getConnections
        _ <- Tracer.info(s"stack $stackNum HARD-CONFIRMED — emitting downstream")
        hardConfirmed = Stack.HardConfirmed(Stack.Round1Confirmed(unsigned))
        _ <- conn.cardanoLiaison ! hardConfirmed
        _ <- conn.stackComposer ! PreviousStackHardConfirmation(stackNum)
        _ <- stateRef.update(_.dropCell(stackNum))
    } yield ()

    // ===================================
    // Verification (against the locally-derived plan + the signer's vkey)
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

    /** Require the signed keyset to exactly match the locally-derived keyset, then verify each
      * signature against its corresponding tx body.
      */
    private def verifyTxMap[K](
        vk: VerificationKey,
        expected: Map[K, Transaction],
        signed: Map[K, TxSignature],
        label: String
    ): IO[Unit] = for {
        _ <- IO.raiseUnless(signed.keySet == expected.keySet)(
          CellError.KeysetMismatch(label, expected.keySet.toString, signed.keySet.toString)
        )
        _ <- expected.toList.traverse_ { case (k, tx) => verifyTx(vk, tx, signed(k)) }
    } yield ()

    private def verifyEvac(
        vk: VerificationKey,
        expected: Option[(BlockNumber, BlockHeader.Minor.Onchain.Serialized)],
        signed: Option[(BlockNumber, BlockHeader.HeaderSignature)]
    ): IO[Unit] = (expected, signed) match {
        case (None, None) => IO.unit
        case (Some((eBn, msg)), Some((sBn, sig))) =>
            IO.raiseUnless(eBn == sBn)(
              CellError.KeysetMismatch("evacCommit blockNum", eBn.toString, sBn.toString)
            ) >> verifyHeader(vk, msg, sig)
        case _ =>
            IO.raiseError(
              CellError.KeysetMismatch(
                "evacCommit presence",
                expected.isDefined.toString,
                signed.isDefined.toString
              )
            )
    }

    private def verifyRound1(
        plan: HardAckSigningPlan.TwoPhase,
        peer: HeadPeerNumber,
        p: HardAck.Round1Payload.Regular
    ): IO[Unit] = for {
        vk <- peerVKey(peer)
        in = plan.round1
        _ <- verifyTxMap(vk, in.settlements, p.settlements, "round1.settlements")
        _ <- verifyTxMap(vk, in.fallbacks, p.fallbacks, "round1.fallbacks")
        _ <- verifyTxMap(vk, in.rollouts, p.rollouts, "round1.rollouts")
        _ <- verifyTxMap(vk, in.refunds, p.refunds, "round1.refunds")
        _ <- verifyEvac(vk, in.evacCommit, p.evacCommit)
        _ <- (in.finalization, p.finalization) match {
            case (None, None)          => IO.unit
            case (Some(tx), Some(sig)) => verifyTx(vk, tx, sig)
            case (e, s) =>
                IO.raiseError(
                  CellError.KeysetMismatch(
                    "round1.finalization presence",
                    e.isDefined.toString,
                    s.isDefined.toString
                  )
                )
        }
    } yield ()

    private def verifyRound2(
        plan: HardAckSigningPlan.TwoPhase,
        peer: HeadPeerNumber,
        p: HardAck.Round2Payload.Regular
    ): IO[Unit] =
        peerVKey(peer).flatMap(vk => verifyTx(vk, plan.round2.unlock, p.firstUnlockSig))

    private def verifySole(
        plan: HardAckSigningPlan.Sole,
        peer: HeadPeerNumber,
        p: HardAck.SolePayload
    ): IO[Unit] = for {
        vk <- peerVKey(peer)
        _ <- verifyTxMap(vk, plan.sole.refunds, p.refunds, "sole.refunds")
        _ <- verifyEvac(vk, Some(plan.sole.evacCommit), Some(p.evacCommit))
    } yield ()

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

    /** Per-stack aggregation cell. */
    sealed trait Cell { def unsigned: Stack.Unsigned }
    object Cell {
        final case class WaitingRound1(
            unsigned: Stack.Unsigned,
            plan: HardAckSigningPlan.TwoPhase,
            round1: Map[HeadPeerNumber, HardAck.Round1Payload.Regular],
            ownRound2: HardAck,
            round2Stash: Map[HeadPeerNumber, HardAck.Round2Payload.Regular]
        ) extends Cell

        final case class WaitingRound2(
            unsigned: Stack.Unsigned,
            plan: HardAckSigningPlan.TwoPhase,
            round2: Map[HeadPeerNumber, HardAck.Round2Payload.Regular]
        ) extends Cell

        final case class WaitingSole(
            unsigned: Stack.Unsigned,
            plan: HardAckSigningPlan.Sole,
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
        case BadSignature(what: String)
        case KeysetMismatch(label: String, expected: String, actual: String)
        override def getMessage: String = this match
            case NoCell(s)      => s"No cell for stack $s"
            case UnknownPeer(p) => s"Unknown head peer $p"
            case UnexpectedPayload(s, r) =>
                s"Unexpected $r payload for stack $s in current phase"
            case BadSignature(w) => s"Hard-ack signature verification failed for $w"
            case KeysetMismatch(l, e, a) =>
                s"Hard-ack keyset mismatch for $l: expected $e, got $a"
}
