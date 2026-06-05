package hydrozoa.multisig.consensus

import cats.effect.{IO, IOLocal, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastOps
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.node.owninfo.OwnPeerPublic
import hydrozoa.lib.logging.Tracer
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.ack.HardAck
import hydrozoa.multisig.consensus.peer.PeerId
import hydrozoa.multisig.ledger.stack.{PartitionEffects, Stack, StackEffects, StackNumber}

/** Slow-consensus actor.
  *
  * Aggregates per-effect hard-acks from all head peers for each closed stack, verifies every
  * signature against the locally-derived, **partition-indexed** effect bodies
  * ([[Stack.Unsigned.effects]] — the canonical structure both StackComposer signs and this actor
  * verifies against; determinism makes the sigs comparable), and on saturation emits
  * `Stack.HardConfirmed` to CardanoLiaison + StackComposer (the latter signals the next stack may
  * close; see [[hydrozoa.multisig.consensus.limiter.Limiter]] for rate-limiting).
  *
  * It owns no wallet: StackComposer signs all of this peer's hard-acks upfront and bundles them
  * into a [[SlowConsensusActor.StackHandoff]] as two explicit acks (round-1 + round-2) or one sole
  * ack. This actor never splits anything — own acks arrive pre-split; it manages only the outbound
  * *schedule* (round-1 / sole broadcast immediately, round-2 withheld until local round-1
  * confirmation — mirrors the fast-side FastConsensusActor's "scheduled own ack" pattern).
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
  * peers join, this additionally requires a coil quorum.
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

    private lazy val allHeadPeers: Set[PeerId] =
        config.headPeerNums.toList.map(n => PeerId.Head(n)).toSet

    private def coilQuorum: Int = config.coilQuorum

    private def coilCount(peers: Set[PeerId]): Int =
        peers.count {
            case _: PeerId.Coil => true
            case _: PeerId.Head => false
        }

    /** A round is saturated once every head peer plus at least `coilQuorum` coil peers have
      * contributed.
      */
    private def isSaturated(present: Set[PeerId]): Boolean =
        allHeadPeers.subsetOf(present) && coilCount(present) >= coilQuorum

    private val ackVerifier = HardAckSignatureVerifier(config)
    private val ackAggregator = HardAckAggregator()

    override def preStart: IO[Unit] = for {
        _ <- context.self ! PreStart
        _ <- context.become(receive)
    } yield ()

    override def receive: Receive[IO, Request] = PartialFunction.fromFunction {
        case PreStart =>
            for {
                _ <- Tracer.routeLocal(s"SlowConsensusActor.${config.ownPeerLabel}")
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

    /** Dispatch on the (locally-derived) effects shape:
      *   - `Initial` ⇒ 2-phase (init flow).
      *   - `Regular` with an unlock ([[PartitionEffects.unlock]] `Some`) ⇒ 2-phase.
      *   - `Regular` with no unlock (all-`Minor`) ⇒ sole / 1-phase.
      */
    private def handleStackHandoff(h: StackHandoff): IO[Unit] =
        Tracer.scopedCtx("stackNum" -> h.unsigned.stackNum.toString) {
            val stackNum = h.unsigned.stackNum
            val ownPeer: PeerId = config.ownPeerId

            h.unsigned.effects match {
                case _: StackEffects.Unsigned.Initial =>
                    parseOwn2PhaseAcks(h, "own round-1 (Initial)", "own round-2 (Initial)")
                        .flatMap { (ownR1, ownR2) =>
                            start2Phase(stackNum, ownPeer, h.unsigned, ownR1, ownR2)
                        }
                case r: StackEffects.Unsigned.Regular =>
                    PartitionEffects.unlock(r.partitions) match {
                        case Some(_) =>
                            parseOwn2PhaseAcks(h, "own round-1 (Regular)", "own round-2 (Regular)")
                                .flatMap { (ownR1, ownR2) =>
                                    start2Phase(stackNum, ownPeer, h.unsigned, ownR1, ownR2)
                                }
                        case None =>
                            parseOwnSoleAck(h).flatMap { ownSole =>
                                startSolePhase(stackNum, ownPeer, h.unsigned, ownSole)
                            }
                    }
            }
        }

    /** Extract this peer's own round-1 + round-2 acks from the handoff (2-phase: Regular or
      * Initial), or fail with [[HandoffError.MissingOwnAck]].
      */
    private def parseOwn2PhaseAcks(
        h: StackHandoff,
        r1Label: String,
        r2Label: String
    ): IO[((HardAck, HardAck.Payload.Round1), HardAck)] = for {
        ownR1 <- h.ownAcks
            .collectFirst { case a @ HardAck(_, _, p: HardAck.Payload.Round1) => (a, p) }
            .liftTo[IO](HandoffError.MissingOwnAck(r1Label))
        ownR2 <- h.ownAcks
            .collectFirst { case a @ HardAck(_, _, _: HardAck.Payload.Round2) => a }
            .liftTo[IO](HandoffError.MissingOwnAck(r2Label))
    } yield (ownR1, ownR2)

    /** Extract this peer's own sole ack from the handoff (1-phase / minor-only stack), or fail with
      * [[HandoffError.MissingOwnAck]].
      */
    private def parseOwnSoleAck(h: StackHandoff): IO[(HardAck, HardAck.SolePayload)] =
        h.ownAcks
            .collectFirst { case a @ HardAck(_, _, p: HardAck.SolePayload) => (a, p) }
            .liftTo[IO](HandoffError.MissingOwnAck("own sole"))

    /** Shared 2-phase cell creation (Regular + Initial): verify own acks against the locally
      * derived effects, create WaitingRound1, broadcast own round-1, replay orphans, advance.
      */
    private def start2Phase(
        stackNum: StackNumber,
        ownPeer: PeerId,
        unsigned: Stack.Unsigned,
        ownR1: (HardAck, HardAck.Payload.Round1),
        ownR2: HardAck
    ): IO[Unit] = for {
        _ <- ackVerifier.verify2PhaseRound1(unsigned, ownPeer, ownR1._2)
        ownR2p <- round2PayloadOf(ownR2)
        _ <- ackVerifier.verify2PhaseRound2(unsigned, ownPeer, ownR2p)
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

    /** Sole (1-phase) counterpart of [[start2Phase]]: verify the own sole ack against the locally
      * derived effects, create WaitingSole, broadcast the own sole ack, replay orphans, advance.
      */
    private def startSolePhase(
        stackNum: StackNumber,
        ownPeer: PeerId,
        unsigned: Stack.Unsigned,
        ownSole: (HardAck, HardAck.SolePayload)
    ): IO[Unit] = for {
        _ <- ackVerifier.verifySole(unsigned, ownPeer, ownSole._2)
        _ <- putCell(
          stackNum,
          Cell.WaitingSole(unsigned = unsigned, sole = Map(ownPeer -> ownSole._2))
        )
        _ <- Tracer.info(s"stack $stackNum handed off (sole); broadcasting own sole ack")
        _ <- broadcast(ownSole._1)
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
            if h.peerId == config.ownPeerId then
                // Our own hard-ack echoed back on the `HubHardAckLane`: a hub re-publishes a coil's
                // acks to every coil it serves, the author included (filtering would punch gaps in
                // the contiguous lane). We already hold our own ack locally, so drop the echo —
                // re-applying it would, once the cell has advanced past the echoed round, hit the
                // "wrong round" guard in `applyRemote`.
                Tracer.debug(s"ignoring echo of own hard-ack for stack ${h.stackNum}")
            else
                for {
                    conn <- getConnections
                    // A hub relays every received hard-ack (other heads' + its coils') to its
                    // coil-link relay, so its coils get the whole hard-ack stream and aggregate
                    // hard-confirmation themselves. No-op off a hub.
                    _ <- conn.coilLinkRelay.traverse_(_ ! h)
                    s <- stateRef.get
                    _ <- s.cells.get(h.stackNum) match {
                        case None =>
                            Tracer.debug(
                              s"orphan hard-ack for stack ${h.stackNum} from peer ${h.peerId} " +
                                  "(no cell yet); buffering"
                            ) >> stateRef.update(_.bufferOrphan(h))
                        case Some(_) =>
                            applyRemote(h) >> tryAdvance(h.stackNum)
                    }
                } yield ()
        }

    /** Apply one remote ack into its (existing) cell, verifying it against the local effects. */
    private def applyRemote(h: HardAck): IO[Unit] = withCell(h.stackNum) { cell =>
        val peer = h.peerId
        cell match {
            case c: Cell.WaitingRound1 =>
                h.payload match {
                    case p: HardAck.Payload.Round1 =>
                        ackVerifier
                            .verify2PhaseRound1(c.unsigned, peer, p)
                            .as(
                              c.copy(round1 = c.round1.updated(peer, p))
                            )
                    case p: HardAck.Payload.Round2 =>
                        ackVerifier
                            .verify2PhaseRound2(c.unsigned, peer, p)
                            .as(
                              c.copy(round2Stash = c.round2Stash.updated(peer, p))
                            )
                    case other =>
                        IO.raiseError(CellError.UnexpectedPayload(h.stackNum, other.roundLabel))
                }
            case c: Cell.WaitingRound2 =>
                // Round-1 is already saturated and the peer-liaison lane delivers each hard-ack
                // exactly once (next-expected cursors, no resends), so only round-2 is expected
                // now; any other payload — including a second round-1 — is a protocol violation.
                h.payload match {
                    case p: HardAck.Payload.Round2 =>
                        ackVerifier
                            .verify2PhaseRound2(c.unsigned, peer, p)
                            .as(
                              c.copy(round2 = c.round2.updated(peer, p))
                            )
                    case other =>
                        IO.raiseError(CellError.UnexpectedPayload(h.stackNum, other.roundLabel))
                }
            case c: Cell.WaitingSole =>
                h.payload match {
                    case p: HardAck.SolePayload =>
                        ackVerifier
                            .verifySole(c.unsigned, peer, p)
                            .as(c.copy(sole = c.sole.updated(peer, p)))
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
            case Some(c: Cell.WaitingRound1) if isSaturated(c.round1.keySet) =>
                completeRound1(stackNum, c)
            // Round 2 completes once the set of peers who signed BOTH rounds is saturated
            // (every head + ≥ coilQuorum coils) — only such peers yield a coherent, fully-signed
            // witness set across the round-1 effects and the round-2 unlock.
            case Some(c: Cell.WaitingRound2)
                if isSaturated(c.round1.keySet.intersect(c.round2.keySet)) =>
                completeStack(stackNum, c)
            case Some(c: Cell.WaitingSole) if isSaturated(c.sole.keySet) =>
                completeStack(stackNum, c)
            case _ => IO.unit
        }
    }

    private def completeRound1(stackNum: StackNumber, c: Cell.WaitingRound1): IO[Unit] = for {
        _ <- Tracer.info(
          s"stack $stackNum round-1 confirmed; releasing own round-2 ack"
        )
        ownPeer = config.ownPeerId
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
      * and emit the now-multisigned [[Stack.HardConfirmed]] to CardanoLiaison (for L1 submission)
      * and StackComposer (to unblock the next stack). The raw acks have served their purpose
      * (verified + aggregated) and are dropped with the cell.
      */
    private def completeStack(
        stackNum: StackNumber,
        cell: Cell.WaitingRound2 | Cell.WaitingSole
    ): IO[Unit] = for {
        conn <- getConnections
        signers = chooseSigners(cell)
        vkeys <- signers
            .traverse(p => ackVerifier.resolvePeerVKey(p).map(p -> _))
            .map(_.toMap)
        wmap = ackAggregator.aggregateTxSignatures(cell, vkeys)
        evac = ackAggregator.collectSecSignatures(cell, signers)
        signed <- ackAggregator.attachWitnesses(cell.unsigned, wmap, evac)
        _ <- Tracer.info(
          s"stack $stackNum HARD-CONFIRMED — aggregated ackAggregator onto ${wmap.size} " +
              "effect tx(s); emitting downstream"
        )
        hardConfirmed = Stack.HardConfirmed(cell.unsigned.brief, signed)
        _ <- conn.cardanoLiaison ! hardConfirmed
        _ <- conn.stackComposer ! hardConfirmed
        _ <- stateRef.update(_.dropCell(stackNum))
    } yield ()

    /** The fixed signer set attached at hard-confirmation: every head peer plus exactly
      * `coilQuorum` coil peers (the lowest-ordered coils that contributed every required round), so
      * every effect tx carries exactly `nHeadPeers + coilQuorum` witnesses regardless of how many
      * coils acked.
      */
    private def chooseSigners(cell: Cell.WaitingRound2 | Cell.WaitingSole): List[PeerId] = {
        val present: Set[PeerId] = cell match {
            case c: Cell.WaitingRound2 => c.round1.keySet.intersect(c.round2.keySet)
            case c: Cell.WaitingSole   => c.sole.keySet
        }
        val coils =
            present.collect { case c: PeerId.Coil => c: PeerId }.toList.sorted.take(coilQuorum)
        allHeadPeers.toList.sorted ++ coils
    }

    // ===================================
    // Plumbing
    // ===================================

    private def broadcast(ack: HardAck): IO[Unit] =
        getConnections.flatMap { conn =>
            // Broadcast our own hard-ack to the head mesh, and (on a hub) tee it to the coil-link
            // relay so our coils receive it.
            (conn.peerLiaisons ! ack).parallel >> conn.coilLinkRelay.traverse_(_ ! ack)
        }

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
                      // Stack.HardConfirmed fan-out to StackComposer goes via the rate limiter
                      // on the SlowConsensusActor → StackComposer lane.
                      stackComposer = c.stackComposerLimiter,
                      cardanoLiaison = c.cardanoLiaison,
                      peerLiaisons = c.peerLiaisons,
                      coilLinkRelay = c.coilLinkRelay
                    )
                  )
                )
            } yield ()
        case x: SlowConsensusActor.Connections => connections.set(Some(x))
    }
}

object SlowConsensusActor {

    type Handle = ActorRef[IO, Request]

    type Config = HeadConfig.Section & OwnPeerPublic.Section

    final case class Connections(
        stackComposer: StackComposer.Handle,
        cardanoLiaison: CardanoLiaison.Handle,
        peerLiaisons: List[PeerLiaison.Handle],
        /** A hub's coil-link relay (§8): every hard-ack this actor sees — its own, other heads',
          * and its coils' — is teed here so the hub's coils get the whole hard-ack stream and
          * aggregate hard-confirmation themselves. `None` off a hub.
          */
        coilLinkRelay: Option[CoilLinkRelay.Handle] = None
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
            round1: Map[PeerId, HardAck.Payload.Round1],
            ownRound2: HardAck,
            round2Stash: Map[PeerId, HardAck.Payload.Round2]
        ) extends Cell

        final case class WaitingRound2(
            unsigned: Stack.Unsigned,
            // Carried through from round 1: needed at hard-confirmation to aggregate the
            // non-unlock per-effect signatures into ackAggregator (round 2 only signs the unlock).
            round1: Map[PeerId, HardAck.Payload.Round1],
            round2: Map[PeerId, HardAck.Payload.Round2]
        ) extends Cell

        final case class WaitingSole(
            unsigned: Stack.Unsigned,
            sole: Map[PeerId, HardAck.SolePayload]
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
        case UnknownPeer(peer: PeerId)
        case UnexpectedPayload(stackNum: StackNumber, roundLabel: String)
        case EffectsPayloadMismatch(round: String, payloadRound: String)
        case BadSignature(what: String)
        case KeysetMismatch(label: String, expected: String, actual: String)
        override def getMessage: String = this match
            case NoCell(s)      => s"No cell for stack $s"
            case UnknownPeer(p) => s"Unknown peer $p"
            case UnexpectedPayload(s, r) =>
                s"Unexpected $r payload for stack $s in current phase"
            case EffectsPayloadMismatch(r, pr) =>
                s"Effects/payload variant mismatch at $r: payload round=$pr"
            case BadSignature(w) => s"Hard-ack signature verification failed for $w"
            case KeysetMismatch(l, e, a) =>
                s"Hard-ack keyset mismatch for $l: expected $e, got $a"
}
