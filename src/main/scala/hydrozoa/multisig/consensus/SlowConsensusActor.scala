package hydrozoa.multisig.consensus

import cats.effect.{IO, IOLocal, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.node.owninfo.OwnHeadPeerPrivate
import hydrozoa.lib.logging.Tracer
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.StackComposer.PreviousStackHardConfirmation
import hydrozoa.multisig.consensus.ack.HardAck
import hydrozoa.multisig.ledger.stack.Stack

/** Slow-consensus actor (M6 — Option A auto-confirm stub).
  *
  * Final design (PR2/M6 full): collects per-effect hard-acks from all head peers across two rounds
  * (or one sole round for minor-only stacks), schedules outbound own-ack broadcast (round-1 / sole
  * immediately, round-2 withheld until local round-1 confirmation), and emits `Stack.HardConfirmed`
  * to CardanoLiaison + `PreviousStackHardConfirmation` back to StackComposer.
  *
  * **This iteration is a stub:** on receiving a `Stack.Unsigned` from the StackComposer, it
  * immediately echoes `PreviousStackHardConfirmation(stackNum)` back to the StackComposer without
  * doing any actual ack collection. Lets the leader/follower wiring close end-to-end for
  * architectural verification before the real ack-aggregation logic lands.
  */
final case class SlowConsensusActor(
    config: SlowConsensusActor.Config,
    pendingConnections: MultisigRegimeManager.PendingConnections | SlowConsensusActor.Connections,
    tracerLocal: IOLocal[Tracer]
) extends Actor[IO, SlowConsensusActor.Request] {
    import SlowConsensusActor.*

    given IOLocal[Tracer] = tracerLocal

    private val connections = Ref.unsafe[IO, Option[Connections]](None)

    override def preStart: IO[Unit] = for {
        _ <- context.self ! PreStart
        _ <- context.become(receive)
    } yield ()

    override def receive: Receive[IO, Request] = PartialFunction.fromFunction {
        case PreStart =>
            for {
                _ <- Tracer.routeLocal(s"SlowConsensusActor.${config.ownHeadPeerNum}")
                _ <- initializeConnections
                _ <- Tracer.info("SlowConsensusActor started (auto-confirm stub).")
            } yield ()
        case h: SlowConsensusActor.StackHandoff =>
            handleStackHandoff(h)
        case _: HardAck =>
            // Stub: ignore incoming hard-acks. Real impl will aggregate per-cell.
            IO.unit
    }

    /** Auto-confirm stub: as soon as the StackComposer hands us a stack + its own hard-acks, signal
      * hard-confirmation back so the next stack can close. Real impl (next slice) will collect
      * remote peers' acks across both rounds (or the single sole round) per
      * [[hydrozoa.multisig.ledger.stack.StackEffects]] variant, and schedule outbound own-ack
      * broadcast (round-1 / sole immediately to PeerLiaisons; round-2 withheld until local round-1
      * confirmation).
      */
    private def handleStackHandoff(h: SlowConsensusActor.StackHandoff): IO[Unit] = for {
        _ <- Tracer.info(
          s"Stub auto-confirming stack ${h.unsigned.brief.stackNum} (own acks: ${h.ownAcks.size})"
        )
        conn <- getConnections
        // Build a stub Stack.HardConfirmed (no remote ack data) — matches auto-confirm
        // behaviour. CardanoLiaison logs the receipt; real submission lands once the on-chain
        // submission code is wired.
        hardConfirmed = Stack.HardConfirmed(Stack.Round1Confirmed(h.unsigned))
        _ <- conn.cardanoLiaison ! hardConfirmed
        _ <- conn.stackComposer ! PreviousStackHardConfirmation(h.unsigned.brief.stackNum)
    } yield ()

    private def getConnections: IO[Connections] = for {
        mConn <- connections.get
        conn <- mConn.fold(
          IO.raiseError(
            java.lang.Error("SlowConsensusActor is missing its connections to other actors.")
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

    /** Bundle sent by [[StackComposer]] when it closes a stack: the unsigned stack plus the
      * leader's (or follower's) own pre-signed hard-acks for every round the stack will need. The
      * SlowConsensusActor manages outbound broadcast scheduling (round-1 / sole acks immediately to
      * PeerLiaisons; round-2 withheld until local round-1 confirmation).
      *
      *   - 2-phase Regular stacks (settlement / finalization present): 2 acks per peer (round1 +
      *     round2 Regular variants).
      *   - 2-phase Initial stack: 2 acks per peer (round1 + round2 Initial variants).
      *   - 1-phase Sole stacks (minor-only): 1 ack per peer (SolePayload).
      */
    final case class StackHandoff(unsigned: Stack.Unsigned, ownAcks: List[HardAck])
}
