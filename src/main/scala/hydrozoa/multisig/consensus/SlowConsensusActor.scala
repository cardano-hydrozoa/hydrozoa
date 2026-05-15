package hydrozoa.multisig.consensus

import cats.effect.{IO, IOLocal, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.node.owninfo.OwnHeadPeerPrivate
import hydrozoa.lib.logging.{Logging, Tracer}
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

    private val logger = Logging.loggerIO(s"SlowConsensusActor.${config.ownHeadPeerNum}")
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
                _ <- logger.info("SlowConsensusActor started (auto-confirm stub).")
            } yield ()
        case s: Stack.Unsigned =>
            handleStackUnsigned(s)
        case _: HardAck =>
            // Stub: ignore incoming hard-acks. Real impl will aggregate per-cell.
            IO.unit
    }

    /** Auto-confirm stub: as soon as the StackComposer hands us a stack, signal hard-confirmation
      * back so the next stack can close. Real impl will wait for all peers' acks across both rounds
      * (or the single sole round) per [[hydrozoa.multisig.ledger.stack.StackEffects]] variant.
      */
    private def handleStackUnsigned(s: Stack.Unsigned): IO[Unit] = for {
        _ <- logger.info(s"Stub auto-confirming stack ${s.brief.stackNum}")
        conn <- getConnections
        _ <- conn.stackComposer ! PreviousStackHardConfirmation(s.brief.stackNum)
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
        peerLiaisons: List[PeerLiaison.Handle]
    )

    type Request = PreStart.type | Stack.Unsigned | HardAck

    case object PreStart
}
