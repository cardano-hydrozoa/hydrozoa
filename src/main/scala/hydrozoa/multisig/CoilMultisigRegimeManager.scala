package hydrozoa.multisig

import cats.effect.{Deferred, IO, IOLocal}
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.SupervisorStrategy.Escalate
import com.suprnation.actor.{OneForOneStrategy, SupervisionStrategy}
import hydrozoa.config.node.NodeConfig
import hydrozoa.lib.logging.Tracer
import hydrozoa.lib.tracing.ProtocolTracer
import hydrozoa.multisig.MultisigRegimeManager.*
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.*
import hydrozoa.multisig.consensus.limiter.Limiter
import hydrozoa.multisig.consensus.peer.PeerId.{Coil, Head}
import hydrozoa.multisig.consensus.peer.{HeadPeerId, RemotePeer}
import hydrozoa.multisig.ledger.joint.JointLedger
import hydrozoa.multisig.ledger.l2.L2Ledger
import scala.concurrent.duration.DurationInt

/** Coil-peer counterpart to [[MultisigRegimeManager]]. A coil runs the same multisig-regime actor
  * set as a head follower — the leadership / soft-ack / hard-ack-author behavior is gated entirely
  * in the config seam (`OwnCoilPeerPrivate`) — so the only structural differences are:
  *   - exactly one [[PeerLiaison]], toward the coil's hub head peer (§8), instead of the head mesh;
  *   - no user-request surface (the spawned [[EventSequencer]] is inert: no HTTP server routes to
  *     it, and a coil authors no requests).
  *
  * It completes the shared [[MultisigRegimeManager.Connections]] so the reused child actors resolve
  * their slots exactly as on a head.
  */
trait CoilMultisigRegimeManager(
    config: NodeConfig,
    cardanoBackend: CardanoBackend[IO],
    l2Ledger: L2Ledger[IO],
    tracerLocal: IOLocal[Tracer]
) extends Actor[IO, Request] {

    given IOLocal[Tracer] = tracerLocal

    val connectionsDeferred: Deferred[IO, Connections] = Deferred.unsafe[IO, Connections]

    override def supervisorStrategy: SupervisionStrategy[IO] =
        OneForOneStrategy[IO](maxNrOfRetries = 3, withinTimeRange = 1.minute) {
            case _: IllegalArgumentException => Escalate
            case _: RuntimeException         => Escalate
            case _: Exception                => Escalate
        }

    override def preStart: IO[Unit] = context.self ! PreStart

    override def receive: Receive[IO, Request] = PartialFunction.fromFunction(receiveTotal)

    private def receiveTotal(req: Request): IO[Unit] = req match {
        case PreStart => preStartLocal
        case TerminatedChild(childType, _) =>
            Tracer.warn(s"Terminated coil child actor: $childType")
        case TerminatedDependency(dependencyType, _) =>
            Tracer.warn(s"Terminated coil dependency: $dependencyType")
    }

    private def preStartLocal: IO[Unit] =
        for {
            _ <- Tracer.routeLocal("CoilMultisigRegimeManager")
            _ <- Tracer.updateLocalCtx("peer" -> config.ownPeerLabel)
            _ <- Tracer.info("Starting coil multisig actors...")

            ownCoilNum = config.ownPeerId match {
                case Coil(n) => n
                case Head(_) =>
                    throw new IllegalStateException(
                      "CoilMultisigRegimeManager runs only on coil peers"
                    )
            }
            hubNum = config
                .coilHub(ownCoilNum)
                .getOrElse(
                  throw new IllegalStateException(s"No hub configured for coil $ownCoilNum")
                )
            hubPeerId = HeadPeerId(hubNum, config.nHeadPeers)

            nodeId = s"coil:${config.ownPeerIndex}"
            tracer <- ProtocolTracer.jsonLines(nodeId)

            pendingConnections <- Deferred[IO, Connections]

            blockWeaver <- context.actorOf(BlockWeaver(config, pendingConnections, tracerLocal))
            blockWeaverLimiter <- context.actorOf(
              Limiter[BlockWeaver.Request](blockWeaver, config, tracerLocal)
            )
            cardanoLiaison <-
                context.actorOf(
                  CardanoLiaison(config, cardanoBackend, pendingConnections, tracerLocal)
                )
            consensusActor <- context.actorOf(
              FastConsensusActor(config, pendingConnections, tracerLocal)
            )
            // Inert on a coil: no HTTP server routes user requests here, and a coil authors none.
            // Present only to fill the shared Connections slot the reused actors resolve.
            eventSequencer <- context.actorOf(EventSequencer(config, pendingConnections))
            jointLedger <- context.actorOf(
              JointLedger(config, pendingConnections, l2Ledger, tracer, tracerLocal)
            )
            stackComposer <- context.actorOf(
              StackComposer(config, pendingConnections, tracerLocal)
            )
            stackComposerLimiter <- context.actorOf(
              Limiter[StackComposer.Request](stackComposer, config, tracerLocal)
            )
            slowConsensusActor <- context.actorOf(
              SlowConsensusActor(config, pendingConnections, tracerLocal)
            )

            // Exactly one liaison, toward the hub head peer (§8).
            hubLiaison <- context.actorOf(
              CoilPeerToHeadLiaison(config, RemotePeer.Head(hubPeerId), pendingConnections)
            )

            connections = Connections(
              blockWeaver = blockWeaver,
              blockWeaverLimiter = blockWeaverLimiter,
              cardanoLiaison = cardanoLiaison,
              consensusActor = consensusActor,
              eventSequencer = eventSequencer,
              jointLedger = jointLedger,
              stackComposer = stackComposer,
              stackComposerLimiter = stackComposerLimiter,
              slowConsensusActor = slowConsensusActor,
              peerLiaisons = List(hubLiaison),
              remotePeerLiaisons = Map.empty,
              tracer = tracer,
            )

            _ <- pendingConnections.complete(connections)
            _ <- connectionsDeferred.complete(connections)

            _ <- Tracer.info("Watching coil multisig actors...")

            _ <- context.watch(blockWeaver, TerminatedChild(Actors.BlockWeaver, blockWeaver))
            _ <- context.watch(hubLiaison, TerminatedChild(Actors.PeerLiaison, hubLiaison))
            _ <- context.watch(
              cardanoLiaison,
              TerminatedChild(Actors.CardanoLiaison, cardanoLiaison)
            )
            _ <- context.watch(stackComposer, TerminatedChild(Actors.StackComposer, stackComposer))
            _ <- context.watch(
              slowConsensusActor,
              TerminatedChild(Actors.SlowConsensus, slowConsensusActor)
            )
        } yield ()
}

object CoilMultisigRegimeManager {
    def apply(
        config: NodeConfig,
        cardanoBackend: CardanoBackend[IO],
        virtualLedger: L2Ledger[IO],
        tracerLocal: IOLocal[Tracer]
    ): IO[CoilMultisigRegimeManager] =
        IO(new CoilMultisigRegimeManager(config, cardanoBackend, virtualLedger, tracerLocal) {})
}
