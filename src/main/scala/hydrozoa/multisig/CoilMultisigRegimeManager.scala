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
import hydrozoa.multisig.consensus.peer.HeadPeerId
import hydrozoa.multisig.consensus.peer.PeerId.{Coil, Head}
import hydrozoa.multisig.ledger.joint.JointLedger
import hydrozoa.multisig.ledger.l2.L2Ledger
import hydrozoa.multisig.persistence.Persistence
import scala.concurrent.duration.DurationInt

/** Coil-peer counterpart to [[MultisigRegimeManager]]. A coil runs the same multisig-regime actor
  * set as a head follower — the leadership / soft-ack / hard-ack-author behavior is gated entirely
  * in the config seam (`OwnCoilPeerPrivate`) — so the only structural differences are:
  *   - exactly one [[PeerLiaisonHeadToHead]], toward the coil peer's hub head peer (§5.5)
  *     [doc-ref], instead of the head mesh;
  *   - no user-request surface (the spawned [[RequestSequencer]] is inert: no HTTP server routes to
  *     it, and a coil peer authors no requests).
  *
  * It completes the shared [[MultisigRegimeManager.Connections]] so the reused child actors resolve
  * their slots exactly as on a head.
  */
trait CoilMultisigRegimeManager(
    config: NodeConfig,
    cardanoBackend: CardanoBackend[IO],
    l2Ledger: L2Ledger[IO],
    tracerLocal: IOLocal[Tracer],
    persistence: Persistence[IO]
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
                .coilPeerHub(ownCoilNum)
                .getOrElse(
                  throw new IllegalStateException(s"No hub configured for coil $ownCoilNum")
                )
            hubPeerId = HeadPeerId(hubNum, config.nHeadPeers)

            nodeId = s"coil:${config.ownPeerIndex}"
            tracer <- ProtocolTracer.jsonLines(nodeId)

            pendingConnections <- Deferred[IO, Connections]

            blockWeaver <- context.actorOf(BlockWeaver(config, pendingConnections, tracerLocal))
            cardanoLiaison <-
                context.actorOf(
                  CardanoLiaison(config, cardanoBackend, pendingConnections, tracerLocal)
                )
            consensusActor <- context.actorOf(
              FastConsensusActor(config, pendingConnections, tracerLocal, persistence)
            )

            // No-op placeholder: a coil peer authors no user requests, but the reused actors resolve
            // the whole `Connections`, so the slot must still hold a valid handle.
            // TODO: restructure `Connections` (a coil-specific subset, or make request-only slots
            // optional) so a coil peer doesn't carry this inert slot at all — rule of least knowledge.
            requestSequencer <- context.actorOf(NoopActor[RequestSequencer.Request])
            jointLedger <- context.actorOf(
              JointLedger(config, pendingConnections, l2Ledger, tracer, tracerLocal, persistence)
            )
            stackComposer <- context.actorOf(
              StackComposer(config, pendingConnections, tracerLocal, persistence)
            )
            slowConsensusActor <- context.actorOf(
              SlowConsensusActor(config, pendingConnections, tracerLocal, persistence)
            )

            // Exactly one liaison, toward the hub head peer (§5.5) [doc-ref]. It projects its connections from
            // the shared `Connections`; the hub-liaison handle (`remoteHubLiaison`) stays empty in
            // this production placeholder (in-process wiring fills it).
            hubLiaison <- context.actorOf(
              liaison.PeerLiaisonCoilToHub(config, hubPeerId, pendingConnections, persistence)
            )

            // A coil peer never leads, so there is nothing to pace against L1 timing: the limiter
            // slots alias the unthrottled handles directly (no `Limiter` actors spawned). A coil has
            // no head mesh — its `SlowConsensusActor` broadcasts its own hard-ack up `coilUplink`.
            connections = Connections(
              blockWeaver = blockWeaver,
              blockWeaverLimiter = blockWeaver,
              cardanoLiaison = cardanoLiaison,
              consensusActor = consensusActor,
              requestSequencer = requestSequencer,
              jointLedger = jointLedger,
              stackComposer = stackComposer,
              stackComposerLimiter = stackComposer,
              slowConsensusActor = slowConsensusActor,
              coilUplink = Some(hubLiaison),
              tracer = tracer,
            )

            _ <- pendingConnections.complete(connections)
            _ <- connectionsDeferred.complete(connections)

            _ <- Tracer.info("Watching coil multisig actors...")

            _ <- context.watch(blockWeaver, TerminatedChild(Actors.BlockWeaver, blockWeaver))
            _ <- context.watch(
              hubLiaison,
              TerminatedChild(Actors.PeerLiaisonHeadToHead, hubLiaison)
            )
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
        tracerLocal: IOLocal[Tracer],
        persistence: Persistence[IO]
    ): IO[CoilMultisigRegimeManager] =
        IO(
          new CoilMultisigRegimeManager(
            config,
            cardanoBackend,
            virtualLedger,
            tracerLocal,
            persistence
          ) {}
        )
}
