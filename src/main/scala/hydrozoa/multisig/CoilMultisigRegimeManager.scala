package hydrozoa.multisig

import cats.*
import cats.effect.{Deferred, IO}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.SupervisorStrategy.Escalate
import com.suprnation.actor.{OneForOneStrategy, SupervisionStrategy}
import hydrozoa.config.node.NodeConfig
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.HeadMultisigRegimeManager.*
import hydrozoa.multisig.HeadMultisigRegimeManagerEvent as MRMEvent
import hydrozoa.multisig.HeadMultisigRegimeManagerEvent.{StartingActors, TerminatedActor, WatchingActors}
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.*
import hydrozoa.multisig.consensus.liaison.PeerLiaisonEvent
import hydrozoa.multisig.consensus.peer.PeerId
import hydrozoa.multisig.consensus.peer.PeerId.{Coil, Head}
import hydrozoa.multisig.ledger.joint.{JointLedger, JointLedgerEvent}
import hydrozoa.multisig.ledger.l2.L2Ledger
import hydrozoa.multisig.persistence.Persistence
import scala.concurrent.duration.DurationInt

/** Coil-peer counterpart to [[HeadMultisigRegimeManager]]. A coil runs the same multisig-regime
  * actor set as a head follower — the leadership / soft-ack / hard-ack-author behavior is gated
  * entirely in the config seam (`OwnCoilPeerPrivate`) — so the only structural differences are:
  *   - exactly one liaison ([[liaison.PeerLiaisonCoilToHub]]), toward the coil peer's hub head peer
  *     (§5.5) [doc-ref], instead of the head mesh;
  *   - no user-request surface (the spawned [[RequestSequencer]] is inert: no HTTP server routes to
  *     it, and a coil peer authors no requests).
  *
  * It completes the shared [[HeadMultisigRegimeManager.Connections]] so the reused child actors
  * resolve their slots exactly as on a head.
  */
trait CoilMultisigRegimeManager(
    config: NodeConfig,
    cardanoBackend: CardanoBackend[IO],
    l2Ledger: L2Ledger[IO],
    persistence: Persistence[IO],
    tracer: ContraTracer[IO, HeadMultisigRegimeManagerEvent]
) extends Actor[IO, Request] {

    /** Specialize the regime-wide tracer down to per-actor channels, exactly as
      * [[HeadMultisigRegimeManager]] does — the reused child actors emit the same event types on a
      * coil peer, so the same roll-up reaches the single sink composition the wiring layer
      * assembled.
      */
    private val bwTracer: ContraTracer[IO, BlockWeaverEvent] =
        tracer.contramap(HeadMultisigRegimeManagerEvent.BW.apply)
    private val jlTracer: ContraTracer[IO, JointLedgerEvent] =
        tracer.contramap(HeadMultisigRegimeManagerEvent.JL.apply)
    private val fcaTracer: ContraTracer[IO, FastConsensusActorEvent] =
        tracer.contramap(HeadMultisigRegimeManagerEvent.FCA.apply)
    private val clTracer: ContraTracer[IO, CardanoLiaisonEvent] =
        tracer.contramap(HeadMultisigRegimeManagerEvent.CL.apply)
    private val scTracer: ContraTracer[IO, StackComposerEvent] =
        tracer.contramap(HeadMultisigRegimeManagerEvent.SC.apply)
    private val scaTracer: ContraTracer[IO, SlowConsensusActorEvent] =
        tracer.contramap(HeadMultisigRegimeManagerEvent.SCA.apply)
    private def plTracer(remotePeerId: PeerId): ContraTracer[IO, PeerLiaisonEvent] =
        tracer.contramap(HeadMultisigRegimeManagerEvent.PL(remotePeerId, _))

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
            tracer.traceWith(TerminatedActor(childType))
        case TerminatedDependency(dependencyType, _) =>
            tracer.traceWith(MRMEvent.TerminatedDependency(dependencyType))
    }

    private def preStartLocal: IO[Unit] =
        for {
            _ <- tracer.traceWith(StartingActors)

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
            pendingConnections <- Deferred[IO, Connections]

            blockWeaver <- context.actorOf(BlockWeaver(config, pendingConnections, bwTracer))
            cardanoLiaison <-
                context.actorOf(
                  CardanoLiaison(config, cardanoBackend, pendingConnections, clTracer, persistence)
                )
            consensusActor <- context.actorOf(
              FastConsensusActor(config, pendingConnections, fcaTracer, persistence)
            )

            // No-op placeholder: a coil peer authors no user requests, but the reused actors resolve
            // the whole `Connections`, so the slot must still hold a valid handle.
            // TODO: restructure `Connections` (a coil-specific subset, or make request-only slots
            // optional) so a coil peer doesn't carry this inert slot at all — rule of least knowledge.
            requestSequencer <- context.actorOf(NoopActor[RequestSequencer.Request])
            jointLedger <- context.actorOf(
              JointLedger(config, pendingConnections, l2Ledger, jlTracer, persistence)
            )
            stackComposer <- context.actorOf(
              StackComposer(config, pendingConnections, scTracer, persistence)
            )
            slowConsensusActor <- context.actorOf(
              SlowConsensusActor(config, pendingConnections, scaTracer, persistence)
            )

            // Exactly one liaison, toward the hub head peer (§5.5) [doc-ref]. It projects its connections from
            // the shared `Connections`; the hub-liaison handle (`remoteHubLiaison`) stays empty in
            // this production placeholder (in-process wiring fills it).
            hubLiaison <- context.actorOf(
              liaison.PeerLiaisonCoilToHub(
                config,
                pendingConnections,
                plTracer(Head(hubNum)),
                persistence
              )
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
            )

            // R3 boot replay (§8 step 3, coil path — §10 Q10): before opening the start barrier,
            // re-feed the consensus actors from the persisted lane tail (head journals + the hubs'
            // HubHardAck + this coil peer's own coil HardAck) and seed BlockWeaver's first L1
            // PollResults. Run INLINE here (Plan A) so every send queues behind each actor's PreStart
            // and drains in order once the barrier opens. Cold store ⇒ near-no-op.
            _ <- ReplayActor.replay(
              persistence,
              cardanoBackend,
              ReplayActor.Targets(
                blockWeaver = blockWeaver,
                fastConsensusActor = consensusActor,
                slowConsensusActor = slowConsensusActor,
                stackComposer = stackComposer
              ),
              own = PeerId.Coil(ownCoilNum),
              peers = config.headPeerIds.map(_.peerNum).toList,
              hubs = config.hubHeadPeerNumbers,
              // A coil peer is never a hub, so it re-feeds no coil-ack gap (its Targets carries no
              // CoilAckSequencer either — the gap step is a no-op).
              coils = Nil,
              treasuryAddress = config.initializationTx.treasuryProduced.address
            )(using config)

            _ <- pendingConnections.complete(connections)
            _ <- connectionsDeferred.complete(connections)

            _ <- tracer.traceWith(WatchingActors)

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
        persistence: Persistence[IO],
        tracer: ContraTracer[IO, HeadMultisigRegimeManagerEvent]
    ): IO[CoilMultisigRegimeManager] =
        IO(
          new CoilMultisigRegimeManager(
            config,
            cardanoBackend,
            virtualLedger,
            persistence,
            tracer
          ) {}
        )
}
