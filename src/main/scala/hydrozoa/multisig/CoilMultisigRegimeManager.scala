package hydrozoa.multisig

import cats.effect.{Deferred, IO, Ref, Resource}
import cats.syntax.all.*
import com.suprnation.actor.ActorContext
import com.suprnation.actor.ActorRef.NoSendActorRef
import hydrozoa.config.node.NodeConfig
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.HeadMultisigRegimeManager.*
import hydrozoa.multisig.LifecycleEvent.{StartingActors, WatchingActors}
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.*
import hydrozoa.multisig.consensus.peer.PeerId
import hydrozoa.multisig.consensus.peer.PeerId.{Coil, Head}
import hydrozoa.multisig.consensus.transport.{CoilTransport, RemoteHubProxy}
import hydrozoa.multisig.ledger.l1.tx.FallbackTx
import hydrozoa.multisig.ledger.l2.L2Ledger
import hydrozoa.multisig.persistence.Persistence
import hydrozoa.rulebased.RuleBasedRegimeManager

/** Coil-peer counterpart to [[HeadMultisigRegimeManager]]. A coil runs the same multisig-regime
  * actor set as a head follower — the leadership / soft-ack / hard-ack-author behavior is gated
  * entirely in the config seam (`OwnCoilPeerPrivate`) — so the only structural differences are:
  *   - exactly one liaison ([[liaison.PeerLiaisonCoilToHub]]), toward the coil peer's hub head peer
  *     (§5.5) [doc-ref], instead of the head mesh;
  *   - no user-request surface (no [[RequestSequencer]] is spawned; `Connections.requestSequencer`
  *     stays `None`).
  *
  * It completes the shared [[HeadMultisigRegimeManager.Connections]] so the reused child actors
  * resolve their slots exactly as on a head.
  */
trait CoilMultisigRegimeManager(
    config: NodeConfig,
    cardanoBackend: CardanoBackend[IO],
    l2Ledger: L2Ledger[IO],
    persistence: Persistence[IO],
    override protected val tracer: ContraTracer[IO, CoilRegimeManagerEvent],
    /** Coil-uplink transport toward this coil peer's single hub. */
    coilTransport: ActorContext[IO, Request, Any] => CoilTransport,
) extends MultisigRegimeManagerBase[CoilRegimeManagerEvent] {

    override protected lazy val tracers: CoilMrmTracers = CoilMrmTracers.fromRoot(tracer)

    /** Children stopped at handoff time; CL is deliberately excluded — it stays alive to process
      * refunds and finish rollouts (mirrors [[HeadMultisigRegimeManager]]).
      */
    private val nonClChildren: Ref[IO, List[NoSendActorRef[IO]]] =
        Ref.unsafe(List.empty)

    override protected def preStartLocal: IO[Unit] =
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

            core <- spawnCoreActors(
              config,
              cardanoBackend,
              l2Ledger,
              persistence,
              pendingConnections,
            )

            // Exactly one liaison, toward the hub head peer (§5.5) [doc-ref]. Register it on the
            // coil-uplink transport for inbound dispatch, then spawn a `RemoteHubProxy` so the
            // local actors talk to the hub through a single uniform handle.
            hubLiaison <- context.actorOf(
              liaison.PeerLiaisonCoilToHub(
                config,
                pendingConnections,
                tracers.peerLiaison(Head(hubNum)),
                persistence
              )
            )
            transport = coilTransport(context)
            _ <- transport.register(hubLiaison)
            remoteHubProxy <- context.actorOf(RemoteHubProxy(transport))

            // A coil peer never leads, so there is nothing to pace against L1 timing: the limiter
            // slots alias the unthrottled handles directly (no `Limiter` actors spawned). A coil has
            // no head mesh — its `SlowConsensusActor` broadcasts its own hard-ack up `coilUplink`.
            connections = Connections(
              blockWeaver = core.blockWeaver,
              blockWeaverLimiter = core.blockWeaver,
              cardanoLiaison = core.cardanoLiaison,
              consensusActor = core.consensusActor,
              jointLedger = core.jointLedger,
              stackComposer = core.stackComposer,
              stackComposerLimiter = core.stackComposer,
              slowConsensusActor = core.slowConsensusActor,
              coilUplink = Some(hubLiaison),
              remoteHubLiaison = Some(remoteHubProxy),
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
                blockWeaver = core.blockWeaver,
                fastConsensusActor = core.consensusActor,
                slowConsensusActor = core.slowConsensusActor,
                stackComposer = core.stackComposer
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

            _ <- watchChildren(
              core.blockWeaver -> Actors.BlockWeaver,
              hubLiaison -> Actors.PeerLiaisonHeadToHead,
              core.cardanoLiaison -> Actors.CardanoLiaison,
              core.stackComposer -> Actors.StackComposer,
              core.slowConsensusActor -> Actors.SlowConsensus,
            )

            _ <- nonClChildren.set(
              List[NoSendActorRef[IO]](
                core.blockWeaver,
                core.consensusActor,
                core.jointLedger,
                core.stackComposer,
                core.slowConsensusActor,
                hubLiaison,
                remoteHubProxy,
              )
            )
        } yield ()

    /** Coil-peer handoff: mirrors [[HeadMultisigRegimeManager.onHandoffToRuleBased]]. Stops the
      * multisig-side children (leaving CL up for refunds/rollouts) and spawns
      * [[RuleBasedRegimeManager]], whose actor will run the coil ratchet path (see
      * `RuleBasedActor.Dispute.handleCoil`).
      */
    // Idempotent — mirrors HMRM.onHandoffToRuleBased.
    override protected def onHandoffToRuleBased(fallback: FallbackTx): IO[Unit] =
        nonClChildren.getAndSet(Nil).flatMap {
            case Nil => IO.unit
            case refs =>
                refs.traverse_(context.stop) >>
                    context
                        .actorOf(
                          RuleBasedRegimeManager(
                            cardanoBackend = cardanoBackend,
                            persistence = persistence,
                            tracer = tracers.ruleBasedActor,
                          )(using config)
                        )
                        .void
        }
}

object CoilMultisigRegimeManager {
    def resource(
        config: NodeConfig,
        cardanoBackend: CardanoBackend[IO],
        virtualLedger: L2Ledger[IO],
        persistence: Persistence[IO],
        tracer: ContraTracer[IO, CoilRegimeManagerEvent],
        coilTransport: Resource[IO, ActorContext[IO, Request, Any] => CoilTransport],
    ): Resource[IO, CoilMultisigRegimeManager] =
        coilTransport.flatMap { factory =>
            Resource.eval(
              IO(
                new CoilMultisigRegimeManager(
                  config,
                  cardanoBackend,
                  virtualLedger,
                  persistence,
                  tracer,
                  factory,
                ) {}
              )
            )
        }
}
