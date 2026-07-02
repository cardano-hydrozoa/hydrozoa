package hydrozoa.multisig

import cats.*
import cats.effect.{Deferred, IO, Ref, Resource}
import cats.implicits.*
import com.suprnation.actor.ActorContext
import com.suprnation.actor.ActorRef.NoSendActorRef
import hydrozoa.config.node.NodeConfig
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.HeadMultisigRegimeManager.*
import hydrozoa.multisig.LifecycleEvent.{StartingActors, WatchingActors}
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.*
import hydrozoa.multisig.consensus.limiter.Limiter
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}
import hydrozoa.multisig.consensus.transport.{HubTransport, PeerTransport, RemoteCoilProxy, RemotePeerProxy}
import hydrozoa.multisig.ledger.joint.JointLedger
import hydrozoa.multisig.ledger.l1.tx.FallbackTx
import hydrozoa.multisig.ledger.l2.L2Ledger
import hydrozoa.multisig.persistence.Persistence
import hydrozoa.rulebased.RuleBasedRegimeManager

trait HeadMultisigRegimeManager(
    config: NodeConfig,
    cardanoBackend: CardanoBackend[IO],
    l2Ledger: L2Ledger[IO],
    persistence: Persistence[IO],
    override protected val tracer: ContraTracer[IO, HeadRegimeManagerEvent],
    peerTransport: ActorContext[IO, Request, Any] => PeerTransport,
    /** Hub-side coil transport, populated iff this peer hubs any coil peers; required when
      * `config.hubbedCoilPeerNums(ownHeadNum)` is non-empty.
      */
    hubCoilTransport: Option[ActorContext[IO, Request, Any] => HubTransport],
) extends MultisigRegimeManagerBase[HeadRegimeManagerEvent] {

    override protected lazy val tracers: MrmTracers = MrmTracers.fromRoot(tracer)

    /** Non-CL child refs, populated once every actor is spawned in [[preStartLocal]]. On
      * [[HandoffToRuleBased]], `onHandoffToRuleBased` reads them and issues `context.stop` to each;
      * CL stays alive to process refunds and finish rollouts.
      */
    private val nonClChildren: Ref[IO, List[NoSendActorRef[IO]]] =
        Ref.unsafe(List.empty)

    override protected def preStartLocal: IO[Unit] =
        for {
            _ <- tracer.traceWith(StartingActors)

            pendingConnections <- Deferred[IO, HeadMultisigRegimeManager.Connections]

            core <- spawnCoreActors(
              config,
              cardanoBackend,
              l2Ledger,
              persistence,
              pendingConnections,
            )

            // Throttles the FastConsensusActor → BlockWeaver soft-block-confirmation lane (see
            // hydrozoa.multisig.consensus.limiter.Limiter). Only the consensus actor's reference
            // to BlockWeaver is routed through this limiter; other senders (JointLedger,
            // PeerLiaisonHeadToHead, …) keep direct refs.
            blockWeaverLimiter <- context.actorOf(
              Limiter[BlockWeaver.Request](core.blockWeaver, config, tracers.blockWeaverLimiter)
            )

            requestSequencer <- context.actorOf(
              RequestSequencer(config, pendingConnections, tracers.eventSequencer, persistence)
            )

            // Throttles the SlowConsensusActor → StackComposer hard-stack-confirmation lane.
            stackComposerLimiter <- context.actorOf(
              Limiter[StackComposer.Request](
                core.stackComposer,
                config,
                tracers.stackComposerLimiter
              )
            )

            // One peer liaison toward every other head peer (the head mesh). The liaisons project
            // their connections from the shared `Connections`; the remote-handle maps stay empty in
            // this production placeholder (in-process wiring fills them — see stage4).
            headPeerLiaisons <-
                config.headPeerIds
                    .filterNot(id => config.ownPeerId == PeerId.Head(id.peerNum))
                    .traverse(pid =>
                        context.actorOf(
                          liaison.PeerLiaisonHeadToHead(
                            config,
                            pid,
                            pendingConnections,
                            tracers.peerLiaison(PeerId.Head(pid.peerNum)),
                            persistence
                          )
                        )
                    )

            ownHeadNum = config.ownPeerId match {
                case PeerId.Head(n) => n
                case PeerId.Coil(_) =>
                    throw new IllegalStateException(
                      "HeadMultisigRegimeManager runs only on head peers"
                    )
            }

            // Register local liaisons and spawn one RemotePeerProxy per remote head peer.
            remoteHeadProxies <- {
                val transport = peerTransport(context)
                val remoteIds = config.headPeerIds.toList.filterNot(_.peerNum == ownHeadNum)
                for {
                    _ <- remoteIds.zip(headPeerLiaisons).traverse_ {
                        case (remoteId, localLiaison) =>
                            transport.register(remoteId, localLiaison)
                    }
                    proxies <- remoteIds.traverse { remoteId =>
                        context
                            .actorOf(RemotePeerProxy(remoteId, transport))
                            .map(remoteId.peerNum -> _)
                    }
                } yield proxies.toMap
            }

            hubbedCoilPeers = config.hubbedCoilPeerNums(ownHeadNum)

            // If this head peer is a hub, spawn the relay sequencer for its coil peers' hard-acks
            // (§5.3) [doc-ref] + the CoilRelay fan-out (§5.4) [doc-ref] + one hub→coil liaison per coil peer it hubs.
            // Non-hub head peers spawn none.
            coilAckSequencer <-
                if hubbedCoilPeers.isEmpty then IO.none[CoilAckSequencer.Handle]
                else
                    context
                        .actorOf(
                          CoilAckSequencer(
                            config,
                            persistence,
                            pendingConnections,
                            tracers.coilAckSequencer
                          )
                        )
                        .map(Some(_))

            coilRelay <-
                if hubbedCoilPeers.isEmpty then IO.none[CoilRelay.Handle]
                else context.actorOf(CoilRelay(pendingConnections)).map(Some(_))

            coilPeerLiaisons <-
                hubbedCoilPeers.traverse(coilNum =>
                    context.actorOf(
                      liaison.PeerLiaisonHubToCoil(
                        config,
                        coilNum,
                        pendingConnections,
                        tracers.peerLiaison(PeerId.Coil(coilNum)),
                        persistence
                      )
                    )
                )

            // Register each local hub→coil liaison on the hub transport for inbound dispatch, then
            // spawn one RemoteCoilProxy per hubbed coil to drive outbound. Mirror of the head-mesh
            // `remoteHeadProxies` wiring above. Errors loudly if a hub is missing its transport.
            remoteCoilLiaisons <-
                if hubbedCoilPeers.isEmpty then
                    IO.pure(Map.empty[CoilPeerNumber, liaison.PeerLiaisonCoilToHub.Handle])
                else
                    hubCoilTransport match {
                        case None =>
                            IO.raiseError(
                              new IllegalStateException(
                                s"head $ownHeadNum hubs ${hubbedCoilPeers.size} coil peer(s) " +
                                    "but no hubCoilTransport was provided"
                              )
                            )
                        case Some(factory) =>
                            val transport = factory(context)
                            for {
                                _ <- hubbedCoilPeers.zip(coilPeerLiaisons).traverse_ {
                                    (coilNum, localLiaison) =>
                                        transport.register(coilNum, localLiaison)
                                }
                                proxies <- hubbedCoilPeers.traverse { coilNum =>
                                    context
                                        .actorOf(RemoteCoilProxy(coilNum, transport))
                                        .map(coilNum -> _)
                                }
                            } yield proxies.toMap
                    }

            connections = HeadMultisigRegimeManager.Connections(
              blockWeaver = core.blockWeaver,
              blockWeaverLimiter = blockWeaverLimiter,
              cardanoLiaison = core.cardanoLiaison,
              consensusActor = core.consensusActor,
              requestSequencer = Some(requestSequencer),
              jointLedger = core.jointLedger,
              stackComposer = core.stackComposer,
              stackComposerLimiter = stackComposerLimiter,
              slowConsensusActor = core.slowConsensusActor,
              headPeerLiaisons = headPeerLiaisons,
              coilRelay = coilRelay,
              coilAckSequencer = coilAckSequencer,
              coilPeerLiaisons = coilPeerLiaisons,
              remoteHeadLiaisons = remoteHeadProxies,
              remoteCoilLiaisons = remoteCoilLiaisons,
            )

            // R3 boot replay (§8 step 3): before opening the start barrier, re-feed the consensus
            // actors from the persisted lane tail and seed BlockWeaver's first L1 PollResults. Run
            // INLINE here, not as a spawned actor, so every send queues in each actor's mailbox
            // behind its PreStart and drains in order once `pendingConnections.complete` opens the
            // barrier (Plan A). On a cold store this is a near-no-op: an empty tail plus the early
            // L1 sample.
            _ <- ReplayActor.replay(
              persistence,
              cardanoBackend,
              ReplayActor.Targets(
                blockWeaver = core.blockWeaver,
                fastConsensusActor = core.consensusActor,
                slowConsensusActor = core.slowConsensusActor,
                stackComposer = core.stackComposer,
                coilAckSequencer = coilAckSequencer
              ),
              own = PeerId.Head(ownHeadNum),
              peers = config.headPeerIds.map(_.peerNum).toList,
              hubs = config.hubHeadPeerNumbers,
              coils = hubbedCoilPeers,
              treasuryAddress = config.initializationTx.treasuryProduced.address
            )(using config)

            _ <- pendingConnections.complete(connections)
            _ <- connectionsDeferred.complete(connections)

            _ <- tracer.traceWith(WatchingActors)

            _ <- watchChildren(
              core.blockWeaver -> Actors.BlockWeaver,
              core.cardanoLiaison -> Actors.CardanoLiaison,
              requestSequencer -> Actors.RequestSequencer,
              core.stackComposer -> Actors.StackComposer,
              core.slowConsensusActor -> Actors.SlowConsensus,
            )
            _ <- (headPeerLiaisons.toList ++ coilPeerLiaisons)
                .traverse_(r => watchChildren(r -> Actors.PeerLiaisonHeadToHead))

            // Record everything the handoff needs to stop. CL is deliberately excluded — it
            // stays alive across the multisig→rule-based transition to process refunds and
            // finish rollouts.
            _ <- nonClChildren.set(
              List[NoSendActorRef[IO]](
                core.blockWeaver,
                core.consensusActor,
                core.jointLedger,
                core.stackComposer,
                core.slowConsensusActor,
                blockWeaverLimiter,
                stackComposerLimiter,
                requestSequencer,
              ) ++ headPeerLiaisons.toList
                  ++ coilPeerLiaisons
                  ++ remoteHeadProxies.values.toList
                  ++ coilAckSequencer.toList
                  ++ coilRelay.toList
                  ++ remoteCoilLiaisons.values.toList
            )
        } yield ()

    override protected def onHandoffToRuleBased(fallback: FallbackTx): IO[Unit] =
        for {
            refs <- nonClChildren.getAndSet(Nil)
            _ <- refs.traverse_(context.stop)
            _ <- context.actorOf(
              RuleBasedRegimeManager(
                cardanoBackend = cardanoBackend,
                persistence = persistence,
                tracer = tracers.ruleBasedActor,
              )(using config)
            )
        } yield ()
}

/** Multisig regime manager starts-up and monitors all the actors of the multisig regime.
  */
object HeadMultisigRegimeManager {
    final case class Connections(
        blockWeaver: BlockWeaver.Handle,
        /** Throttled-write handle for the FastConsensusActor → BlockWeaver lane. Other senders use
          * `blockWeaver` directly.
          */
        blockWeaverLimiter: BlockWeaver.Handle,
        cardanoLiaison: CardanoLiaison.Handle,
        consensusActor: FastConsensusActor.Handle,
        /** Head-peer-only: the request sequencer that accepts user submissions. `None` on a coil
          * peer (followers don't accept user requests).
          */
        requestSequencer: Option[RequestSequencer.Handle] = None,
        jointLedger: JointLedger.Handle,
        stackComposer: StackComposer.Handle,
        /** Throttled-write handle for the SlowConsensusActor → StackComposer lane. */
        stackComposerLimiter: StackComposer.Handle,
        slowConsensusActor: SlowConsensusActor.Handle,
        // ---- Producer broadcast targets (§5.2) [doc-ref] ----
        /** Head-peer-mesh liaisons (one per other head peer); empty on a coil peer. Producers
          * broadcast their own artifacts here. `ActorRef` is contravariant in its message type, so
          * a handle is usable as `ActorRef[IO, <any artifact in its Request>]`.
          */
        headPeerLiaisons: List[liaison.PeerLiaisonHeadToHead.Handle] = Nil,
        /** A coil peer's single uplink to its hub; `None` on a head peer. `SlowConsensusActor`
          * broadcasts its own hard-ack to `headPeerLiaisons ++ coilUplink`.
          */
        coilUplink: Option[liaison.PeerLiaisonCoilToHub.Handle] = None,
        /** Present only on a hub head peer (§5.4) [doc-ref]: the fan-out that relays the population
          * to its coil peers. Producers send only their own production here. `None` elsewhere.
          */
        coilRelay: Option[CoilRelay.Handle] = None,
        // ---- Remote-handle resolution for spawned liaisons (in-process only) ----
        // Only the in-process harness (stage4 / unit tests) populates these; in a real deployment
        // the counterpart is another process reached over the transport, so they stay empty.
        remoteHeadLiaisons: Map[HeadPeerNumber, liaison.PeerLiaisonHeadToHead.Handle] = Map.empty,
        remoteCoilLiaisons: Map[CoilPeerNumber, liaison.PeerLiaisonCoilToHub.Handle] = Map.empty,
        remoteHubLiaison: Option[liaison.PeerLiaisonHubToCoil.Handle] = None,
        /** Present only on a hub head peer (§5.3) [doc-ref]: re-sequences its coil peers' hard-acks
          * onto the `HubHardAckLane`. `None` elsewhere.
          */
        coilAckSequencer: Option[CoilAckSequencer.Handle] = None,
        /** Hub→coil liaisons this hub runs (for `CoilRelay`'s fan-out); empty elsewhere. */
        coilPeerLiaisons: List[liaison.PeerLiaisonHubToCoil.Handle] = Nil,
    )

    type PendingConnections = Deferred[IO, Connections]

    def resource(
        config: NodeConfig,
        cardanoBackend: CardanoBackend[IO],
        virtualLedger: L2Ledger[IO],
        persistence: Persistence[IO],
        tracer: ContraTracer[IO, HeadRegimeManagerEvent],
        peerTransport: Resource[IO, ActorContext[IO, Request, Any] => PeerTransport],
        hubCoilTransport: Option[Resource[IO, ActorContext[IO, Request, Any] => HubTransport]] =
            None,
    ): Resource[IO, HeadMultisigRegimeManager] =
        for {
            peerFactory <- peerTransport
            hubFactory <- hubCoilTransport.sequence
            mrm <- Resource.eval(
              IO(
                new HeadMultisigRegimeManager(
                  config,
                  cardanoBackend,
                  virtualLedger,
                  persistence,
                  tracer,
                  peerFactory,
                  hubFactory,
                ) {}
              )
            )
        } yield mrm

    /** Multisig regime's protocol for actor requests and responses. See diagram:
      * [[https://app.excalidraw.com/s/9N3iw9j24UW/9eRJ7Dwu42X]]
      */
    enum Actors:
        case BlockWeaver, CardanoLiaison, Consensus, JointLedger, PeerLiaisonHeadToHead,
            RequestSequencer,
            StackComposer, SlowConsensus

    /** Requests received by the multisig regime manager. */
    type Request = PreStart.type | TerminatedChild | TerminatedDependency | HandoffToRuleBased

    type Children = Actors

    enum Dependencies:
        case CardanoBackend, Persistence

    // ===================================
    // Multisig regime manager's messages
    // ===================================

    case object PreStart

    final case class TerminatedChild(childType: Actors, ref: NoSendActorRef[IO])

    final case class TerminatedDependency(dependencyType: Dependencies, ref: NoSendActorRef[IO])

    /** Sent by [[CardanoLiaison]] once a `FallbackToRuleBased` action has been dispatched. HMRM
      * responds by gracefully stopping every multisig child except CL (which stays up to process
      * refunds and finish rollouts) and spawning [[hydrozoa.rulebased.RuleBasedRegimeManager]].
      */
    final case class HandoffToRuleBased(fallback: FallbackTx)
}
