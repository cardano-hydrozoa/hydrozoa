package hydrozoa.multisig

import cats.*
import cats.effect.{Deferred, IO}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.NoSendActorRef
import com.suprnation.actor.SupervisorStrategy.Escalate
import com.suprnation.actor.{OneForOneStrategy, SupervisionStrategy}
import hydrozoa.config.node.NodeConfig
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.MultisigRegimeManager.*
import hydrozoa.multisig.MultisigRegimeManagerEvent as MRMEvent
import hydrozoa.multisig.MultisigRegimeManagerEvent.{StartingActors, TerminatedActor, WatchingActors}
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.*
import hydrozoa.multisig.consensus.liaison.PeerLiaisonEvent
import hydrozoa.multisig.consensus.limiter.{Limiter, LimiterEvent}
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.joint.{JointLedger, JointLedgerEvent}
import hydrozoa.multisig.ledger.l2.L2Ledger
import hydrozoa.multisig.persistence.Persistence
import scala.concurrent.duration.DurationInt

trait MultisigRegimeManager(
    config: NodeConfig,
    cardanoBackend: CardanoBackend[IO],
    l2Ledger: L2Ledger[IO],
    persistence: Persistence[IO],
    tracer: ContraTracer[IO, MultisigRegimeManagerEvent]
) extends Actor[IO, Request] {

    /** Specialize the regime-wide tracer down to per-actor channels. The contramap pushes the
      * producer's narrow event type up into [[MultisigRegimeManagerEvent]] so it can reach the
      * single sink composition the wiring layer assembled.
      */
    private val bwTracer: ContraTracer[IO, BlockWeaverEvent] =
        tracer.contramap(MultisigRegimeManagerEvent.BW.apply)
    private val jlTracer: ContraTracer[IO, JointLedgerEvent] =
        tracer.contramap(MultisigRegimeManagerEvent.JL.apply)
    private val fcaTracer: ContraTracer[IO, FastConsensusActorEvent] =
        tracer.contramap(MultisigRegimeManagerEvent.FCA.apply)
    private val clTracer: ContraTracer[IO, CardanoLiaisonEvent] =
        tracer.contramap(MultisigRegimeManagerEvent.CL.apply)
    private val scTracer: ContraTracer[IO, StackComposerEvent] =
        tracer.contramap(MultisigRegimeManagerEvent.SC.apply)
    private val scaTracer: ContraTracer[IO, SlowConsensusActorEvent] =
        tracer.contramap(MultisigRegimeManagerEvent.SCA.apply)
    private val esTracer: ContraTracer[IO, EventSequencerEvent] =
        tracer.contramap(MultisigRegimeManagerEvent.ES.apply)
    private def plTracer(remotePeerId: PeerId): ContraTracer[IO, PeerLiaisonEvent] =
        tracer.contramap(MultisigRegimeManagerEvent.PL(remotePeerId, _))
    private val bwlTracer: ContraTracer[IO, LimiterEvent] =
        tracer.contramap(MultisigRegimeManagerEvent.BWL.apply)
    private val sclTracer: ContraTracer[IO, LimiterEvent] =
        tracer.contramap(MultisigRegimeManagerEvent.SCL.apply)

    /** Deferred that will be completed with connections once actors are started */
    val connectionsDeferred: Deferred[IO, Connections] = Deferred.unsafe[IO, Connections]

    override def supervisorStrategy: SupervisionStrategy[IO] =
        OneForOneStrategy[IO](maxNrOfRetries = 3, withinTimeRange = 1.minute) {
            case _: IllegalArgumentException =>
                Escalate // Normally `Stop` but we can't handle stopped actors yet
            case _: RuntimeException =>
                Escalate // Normally `Restart` but our actors can't do that yet
            case _: Exception => Escalate
        }

    override def preStart: IO[Unit] = {
        context.self ! PreStart
    }

    override def receive: Receive[IO, Request] = PartialFunction.fromFunction(receiveTotal)

    private def receiveTotal(req: Request): IO[Unit] = req match {
        case PreStart => preStartLocal
        case TerminatedChild(childType, _) =>
            tracer.traceWith(TerminatedActor(childType))
        case TerminatedDependency(dependencyType, _) =>
            tracer.traceWith(MRMEvent.TerminatedDependency(dependencyType))
        // TODO: Implement a way to receive a remote comm actor and connect it to its corresponding local comm actor
    }

    def preStartLocal: IO[Unit] =
        for {
            _ <- tracer.traceWith(StartingActors)

            pendingConnections <- Deferred[IO, MultisigRegimeManager.Connections]

            blockWeaver <- context.actorOf(BlockWeaver(config, pendingConnections, bwTracer))

            // Throttles the FastConsensusActor → BlockWeaver soft-block-confirmation lane (see
            // hydrozoa.multisig.consensus.limiter.Limiter). Only the consensus actor's reference
            // to BlockWeaver is routed through this limiter; other senders (JointLedger,
            // PeerLiaisonHeadToHead, …) keep direct refs.
            blockWeaverLimiter <- context.actorOf(
              Limiter[BlockWeaver.Request](blockWeaver, config, bwlTracer)
            )

            cardanoLiaison <-
                context.actorOf(
                  CardanoLiaison(config, cardanoBackend, pendingConnections, clTracer, persistence)
                )

            consensusActor <- context.actorOf(
              FastConsensusActor(config, pendingConnections, fcaTracer, persistence)
            )

            requestSequencer <- context.actorOf(
              RequestSequencer(config, pendingConnections, esTracer, persistence)
            )

            jointLedger <- context.actorOf(
              JointLedger(config, pendingConnections, l2Ledger, jlTracer, persistence)
            )

            stackComposer <- context.actorOf(
              StackComposer(config, pendingConnections, scTracer, persistence)
            )

            // Throttles the SlowConsensusActor → StackComposer hard-stack-confirmation lane.
            stackComposerLimiter <- context.actorOf(
              Limiter[StackComposer.Request](stackComposer, config, sclTracer)
            )

            slowConsensusActor <- context.actorOf(
              SlowConsensusActor(config, pendingConnections, scaTracer, persistence)
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
                            plTracer(PeerId.Head(pid.peerNum)),
                            persistence
                          )
                        )
                    )

            ownHeadNum = config.ownPeerId match {
                case PeerId.Head(n) => n
                case PeerId.Coil(_) =>
                    throw new IllegalStateException(
                      "MultisigRegimeManager runs only on head peers"
                    )
            }

            hubbedCoilPeers = config.hubbedCoilPeerNums(ownHeadNum)

            // If this head peer is a hub, spawn the relay sequencer for its coil peers' hard-acks
            // (§5.3) [doc-ref] + the CoilRelay fan-out (§5.4) [doc-ref] + one hub→coil liaison per coil peer it hubs.
            // Non-hub head peers spawn none.
            coilAckSequencer <-
                if hubbedCoilPeers.isEmpty then IO.none[CoilAckSequencer.Handle]
                else
                    context
                        .actorOf(CoilAckSequencer(config, persistence, pendingConnections))
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
                        plTracer(PeerId.Coil(coilNum)),
                        persistence
                      )
                    )
                )

            connections = MultisigRegimeManager.Connections(
              blockWeaver = blockWeaver,
              blockWeaverLimiter = blockWeaverLimiter,
              cardanoLiaison = cardanoLiaison,
              consensusActor = consensusActor,
              requestSequencer = requestSequencer,
              jointLedger = jointLedger,
              stackComposer = stackComposer,
              stackComposerLimiter = stackComposerLimiter,
              slowConsensusActor = slowConsensusActor,
              headPeerLiaisons = headPeerLiaisons,
              coilRelay = coilRelay,
              coilAckSequencer = coilAckSequencer,
              coilPeerLiaisons = coilPeerLiaisons,
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
                blockWeaver = blockWeaver,
                fastConsensusActor = consensusActor,
                slowConsensusActor = slowConsensusActor,
                stackComposer = stackComposer,
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

            _ <- context.watch(blockWeaver, TerminatedChild(Actors.BlockWeaver, blockWeaver))
            _ <- headPeerLiaisons.traverse(r =>
                context.watch(r, TerminatedChild(Actors.PeerLiaisonHeadToHead, r))
            )
            _ <- coilPeerLiaisons.traverse(r =>
                context.watch(r, TerminatedChild(Actors.PeerLiaisonHeadToHead, r))
            )
            _ <- context.watch(
              cardanoLiaison,
              TerminatedChild(Actors.CardanoLiaison, cardanoLiaison)
            )
            _ <- context.watch(
              requestSequencer,
              TerminatedChild(Actors.RequestSequencer, requestSequencer)
            )
            _ <- context.watch(
              stackComposer,
              TerminatedChild(Actors.StackComposer, stackComposer)
            )
            _ <- context.watch(
              slowConsensusActor,
              TerminatedChild(Actors.SlowConsensus, slowConsensusActor)
            )
        } yield ()
}

/** Multisig regime manager starts-up and monitors all the actors of the multisig regime.
  */
object MultisigRegimeManager {
    final case class Connections(
        blockWeaver: BlockWeaver.Handle,
        /** Throttled-write handle for the FastConsensusActor → BlockWeaver lane. Other senders use
          * `blockWeaver` directly.
          */
        blockWeaverLimiter: BlockWeaver.Handle,
        cardanoLiaison: CardanoLiaison.Handle,
        consensusActor: FastConsensusActor.Handle,
        requestSequencer: RequestSequencer.Handle,
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

    def apply(
        config: NodeConfig,
        cardanoBackend: CardanoBackend[IO],
        virtualLedger: L2Ledger[IO],
        persistence: Persistence[IO],
        tracer: ContraTracer[IO, MultisigRegimeManagerEvent]
    ): IO[MultisigRegimeManager] =
        IO(
          new MultisigRegimeManager(
            config,
            cardanoBackend,
            virtualLedger,
            persistence,
            tracer
          ) {}
        )

    /** Multisig regime's protocol for actor requests and responses. See diagram:
      * [[https://app.excalidraw.com/s/9N3iw9j24UW/9eRJ7Dwu42X]]
      */
    enum Actors:
        case BlockWeaver, CardanoLiaison, Consensus, JointLedger, PeerLiaisonHeadToHead,
            RequestSequencer,
            StackComposer, SlowConsensus

    /** Requests received by the multisig regime manager. */
    type Request = PreStart.type | TerminatedChild | TerminatedDependency

    type Children = Actors

    enum Dependencies:
        case CardanoBackend, Persistence

    // ===================================
    // Multisig regime manager's messages
    // ===================================

    case object PreStart

    final case class TerminatedChild(childType: Actors, ref: NoSendActorRef[IO])

    final case class TerminatedDependency(dependencyType: Dependencies, ref: NoSendActorRef[IO])
}
