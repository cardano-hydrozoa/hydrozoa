package hydrozoa.multisig

import cats.*
import cats.effect.{Deferred, IO, IOLocal}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.NoSendActorRef
import com.suprnation.actor.SupervisorStrategy.Escalate
import com.suprnation.actor.{OneForOneStrategy, SupervisionStrategy}
import hydrozoa.config.node.NodeConfig
import hydrozoa.lib.logging.Tracer
import hydrozoa.lib.tracing.ProtocolTracer
import hydrozoa.multisig.MultisigRegimeManager.*
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.*
import hydrozoa.multisig.consensus.limiter.Limiter
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.joint.JointLedger
import hydrozoa.multisig.ledger.l2.L2Ledger
import scala.concurrent.duration.DurationInt

trait MultisigRegimeManager(
    config: NodeConfig,
    cardanoBackend: CardanoBackend[IO],
    l2Ledger: L2Ledger[IO],
    tracerLocal: IOLocal[Tracer]
) extends Actor[IO, Request] {

    given IOLocal[Tracer] = tracerLocal

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
            childType match {
                case Actors.BlockWeaver =>
                    Tracer.warn("Terminated block weaver actor")
                case Actors.CardanoLiaison =>
                    Tracer.warn("Terminated Cardano liaison actor")
                case Actors.Consensus =>
                    Tracer.warn("Terminated consensus actor")
                case Actors.JointLedger =>
                    Tracer.warn("Terminated joint ledger actor")
                case Actors.PeerLiaisonHeadToHead =>
                    Tracer.warn("Terminated peer liaison actor")
                case Actors.EventSequencer =>
                    Tracer.warn("Terminated event sequencer actor")
                case Actors.StackComposer =>
                    Tracer.warn("Terminated stack composer actor")
                case Actors.SlowConsensus =>
                    Tracer.warn("Terminated slow consensus actor")
            }
        case TerminatedDependency(dependencyType, _) =>
            dependencyType match {
                case Dependencies.CardanoBackend =>
                    Tracer.warn("Terminated cardano backend")
                case Dependencies.Persistence =>
                    Tracer.warn("Terminated persistence")
            }
        // TODO: Implement a way to receive a remote comm actor and connect it to its corresponding local comm actor
    }

    def preStartLocal: IO[Unit] =
        for {
            _ <- Tracer.routeLocal("MultisigRegimeManager")
            _ <- Tracer.updateLocalCtx("peer" -> config.ownPeerLabel)
            _ <- Tracer.info("Starting multisig actors...")

            nodeId = s"head:${config.ownPeerIndex}"
            tracer <- ProtocolTracer.jsonLines(nodeId)

            pendingConnections <- Deferred[IO, MultisigRegimeManager.Connections]

            blockWeaver <- context.actorOf(BlockWeaver(config, pendingConnections, tracerLocal))

            // Throttles the FastConsensusActor → BlockWeaver soft-block-confirmation lane (see
            // hydrozoa.multisig.consensus.limiter.Limiter). Only the consensus actor's reference
            // to BlockWeaver is routed through this limiter; other senders (JointLedger,
            // PeerLiaisonHeadToHead, …) keep direct refs.
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

            eventSequencer <- context.actorOf(EventSequencer(config, pendingConnections))

            jointLedger <- context.actorOf(
              JointLedger(config, pendingConnections, l2Ledger, tracer, tracerLocal)
            )

            stackComposer <- context.actorOf(
              StackComposer(config, pendingConnections, tracerLocal)
            )

            // Throttles the SlowConsensusActor → StackComposer hard-stack-confirmation lane.
            stackComposerLimiter <- context.actorOf(
              Limiter[StackComposer.Request](stackComposer, config, tracerLocal)
            )

            slowConsensusActor <- context.actorOf(
              SlowConsensusActor(config, pendingConnections, tracerLocal)
            )

            // One peer liaison toward every other head peer (the head mesh). The liaisons project
            // their connections from the shared `Connections`; the remote-handle maps stay empty in
            // this production placeholder (in-process wiring fills them — see stage4).
            headPeerLiaisons <-
                config.headPeerIds
                    .filterNot(id => config.ownPeerId == PeerId.Head(id.peerNum))
                    .traverse(pid =>
                        context.actorOf(
                          liaison.PeerLiaisonHeadToHead(config, pid, pendingConnections)
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
            // (§8.4) + the CoilRelay fan-out (§8.3) + one hub→coil liaison per coil peer it hubs.
            // Non-hub head peers spawn none.
            coilAckSequencer <-
                if hubbedCoilPeers.isEmpty then IO.none[CoilAckSequencer.Handle]
                else context.actorOf(CoilAckSequencer(config, pendingConnections)).map(Some(_))

            coilRelay <-
                if hubbedCoilPeers.isEmpty then IO.none[liaison.CoilRelay.Handle]
                else context.actorOf(liaison.CoilRelay(pendingConnections)).map(Some(_))

            coilPeerLiaisons <-
                hubbedCoilPeers.traverse(coilNum =>
                    context.actorOf(
                      liaison.PeerLiaisonHubToCoil(config, coilNum, pendingConnections)
                    )
                )

            connections = MultisigRegimeManager.Connections(
              blockWeaver = blockWeaver,
              blockWeaverLimiter = blockWeaverLimiter,
              cardanoLiaison = cardanoLiaison,
              consensusActor = consensusActor,
              eventSequencer = eventSequencer,
              jointLedger = jointLedger,
              stackComposer = stackComposer,
              stackComposerLimiter = stackComposerLimiter,
              slowConsensusActor = slowConsensusActor,
              headPeerLiaisons = headPeerLiaisons,
              coilRelay = coilRelay,
              coilAckSequencer = coilAckSequencer,
              coilPeerLiaisons = coilPeerLiaisons,
              tracer = tracer,
            )

            _ <- pendingConnections.complete(connections)
            _ <- connectionsDeferred.complete(connections)

            _ <- Tracer.info("Watching multisig actors...")

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
              eventSequencer,
              TerminatedChild(Actors.EventSequencer, eventSequencer)
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
        eventSequencer: EventSequencer.Handle,
        jointLedger: JointLedger.Handle,
        stackComposer: StackComposer.Handle,
        /** Throttled-write handle for the SlowConsensusActor → StackComposer lane. */
        stackComposerLimiter: StackComposer.Handle,
        slowConsensusActor: SlowConsensusActor.Handle,
        // ---- Producer broadcast targets (§8) ----
        /** Head-peer-mesh liaisons (one per other head peer); empty on a coil peer. Producers
          * broadcast their own artifacts here. `ActorRef` is contravariant in its message type, so
          * a handle is usable as `ActorRef[IO, <any artifact in its Request>]`.
          */
        headPeerLiaisons: List[liaison.PeerLiaisonHeadToHead.Handle] = Nil,
        /** A coil peer's single uplink to its hub; `None` on a head peer. `SlowConsensusActor`
          * broadcasts its own hard-ack to `headPeerLiaisons ++ coilUplink`.
          */
        coilUplink: Option[liaison.PeerLiaisonCoilToHub.Handle] = None,
        /** Present only on a hub head peer (§8.3): the fan-out that relays the population to its
          * coil peers. Producers send only their own production here. `None` elsewhere.
          */
        coilRelay: Option[liaison.CoilRelay.Handle] = None,
        // ---- Remote-handle resolution for spawned liaisons (in-process only) ----
        // Only the in-process harness (stage4 / unit tests) populates these; in a real deployment
        // the counterpart is another process reached over the transport, so they stay empty.
        remoteHeadLiaisons: Map[HeadPeerNumber, liaison.PeerLiaisonHeadToHead.Handle] = Map.empty,
        remoteCoilLiaisons: Map[CoilPeerNumber, liaison.PeerLiaisonCoilToHub.Handle] = Map.empty,
        remoteHubLiaison: Option[liaison.PeerLiaisonHubToCoil.Handle] = None,
        /** Present only on a hub head peer (§8): re-sequences its coil peers' hard-acks onto the
          * `HubHardAckLane`. `None` elsewhere.
          */
        coilAckSequencer: Option[CoilAckSequencer.Handle] = None,
        /** Hub→coil liaisons this hub runs (for `CoilRelay`'s fan-out); empty elsewhere. */
        coilPeerLiaisons: List[liaison.PeerLiaisonHubToCoil.Handle] = Nil,
        tracer: ProtocolTracer = ProtocolTracer.noop,
    )

    type PendingConnections = Deferred[IO, Connections]

    def apply(
        config: NodeConfig,
        cardanoBackend: CardanoBackend[IO],
        virtualLedger: L2Ledger[IO],
        tracerLocal: IOLocal[Tracer]
    ): IO[MultisigRegimeManager] =
        IO(new MultisigRegimeManager(config, cardanoBackend, virtualLedger, tracerLocal) {})

    /** Multisig regime's protocol for actor requests and responses. See diagram:
      * [[https://app.excalidraw.com/s/9N3iw9j24UW/9eRJ7Dwu42X]]
      */
    enum Actors:
        case BlockWeaver, CardanoLiaison, Consensus, JointLedger, PeerLiaisonHeadToHead,
            EventSequencer,
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
