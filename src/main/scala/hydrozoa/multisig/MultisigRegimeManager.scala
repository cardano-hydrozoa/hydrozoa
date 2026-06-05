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
import hydrozoa.multisig.consensus.peer.PeerId
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

            // One peer liaison toward every other head peer (the head mesh).
            headPeerLiaisons <-
                config.headPeerIds
                    .filterNot(id => config.ownPeerId == PeerId.Head(id.peerNum))
                    .traverse(pid =>
                        context.actorOf(
                          PeerLiaisonHeadToHead(config, pid, pendingConnections)
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
            // (§8) plus one head→coil liaison per coil peer it hubs. Non-hub head peers spawn
            // neither.
            coilAckSequencer <-
                if hubbedCoilPeers.isEmpty then IO.none[CoilAckSequencer.Handle]
                else context.actorOf(CoilAckSequencer(config, pendingConnections)).map(Some(_))

            coilLinkRelay <-
                if hubbedCoilPeers.isEmpty then IO.none[CoilLinkRelay.Handle]
                else context.actorOf(CoilLinkRelay(config, pendingConnections)).map(Some(_))

            coilPeerLiaisons <-
                hubbedCoilPeers.traverse(coilNum =>
                    context.actorOf(
                      PeerLiaisonHeadToCoil(config, coilNum, pendingConnections)
                    )
                )

            // Watched together, but wired separately: the head mesh vs the coil-ward liaisons.
            localPeerLiaisons = headPeerLiaisons ++ coilPeerLiaisons

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
              coilPeerLiaisons = coilPeerLiaisons,
              coilAckSequencer = coilAckSequencer,
              coilLinkRelay = coilLinkRelay,
              tracer = tracer,
            )

            _ <- pendingConnections.complete(connections)
            _ <- connectionsDeferred.complete(connections)

            _ <- Tracer.info("Watching multisig actors...")

            _ <- context.watch(blockWeaver, TerminatedChild(Actors.BlockWeaver, blockWeaver))
            _ <- localPeerLiaisons.traverse(r =>
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
        /** Head-peer-mesh liaisons (one per other head peer). */
        headPeerLiaisons: List[PeerLiaisonHeadToHead.Handle],
        /** In-process map from a remote peer's id to the local ActorRef of its counterpart liaison.
          * Only the in-process harness (stage4 / unit tests) populates it; in a real deployment the
          * counterpart is another process reached over the transport, so it stays empty.
          */
        remotePeerLiaisons: Map[PeerId, PeerLiaisonHeadToHead.Handle] = Map.empty,
        /** Hub→coil liaisons (one per coil peer this head peer hubs); empty on non-hub head peers
          * and on coil peers. The hub feed (briefs + the relayed ack stream) is fanned to these,
          * separately from the head-peer mesh (§8).
          */
        coilPeerLiaisons: List[PeerLiaisonHeadToHead.Handle] = Nil,
        /** Present only on a hub head peer (§8): the relay sequencer for its coil peers' hard-acks
          * (onto the head-peer mesh's `HubHardAckLane`). `None` on non-hub head peers and on coil
          * peers.
          */
        coilAckSequencer: Option[CoilAckSequencer.Handle] = None,
        /** Present only on a hub head peer (§8): the relay that fans the population's
          * soft/hard-acks down to its coil peers (the coil-link `relayedMsg` lane). `None`
          * elsewhere.
          */
        coilLinkRelay: Option[CoilLinkRelay.Handle] = None,
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
