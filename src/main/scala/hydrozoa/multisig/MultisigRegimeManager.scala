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
import hydrozoa.multisig.consensus.limiter.{Limiter, LimiterEvent}
import hydrozoa.multisig.consensus.peer.HeadPeerId
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
    private def plTracer(pid: HeadPeerId): ContraTracer[IO, PeerLiaisonEvent] =
        tracer.contramap(MultisigRegimeManagerEvent.PL(pid, _))
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
            // PeerLiaison, …) keep direct refs.
            blockWeaverLimiter <- context.actorOf(
              Limiter[BlockWeaver.Request](blockWeaver, config, bwlTracer)
            )

            cardanoLiaison <-
                context.actorOf(
                  CardanoLiaison(config, cardanoBackend, pendingConnections, clTracer)
                )

            consensusActor <- context.actorOf(
              FastConsensusActor(config, pendingConnections, fcaTracer, persistence)
            )

            eventSequencer <- context.actorOf(
              EventSequencer(config, pendingConnections, esTracer, persistence)
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

            localPeerLiaisons <-
                config.headPeerIds
                    .filterNot(_ == config.ownHeadPeerId)
                    .traverse(pid =>
                        for {
                            localPeerLiaison <-
                                context.actorOf(
                                  PeerLiaison(
                                    config,
                                    pid,
                                    pendingConnections,
                                    plTracer(pid),
                                    persistence
                                  )
                                )
                        } yield localPeerLiaison
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
              peerLiaisons = localPeerLiaisons,
              remotePeerLiaisons = Map.empty,
            )

            _ <- pendingConnections.complete(connections)
            _ <- connectionsDeferred.complete(connections)

            _ <- tracer.traceWith(WatchingActors)

            _ <- context.watch(blockWeaver, TerminatedChild(Actors.BlockWeaver, blockWeaver))
            _ <- localPeerLiaisons.traverse(r =>
                context.watch(r, TerminatedChild(Actors.PeerLiaison, r))
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
        peerLiaisons: List[PeerLiaison.Handle],
        remotePeerLiaisons: Map[HeadPeerId, PeerLiaison.Handle],
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
        case BlockWeaver, CardanoLiaison, Consensus, JointLedger, PeerLiaison, EventSequencer,
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
