package hydrozoa.multisig

import cats.*
import cats.effect.{Deferred, IO}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.NoSendActorRef
import com.suprnation.actor.SupervisorStrategy.Escalate
import com.suprnation.actor.{OneForOneStrategy, SupervisionStrategy}
import hydrozoa.config.node.NodeConfig
import hydrozoa.lib.logging.{Logging, Tracer}
import hydrozoa.lib.tracing.ProtocolTracer
import hydrozoa.multisig.MultisigRegimeManager.*
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.*
import hydrozoa.multisig.consensus.ack.AckBlock
import hydrozoa.multisig.consensus.peer.HeadPeerId
import hydrozoa.multisig.ledger.joint.JointLedger
import hydrozoa.multisig.ledger.l2.L2Ledger
import scala.concurrent.duration.DurationInt

trait MultisigRegimeManager(
    config: NodeConfig,
    cardanoBackend: CardanoBackend[IO],
    l2Ledger: L2Ledger[IO]
) extends Actor[IO, Request] {

    private val logger = Logging.loggerIO("hydrozoa.multisig.MultisigRegimeManager")

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
                    logger.warn("Terminated block weaver actor")
                case Actors.CardanoLiaison =>
                    logger.warn("Terminated Cardano liaison actor")
                case Actors.Consensus =>
                    logger.warn("Terminated consensus actor")
                case Actors.JointLedger =>
                    logger.warn("Terminated joint ledger actor")
                case Actors.PeerLiaison =>
                    logger.warn("Terminated peer liaison actor")
                case Actors.EventSequencer =>
                    logger.warn("Terminated event sequencer actor")
            }
        case TerminatedDependency(dependencyType, _) =>
            dependencyType match {
                case Dependencies.CardanoBackend =>
                    logger.warn("Terminated cardano backend")
                case Dependencies.Persistence =>
                    logger.warn("Terminated persistence")
            }
        // TODO: Implement a way to receive a remote comm actor and connect it to its corresponding local comm actor
    }

    def preStartLocal: IO[Unit] =
        for {
            pendingConnections <- Deferred[IO, MultisigRegimeManager.Connections]

            nodeId = s"head:${config.ownHeadPeerNum: Int}"
            tracer <- ProtocolTracer.jsonLines(nodeId)
            _ <- tracer.traceError(0, "foo", "bar")

            _ <- logger.info("Starting multisig actors...")

            blockWeaver <- context.actorOf(BlockWeaver(config, pendingConnections))

            cardanoLiaisonTracer <- Tracer.makeLocal("CardanoLiaison")
            cardanoLiaison <-
                context.actorOf(
                  CardanoLiaison(config, cardanoBackend, pendingConnections, cardanoLiaisonTracer)
                )

            consensusActor <- context.actorOf(ConsensusActor(config, pendingConnections))

            eventSequencer <- context.actorOf(EventSequencer(config, pendingConnections))

            jointLedgerTracerLocal <- Tracer.makeLocal("JointLedger")
            jointLedger <- context.actorOf(
              JointLedger(config, pendingConnections, l2Ledger, tracer, jointLedgerTracerLocal)
            )

            localPeerLiaisons <-
                config.headPeerIds
                    .filterNot(_ == config.ownHeadPeerId)
                    .traverse(pid =>
                        for {
                            localPeerLiaison <-
                                context.actorOf(PeerLiaison(config, pid, pendingConnections))
                        } yield localPeerLiaison
                    )

            connections = MultisigRegimeManager.Connections(
              blockWeaver = blockWeaver,
              cardanoLiaison = cardanoLiaison,
              consensusActor = consensusActor,
              eventSequencer = eventSequencer,
              jointLedger = jointLedger,
              peerLiaisons = localPeerLiaisons,
              remotePeerLiaisons = Map.empty,
              tracer = tracer,
            )

            _ <- pendingConnections.complete(connections)
            _ <- connectionsDeferred.complete(connections)

            _ <- logger.info("Watching multisig actors...")

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
        } yield ()
}

/** Multisig regime manager starts-up and monitors all the actors of the multisig regime.
  */
object MultisigRegimeManager {
    final case class Connections(
        blockWeaver: BlockWeaver.Handle,
        cardanoLiaison: CardanoLiaison.Handle,
        consensusActor: ConsensusActor.Handle,
        eventSequencer: EventSequencer.Handle,
        jointLedger: JointLedger.Handle,
        peerLiaisons: List[PeerLiaison.Handle],
        remotePeerLiaisons: Map[HeadPeerId, PeerLiaison.Handle],
        tracer: ProtocolTracer = ProtocolTracer.noop,
    )

    type PendingConnections = Deferred[IO, Connections]

    def apply(
        config: NodeConfig,
        cardanoBackend: CardanoBackend[IO],
        virtualLedger: L2Ledger[IO]
    ): IO[MultisigRegimeManager] =
        IO(new MultisigRegimeManager(config, cardanoBackend, virtualLedger) {})

    /** Multisig regime's protocol for actor requests and responses. See diagram:
      * [[https://app.excalidraw.com/s/9N3iw9j24UW/9eRJ7Dwu42X]]
      */
    enum Actors:
        case BlockWeaver, CardanoLiaison, Consensus, JointLedger, PeerLiaison, EventSequencer

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
