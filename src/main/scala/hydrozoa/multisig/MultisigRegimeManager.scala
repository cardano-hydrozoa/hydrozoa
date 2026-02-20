package hydrozoa.multisig

import cats.*
import cats.effect.{Deferred, IO}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.NoSendActorRef
import com.suprnation.actor.SupervisorStrategy.Escalate
import com.suprnation.actor.{OneForOneStrategy, SupervisionStrategy}
import hydrozoa.config.node.NodeConfig
import hydrozoa.lib.logging.Logging
import hydrozoa.lib.tracing.ProtocolTracer
import hydrozoa.multisig.MultisigRegimeManager.*
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.*
import hydrozoa.multisig.consensus.ack.AckBlock
import hydrozoa.multisig.consensus.peer.HeadPeerId
import hydrozoa.multisig.ledger.JointLedger
import scala.concurrent.duration.DurationInt

trait MultisigRegimeManager(config: NodeConfig, cardanoBackend: CardanoBackend[IO])
    extends Actor[IO, Request] {

    private val logger = Logging.loggerIO("hydrozoa.multisig.MultisigRegimeManager")

    override def supervisorStrategy: SupervisionStrategy[IO] =
        OneForOneStrategy[IO](maxNrOfRetries = 3, withinTimeRange = 1.minute) {
            case _: IllegalArgumentException =>
                Escalate // Normally `Stop` but we can't handle stopped actors yet
            case _: RuntimeException =>
                Escalate // Normally `Restart` but our actors can't do that yet
            case _: Exception => Escalate
        }

    override def preStart: IO[Unit] =
        for {
            pendingConnections <- Deferred[IO, MultisigRegimeManager.Connections]

            nodeId = s"head:${config.ownHeadPeerNum: Int}"
            tracer <- ProtocolTracer.jsonLines(nodeId)

            blockWeaver <- context.actorOf(BlockWeaver(config, pendingConnections))

            cardanoLiaison <-
                context.actorOf(CardanoLiaison(config, cardanoBackend, pendingConnections))

            consensusActor <- context.actorOf(ConsensusActor(config, pendingConnections))

            eventSequencer <- context.actorOf(EventSequencer(config, pendingConnections))

            jointLedger <- context.actorOf(JointLedger(config, pendingConnections))

            localPeerLiaisons <-
                config.headPeerIds
                    .filterNot(_ == config.ownHeadPeerId)
                    .traverse(pid =>
                        for {
                            localPeerLiaison <-
                                context.actorOf(PeerLiaison(config, pid, pendingConnections))
                        } yield localPeerLiaison
                    )

            _ <- pendingConnections.complete(
              MultisigRegimeManager.Connections(
                blockWeaver = blockWeaver,
                cardanoLiaison = cardanoLiaison,
                consensusActor = consensusActor,
                eventSequencer = eventSequencer,
                jointLedger = jointLedger,
                peerLiaisons = localPeerLiaisons,
                // FIXME:
                remotePeerLiaisons = ???,
                tracer = tracer,
              )
            )

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

    override def receive: Receive[IO, Request] =
        PartialFunction.fromFunction {
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

    def apply(config: NodeConfig, cardanoBackend: CardanoBackend[IO]): IO[MultisigRegimeManager] =
        IO(new MultisigRegimeManager(config, cardanoBackend) {})

    /** Multisig regime's protocol for actor requests and responses. See diagram:
      * [[https://app.excalidraw.com/s/9N3iw9j24UW/9eRJ7Dwu42X]]
      */
    enum Actors:
        case BlockWeaver, CardanoLiaison, Consensus, JointLedger, PeerLiaison, EventSequencer

    /** Requests received by the multisig regime manager. */
    type Request = TerminatedChild | TerminatedDependency

    type Children = Actors

    enum Dependencies:
        case CardanoBackend, Persistence

    /** ==Multisig regime manager's messages== */
    final case class TerminatedChild(childType: Actors, ref: NoSendActorRef[IO])

    final case class TerminatedDependency(dependencyType: Dependencies, ref: NoSendActorRef[IO])
}
