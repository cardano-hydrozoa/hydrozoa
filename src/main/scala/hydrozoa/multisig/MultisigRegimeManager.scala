package hydrozoa.multisig

import cats.*
import cats.effect.{Deferred, IO}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.NoSendActorRef
import com.suprnation.actor.SupervisorStrategy.Escalate
import com.suprnation.actor.{OneForOneStrategy, SupervisionStrategy}
import hydrozoa.multisig.MultisigRegimeManager.*
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.*
import hydrozoa.multisig.consensus.ack.AckBlock
import hydrozoa.multisig.consensus.peer.PeerId
import hydrozoa.multisig.ledger.JointLedger
import hydrozoa.multisig.ledger.dapp.tx.{FallbackTx, InitializationTx}
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps
import scalus.cardano.ledger.SlotConfig

trait MultisigRegimeManager(config: Config) extends Actor[IO, Request] {

    override def supervisorStrategy: SupervisionStrategy[IO] =
        OneForOneStrategy[IO](maxNrOfRetries = 3, withinTimeRange = 1 minute) {
            case _: IllegalArgumentException =>
                Escalate // Normally `Stop` but we can't handle stopped actors yet
            case _: RuntimeException =>
                Escalate // Normally `Restart` but our actors can't do that yet
            case _: Exception => Escalate
        }

    override def preStart: IO[Unit] =
        for {
            pendingConnections <- Deferred[IO, MultisigRegimeManager.Connections]

            blockWeaver <-
                context.actorOf(
                  BlockWeaver(
                    BlockWeaver.Config(
                      lastKnownBlock = ???,
                      peerId = config.peerId,
                      recoveredMempool = BlockWeaver.Mempool.empty,
                      slotConfig = ???
                    ),
                    pendingConnections
                  )
                )

            cardanoLiaison <-
                context.actorOf(
                  CardanoLiaison(
                    CardanoLiaison.Config(
                      cardanoBackend = config.cardanoBackend,
                      initializationTx = config.initializationTx,
                      initializationFallbackTx = config.fallbackTx,
                      receiveTimeout = 10.seconds,
                      slotConfig = config.slotConfig
                    ),
                    pendingConnections
                  )
                )

            consensusActor <- context.actorOf(
              ConsensusActor(
                ConsensusActor.Config(
                  peerId = ???,
                  verificationKeys = ???,
                  recoveredRequests = ???
                ),
                pendingConnections
              )
            )

            eventSequencer <- context.actorOf(
              EventSequencer(
                EventSequencer.Config(peerId = config.peerId),
                pendingConnections
              )
            )

            // FIXME
            jointLedger <- context.actorOf(???)

            localPeerLiaisons <-
                config.peers
                    .filterNot(_ == config.peerId)
                    .traverse(pid =>
                        for {
                            localPeerLiaison <- context.actorOf(
                              PeerLiaison(
                                PeerLiaison.Config(
                                  ownPeerId = config.peerId,
                                  remotePeerId = pid
                                ),
                                pendingConnections
                              )
                            )
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
                remotePeerLiaisons = ???
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
                        IO.println("Terminated block weaver actor")
                    case Actors.CardanoLiaison =>
                        IO.println("Terminated Cardano liaison actor")
                    case Actors.Consensus =>
                        IO.println("Terminated consensus actor")
                    case Actors.JointLedger =>
                        IO.println("Terminated joint ledger actor")
                    case Actors.PeerLiaison =>
                        IO.println("Terminated peer liaison actor")
                    case Actors.EventSequencer =>
                        IO.println("Terminated event sequencer actor")
                }
            case TerminatedDependency(dependencyType, _) =>
                dependencyType match {
                    case Dependencies.CardanoBackend =>
                        IO.println("Terminated cardano backend")
                    case Dependencies.Persistence =>
                        IO.println("Terminated persistence")
                }
            // TODO: Implement a way to receive a remote comm actor and connect it to its corresponding local comm actor
        }

}

/** Multisig regime manager starts-up and monitors all the actors of the multisig regime.
  */
object MultisigRegimeManager {
    final case class Config(
        peerId: PeerId,
        peers: List[PeerId],
        cardanoBackend: CardanoBackend[IO],
        initializationTx: InitializationTx,
        fallbackTx: FallbackTx,
        slotConfig: SlotConfig
    )

    final case class Connections(
        blockWeaver: BlockWeaver.Handle,
        cardanoLiaison: CardanoLiaison.Handle,
        consensusActor: ConsensusActor.Handle,
        eventSequencer: EventSequencer.Handle,
        jointLedger: JointLedger.Handle,
        peerLiaisons: List[PeerLiaison.Handle],
        remotePeerLiaisons: Map[PeerId, PeerLiaison.Handle],
    )

    type PendingConnections = Deferred[IO, Connections]

    def apply(config: Config): IO[MultisigRegimeManager] =
        IO(new MultisigRegimeManager(config) {})

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
