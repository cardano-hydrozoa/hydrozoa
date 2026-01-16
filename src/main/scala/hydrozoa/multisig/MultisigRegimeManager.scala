package hydrozoa.multisig

import cats.*
import cats.effect.{Deferred, IO}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.SupervisorStrategy.Escalate
import com.suprnation.actor.{OneForOneStrategy, SupervisionStrategy}
import hydrozoa.multisig.MultisigRegimeManager.Config
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.*
import hydrozoa.multisig.ledger.JointLedger
import hydrozoa.multisig.ledger.dapp.tx.{FallbackTx, InitializationTx}
import hydrozoa.multisig.protocol.ConsensusProtocol
import hydrozoa.multisig.protocol.ConsensusProtocol.Actors
import hydrozoa.multisig.protocol.ManagerProtocol.Manager.*
import hydrozoa.multisig.protocol.types.Peer
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps
import scalus.cardano.ledger.SlotConfig

/** Multisig regime manager starts-up and monitors all the actors of the multisig regime.
  */
object MultisigRegimeManager {
    final case class Config(
        peerId: Peer.Number,
        peers: List[Peer.Number],
        cardanoBackend: CardanoBackend[IO],
        initializationTx: InitializationTx,
        fallbackTx: FallbackTx,
        slotConfig: SlotConfig
    )

    def apply(config: Config): IO[MultisigRegimeManager] =
        IO(new MultisigRegimeManager(config) {})
}

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

            pendingBlockWeaver <- Deferred[IO, BlockWeaver.Handle]
            pendingLocalPeerLiaisons <- Deferred[IO, List[PeerLiaison.Handle]]
            pendingCardanoLiaison <- Deferred[IO, CardanoLiaison.Handle]
            pendingEventSequencer <- Deferred[IO, ConsensusProtocol.EventSequencer.Ref]
            pendingJointLedger <- Deferred[IO, JointLedger.Handle]
            pendingConsensusActor <- Deferred[IO, ConsensusActor.Handle]

            blockWeaver <- {
                import BlockWeaver.Config
                context.actorOf(
                  BlockWeaver(
                    Config(
                      lastKnownBlock = ???,
                      peerId = config.peerId,
                      numberOfPeers = ???,
                      blockLeadTurn = ???,
                      recoveredMempool = BlockWeaver.Mempool.empty,
                      jointLedger = ???,
                      slotConfig = ???
                    )
                  )
                )
            }

            localPeerLiaisonsPendingRemoteActors <- {
                import PeerLiaison.{Config, ConnectionsPending}
                config.peers
                    .filterNot(_ == config.peerId)
                    .traverse(pid =>
                        for {
                            pendingRemotePeerLiaison <- Deferred[IO, PeerLiaison.Handle]
                            localPeerLiaison <- context.actorOf(
                              PeerLiaison(
                                Config(
                                  ownPeerId = config.peerId,
                                  remotePeerId = pid
                                ),
                                ConnectionsPending(
                                  blockWeaver = pendingBlockWeaver,
                                  consensusActor = pendingConsensusActor,
                                  remotePeerLiaison = pendingRemotePeerLiaison
                                )
                              )
                            )
                        } yield (localPeerLiaison, pendingRemotePeerLiaison)
                    )
            }

            localPeerLiaisons = localPeerLiaisonsPendingRemoteActors.map(_._1)

            cardanoLiaison <- {
                import CardanoLiaison.Config
                context.actorOf(
                  CardanoLiaison(
                    Config(
                      cardanoBackend = config.cardanoBackend,
                      initializationTx = config.initializationTx,
                      initializationFallbackTx = config.fallbackTx,
                      receiveTimeout = 10.seconds,
                      slotConfig = config.slotConfig,
                      blockWeaver = blockWeaver
                    )
                  )
                )
            }

            transactionSequencer <- {
                import EventSequencer.{Config, ConnectionsPending}
                context.actorOf(
                  EventSequencer(
                    Config(peerId = config.peerId),
                    ConnectionsPending(
                      blockWeaver = pendingBlockWeaver,
                      peerLiaisons = pendingLocalPeerLiaisons
                    )
                  )
                )
            }

            _ <- pendingBlockWeaver.complete(blockWeaver)
            _ <- pendingLocalPeerLiaisons.complete(localPeerLiaisons)
            _ <- pendingCardanoLiaison.complete(cardanoLiaison)
            _ <- pendingEventSequencer.complete(transactionSequencer)

            _ <- context.watch(blockWeaver, TerminatedChild(Actors.BlockWeaver, blockWeaver))
            _ <- localPeerLiaisons.traverse(r =>
                context.watch(r, TerminatedChild(Actors.PeerLiaison, r))
            )
            _ <- context.watch(
              cardanoLiaison,
              TerminatedChild(Actors.CardanoLiaison, cardanoLiaison)
            )
            _ <- context.watch(
              transactionSequencer,
              TerminatedChild(Actors.EventSequencer, transactionSequencer)
            )

            // TODO: Store the deferred remote comm actor refs (cas._2) for later
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
