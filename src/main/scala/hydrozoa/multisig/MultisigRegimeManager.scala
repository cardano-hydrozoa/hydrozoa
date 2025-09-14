package hydrozoa.multisig

import cats.*
import cats.effect.{Deferred, IO}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.{OneForOneStrategy, SupervisionStrategy}
import com.suprnation.actor.SupervisorStrategy.Escalate
import hydrozoa.multisig.consensus.*
import hydrozoa.multisig.protocol.ManagerProtocol.Manager.*
import hydrozoa.multisig.protocol.Identifiers.*
import hydrozoa.multisig.protocol.ConsensusProtocol
import hydrozoa.multisig.protocol.CardanoBackendProtocol.*
import hydrozoa.multisig.protocol.ConsensusProtocol.Actors
import hydrozoa.multisig.protocol.PersistenceProtocol.*

import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

import MultisigRegimeManager.Config

/** Multisig regime manager starts-up and monitors all the actors of the multisig regime.
  */
object MultisigRegimeManager {
    final case class Config(
        peerId: PeerId,
        peers: List[PeerId],
        cardanoBackend: CardanoBackend.Ref,
        persistence: Persistence.Ref
    )

    def create(config: Config): IO[MultisigRegimeManager] =
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
            _ <- context.watch(
              config.cardanoBackend,
              TerminatedDependency(Dependencies.CardanoBackend, config.cardanoBackend)
            )
            _ <- context.watch(
              config.persistence,
              TerminatedDependency(Dependencies.Persistence, config.persistence)
            )

            pendingBlockProducer <- Deferred[IO, ConsensusProtocol.BlockProducer.Ref]
            pendingLocalPeerLiaisons <- Deferred[IO, List[ConsensusProtocol.PeerLiaison.Ref]]
            pendingCardanoLiaison <- Deferred[IO, ConsensusProtocol.CardanoLiaison.Ref]
            pendingTransactionSequencer <- Deferred[IO, ConsensusProtocol.TransactionSequencer.Ref]

            blockProducer <- {
                import BlockProducer.{Config, ConnectionsPending}
                context.actorOf(
                  BlockProducer(
                    Config(peerId = config.peerId, persistence = config.persistence),
                    ConnectionsPending(
                      cardanoLiaison = pendingCardanoLiaison,
                      peerLiaisons = pendingLocalPeerLiaisons,
                      transactionSequencer = pendingTransactionSequencer
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
                            pendingRemotePeerLiaison <- Deferred[
                              IO,
                              ConsensusProtocol.PeerLiaison.Ref
                            ]
                            localPeerLiaison <- context.actorOf(
                              PeerLiaison(
                                Config(
                                  peerId = config.peerId,
                                  remotePeerId = pid,
                                  persistence = config.persistence
                                ),
                                ConnectionsPending(
                                  blockProducer = pendingBlockProducer,
                                  remotePeerLiaison = pendingRemotePeerLiaison
                                )
                              )
                            )
                        } yield (localPeerLiaison, pendingRemotePeerLiaison)
                    )
            }

            localPeerLiaisons = localPeerLiaisonsPendingRemoteActors.map(_._1)

            cardanoLiaison <- {
                import CardanoLiaison.{Config, ConnectionsPending}
                context.actorOf(
                  CardanoLiaison(
                    Config(
                      persistence = config.persistence,
                      cardanoBackend = config.cardanoBackend
                    ),
                    ConnectionsPending(
                    )
                  )
                )
            }

            transactionSequencer <- {
                import TransactionSequencer.{Config, ConnectionsPending}
                context.actorOf(
                  TransactionSequencer(
                    Config(peerId = config.peerId, persistence = config.persistence),
                    ConnectionsPending(
                      blockProducer = pendingBlockProducer,
                      peerLiaisons = pendingLocalPeerLiaisons
                    )
                  )
                )
            }

            _ <- pendingBlockProducer.complete(blockProducer)
            _ <- pendingLocalPeerLiaisons.complete(localPeerLiaisons)
            _ <- pendingCardanoLiaison.complete(cardanoLiaison)
            _ <- pendingTransactionSequencer.complete(transactionSequencer)

            _ <- context.watch(blockProducer, TerminatedChild(Actors.BlockProducer, blockProducer))
            _ <- localPeerLiaisons.traverse(r =>
                context.watch(r, TerminatedChild(Actors.PeerLiaison, r))
            )
            _ <- context.watch(
              cardanoLiaison,
              TerminatedChild(Actors.CardanoLiaison, cardanoLiaison)
            )
            _ <- context.watch(
              transactionSequencer,
              TerminatedChild(Actors.TransactionSequencer, transactionSequencer)
            )

            // TODO: Store the deferred remote comm actor refs (cas._2) for later
        } yield ()

    override def receive: Receive[IO, Request] =
        PartialFunction.fromFunction({
            case TerminatedChild(childType, _) =>
                childType match {
                    case Actors.BlockProducer =>
                        IO.println("Terminated block actor")
                    case Actors.CardanoLiaison =>
                        IO.println("Terminated Cardano event actor")
                    case Actors.PeerLiaison =>
                        IO.println("Terminated comm actor")
                    case Actors.TransactionSequencer =>
                        IO.println("Terminated ledger event actor")
                }
            case TerminatedDependency(dependencyType, _) =>
                dependencyType match {
                    case Dependencies.CardanoBackend =>
                        IO.println("Terminated cardano backend")
                    case Dependencies.Persistence =>
                        IO.println("Terminated persistence")
                }
            // TODO: Implement a way to receive a remote comm actor and connect it to its corresponding local comm actor
        })

}
