package hydrozoa.multisig

import cats.*
import cats.effect.{Deferred, IO, Ref}
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

/** Multisig regime manager starts-up and monitors all the actors of the multisig regime.
  */
object MultisigRegimeManager {
    def create(
        peerId: PeerId,
        peers: List[PeerId],
        cba: CardanoBackend.Ref,
        per: Persistence.Ref
    ): IO[MultisigRegimeManager] =
        for {
            cardanoBackend <- Ref[IO].of(cba)
            persistence <- Ref[IO].of(per)
        } yield MultisigRegimeManager(peerId, peers)(cardanoBackend, persistence)
}

final case class MultisigRegimeManager(peerId: PeerId, peers: List[PeerId])(
    private val cardanoBackendRef: Ref[IO, CardanoBackend.Ref],
    private val persistenceRef: Ref[IO, Persistence.Ref]
) extends Actor[IO, Request] {

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
            cardanoBackend <- cardanoBackendRef.get
            persistence <- persistenceRef.get

            _ <- context.watch(
              cardanoBackend,
              TerminatedDependency(Dependencies.CardanoBackend, cardanoBackend)
            )
            _ <- context.watch(
              persistence,
              TerminatedDependency(Dependencies.Persistence, persistence)
            )

            pendingCardanoBackend <- Deferred[IO, CardanoBackend.Ref]
            pendingPersistence <- Deferred[IO, Persistence.Ref]
            pendingBlockProducer <- Deferred[IO, ConsensusProtocol.BlockProducer.Ref]
            pendingLocalPeerLiaisons <- Deferred[IO, List[ConsensusProtocol.PeerLiaison.Ref]]
            pendingCardanoLiaison <- Deferred[IO, ConsensusProtocol.CardanoLiaison.Ref]
            pendingTransactionSequencer <- Deferred[IO, ConsensusProtocol.TransactionSequencer.Ref]

            _ <- pendingCardanoBackend.complete(cardanoBackend)
            _ <- pendingPersistence.complete(persistence)

            blockProducer <- context.actorOf(
              BlockProducer.create(
                BlockProducer.Config(peerId),
                BlockProducer.ConnectionsPending(
                  cardanoLiaison = pendingCardanoLiaison,
                  peerLiaisons = pendingLocalPeerLiaisons,
                  transactionSequencer = pendingTransactionSequencer,
                  persistence = pendingPersistence
                )
              )
            )

            localPeerLiaisonsPendingRemoteActors <- peers
                .filterNot(_ == peerId)
                .traverse(pid =>
                    for {
                        pendingRemotePeerLiaison <- Deferred[IO, ConsensusProtocol.PeerLiaison.Ref]
                        localPeerLiaison <- context.actorOf(
                          PeerLiaison.create(
                            PeerLiaison.Config(peerId, pid),
                            PeerLiaison.ConnectionsPending(
                              blockProducer = pendingBlockProducer,
                              persistence = pendingPersistence,
                              remotePeerLiaison = pendingRemotePeerLiaison
                            )
                          )
                        )
                    } yield (localPeerLiaison, pendingRemotePeerLiaison)
                )

            localPeerLiaisons = localPeerLiaisonsPendingRemoteActors.map(_._1)

            cardanoLiaison <- context.actorOf(
              CardanoLiaison.create(
                CardanoLiaison.Config(),
                CardanoLiaison.ConnectionsPending(
                  cardanoBackend = pendingCardanoBackend,
                  persistence = pendingPersistence
                )
              )
            )

            transactionSequencer <- context.actorOf(
              TransactionSequencer.create(
                TransactionSequencer.Config(peerId),
                TransactionSequencer.ConnectionsPending(
                  blockProducer = pendingBlockProducer,
                  peerLiaisons = pendingLocalPeerLiaisons,
                  persistence = pendingPersistence
                )
              )
            )

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
