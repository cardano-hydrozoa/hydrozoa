package hydrozoa.multisig.actors

import cats._
import cats.effect.Deferred
import cats.effect.IO
import cats.effect.Ref
import cats.implicits._
import com.suprnation.actor.Actor.Actor
import com.suprnation.actor.Actor.Receive
import com.suprnation.actor.OneForOneStrategy
import com.suprnation.actor.SupervisionStrategy
import com.suprnation.actor.SupervisorStrategy.Escalate
import hydrozoa.multisig.backend.cardano.CardanoBackendRef
import hydrozoa.multisig.persistence.PersistenceActorRef

import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

/** Multisig regime manager starts-up and monitors all the actors of the multisig regime.
  */
object MultisigRegimeManager {
    def create(
        peerId: PeerId,
        peers: List[PeerId],
        cba: CardanoBackendRef,
        per: PersistenceActorRef
    ): IO[MultisigRegimeManager] =
        for {
            cardanoBackend <- Ref[IO].of(cba)
            persistence <- Ref[IO].of(per)
        } yield MultisigRegimeManager(peerId, peers)(cardanoBackend, persistence)
}

final case class MultisigRegimeManager(peerId: PeerId, peers: List[PeerId])(
    private val cardanoBackend: Ref[IO, CardanoBackendRef],
    private val persistence: Ref[IO, PersistenceActorRef]
) extends Actor[IO, MultisigRegimeManagerReq] {

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
            pendingCardanoBackend <- Deferred[IO, CardanoBackendRef]
            pendingPersistence <- Deferred[IO, PersistenceActorRef]
            pendingBlockProducer <- Deferred[IO, BlockProducerRef]
            pendingLocalPeerLiaisons <- Deferred[IO, List[PeerLiaisonRef]]
            pendingCardanoLiaison <- Deferred[IO, CardanoLiaisonRef]
            pendingTransactionSequencer <- Deferred[IO, TransactionSequencerRef]

            _ <- cardanoBackend.get.flatMap(pendingCardanoBackend.complete)
            _ <- persistence.get.flatMap(pendingPersistence.complete)

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
                        pendingRemotePeerLiaison <- Deferred[IO, PeerLiaisonRef]
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

            _ <- context.watch(blockProducer, TerminatedBlockProducer(blockProducer))
            _ <- localPeerLiaisons.traverse(r => context.watch(r, TerminatedPeerLiaison(r)))
            _ <- context.watch(cardanoLiaison, TerminatedCardanoLiaison(cardanoLiaison))
            _ <- context.watch(
              transactionSequencer,
              TerminatedTransactionSequencer(transactionSequencer)
            )

            // TODO: Store the deferred remote comm actor refs (cas._2) for later
        } yield ()

    override def receive: Receive[IO, MultisigRegimeManagerReq] =
        PartialFunction.fromFunction({
            case TerminatedBlockProducer(_) =>
                IO.println("Terminated block actor")
            case TerminatedCardanoLiaison(_) =>
                IO.println("Terminated Cardano event actor")
            case TerminatedPeerLiaison(_) =>
                IO.println("Terminated comm actor")
            case TerminatedTransactionSequencer(_) =>
                IO.println("Terminated ledger event actor")
            case TerminatedCardanoBackend(_) =>
                IO.println("Terminated cardano backend")
            case TerminatedPersistenceActor(_) =>
                IO.println("Terminated persistence")
            // TODO: Implement a way to receive a remote comm actor and connect it to its corresponding local comm actor
        })

}
