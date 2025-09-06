package hydrozoa.multisig.actors.pure

import cats.*
import cats.implicits.*
import cats.effect.{Deferred, IO, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.SupervisorStrategy.Escalate
import com.suprnation.actor.{OneForOneStrategy, SupervisionStrategy}
import hydrozoa.multisig.backend.cardano.pure.CardanoBackendRef
import hydrozoa.multisig.persistence.pure.PersistenceActorRef

import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

/**
 * Multisig regime manager starts-up and monitors all the actors of the multisig regime.
 */
object MultisigRegimeManager {
    def create(peerId: PeerId,
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
    ) extends Actor[IO, MultisigRegimeManagerReq]{

    override def supervisorStrategy: SupervisionStrategy[IO] =
        OneForOneStrategy[IO](maxNrOfRetries = 3, withinTimeRange = 1 minute) {
            case _: IllegalArgumentException => Escalate // Normally `Stop` but we can't handle stopped actors yet
            case _: RuntimeException         => Escalate // Normally `Restart` but our actors can't do that yet
            case _: Exception                => Escalate
        }

    override def preStart: IO[Unit] =
        for {
            pendingCardanoBackend <- Deferred[IO, CardanoBackendRef]
            pendingPersistence <- Deferred[IO, PersistenceActorRef]
            pendingBlockActor <- Deferred[IO, BlockActorRef]
            pendingLocalCommActors <- Deferred[IO, List[CommActorRef]]
            pendingCardanoEventActor <- Deferred[IO, CardanoEventActorRef]
            pendingLedgerEventActor <- Deferred[IO, LedgerEventActorRef]

            _ <- cardanoBackend.get.flatMap(pendingCardanoBackend.complete)
            _ <- persistence.get.flatMap(pendingPersistence.complete)

            blockActor <- context.actorOf(BlockActor.create(
                BlockActor.Config(peerId),
                BlockActor.ConnectionsPending(
                    cardanoEventActor = pendingCardanoEventActor,
                    commActors = pendingLocalCommActors,
                    ledgerEventActor = pendingLedgerEventActor,
                    persistence = pendingPersistence
                )
            ))

            localCommActorsPendingRemoteActors <- peers.filterNot(_ == peerId).traverse(pid =>
                for {
                    pendingRemoteCommActor <- Deferred[IO, CommActorRef]
                    localCommActor <- context.actorOf(CommActor.create(
                        CommActor.Config(peerId, pid),
                        CommActor.ConnectionsPending(
                            blockActor = pendingBlockActor,
                            persistence = pendingPersistence,
                            remoteCommActor = pendingRemoteCommActor
                        )
                    ))
                } yield (localCommActor, pendingRemoteCommActor)
            )

            localCommActors = localCommActorsPendingRemoteActors.map(_._1)

            cardanoEventActor <- context.actorOf(CardanoEventActor.create(
                CardanoEventActor.Config(),
                CardanoEventActor.ConnectionsPending(
                    cardanoBackend = pendingCardanoBackend,
                    persistence = pendingPersistence
                )
            ))

            ledgerEventActor <- context.actorOf(LedgerEventActor.create(
                LedgerEventActor.Config(peerId),
                LedgerEventActor.ConnectionsPending(
                    blockActor = pendingBlockActor,
                    commActors = pendingLocalCommActors,
                    persistence = pendingPersistence
                )
            ))

            _ <- pendingBlockActor.complete(blockActor)
            _ <- pendingLocalCommActors.complete(localCommActors)
            _ <- pendingCardanoEventActor.complete(cardanoEventActor)
            _ <- pendingLedgerEventActor.complete(ledgerEventActor)

            _ <- context.watch(blockActor, TerminatedBlockActor(blockActor))
            _ <- localCommActors.traverse(r => context.watch(r, TerminatedCommActor(r)))
            _ <- context.watch(cardanoEventActor, TerminatedCardanoEventActor(cardanoEventActor))
            _ <- context.watch(ledgerEventActor, TerminatedLedgerEventActor(ledgerEventActor))

            // TODO: Store the deferred remote comm actor refs (cas._2) for later
        } yield ()
        
    override def receive: Receive[IO, MultisigRegimeManagerReq] =
        PartialFunction.fromFunction({
            case TerminatedBlockActor(_) =>
                IO.println("Terminated block actor")
            case TerminatedCardanoEventActor(_) =>
                IO.println("Terminated Cardano event actor")
            case TerminatedCommActor(_) =>
                IO.println("Terminated comm actor")
            case TerminatedLedgerEventActor(_) =>
                IO.println("Terminated ledger event actor")
            case TerminatedCardanoBackend(_) =>
                IO.println("Terminated cardano backend")
            case TerminatedPersistenceActor(_) =>
                IO.println("Terminated persistence")
            // TODO: Implement a way to receive a remote comm actor and connect it to its corresponding local comm actor
        })
        
}
