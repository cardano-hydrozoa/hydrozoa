package hydrozoa.multisig.actors.pure

import cats.*
import cats.implicits.*
//import cats.effect.syntax._
import cats.effect.{Deferred, IO, Ref}

import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.actor.SupervisorStrategy.Escalate
import com.suprnation.actor.{OneForOneStrategy, SupervisionStrategy}

import hydrozoa.multisig.backend.cardano.pure.CardanoBackendReq
import hydrozoa.multisig.persistence.pure.PersistenceReq

import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

type BlockActorRef = ActorRef[IO, BlockActorReq]
type CardanoEventActorRef = ActorRef[IO, CardanoEventActorReq]
type CommActorRef = ActorRef[IO, CommActorReq]
type LedgerEventActorRef = ActorRef[IO, LedgerEventActorReq]

type CardanoBackendRef = ActorRef[IO, CardanoBackendReq]
type PersistenceRef = ActorRef[IO, PersistenceReq]

/**
 * Multisig regime manager starts-up and monitors all the actors of the multisig regime.
 */
object MultisigRegimeManager {
    def create(peerId: PeerId,
               peers: List[PeerId],
               cba: CardanoBackendRef,
               per: PersistenceRef
              ): IO[MultisigRegimeManager] =
        for {
            cardanoBackend <- Ref[IO].of(cba)
            persistence <- Ref[IO].of(per)
        } yield MultisigRegimeManager(peerId, peers)(cardanoBackend, persistence)
}

final case class MultisigRegimeManager(peerId: PeerId, peers: List[PeerId])(
    private val cardanoBackend: Ref[IO, CardanoBackendRef],
    private val persistence: Ref[IO, PersistenceRef]
    ) extends Actor[IO, MultisigRegimeManagerReq]{

    override def supervisorStrategy: SupervisionStrategy[IO] =
        OneForOneStrategy[IO](maxNrOfRetries = 3, withinTimeRange = 1 minute) {
            case _: IllegalArgumentException => Escalate // Normally `Stop` but we can't handle stopped actors yet
            case _: RuntimeException         => Escalate // Normally `Restart` but our actors can't do that yet
            case _: Exception                => Escalate
        }

    override def preStart: IO[Unit] =
        for {
            cba0 <- Deferred[IO, CardanoBackendRef]
            per0 <- Deferred[IO, PersistenceRef]
            bla0 <- Deferred[IO, BlockActorRef]
            cas0 <- Deferred[IO, List[CommActorRef]]
            cea0 <- Deferred[IO, CardanoEventActorRef]
            lea0 <- Deferred[IO, LedgerEventActorRef]

            _ <- cardanoBackend.get.flatMap(cba0.complete)
            _ <- persistence.get.flatMap(per0.complete)

            bla <- context.actorOf(BlockActor.create(
                BlockActor.Config(peerId),
                BlockActor.ConnectionsPending(cea0, cas0, lea0, per0)
            ))
            cas <- peers.filterNot(_ == peerId).traverse(pid =>
                for {
                    rca0 <- Deferred[IO, CommActorRef]
                    ca <- context.actorOf(CommActor.create(
                        CommActor.Config(peerId, pid),
                        CommActor.ConnectionsPending(bla0, per0, rca0)
                    ))
                } yield (ca, rca0)
            )
            cea <- context.actorOf(CardanoEventActor.create(
                CardanoEventActor.Config(),
                CardanoEventActor.ConnectionsPending(cba0, per0)
            ))
            lea <- context.actorOf(LedgerEventActor.create(
                LedgerEventActor.Config(peerId),
                LedgerEventActor.ConnectionsPending(bla0, cas0, per0)
            ))
            
            _ <- bla0.complete(bla)
            _ <- cas0.complete(cas.map(_._1))
            _ <- cea0.complete(cea)
            _ <- lea0.complete(lea)
            
            _ <- context.watch(bla, TerminatedBlockActor(bla))
            _ <- cas.traverse(r => context.watch(r._1, TerminatedCommActor(r._1)))
            _ <- context.watch(cea, TerminatedCardanoEventActor(cea))
            _ <- context.watch(lea, TerminatedLedgerEventActor(lea))

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
