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
            cba0 <- cardanoBackend.get
            per0 <- persistence.get
            
            bla0 <- Deferred[IO, BlockActorRef]
            cas0 <- Deferred[IO, List[CommActorRef]]
            cea0 <- Deferred[IO, CardanoEventActorRef]
            lea0 <- Deferred[IO, LedgerEventActorRef]
            
            bla <- context.actorOf(BlockActor.create(peerId, cea0, cas0, lea0, per0))
            cas <- peers.filterNot(_ == peerId)
                .traverse(pid => context.actorOf(CommActor.create(peerId, pid, bla0, per0)))
            cea <- context.actorOf(CardanoEventActor.create(peerId, cba0, per0))
            lea <- context.actorOf(LedgerEventActor.create(peerId, bla0, cas0, per0))
            
            _ <- bla0.complete(bla)
            _ <- cas0.complete(cas)
            _ <- cea0.complete(cea)
            _ <- lea0.complete(lea)
            
            _ <- context.watch(bla, TerminatedBlockActor(bla))
            _ <- cas.traverse(r => context.watch(r, TerminatedCommActor(r)))
            _ <- context.watch(cea, TerminatedCardanoEventActor(cea))
            _ <- context.watch(lea, TerminatedLedgerEventActor(lea))
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
        })
        
}
