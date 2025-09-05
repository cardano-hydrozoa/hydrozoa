package hydrozoa.multisig.actors.pure

import cats.effect.{Deferred, IO, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.typelevel.actors.syntax.BroadcastSyntax.*
import hydrozoa.multisig.persistence.pure.PutNewLedgerEvent

/**
 * Event actor is the source of new L1 deposits and L2 transactions for the head.
 */
object LedgerEventActor {
    def create(peerId: PeerId,
               bla0: Deferred[IO, BlockActorRef],
               cas0: Deferred[IO, List[CommActorRef]],
               per0: PersistenceRef
              ): IO[LedgerEventActor] =
      for {
          bla <- Ref.of[IO, Option[BlockActorRef]](None)
          cas <- Ref.of[IO, Option[List[CommActorRef]]](None)
          per <- Ref.of[IO, Option[PersistenceRef]](Some(per0))
          nEvent <- Ref.of[IO, LedgerEventNum](0)
      } yield LedgerEventActor(peerId)(bla0, cas0)(bla, cas, per, nEvent)
}

final case class LedgerEventActor(peerId: PeerId)(
    private val blockActor0: Deferred[IO, BlockActorRef],
    private val commActors0: Deferred[IO, List[CommActorRef]],
    ) (
    private val blockActor: Ref[IO, Option[BlockActorRef]],
    private val commActors: Ref[IO, Option[List[CommActorRef]]],
    private val persistence: Ref[IO, Option[PersistenceRef]],
    private val nEvent: Ref[IO, LedgerEventNum]
    ) extends Actor[IO, LedgerEventActorReq]{
    override def preStart: IO[Unit] =
        for {
            bla <- blockActor0.get; _ <- blockActor.set(Some(bla))
            cas <- commActors0.get; _ <- commActors.set(Some(cas))
        } yield ()
    
    override def receive: Receive[IO, LedgerEventActorReq] =
        PartialFunction.fromFunction({
            case x: SubmitLedgerEvent =>
                for {
                    newNum <- nEvent.updateAndGet(x => x + 1)
                    t <- IO.monotonic
                    newId = (peerId, newNum)
                    newEvent = NewLedgerEvent(newId, t, x.event)
                    _ <- persistence.get.flatMap(_.map(_ ? PutNewLedgerEvent(newId, newEvent)).getOrElse(IO.pure(())))
                    _ <- blockActor.get.flatMap(_.map(_ ! newEvent).getOrElse(IO.pure(())))
                    _ <- commActors.get.flatMap(_.map(y => (y ! newEvent).parallel).getOrElse(IO.pure(())))
                } yield ()
            case x: ConfirmBlock => ???
        })
}
