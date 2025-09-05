package hydrozoa.multisig.actors.pure

import cats.effect.{Deferred, IO, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}

/**
 * Communication actor is connected to its counterpart at another peer:
 *
 *   - Requests communication batches from the counterpart.
 *   - Responds to the counterpart's requests for communication batches.
 */
object CommActor {
    def create(peerId: PeerId,
               remotePeerId: PeerId,
               bla0: Deferred[IO, BlockActorRef],
               per0: PersistenceRef
              ): IO[CommActor] =
      for {
          bla <- Ref.of[IO, Option[BlockActorRef]](None)
          per <- Ref.of[IO, Option[PersistenceRef]](Some(per0))
          rca <- Ref.of[IO, Option[CommActorRef]](None)
      } yield CommActor(peerId, remotePeerId)(bla0)(bla, per, rca)
}

final case class CommActor(peerId: PeerId, remotePeerId: PeerId)(
    private val blockActor0: Deferred[IO, BlockActorRef],
    ) (
    private val blockActor: Ref[IO, Option[BlockActorRef]],
    private val persistence: Ref[IO, Option[PersistenceRef]],
    private val remoteCommActor: Ref[IO, Option[CommActorRef]]
    ) extends Actor[IO, CommActorReq]{
    override def preStart: IO[Unit] =
        for {
            bla <- blockActor0.get; _ <- blockActor.set(Some(bla))
        } yield ()
    
    override def receive: Receive[IO, CommActorReq] =
        PartialFunction.fromFunction({
            case x: NewLedgerEvent => ???
            case x: NewBlock => ???
            case x: AckBlock => ???
            case x: GetMsgBatch => ???
            case x: NewMsgBatch => ???
            case ConnectRemoteCommActor(rca) => connectRemoteActor(rca)
                
        })
        
    private def connectRemoteActor(rca: CommActorRef): IO[Unit] =
        remoteCommActor.set(Some(rca))
}
