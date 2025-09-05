package hydrozoa.multisig.actors.pure

import cats.effect.{Deferred, IO, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}

import scala.collection.immutable.Queue

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
          qEvent <- Ref.of[IO, Queue[NewLedgerEvent]](Queue())
          qBlock <- Ref.of[IO, Queue[NewBlock]](Queue())
          qAck <- Ref.of[IO, Queue[AckBlock]](Queue())
          mDeferredMsgBatch <- Ref.of[IO, Option[Deferred[IO, NewMsgBatch]]](None)
          nBatch <- Ref.of[IO, BatchNum](0)
      } yield CommActor(peerId, remotePeerId)(bla0)(bla, per, rca, qEvent, qBlock, qAck, mDeferredMsgBatch, nBatch)
}

final case class CommActor(peerId: PeerId, remotePeerId: PeerId)(
    private val blockActor0: Deferred[IO, BlockActorRef],
    ) (
    private val blockActor: Ref[IO, Option[BlockActorRef]],
    private val persistence: Ref[IO, Option[PersistenceRef]],
    private val remoteCommActor: Ref[IO, Option[CommActorRef]],
    private val qEvent: Ref[IO, Queue[NewLedgerEvent]],
    private val qBlock: Ref[IO, Queue[NewBlock]],
    private val qAck: Ref[IO, Queue[AckBlock]],
    private val mDeferredMsgBatch: Ref[IO, Option[Deferred[IO, NewMsgBatch]]],
    private val nBatch: Ref[IO, BatchNum]
    ) extends Actor[IO, CommActorReq]{
    override def preStart: IO[Unit] =
        for {
            bla <- blockActor0.get; _ <- blockActor.set(Some(bla))
        } yield ()
    
    override def receive: Receive[IO, CommActorReq] =
        PartialFunction.fromFunction({
            case x: NewLedgerEvent =>
                for {
                    mdmb <- mDeferredMsgBatch.get
                    _ <- mdmb match {
                        case Some(y) =>
                            for {
                                rca <- remoteCommActor.get
                                _ <- rca match {
                                    case Some(z) =>
                                        for {
                                            batchNum <- nBatch.updateAndGet(a => a + 1)
                                            _ <- z ! NewMsgBatch(batchNum, None, None, Queue(x))
                                        } yield ()
                                    case None =>
                                        qEvent.update(y => y :+ x)
                                }
                            } yield ()
                        case None =>
                            qEvent.update(y => y :+ x)
                    }
                } yield ()
            case x: NewBlock => ???
                for {
                    _ <- qBlock.update(y => y :+ x)
                } yield ()
            case x: AckBlock => ???
                for {
                    _ <- qAck.update(y => y :+ x)
                } yield ()
            case x: GetMsgBatch => ???
            case x: NewMsgBatch => ???
            case ConnectRemoteCommActor(rca) => connectRemoteActor(rca)
                
        })
        
    private def connectRemoteActor(rca: CommActorRef): IO[Unit] =
        remoteCommActor.set(Some(rca))
}
