package hydrozoa.multisig.actors.pure

import cats.effect.{Deferred, IO, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}

/**
 * Block actor:
 *
 *   - When leader, receives L1 deposits + L2 txs and packages them into a new block.
 *   - When follower, receives L2 blocks and broadcasts L2 block acks for valid blocks.
 *   - When leader or follower, collects L2 block acks to confirm block effects and trigger leader/follower switch.
 */
object BlockActor {
    def create(peerId: PeerId,
               cea0: Deferred[IO, CardanoEventActorRef],
               cas0: Deferred[IO, List[CommActorRef]],
               lea0: Deferred[IO, LedgerEventActorRef],
               per0: PersistenceRef
              ): IO[BlockActor] = {
        for {
            cea <- Ref.of[IO, Option[CardanoEventActorRef]](None)
            cas <- Ref.of[IO, Option[List[CommActorRef]]](None)
            lea <- Ref.of[IO, Option[LedgerEventActorRef]](None)
            per <- Ref.of[IO, Option[PersistenceRef]](Some(per0))
        } yield BlockActor(peerId)(cea0, cas0, lea0)(cea, cas, lea, per)
    }
}

final case class BlockActor(peerId: PeerId)(
    private val cardanoEventActor0: Deferred[IO, CardanoEventActorRef],
    private val commActors0: Deferred[IO, List[CommActorRef]],
    private val ledgerEventActor0: Deferred[IO, LedgerEventActorRef],
    )(
    private val cardanoEventActor: Ref[IO, Option[CardanoEventActorRef]],
    private val commActors: Ref[IO, Option[List[CommActorRef]]],
    private val ledgerEventActor: Ref[IO, Option[LedgerEventActorRef]],
    private val persistence: Ref[IO, Option[PersistenceRef]]
    ) extends Actor[IO, BlockActorReq]{
    override def preStart: IO[Unit] =
        for {
            cea <- cardanoEventActor0.get; _ <- cardanoEventActor.set(Some(cea))
            cas <- commActors0.get; _ <- commActors.set(Some(cas))
            lea <- ledgerEventActor0.get; _ <- ledgerEventActor.set(Some(lea))
        } yield ()
        
    override def receive: Receive[IO, BlockActorReq] =
        PartialFunction.fromFunction({
            case x: NewLedgerEvent => ???
            case x: NewBlock => ???
            case x: AckBlock => ???
        })
}
