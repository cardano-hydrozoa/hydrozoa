package hydrozoa.multisig.actors.pure

import cats.implicits.*
import cats.effect.{Deferred, IO, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}
import hydrozoa.multisig.persistence.pure.PersistenceActorRef

/**
 * Block actor:
 *
 *   - When leader, receives L1 deposits + L2 txs and packages them into a new block.
 *   - When follower, receives L2 blocks and broadcasts L2 block acks for valid blocks.
 *   - When leader or follower, collects L2 block acks to confirm block effects and trigger leader/follower switch.
 */
object BlockActor {
    final case class Config(peerId: PeerId)

    object State {
        def create: IO[State] =
            for {
                nBlock <- Ref.of[IO, BlockNum](0)
            } yield State(nBlock)
    }
    final case class State(nBlock: Ref[IO, BlockNum])

    final case class ConnectionsPending(
        cardanoEventActor: Deferred[IO, CardanoEventActorRef],
        commActors: Deferred[IO, List[CommActorRef]],
        ledgerEventActor: Deferred[IO, LedgerEventActorRef],
        persistence: Deferred[IO, PersistenceActorRef],
    )

    final case class Subscribers(
        ackBlock: List[AckBlockSubscriber],
        newBlock: List[NewBlockSubscriber],
        confirmBlock: List[ConfirmBlockSubscriber],
        persistence: PersistenceActorRef,
    )

    def create(config: Config, connections: ConnectionsPending): IO[BlockActor] = {
        for {
            subscribers <- Ref.of[IO, Option[Subscribers]](None)
            state <- State.create
        } yield BlockActor(config)(connections)(subscribers, state)
    }
}

// Not sure why this is needed, but otherwise Scala doesn't allow the companion object's nested classes
// to be used directly in the case class, and it also wrongly says that Subscribers can be private.
import BlockActor.{Config, State, ConnectionsPending, Subscribers}

final case class BlockActor(config: Config)(
    private val connections: ConnectionsPending
    )(
    private val subscribers: Ref[IO, Option[Subscribers]],
    private val state: State
    ) extends Actor[IO, BlockActorReq]{
    override def preStart: IO[Unit] =
        for {
            cardanoEventActor <- connections.cardanoEventActor.get
            commActors <- connections.commActors.get
            ledgerEventActor <- connections.ledgerEventActor.get
            persistence <- connections.persistence.get
            _ <- subscribers.set(Some(Subscribers(
                ackBlock = commActors,
                newBlock = commActors,
                confirmBlock = List(cardanoEventActor, ledgerEventActor),
                persistence = persistence
            )))
        } yield ()
        
    override def receive: Receive[IO, BlockActorReq] =
        PartialFunction.fromFunction(req =>
            subscribers.get.flatMap({
                case Some(subs) =>
                    this.receiveTotal(req, subs)
                case _ =>
                    Error("Impossible: Block actor is receiving before its connections are live.").raiseError
            }))

    private def receiveTotal(req: BlockActorReq, subs: Subscribers): IO[Unit] =
        req match {
            case x: NewLedgerEvent =>
                ???
            case x: NewBlock =>
                ???
            case x: AckBlock =>
                ???
        }
}
