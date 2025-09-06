package hydrozoa.multisig.actors.pure

import cats.implicits._
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
    final case class Config(peerId: PeerId)

    object State {
        def create: IO[State] =
            for {
                nBlock <- Ref.of[IO, BlockNum](0)
            } yield State(nBlock)
    }
    final case class State(nBlock: Ref[IO, BlockNum])

    sealed trait Connections
    final case class ConnectionsLive(
        cardanoEventActor: CardanoEventActorRef,
        commActors: List[CommActorRef],
        ledgerEventActor: LedgerEventActorRef,
        persistence: PersistenceRef,
        ) extends Connections
    final case class ConnectionsPending(
        cardanoEventActor: Deferred[IO, CardanoEventActorRef],
        commActors: Deferred[IO, List[CommActorRef]],
        ledgerEventActor: Deferred[IO, LedgerEventActorRef],
        persistence: Deferred[IO, PersistenceRef],
        ) extends Connections

    def create(config: Config, conn0: Connections): IO[BlockActor] = {
        for {
            conn <- Ref.of[IO, Connections](conn0)
            state <- State.create
        } yield BlockActor(config)(conn, state)
    }
}

final case class BlockActor(config: BlockActor.Config)(
    private val connections: Ref[IO, BlockActor.Connections],
    private val state: BlockActor.State
    ) extends Actor[IO, BlockActorReq]{
    override def preStart: IO[Unit] =
        connections.get.flatMap({
            case x: BlockActor.ConnectionsPending =>
                for {
                    cea <- x.cardanoEventActor.get
                    cas <- x.commActors.get
                    lea <- x.ledgerEventActor.get
                    per <- x.persistence.get
                    _ <- connections.set(BlockActor.ConnectionsLive(cea, cas, lea, per))
                } yield ()
            case x: BlockActor.ConnectionsLive =>
                ().pure
        })
        
    override def receive: Receive[IO, BlockActorReq] =
        PartialFunction.fromFunction(req =>
            connections.get.flatMap({
                case conn: BlockActor.ConnectionsLive =>
                    this.receiveTotal(req, conn)
                case _ =>
                    Error("Impossible: Block actor is receiving before its connections are live.").raiseError
            }))

    private def receiveTotal(req: BlockActorReq, conn: BlockActor.ConnectionsLive): IO[Unit] =
        req match {
            case x: NewLedgerEvent =>
                ???
            case x: NewBlock =>
                ???
            case x: AckBlock =>
                ???
        }
}
