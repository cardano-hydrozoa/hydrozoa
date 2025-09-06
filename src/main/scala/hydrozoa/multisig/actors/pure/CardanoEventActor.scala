package hydrozoa.multisig.actors.pure

import cats.implicits._
import cats.effect.{Deferred, IO, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}
import hydrozoa.multisig.actors.pure

/**
 * Cardano actor:
 *
 *   - Keeps track of confirmed L1 effects of L2 blocks.
 *   - Periodically polls the Cardano blockchain for the head's utxo state.
 *   - Submits whichever L1 effects are not yet reflected in the Cardano blockchain.
 */
object CardanoEventActor {
    final case class Config()

    object State {
        def create: IO[State] =
            State().pure
    }
    final case class State()

    sealed trait Connections
    final case class ConnectionsLive(
        cardanoBackend: CardanoBackendRef,
        persistence: PersistenceRef
        ) extends Connections
    final case class ConnectionsPending(
        cardanoBackend: Deferred[IO, CardanoBackendRef],
        persistence: Deferred[IO, PersistenceRef]
        ) extends Connections

    def create(config: Config, conn0: Connections): IO[CardanoEventActor] = {
        for {
            conn <- Ref.of[IO, Connections](conn0)
            state <- State.create
        } yield CardanoEventActor(config)(conn, state)
    }
}

final case class CardanoEventActor(config: CardanoEventActor.Config) (
    private val connections: Ref[IO, CardanoEventActor.Connections],
    private val state: pure.CardanoEventActor.State
    ) extends Actor[IO, CardanoEventActorReq]{
    override def preStart: IO[Unit] =
        connections.get.flatMap({
            case x: CardanoEventActor.ConnectionsPending =>
                for {
                    cba <- x.cardanoBackend.get
                    per <- x.persistence.get
                    _ <- connections.set(CardanoEventActor.ConnectionsLive(cba, per))
                } yield ()
            case x: CardanoEventActor.ConnectionsLive =>
                ().pure
        })
    
    override def receive: Receive[IO, CardanoEventActorReq] =
        PartialFunction.fromFunction(req =>
            connections.get.flatMap({
                case conn: pure.CardanoEventActor.ConnectionsLive =>
                    this.receiveTotal(req, conn)
                case _ =>
                    Error("Impossible: Cardano event actor is receiving before its connections are live.").raiseError
            }))

    private def receiveTotal(req: CardanoEventActorReq, conn: pure.CardanoEventActor.ConnectionsLive): IO[Unit] =
        req match {
            case x: ConfirmBlock =>
                ???
        }
}
