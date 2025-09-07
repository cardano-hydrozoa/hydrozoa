package hydrozoa.multisig.actors.pure

import cats.implicits.*
import cats.effect.{Deferred, IO, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}
import hydrozoa.multisig.actors.pure
import hydrozoa.multisig.backend.cardano.pure.CardanoBackendRef
import hydrozoa.multisig.persistence.pure.PersistenceActorRef

// Not sure why this is needed, but otherwise Scala doesn't allow the companion object's nested classes
// to be used directly in the case class, and it also wrongly says that Subscribers can be private.
import CardanoEventActor.{Config, State, ConnectionsPending, Subscribers}

final case class CardanoEventActor(config: Config)(
    private val connections: ConnectionsPending
    ) (
    private val subscribers: Ref[IO, Option[Subscribers]],
    private val state: State
    ) extends Actor[IO, CardanoEventActorReq]{
    override def preStart: IO[Unit] =
        for {
            cardanoBackend <- connections.cardanoBackend.get
            persistence <- connections.persistence.get
            _ <- subscribers.set(Some(Subscribers(
                cardanoBackend = cardanoBackend,
                persistence = persistence
            )))
        } yield ()
    
    override def receive: Receive[IO, CardanoEventActorReq] =
        PartialFunction.fromFunction(req =>
            subscribers.get.flatMap({
                case Some(subs) =>
                    this.receiveTotal(req, subs)
                case _ =>
                    Error("Impossible: Cardano event actor is receiving before its preStart provided subscribers.").raiseError
            }))

    private def receiveTotal(req: CardanoEventActorReq, subs: Subscribers): IO[Unit] =
        req match {
            case x: ConfirmBlock =>
                ???
        }
}

/**
 * Cardano actor:
 *
 *   - Keeps track of confirmed L1 effects of L2 blocks.
 *   - Periodically polls the Cardano blockchain for the head's utxo state.
 *   - Submits whichever L1 effects are not yet reflected in the Cardano blockchain.
 */
object CardanoEventActor {
    final case class Config()

    final case class ConnectionsPending(
        cardanoBackend: Deferred[IO, CardanoBackendRef],
        persistence: Deferred[IO, PersistenceActorRef]
        )

    final case class Subscribers(
        cardanoBackend: CardanoBackendRef,
        persistence: PersistenceActorRef
        )

    def create(config: Config, connections: ConnectionsPending): IO[CardanoEventActor] = {
        for {
            subscribers <- Ref.of[IO, Option[Subscribers]](None)
            state <- State.create
        } yield CardanoEventActor(config)(connections)(subscribers, state)
    }

    final case class State()

    object State {
        def create: IO[State] =
            State().pure
    }
}
