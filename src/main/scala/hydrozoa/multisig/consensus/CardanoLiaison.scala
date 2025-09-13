package hydrozoa.multisig.consensus

import cats.effect.Deferred
import cats.effect.IO
import cats.effect.Ref
import cats.implicits.*
import com.suprnation.actor.Actor.Actor
import com.suprnation.actor.Actor.Receive
import CardanoLiaison.{Config, ConnectionsPending}
import hydrozoa.multisig.protocol.CardanoBackendProtocol.*
import hydrozoa.multisig.protocol.ConsensusProtocol.*
import hydrozoa.multisig.protocol.PersistenceProtocol.*
import hydrozoa.multisig.protocol.ConsensusProtocol.CardanoLiaison.*

/** Cardano actor:
  *
  *   - Keeps track of confirmed L1 effects of L2 blocks.
  *   - Periodically polls the Cardano blockchain for the head's utxo state.
  *   - Submits whichever L1 effects are not yet reflected in the Cardano blockchain.
  */
object CardanoLiaison {
    final case class Config()

    final case class ConnectionsPending(
        cardanoBackend: Deferred[IO, CardanoBackend.Ref],
        persistence: Deferred[IO, Persistence.Ref]
    )

    def create(config: Config, connections: ConnectionsPending): IO[CardanoLiaison] = {
        IO(CardanoLiaison(config, connections))
    }
}

final class CardanoLiaison private (config: Config, private val connections: ConnectionsPending)
    extends Actor[IO, Request] {
    private val subscribers = Ref.unsafe[IO, Option[Subscribers]](None)
    private val state = State()

    private final case class Subscribers(
        cardanoBackend: CardanoBackend.Ref,
        persistence: Persistence.Ref
    )

    override def preStart: IO[Unit] =
        for {
            cardanoBackend <- connections.cardanoBackend.get
            persistence <- connections.persistence.get
            _ <- subscribers.set(
              Some(
                Subscribers(
                  cardanoBackend = cardanoBackend,
                  persistence = persistence
                )
              )
            )
        } yield ()

    override def receive: Receive[IO, Request] =
        PartialFunction.fromFunction(req =>
            subscribers.get.flatMap({
                case Some(subs) =>
                    this.receiveTotal(req, subs)
                case _ =>
                    Error(
                      "Impossible: Cardano event actor is receiving before its preStart provided subscribers."
                    ).raiseError
            })
        )

    private def receiveTotal(req: Request, subs: Subscribers): IO[Unit] =
        req match {
            case x: ConfirmBlock =>
                ???
        }

    private final class State
}
