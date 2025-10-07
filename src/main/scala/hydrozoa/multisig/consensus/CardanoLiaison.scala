package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import hydrozoa.multisig.protocol.CardanoBackendProtocol.*
import hydrozoa.multisig.protocol.ConsensusProtocol.*
import hydrozoa.multisig.protocol.ConsensusProtocol.CardanoLiaison.*
import hydrozoa.multisig.protocol.PersistenceProtocol.*

import CardanoLiaison.{Config, ConnectionsPending}

/** Cardano actor:
  *
  *   - Keeps track of confirmed L1 effects of L2 blocks.
  *   - Periodically polls the Cardano blockchain for the head's utxo state.
  *   - Submits whichever L1 effects are not yet reflected in the Cardano blockchain.
  */
object CardanoLiaison {
    final case class Config(
        cardanoBackend: CardanoBackend.Ref,
        persistence: Persistence.Ref
    )

    final case class ConnectionsPending()

    def apply(config: Config, connections: ConnectionsPending): IO[CardanoLiaison] = {
        IO(new CardanoLiaison(config, connections) {})
    }
}

trait CardanoLiaison(config: Config, connections: ConnectionsPending) extends Actor[IO, Request] {
    private val subscribers = Ref.unsafe[IO, Option[Subscribers]](None)
    State()

    private final case class Subscribers()

    override def preStart: IO[Unit] =
        for {
            _ <- subscribers.set(
              Some(
                Subscribers(
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
