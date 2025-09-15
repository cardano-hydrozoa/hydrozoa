package hydrozoa.multisig.backend.cardano

import cats.effect.IO
import cats.implicits.*
import com.suprnation.actor.Actor.Actor
import com.suprnation.actor.Actor.Receive
import hydrozoa.multisig.protocol.CardanoBackendProtocol.CardanoBackend.*

/** Cardano backend actor is a mock interface to the Cardano blockchain:
  *
  *   - Receives L1 effects
  *   - Responds to queries about utxo state.
  */
object CardanoBackend {
    def create(): IO[CardanoBackend] =
        IO.pure(new CardanoBackend {})
}

trait CardanoBackend extends Actor[IO, Request] {
    override def receive: Receive[IO, Request] =
        PartialFunction.fromFunction({
            case x: SubmitL1Effects     => ???
            case x: GetCardanoHeadState => ???
        })
}
