package hydrozoa.multisig.backend.cardano

import hydrozoa.multisig.protocol.CardanoBackendProtocol.CardanoBackend._

import com.suprnation.actor.Actor.{Actor, Receive}

import cats.effect.IO

/** Cardano backend actor is a mock interface to the Cardano blockchain:
  *
  *   - Receives L1 effects
  *   - Responds to queries about utxo state.
  */
object CardanoBackend {
    def apply(): IO[CardanoBackend] =
        IO.pure(new CardanoBackend {})
}

trait CardanoBackend extends Actor[IO, Request] {
    override def receive: Receive[IO, Request] =
        PartialFunction.fromFunction({
            case x: SubmitL1Effects     => ???
            case x: GetCardanoHeadState => ???
        })
}
