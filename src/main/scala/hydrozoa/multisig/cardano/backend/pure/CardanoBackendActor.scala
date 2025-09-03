package hydrozoa.multisig.cardano.backend.pure

import cats.effect.IO
import com.suprnation.actor.Actor.{Actor, Receive}

/**
 * Cardano blockchain actor is a mock interface to the Cardano blockchain:
 *
 *   - Receives L1 effects
 *   - Responds to queries about utxo state.
 */
object CardanoBackendActor {
    def create(): IO[CardanoBackendActor] =
        IO.pure(CardanoBackendActor())
}

case class CardanoBackendActor()
    extends Actor[IO, CardanoBackendActorReq]{
    override def receive: Receive[IO, CardanoBackendActorReq] =
        PartialFunction.fromFunction({
            case x: SubmitL1Effects => ???
            case x: GetCardanoHeadState => ???
        })
}

/**
 * Cardano backend protocol.
 * See diagram: [[https://app.excalidraw.com/s/9N3iw9j24UW/9eRJ7Dwu42X]]
 */
sealed trait CardanoBackendActorProtocol
sealed trait CardanoBackendActorReq extends CardanoBackendActorProtocol
sealed trait CardanoBackendActorResp extends CardanoBackendActorProtocol

/**
 * Submit L1 effects to the Cardano backend.
 * The response from the backend is ignored, so we model this as an async request in the pure model.
 */
case class SubmitL1Effects (
    ) extends CardanoBackendActorReq

/** Get the head's current utxo state in Cardano. */
case class GetCardanoHeadState(
    ) extends CardanoBackendActorReq

/** The head's current utxo state in Cardano, provided in response to [[GetCardanoHeadState]]. */
case class GetCardanoHeadStateResp(
    ) extends CardanoBackendActorResp

