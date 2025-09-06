package hydrozoa.multisig.backend.cardano.pure

import cats.implicits._
import cats.effect.IO
import com.suprnation.actor.Actor.{Actor, Receive}

/**
 * Cardano backend actor is a mock interface to the Cardano blockchain:
 *
 *   - Receives L1 effects
 *   - Responds to queries about utxo state.
 */
object CardanoBackend {
    def create(): IO[CardanoBackend] =
        CardanoBackend().pure
}

final case class CardanoBackend()
    extends Actor[IO, CardanoBackendReq]{
    override def receive: Receive[IO, CardanoBackendReq] =
        PartialFunction.fromFunction({
            case x: SubmitL1Effects => ???
            case x: GetCardanoHeadState => ???
        })
}

/**
 * Cardano backend protocol.
 * See diagram: [[https://app.excalidraw.com/s/9N3iw9j24UW/9eRJ7Dwu42X]]
 */
sealed trait CardanoBackendProtocol
sealed trait CardanoBackendReq extends CardanoBackendProtocol
sealed trait CardanoBackendResp extends CardanoBackendProtocol

/**
 * Submit L1 effects to the Cardano backend.
 * The response from the backend is ignored, so we model this as an async request in the pure model.
 */
final case class SubmitL1Effects (
    ) extends CardanoBackendReq

/** Get the head's current utxo state in Cardano. */
final case class GetCardanoHeadState(
    ) extends CardanoBackendReq

/** The head's current utxo state in Cardano, provided in response to [[GetCardanoHeadState]]. */
final case class GetCardanoHeadStateResp(
    ) extends CardanoBackendResp

