package hydrozoa.multisig.actors.pure

import cats.effect.IO
import com.suprnation.actor.Actor.{Actor, Receive}

/**
 * Cardano actor:
 *
 *   - Keeps track of confirmed L1 effects of L2 blocks.
 *   - Periodically polls the Cardano blockchain for the head's utxo state.
 *   - Submits whichever L1 effects are not yet reflected in the Cardano blockchain.
 */
object CardanoEventActor {
    def create(peerId: PeerId): IO[CardanoEventActor] =
        IO.pure(CardanoEventActor())
}

case class CardanoEventActor()
    extends Actor[IO, CardanoActorReq]{
    override def receive: Receive[IO, CardanoActorReq] =
        PartialFunction.fromFunction({
            case x: ConfirmBlock => ???
        })
}
