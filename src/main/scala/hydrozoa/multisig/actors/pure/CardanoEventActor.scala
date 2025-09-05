package hydrozoa.multisig.actors.pure

import cats.effect.{Deferred, IO, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}

/**
 * Cardano actor:
 *
 *   - Keeps track of confirmed L1 effects of L2 blocks.
 *   - Periodically polls the Cardano blockchain for the head's utxo state.
 *   - Submits whichever L1 effects are not yet reflected in the Cardano blockchain.
 */
object CardanoEventActor {
    def create(peerId: PeerId,
               cba0: CardanoBackendRef,
               per0: PersistenceRef
              ): IO[CardanoEventActor] = {
        for {
            cba <- Ref.of[IO, Option[CardanoBackendRef]](Some(cba0))
            per <- Ref.of[IO, Option[PersistenceRef]](Some(per0))
        } yield CardanoEventActor()(cba, per)
    }
}

final case class CardanoEventActor() (
    private val cardanoBackend: Ref[IO, Option[CardanoBackendRef]],
    private val persistence: Ref[IO, Option[PersistenceRef]]
    ) extends Actor[IO, CardanoEventActorReq]{
    override def receive: Receive[IO, CardanoEventActorReq] =
        PartialFunction.fromFunction({
            case x: ConfirmBlock => ???
        })
}
