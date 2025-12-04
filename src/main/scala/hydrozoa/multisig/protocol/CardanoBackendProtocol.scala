package hydrozoa.multisig.protocol

import cats.effect.IO
import com.suprnation.actor.ReplyingActorRef
import hydrozoa.UtxoIdL1
import scalus.cardano.ledger.{Slot, Transaction}

object CardanoBackendProtocol {
    object CardanoBackend {
        type CardanoBackendRef = Ref
        type Ref = ReplyingActorRef[IO, Request, Response]

        type Request = SubmitL1Effects | GetCardanoHeadState

        type Response = GetCardanoHeadStateResp

        /** Submit L1 effects to the Cardano backend. The response from the backend is ignored, so
          * we model this as an async request in the pure model.
          */
        final case class SubmitL1Effects(
            txs: List[Transaction]
        )

        /** Get the head's current utxo state in Cardano. */
        final case class GetCardanoHeadState(
        )

        /** The head's current utxo state in Cardano, provided in response to
          * [[GetCardanoHeadState]]. Contains the list of all utxos found at the head multisig
          * address.
          */
        final case class GetCardanoHeadStateResp(
            utxoIds: Seq[UtxoIdL1],
            currentSlot: Slot
        )
    }
}
