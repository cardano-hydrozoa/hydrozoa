package hydrozoa.multisig.protocol

import com.suprnation.actor.ReplyingActorRef

import cats.effect.IO

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
        )

        /** Get the head's current utxo state in Cardano. */
        final case class GetCardanoHeadState(
        )

        /** The head's current utxo state in Cardano, provided in response to
          * [[GetCardanoHeadState]].
          */
        final case class GetCardanoHeadStateResp(
        )

    }
}
