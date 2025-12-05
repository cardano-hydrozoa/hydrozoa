package hydrozoa.multisig.protocol

import cats.effect.{Deferred, IO}
import com.suprnation.actor.ReplyingActorRef
import hydrozoa.UtxoIdL1
import hydrozoa.lib.actor.SyncRequest
import scalus.cardano.ledger.{Slot, Transaction, TransactionHash}

object CardanoBackendProtocol {
    object CardanoBackend {
        type CardanoBackendRef = Ref
        type Ref = ReplyingActorRef[IO, Request, Response]

        type Request = SubmitL1Effects | GetCardanoHeadState | GetTxInfo

        type Response = GetCardanoHeadStateResp | GetTxInfoResp

        /** Submit L1 effects to the Cardano backend. The response from the backend is ignored, so
          * we model this as an async request in the pure model.
          */
        final case class SubmitL1Effects(
            txs: List[Transaction]
        )

        /** Get the head's current utxo state in Cardano. */
        // TODO: make object?
        final case class GetCardanoHeadState(
        ) extends SyncRequest[IO, CardanoBackendError.type, GetCardanoHeadStateResp] {
            override def dResponse
                : Deferred[IO, Either[CardanoBackendError.type, GetCardanoHeadStateResp]] = ???
        }

        /** The head's current utxo state in Cardano, provided in response to
          * [[GetCardanoHeadState]]. Contains the list of all utxos found at the head multisig
          * address.
          */
        final case class GetCardanoHeadStateResp(
            utxoIds: Seq[UtxoIdL1],
            currentSlot: Slot
        )

        /** Get the information about a particular transaction.
          */
        final case class GetTxInfo(
            txHash: TransactionHash
        ) extends SyncRequest[IO, CardanoBackendError.type, GetTxInfoResp] {
            override def dResponse: Deferred[IO, Either[CardanoBackendError.type, GetTxInfoResp]] =
                ???
        }

        /** Response to [[GetTxInfo]].
          */
        final case class GetTxInfoResp(
            isKnown: Boolean
            // TODO: add more fields?
        )

        object CardanoBackendError extends Throwable
    }
}
