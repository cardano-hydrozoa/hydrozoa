package hydrozoa.multisig.protocol

import cats.effect.{Deferred, IO}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.UtxoIdL1
import hydrozoa.lib.actor.SyncRequest
import scalus.cardano.ledger.{Slot, Transaction, TransactionHash}

object CardanoBackendProtocol {
    object CardanoBackend {
        type CardanoBackendRef = Ref
        type Ref = ActorRef[IO, Request]

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
            override val dResponse: Deferred[
              IO,
              Either[CardanoBackendError, GetCardanoHeadStateResp]
            ]
        ) extends SyncRequest[IO, CardanoBackendError, GetCardanoHeadStateResp]

        object GetCardanoHeadState:
            def apply(): IO[GetCardanoHeadState] = for {
                dResponse <- Deferred[IO, Either[CardanoBackendError, GetCardanoHeadStateResp]]
            } yield GetCardanoHeadState(dResponse)

        /** The head's current utxo state in Cardano, provided in response to
          * [[GetCardanoHeadState]]. Contains the list of all utxos found at the head multisig
          * address.
          */
        final case class GetCardanoHeadStateResp(
            utxoIds: Set[UtxoIdL1],
            currentSlot: Slot
        )

        /** Get the information about a particular transaction.
          */
        final case class GetTxInfo(
            txHash: TransactionHash,
            override val dResponse: Deferred[IO, Either[CardanoBackendError, GetTxInfoResp]]
        ) extends SyncRequest[IO, CardanoBackendError, GetTxInfoResp]

        object GetTxInfo:
            def apply(txHash: TransactionHash): IO[GetTxInfo] =
                for {
                    dResponse <- Deferred[IO, Either[CardanoBackendError, GetTxInfoResp]]
                } yield GetTxInfo(txHash, dResponse)

        /** Response to [[GetTxInfo]].
          */
        final case class GetTxInfoResp(
            isKnown: Boolean
            // TODO: add more fields?
        )

        case class CardanoBackendError(msg: String) extends Throwable
    }
}
