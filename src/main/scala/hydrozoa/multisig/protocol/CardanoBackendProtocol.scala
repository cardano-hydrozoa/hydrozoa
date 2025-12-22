package hydrozoa.multisig.protocol

import cats.effect.IO
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.UtxoIdL1
import hydrozoa.lib.actor.{SyncRequest, SyncRequestE}
import scalus.cardano.ledger.{Slot, Transaction, TransactionHash}

object CardanoBackendProtocol {
    object CardanoBackend {
        type CardanoBackendRef = Ref
        type Ref = ActorRef[IO, Request]

        type Request = SubmitL1Effects | GetCardanoHeadState.Sync | GetTxInfo.Sync

        /** Submit L1 effects to the Cardano backend. The response from the backend is ignored, so
          * we model this as an async request in the pure model.
          */
        final case class SubmitL1Effects(txs: List[Transaction])

        /** Get the head's current utxo state in Cardano. */
        case object GetCardanoHeadState
            extends SyncRequestE[
              IO,
              GetCardanoHeadState.type,
              CardanoBackendError,
              GetCardanoHeadState.Response
            ] {
            type Sync = SyncRequest.EnvelopeE[
              IO,
              GetCardanoHeadState.type,
              CardanoBackendError,
              GetCardanoHeadState.Response
            ]

            /** The head's current utxo state in Cardano, provided in response to
              * [[GetCardanoHeadState]]. Contains the list of all utxos found at the head multisig
              * address.
              */
            final case class Response(
                utxoIds: Set[UtxoIdL1],
                currentSlot: Slot
            )

            def ?: : this.Send = SyncRequest.send(_, this)
        }

        /** Get the information about a particular transaction.
          */
        final case class GetTxInfo(txHash: TransactionHash)
            extends SyncRequestE[IO, GetTxInfo, CardanoBackendError, GetTxInfo.Response] {
            export GetTxInfo.Sync
            def ?: : this.Send = SyncRequest.send(_, this)
        }

        object GetTxInfo {
            type Sync =
                SyncRequest.EnvelopeE[IO, GetTxInfo, CardanoBackendError, GetTxInfo.Response]

            /** Response to [[GetTxInfo]].
              */
            final case class Response(
                isKnown: Boolean
                // TODO: add more fields?
            )
        }

        case class CardanoBackendError(msg: String) extends Throwable
    }
}
