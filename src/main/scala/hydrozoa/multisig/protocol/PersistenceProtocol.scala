package hydrozoa.multisig.protocol

import cats.effect.IO
import cats.syntax.all.*
import com.suprnation.actor.ReplyingActorRef
import hydrozoa.lib.actor.SyncRequest
import hydrozoa.multisig.protocol.ConsensusProtocol.Persisted

object PersistenceProtocol {
    object Persistence {
        type PersistenceRef = Ref
        type Ref = ReplyingActorRef[IO, Request, Response]
        type Request =
            SyncRequest[IO, PersistRequest, PutResponse] |
                SyncRequest[IO, PutL1Effects.type, PutResponse] |
                SyncRequest[IO, PutCardanoHeadState.type, PutResponse] |
                SyncRequest[IO, GetBlockData.type, GetBlockDataResp] |
                SyncRequest[IO, GetConfirmedL1Effects.type, GetConfirmedL1EffectsResp] |
                SyncRequest[IO, GetConfirmedLocalEvents.type, GetConfirmedLocalEventsResp]

        type Response =
            PutResponse | GetBlockDataResp | GetConfirmedL1EffectsResp | GetConfirmedLocalEventsResp

        /** ==Put/write data into the persistence system== */
        final case class PersistRequest(data: Persisted.Request) {
            def ?: : SyncRequest.Send[IO, PersistRequest, PutResponse] =
                SyncRequest.send(_, this)
        }

        /** Successfully persisted the data. */
        enum PutResponse:
            case PutSucceeded
            // case PutFailed

        /** Persist L1 effects of L2 blocks */
        case object PutL1Effects {
            def ?: : SyncRequest.Send[IO, PutL1Effects.type, PutResponse] =
                SyncRequest.send(_, this)
        }

        /** Persist the head's latest utxo state in Cardano */
        case object PutCardanoHeadState {
            def ?: : SyncRequest.Send[IO, PutCardanoHeadState.type, PutResponse] =
                SyncRequest.send(_, this)
        }

        /** ==Get/read data from the persistence system== */

        /** Request data referenced by a block (e.g. multi-ledger events and absorbed/rejected L1
          * deposits).
          */
        case object GetBlockData {
            def ?: : SyncRequest.Send[IO, GetBlockData.type, GetBlockDataResp] =
                SyncRequest.send(_, this)
        }

        /** Response to [[GetBlockData]]. */
        final case class GetBlockDataResp(
        )

        /** Retrieve L1 effects of confirmed L2 blocks. */
        case object GetConfirmedL1Effects {
            def ?: : SyncRequest.Send[IO, GetConfirmedL1Effects.type, GetConfirmedL1EffectsResp] =
                SyncRequest.send(_, this)
        }

        /** Response to [[GetConfirmedL1Effects]]. */
        final case class GetConfirmedL1EffectsResp(
        )

        /** Retrieve local events referenced by a confirmed block:
          *
          *   - Event IDs for the L2 transactions and withdrawals referenced by the block.
          *   - Multi-signed deposit and post-dated refund transactions for the deposit events
          *     referenced by the block.
          */
        case object GetConfirmedLocalEvents {
            def ?:
                : SyncRequest.Send[IO, GetConfirmedLocalEvents.type, GetConfirmedLocalEventsResp] =
                SyncRequest.send(_, this)
        }

        /** Response to [[GetConfirmedLocalEvents]]. */
        final case class GetConfirmedLocalEventsResp(
        )
    }
}
