package hydrozoa.multisig.protocol

import cats.effect.IO
import cats.syntax.all.*
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.lib.actor.SyncRequest
import hydrozoa.multisig.protocol.ConsensusProtocol.Persisted

object PersistenceProtocol {
    object Persistence {
        type PersistenceRef = Ref
        type Ref = ActorRef[IO, Request]
        type Request =
            PersistRequest.Sync | PutL1Effects.Sync | PutCardanoHeadState.Sync | GetBlockData.Sync |
                GetConfirmedL1Effects.Sync | GetConfirmedLocalEvents.Sync

        type Response =
            PutResponse | GetBlockDataResp | GetConfirmedL1EffectsResp | GetConfirmedLocalEventsResp

        /** ==Put/write data into the persistence system== */
        final case class PersistRequest(data: Persisted.Request)
            extends SyncRequest[IO, PersistRequest, PutResponse] {
            export PersistRequest.Sync
            def ?: : this.Send = SyncRequest.send(_, this)
        }

        object PersistRequest {
            type Sync = SyncRequest.Envelope[IO, PersistRequest, PutResponse]
        }

        /** Successfully persisted the data. */
        enum PutResponse:
            case PutSucceeded
            // case PutFailed

        /** Persist L1 effects of L2 blocks */
        case object PutL1Effects extends SyncRequest[IO, PutL1Effects.type, PutResponse] {
            type Sync = SyncRequest.Envelope[IO, PutL1Effects.type, PutResponse]
            def ?: : this.Send =
                SyncRequest.send(_, this)
        }

        /** Persist the head's latest utxo state in Cardano */
        case object PutCardanoHeadState
            extends SyncRequest[IO, PutCardanoHeadState.type, PutResponse] {
            type Sync = SyncRequest.Envelope[IO, PutCardanoHeadState.type, PutResponse]
            def ?: : this.Send = SyncRequest.send(_, this)
        }

        /** ==Get/read data from the persistence system== */

        /** Request data referenced by a block (e.g. multi-ledger events and absorbed/rejected L1
          * deposits).
          */
        case object GetBlockData extends SyncRequest[IO, GetBlockData.type, GetBlockDataResp] {
            type Sync = SyncRequest.Envelope[IO, GetBlockData.type, GetBlockDataResp]
            def ?: : this.Send = SyncRequest.send(_, this)
        }

        /** Response to [[GetBlockData]]. */
        final case class GetBlockDataResp(
        )

        /** Retrieve L1 effects of confirmed L2 blocks. */
        case object GetConfirmedL1Effects
            extends SyncRequest[IO, GetConfirmedL1Effects.type, GetConfirmedL1EffectsResp] {
            type Sync =
                SyncRequest.Envelope[IO, GetConfirmedL1Effects.type, GetConfirmedL1EffectsResp]
            def ?: : this.Send = SyncRequest.send(_, this)
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
        case object GetConfirmedLocalEvents
            extends SyncRequest[IO, GetConfirmedLocalEvents.type, GetConfirmedLocalEventsResp] {
            type Sync =
                SyncRequest.Envelope[IO, GetConfirmedLocalEvents.type, GetConfirmedLocalEventsResp]
            def ?: : this.Send = SyncRequest.send(_, this)
        }

        /** Response to [[GetConfirmedLocalEvents]]. */
        final case class GetConfirmedLocalEventsResp(
        )
    }
}
