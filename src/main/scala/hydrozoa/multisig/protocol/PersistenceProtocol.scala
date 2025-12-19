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
            SyncRequest[IO, Throwable, PersistRequest, PutResponse] |
                SyncRequest[IO, Throwable, PutL1Effects.type, PutResponse] |
                SyncRequest[IO, Throwable, PutCardanoHeadState.type, PutResponse] |
                SyncRequest[IO, Throwable, GetBlockData.type, GetBlockDataResp] |
                SyncRequest[IO, Throwable, GetConfirmedL1Effects.type, GetConfirmedL1EffectsResp] |
                SyncRequest[
                  IO,
                  Throwable,
                  GetConfirmedLocalEvents.type,
                  GetConfirmedLocalEventsResp
                ]

        type Response =
            PutResponse | GetBlockDataResp | GetConfirmedL1EffectsResp | GetConfirmedLocalEventsResp

        /** ==Put/write data into the persistence system== */
        final case class PersistRequest(data: Persisted.Request)
            extends SyncRequest.Send[IO, Throwable, PersistRequest, PutResponse]

        /** Successfully persisted the data. */
        enum PutResponse:
            case PutSucceeded
            // case PutFailed

        /** Persist L1 effects of L2 blocks */
        case object PutL1Effects
            extends SyncRequest.Send[IO, Throwable, PutL1Effects.type, PutResponse]

        /** Persist the head's latest utxo state in Cardano */
        case object PutCardanoHeadState
            extends SyncRequest.Send[IO, Throwable, PutCardanoHeadState.type, PutResponse]

        /** ==Get/read data from the persistence system== */

        /** Request data referenced by a block (e.g. multi-ledger events and absorbed/rejected L1
          * deposits).
          */
        case object GetBlockData
            extends SyncRequest.Send[IO, Throwable, GetBlockData.type, GetBlockDataResp]

        /** Response to [[GetBlockData]]. */
        final case class GetBlockDataResp(
        )

        /** Retrieve local events referenced by a confirmed block:
          *
          *   - Event IDs for the L2 transactions and withdrawals referenced by the block.
          *   - Multi-signed deposit and post-dated refund transactions for the deposit events
          *     referenced by the block.
          */
        case object GetConfirmedLocalEvents
            extends SyncRequest.Send[
              IO,
              Throwable,
              GetConfirmedLocalEvents.type,
              GetConfirmedLocalEventsResp
            ]

        /** Response to [[GetConfirmedLocalEvents]]. */
        final case class GetConfirmedLocalEventsResp(
        )

        /** Retrieve L1 effects of confirmed L2 blocks. */
        case object GetConfirmedL1Effects
            extends SyncRequest.Send[
              IO,
              Throwable,
              GetConfirmedL1Effects.type,
              GetConfirmedL1EffectsResp
            ]

        /** Response to [[GetConfirmedL1Effects]]. */
        final case class GetConfirmedL1EffectsResp(
        )
    }
}
