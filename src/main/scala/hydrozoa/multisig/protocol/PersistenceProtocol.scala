package hydrozoa.multisig.protocol

import cats.effect.{Deferred, IO}
import cats.syntax.all.*
import com.suprnation.actor.ReplyingActorRef
import hydrozoa.lib.actor.SyncRequest
import hydrozoa.lib.actor.SyncRequest.*
import hydrozoa.multisig.protocol.ConsensusProtocol.Persisted

object PersistenceProtocol {
    object Persistence {
        type PersistenceRef = Ref
        type Ref = ReplyingActorRef[IO, Request, Response]
        type Request =
            PersistRequest | PutL1Effects | PutCardanoHeadState | GetBlockData |
                GetConfirmedL1Effects | GetConfirmedLocalEvents

        type Response =
            PutResponse | GetBlockDataResp | GetConfirmedL1EffectsResp | GetConfirmedLocalEventsResp

        /** ==Put/write data into the persistence system== */
        final case class PersistRequest(
            data: Persisted.Request,
            override val dResponse: DeferredResponse[IO, PutResponse]
        ) extends SyncRequest[IO, PutResponse]

        object PersistRequest {
            def apply(data: Persisted.Request): IO[PersistRequest] = for {
                deferredResponse <- Deferred[IO, Either[Throwable, PutResponse]]
            } yield PersistRequest(data, deferredResponse)
        }

        /** Successfully persisted the data. */
        enum PutResponse:
            case PutSucceeded
            // case PutFailed

        /** Persist L1 effects of L2 blocks */
        final case class PutL1Effects(
            override val dResponse: DeferredResponse[IO, PutResponse]
        ) extends SyncRequest[IO, PutResponse]

        object PutL1Effects {
            def apply(): IO[PutL1Effects] = for {
                deferredResponse <- Deferred[IO, Either[Throwable, PutResponse]]
            } yield PutL1Effects(deferredResponse)
        }

        /** Persist the head's latest utxo state in Cardano */
        final case class PutCardanoHeadState(
            override val dResponse: DeferredResponse[IO, PutResponse]
        ) extends SyncRequest[IO, PutResponse]

        object PutCardanoHeadState {
            def apply(): IO[PutCardanoHeadState] = for {
                deferredResponse <- Deferred[IO, Either[Throwable, PutResponse]]
            } yield PutCardanoHeadState(deferredResponse)
        }

        /** ==Get/read data from the persistence system== */

        /** Request data referenced by a block (e.g. multi-ledger events and absorbed/rejected L1
          * deposits).
          */
        final case class GetBlockData(
            override val dResponse: DeferredResponse[IO, GetBlockDataResp]
        ) extends SyncRequest[IO, GetBlockDataResp]

        /** Response to [[GetBlockData]]. */
        final case class GetBlockDataResp(
        )

        object GetBlockData {
            def apply(): IO[GetBlockData] = for {
                deferredResponse <- Deferred[IO, Either[Throwable, GetBlockDataResp]]
            } yield GetBlockData(deferredResponse)
        }

        /** Retrieve local events referenced by a confirmed block:
          *
          *   - Event IDs for the L2 transactions and withdrawals referenced by the block.
          *   - Multi-signed deposit and post-dated refund transactions for the deposit events
          *     referenced by the block.
          */
        final case class GetConfirmedLocalEvents(
            override val dResponse: DeferredResponse[IO, GetConfirmedLocalEventsResp]
        ) extends SyncRequest[IO, GetConfirmedLocalEventsResp]

        /** Response to [[GetConfirmedLocalEvents]]. */
        final case class GetConfirmedLocalEventsResp(
        )

        object GetConfirmedLocalEvents {
            def apply(): IO[GetConfirmedLocalEvents] = for {
                deferredResponse <- Deferred[IO, Either[Throwable, GetConfirmedLocalEventsResp]]
            } yield GetConfirmedLocalEvents(deferredResponse)
        }

        /** Retrieve L1 effects of confirmed L2 blocks. */
        final case class GetConfirmedL1Effects(
            override val dResponse: DeferredResponse[IO, GetConfirmedL1EffectsResp]
        ) extends SyncRequest[IO, GetConfirmedL1EffectsResp]

        /** Response to [[GetConfirmedL1Effects]]. */
        final case class GetConfirmedL1EffectsResp(
        )

        object GetConfirmedL1Effects {
            def apply(): IO[GetConfirmedL1Effects] = for {
                deferredResponse <- Deferred[IO, Either[Throwable, GetConfirmedL1EffectsResp]]
            } yield GetConfirmedL1Effects(deferredResponse)
        }
    }
}
