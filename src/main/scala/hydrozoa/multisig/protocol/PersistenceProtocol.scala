package hydrozoa.multisig.protocol

import cats.effect.IO
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.lib.handle.Handle.RequestSync
import hydrozoa.lib.handle.actor.ActorHandle.ActorRequestSync
import hydrozoa.multisig.protocol.ConsensusProtocol.Persisted

object PersistenceProtocol {
    object Persistence {
        type PersistenceRef = Ref
        type Ref = ActorRef[IO, Request]
        type Request =
            ActorRequestSync[IO, PersistRequest] |
            ActorRequestSync[IO, PutL1Effects] |
            ActorRequestSync[IO, PutCardanoHeadState] |
            ActorRequestSync[IO, GetBlockData] |
            ActorRequestSync[IO, GetConfirmedL1Effects] |
            ActorRequestSync[IO, GetConfirmedLocalEvents]

        /** ==Put/write data into the persistence system== */
        final case class PersistRequest(
            data: Persisted.Request
        ) extends RequestSync {
            override type Response = PutResponse
        }

        /** Successfully persisted the data. */
        enum PutResponse:
            case PutSucceeded
            case PutFailed

        /** Persist L1 effects of L2 blocks */
        final case class PutL1Effects(
        ) extends RequestSync {
            override type Response = PutResponse
        }

        /** Persist the head's latest utxo state in Cardano */
        final case class PutCardanoHeadState(
        ) extends RequestSync {
            override type Response = PutResponse
        }

        /** ==Get/read data from the persistence system== */

        /** Request data referenced by a block (e.g. multi-ledger events and absorbed/rejected L1
          * deposits).
          */
        final case class GetBlockData(
        ) extends RequestSync {
            override type Response = GetBlockDataResp
        }

        /** Response to [[GetBlockData]]. */
        final case class GetBlockDataResp(
        )

        /** Retrieve local events referenced by a confirmed block:
          *
          *   - Event IDs for the L2 transactions and withdrawals referenced by the block.
          *   - Multi-signed deposit and post-dated refund transactions for the deposit events
          *     referenced by the block.
          */
        final case class GetConfirmedLocalEvents(
        ) extends RequestSync {
            override type Response = GetConfirmedLocalEventsResp
        }

        /** Response to [[GetConfirmedLocalEvents]]. */
        final case class GetConfirmedLocalEventsResp(
        )

        /** Retrieve L1 effects of confirmed L2 blocks. */
        final case class GetConfirmedL1Effects(
        ) extends RequestSync {
            override type Response = GetConfirmedL1EffectsResp
        }

        /** Response to [[GetConfirmedL1Effects]]. */
        final case class GetConfirmedL1EffectsResp(
        )
    }
}
