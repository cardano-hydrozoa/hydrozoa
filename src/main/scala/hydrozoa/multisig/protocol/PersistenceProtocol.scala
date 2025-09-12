package hydrozoa.multisig.protocol

import cats.effect.IO
import com.suprnation.actor.ReplyingActorRef
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
            data: Persisted.Request
        )

        /** Successfully persisted the data. */
        enum PutResponse:
            case PutSucceeded
            // case PutFailed

        /** Persist L1 effects of L2 blocks */
        final case class PutL1Effects(
        )

        /** Persist the head's latest utxo state in Cardano */
        final case class PutCardanoHeadState(
        )

        /** ==Get/read data from the persistence system== */

        /** Request data referenced by a block (e.g. multi-ledger events and absorbed/rejected L1
          * deposits).
          */
        final case class GetBlockData(
        )

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
        )

        /** Response to [[GetConfirmedLocalEvents]]. */
        final case class GetConfirmedLocalEventsResp(
        )

        /** Retrieve L1 effects of confirmed L2 blocks. */
        final case class GetConfirmedL1Effects(
        )

        /** Response to [[GetConfirmedL1Effects]]. */
        final case class GetConfirmedL1EffectsResp(
        )
    }
}
