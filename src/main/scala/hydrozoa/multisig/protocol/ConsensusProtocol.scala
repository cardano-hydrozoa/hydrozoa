package hydrozoa.multisig.protocol

import cats.effect.{Deferred, IO}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.UtxoIdL1
import hydrozoa.multisig.ledger.dapp.tx.FallbackTx
import hydrozoa.multisig.ledger.dapp.txseq.{FinalizationTxSeq, SettlementTxSeq}
import hydrozoa.multisig.protocol.types.Block.*
import hydrozoa.multisig.protocol.types.{AckBlock, Batch, Block, LedgerEvent, LedgerEventId}
import scala.concurrent.duration.FiniteDuration

object ConsensusProtocol {

    /** =Multisig regime protocol= */

    /** Multisig regime's protocol for actor requests and responses. See diagram:
      * [[https://app.excalidraw.com/s/9N3iw9j24UW/9eRJ7Dwu42X]]
      */
    enum Actors:
        case BlockWeaver, CardanoLiaison, Consensus, JointLedger, PeerLiaison, EventSequencer

    // TODO: move to the weaver sources
    object BlockWeaver {
        type BlockWeaverRef = Ref
        type Ref = ActorRef[IO, Request]
        // TODO: use Block.Next not Block here
        type Request = LedgerEvent | Block | BlockConfirmed | PollResults

        /** Block confirmation.
          *
          * @param blockNumber
          */
        final case class BlockConfirmed(
            blockNumber: Block.Number,
            finalizationRequested: Boolean = false
        )

        /** So-called "poll results" from the Cardano Liaison, i.e., a set of all utxos ids found at
          * the multisig head address.
          *
          * @param utxos
          *   all utxos found
          */
        final case class PollResults(utxos: Set[UtxoIdL1])
    }

    /** TODO: I would like to have it in the CardanoLiaison.scala and not here.
      */
    object CardanoLiaison {
        type CardanoLiaisonRef = Ref
        type Ref = ActorRef[IO, Request]

        import hydrozoa.multisig.consensus.CardanoLiaison as ThatCardanoLiaison
        type Request = ConfirmMajorBlock | ConfirmFinalBlock | ThatCardanoLiaison.Timeout.type
    }

    object PeerLiaison {
        type PeerLiaisonRef = Ref
        type Ref = ActorRef[IO, Request]
        type Request =
            RemoteBroadcast.Request | GetMsgBatch | NewMsgBatch
    }

    object EventSequencer {
        type EventSequencerRef = Ref
        type Ref = ActorRef[IO, Request]
        type Request =
            SubmitLedgerEvent | ConfirmBlock
    }

    object Persisted {
        type Request =
            LedgerEvent | Block | AckBlock | ConfirmBlock | NewMsgBatch
    }

    object RemoteBroadcast {
        type Request =
            BlockWeaver.Request
    }

    /** Submit a new ledger event to the head via a peer's ledger event actor. */
    final case class SubmitLedgerEvent(
        time: FiniteDuration,
        event: Unit, // FIXME
        deferredEventOutcome: Deferred[IO, Unit] // FIXME: LedgerEventOutcome]
    )

    /** A ledger event submission is constructed by taking a ledger event, timestamping it, and
      * creating a deferred cell where the outcome of the event can later be placed. The intention
      * is for the fiber handling the user's event submission to construct this
      * [[SubmitLedgerEvent]] and send it to the ledger event actor.
      */
    // TODO: for deposit txs, the tx time bounds and deposit utxo datum will need to be adjusted based on the timestamp.
    object SubmitLedgerEvent {
        def create(event: Unit): // FIXME
        IO[SubmitLedgerEvent] =
            for {
                time <- IO.monotonic
                eventOutcome <- Deferred[IO, Unit] // FIXME: LedgerEventOutcome]
            } yield SubmitLedgerEvent(time, event, eventOutcome)
    }

    // TODO: move to cardano liaison
    /** L2 block confirmations (local-only signal) */
    sealed trait ConfirmBlock {
        def id: Block.Number
    }

    object ConfirmBlock {
        type Subscriber = ActorRef[IO, ConfirmBlock]
    }

    final case class ConfirmMinorBlock(
        override val id: Block.Number
        // TODO: add block signatures
    ) extends ConfirmBlock

    object ConfirmMajorFinalBlock {
        type Subscriber = ActorRef[IO, ConfirmMajorBlock | ConfirmFinalBlock]
    }

    final case class ConfirmMajorBlock(
        override val id: Block.Number,
        // The settlement tx + optional rollouts
        // TODO: (with all signatures, the idea is to put signatures right into `Transaction`s)
        settlementTxSeq: SettlementTxSeq,
        // The fallback tx
        // TODO: (with all signatures, the idea is to put signatures right into `Transaction`s)
        fallbackTx: FallbackTx
    ) extends ConfirmBlock

    /** TODO: this is a message that the cardano liaison expects to see once a final block gets
      * confirmed.
      */
    final case class ConfirmFinalBlock(
        override val id: Block.Number,
        // The finalization tx + optional rollout txs
        // TODO: (with all signatures, the idea is to put signatures right into `Transaction`s)
        finalizationTxSeq: FinalizationTxSeq,
    ) extends ConfirmBlock

    /** Request by a comm actor to its remote comm-actor counterpart for a batch of events, blocks,
      * or block acknowledgements originating from the remote peer.
      *
      * @param id
      *   Batch number that increases by one for every consecutive batch.
      * @param ackNum
      *   The requester's last seen block acknowledgement from the remote peer.
      * @param blockNum
      *   The requester's last seen block from the remote peer.
      * @param eventNum
      *   The requester's last seen event number from the remote peer.
      */
    final case class GetMsgBatch(
        id: Batch.Id,
        ackNum: AckBlock.Number,
        blockNum: Block.Number,
        eventNum: LedgerEventId.Number
    )

    /** Comm actor provides a batch in response to its remote comm-actor counterpart's request.
      *
      * @param id
      *   Batch number matching the one from the request.
      * @param ackNum
      *   The latest acknowledgement number originating from the responder.
      * @param blockNum
      *   The latest block number originating from the responder.
      * @param eventNum
      *   The latest event number originating from the responder.
      * @param ack
      *   If provided, a block acknowledgment originating from the responder after the requested
      *   [[AckNum]].
      * @param block
      *   If provided, a block originating from the responder after the requested [[Number]].
      * @param events
      *   A possibly empty list of events originating from the responder after the requested
      *   [[LedgerEventNum]].
      */
    final case class NewMsgBatch(
        id: Batch.Id,
        ackNum: AckBlock.Number,
        blockNum: Block.Number,
        eventNum: LedgerEventId.Number,
        ack: Option[AckBlock],
        block: Option[Block],
        events: List[LedgerEvent]
    ) {
        def nextGetMsgBatch = GetMsgBatch(
          id.increment,
          ackNum,
          blockNum,
          eventNum
        )
    }
}
