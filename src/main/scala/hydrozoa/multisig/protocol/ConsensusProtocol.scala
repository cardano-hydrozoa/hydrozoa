package hydrozoa.multisig.protocol

import cats.effect.{Deferred, IO}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.multisig.protocol.types.Block.*
import hydrozoa.multisig.protocol.types.{Ack, Batch, Block, LedgerEvent}

import scala.concurrent.duration.FiniteDuration

object ConsensusProtocol {

    /** =Multisig regime protocol= */

    /** Multisig regime's protocol for actor requests and responses. See diagram:
      * [[https://app.excalidraw.com/s/9N3iw9j24UW/9eRJ7Dwu42X]]
      */
    enum Actors:
        case BlockProducer, CardanoLiaison, PeerLiaison, TransactionSequencer

    object BlockProducer {
        type BlockProducerRef = Ref
        type Ref = ActorRef[IO, Request]
        type Request =
            NewLedgerEvent | Block | AckBlock
    }

    object CardanoLiaison {
        type CardanoLiaisonRef = Ref
        type Ref = ActorRef[IO, Request]
        type Request =
            ConfirmBlock
    }

    object PeerLiaison {
        type PeerLiaisonRef = Ref
        type Ref = ActorRef[IO, Request]
        type Request =
            RemoteBroadcast.Request | GetMsgBatch | NewMsgBatch
    }

    object TransactionSequencer {
        type TransactionSequencerRef = Ref
        type Ref = ActorRef[IO, Request]
        type Request =
            SubmitLedgerEvent | ConfirmBlock
    }

    object Persisted {
        type Request =
            NewLedgerEvent | Block | AckBlock | ConfirmBlock | NewMsgBatch
    }

    object RemoteBroadcast {
        type Request =
            BlockProducer.Request
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

    /** The ledger event actor announces a new multi-ledger ledger event, timestamped and assigned a
      * LedgerEventId.
      */
    final case class NewLedgerEvent(
        id: LedgerEvent.Id,
        time: FiniteDuration,
        event: Unit // FIXME LedgerEvent
    )

    object NewLedgerEvent {
        type Subscriber = ActorRef[IO, NewLedgerEvent]
    }

    /** A peer's block actor announces its acknowledgement of an L2 block. When a peer's block actor
      * acknowledges a block and receives all other peers' acknowledgement of the block, then the
      * peer can consider the block to be confirmed by multisig consensus. Note that for major and
      * final blocks, two rounds of acknowledgements are needed to confirm.
      */
    final case class AckBlock(
                                 id: Ack.Id,
                                 time: FiniteDuration
    )

    object AckBlock {
        type Subscriber = ActorRef[IO, AckBlock]
    }

    /** L2 block confirmations (local-only signal) */
    final case class ConfirmBlock(
        id: Block.Number
    )

    object ConfirmBlock {
        type Subscriber = ActorRef[IO, ConfirmBlock]
    }

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
                                    ackNum: Ack.Number,
                                    blockNum: Block.Number,
                                    eventNum: LedgerEvent.Number
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
                                    ackNum: Ack.Number,
                                    blockNum: Block.Number,
                                    eventNum: LedgerEvent.Number,
                                    ack: Option[AckBlock],
                                    block: Option[Block],
                                    events: List[NewLedgerEvent]
    ) {
        def nextGetMsgBatch = GetMsgBatch(
          id.increment,
          ackNum,
          blockNum,
          eventNum
        )
    }
}
