package hydrozoa.multisig.consensus

import hydrozoa.multisig.consensus.PeerLiaison.Batch
import hydrozoa.multisig.consensus.ack.{HardAckNumber, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.ledger.stack.StackNumber

/** Typed events emitted by [[PeerLiaison]]. Pure data; formatters in [[PeerLiaisonEventFormat]]
  * decide how each variant is rendered to a particular sink.
  */
sealed trait PeerLiaisonEvent

object PeerLiaisonEvent:

    final case class Started(remotePeerNum: HeadPeerNumber) extends PeerLiaisonEvent

    final case class ResendTick(
        batchNum: Batch.Number,
        ackNum: SoftAckNumber,
        blockNum: BlockNumber,
        stackNum: StackNumber,
        hardAckNum: HardAckNumber,
        requestNum: RequestNumber
    ) extends PeerLiaisonEvent

    final case class OutboxRequest(peerNum: HeadPeerNumber, requestNum: RequestNumber)
        extends PeerLiaisonEvent

    final case class OutboxSoftAck(blockNum: BlockNumber, peerNum: HeadPeerNumber)
        extends PeerLiaisonEvent

    final case class OutboxBlock(blockNum: BlockNumber) extends PeerLiaisonEvent

    final case class OutboxStackBrief(stackNum: StackNumber) extends PeerLiaisonEvent

    /** `round` is the string form of the hard-ack's round field. */
    final case class OutboxHardAck(stackNum: StackNumber, peerNum: HeadPeerNumber, round: String)
        extends PeerLiaisonEvent

    final case class GetMsgBatchReceived(
        batchNum: Batch.Number,
        ackNum: SoftAckNumber,
        blockNum: BlockNumber,
        stackNum: StackNumber,
        hardAckNum: HardAckNumber,
        requestNum: RequestNumber
    ) extends PeerLiaisonEvent

    /** Emitted when a [[PeerLiaison.Request.NewMsgBatch]] is sent to the remote peer.
      *
      * `hardAck` is the string form of `stackNum/round`; `requestIds` is the rendered list.
      */
    final case class NewMsgBatchSent(
        batchNum: Batch.Number,
        ackNum: Option[SoftAckNumber],
        blockNum: Option[BlockNumber],
        stackNum: Option[StackNumber],
        hardAck: Option[String],
        requestIds: List[RequestId]
    ) extends PeerLiaisonEvent

    final case class NewMsgBatchReceived(
        batchNum: Batch.Number,
        ackNum: Option[SoftAckNumber],
        blockNum: Option[BlockNumber],
        stackNum: Option[StackNumber],
        hardAck: Option[String],
        requestCount: Int
    ) extends PeerLiaisonEvent

    /** Emitted when a verified [[PeerLiaison.Request.NewMsgBatch]] is accepted and the outstanding
      * [[PeerLiaison.Request.GetMsgBatch]] cursor advances. Also doubles as a Mermaid line.
      */
    final case class BatchAdvanced(
        batchNum: Batch.Number,
        ackNum: SoftAckNumber,
        blockNum: BlockNumber,
        stackNum: StackNumber,
        hardAckNum: HardAckNumber,
        requestNum: RequestNumber
    ) extends PeerLiaisonEvent

    final case class BatchRejected(batchNum: Batch.Number, reason: String) extends PeerLiaisonEvent

    final case class BlockConfirmedNonFinal(blockNum: BlockNumber) extends PeerLiaisonEvent

    final case class BlockConfirmedFinal(blockNum: BlockNumber) extends PeerLiaisonEvent
