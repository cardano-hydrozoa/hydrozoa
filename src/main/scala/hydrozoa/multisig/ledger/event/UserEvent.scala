package hydrozoa.multisig.ledger.event

import cats.syntax.all.*
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import scalus.cardano.ledger.{Coin, Value}

/** This is just a wrapper that adds event id. */
sealed trait UserEvent {
    def eventId: LedgerEventId

    final transparent inline def eventNum: LedgerEventNumber = eventId.eventNum
    final transparent inline def peerNum: HeadPeerNumber = eventId.peerNum
}

object UserEvent {

    final case class L2Event(
        override val eventId: LedgerEventId,
        l2Payload: Array[Byte]
    ) extends UserEvent

    // TODO: factor out a true request type - depositTxBytes + refundTxBytes + virtualOutputsBytes + depositFee
    // TODO: See also: EventSequencer.DepositRequest
    final case class DepositEvent(
        override val eventId: LedgerEventId,
        depositTxBytes: Array[Byte],
        refundTxBytes: Array[Byte],
        l2Payload: Array[Byte],
        l2Value: Value,
        // TODO: explain the name, previously was known as donationToTreasury
        depositFee: Coin,
    ) extends UserEvent

}
