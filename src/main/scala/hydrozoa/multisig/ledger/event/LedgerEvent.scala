package hydrozoa.multisig.ledger.event

import cats.syntax.all.*
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import scalus.cardano.ledger.{Coin, Value}

/** This is just a wrapper that adds event id. */
sealed trait LedgerEvent {
    def eventId: LedgerEventId

    final transparent inline def eventNum: LedgerEventNumber = eventId.eventNum
    final transparent inline def peerNum: HeadPeerNumber = eventId.peerNum
}

object LedgerEvent {

    final case class L2TxEvent(
        override val eventId: LedgerEventId,
        tx: Array[Byte]
    ) extends LedgerEvent

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
    ) extends LedgerEvent

}
